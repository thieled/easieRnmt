from easynmt import EasyNMT
import torch
from tqdm import tqdm
import random
import numpy as np
import pandas as pd
from datetime import datetime


# --- reproducibility setup ---
def set_seed(seed: int = 42, deterministic: bool = True):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.deterministic = deterministic
        torch.backends.cudnn.benchmark = not deterministic


# lazy model cache
_models = {}

def get_model(model_name: str = "opus-mt"):
    if model_name not in _models:
        _models[model_name] = EasyNMT(model_name)
    return _models[model_name]


def easynmt_translate(
    df,
    target_lang: str,
    model: str = "opus-mt",
    seed: int = 42,
    deterministic: bool = True,
    max_length_tl: int = 512,
    batch_size: int = 20,
    beam_size: int = 1,
    verbose: bool = True,
    check_translation: bool = False,
    n_retries: int = 3,
    check_threshold: float = 0.6
):
    """
    Safe translation wrapper for EasyNMT.
    Adds optional retry mechanism based on unique token ratio.

    Heuristic:
        unique_tokens_ratio = n_unique_trg / n_unique_src
    Retry is triggered if ratio < check_threshold.
    Edge cases:
        - If source has no tokens, ratio = Inf (never triggers).
        - If target has no tokens, ratio = 0 (always triggers).
    """

    set_seed(seed=seed, deterministic=deterministic)

    if not isinstance(df, pd.DataFrame):
        df = pd.DataFrame(df)

    if not {"row_id", "text_clean", "lang"}.issubset(df.columns):
        raise ValueError("Input must contain columns: row_id, text_clean, lang")

    texts = df["text_clean"].tolist()
    ids = df["row_id"].tolist()
    source_langs = df["lang"].tolist()

    translations, errors = [], []

    model_obj = get_model(model)

    # --- validate max_length ---
    safe_max_len = min(max_length_tl, 512)

    iterator = range(0, len(texts), batch_size)
    if verbose:
        iterator = tqdm(iterator, desc="Translating batches", unit="batch")

    for i in iterator:
        batch_texts = texts[i:i + batch_size]
        batch_ids = ids[i:i + batch_size]
        batch_langs = source_langs[i:i + batch_size]

        try:
            batch_translations = model_obj.translate(
                batch_texts,
                source_lang=batch_langs[0] if batch_langs[0] else None,
                target_lang=target_lang,
                max_length=safe_max_len,
                beam_size=beam_size,
                perform_sentence_splitting=False,
            )

            if isinstance(batch_translations, str):
                batch_translations = [batch_translations]

            translations.extend(batch_translations)
            errors.extend([None] * len(batch_texts))

        except Exception as e:
            translations.extend([None] * len(batch_texts))
            errors.extend([str(e)] * len(batch_texts))

        # cleanup only if CUDA is available
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
            torch.cuda.ipc_collect()

    out = pd.DataFrame({
        "row_id": ids,
        "text": texts,
        "lang_source": source_langs,
        "lang_target": [target_lang] * len(texts),
        "translation": translations,
        "error": errors,
        "tl_datetime": datetime.utcnow().isoformat(),
        "tl_model": model,
        "max_length_used": [safe_max_len] * len(texts),
    })

    # --- optional retry check ---
    if check_translation:
        for idx, row in out.iterrows():
            src_tokens = str(row["text"]).split()
            trg_tokens = str(row["translation"]).split() if row["translation"] else []

            n_unique_src = len(set(src_tokens))
            n_unique_trg = len(set(trg_tokens))

            if n_unique_src == 0:
                unique_ratio = float("inf")
            else:
                unique_ratio = n_unique_trg / n_unique_src

            retry_count = 0
            while unique_ratio < check_threshold and retry_count < n_retries:
                retry_count += 1
                set_seed(seed + retry_count, deterministic=False)
                try:
                    new_translation = model_obj.translate(
                        [row["text"]],
                        source_lang=row["lang_source"],
                        target_lang=target_lang,
                        max_length=safe_max_len,
                        beam_size=max(beam_size, 5),
                        perform_sentence_splitting=False,
                    )
                    if isinstance(new_translation, list):
                        new_translation = new_translation[0]

                    new_tokens = new_translation.split()
                    new_unique_trg = len(set(new_tokens))
                    unique_ratio = (
                        float("inf")
                        if n_unique_src == 0
                        else new_unique_trg / n_unique_src
                    )

                    out.at[idx, "translation"] = new_translation
                    out.at[idx, "error"] = f"Retry attempt {retry_count}"

                except Exception as e:
                    out.at[idx, "error"] = f"Retry {retry_count} failed: {e}"
                    break

    # --- sort by row_id for stability ---
    out = out.sort_values("row_id").reset_index(drop=True)

    return out
