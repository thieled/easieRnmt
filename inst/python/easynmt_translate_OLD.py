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
        # cudnn backend only exists if CUDA is available
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
    div_threshold: float = 0.5
):
    """
    Safe translation wrapper for EasyNMT.
    Adds optional retry mechanism if translations look collapsed.
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
        retry_rows = []
        for idx, row in out.iterrows():
            src_div = len(set(row["text"].split()))
            trg_div = len(set(str(row["translation"]).split())) if row["translation"] else 0

            if src_div > 0 and trg_div < src_div * div_threshold:
                # retry loop
                fixed = False
                for r in range(1, n_retries + 1):
                    set_seed(seed + r, deterministic=deterministic)
                    try:
                        new_translation = model_obj.translate(
                            [row["text"]],
                            source_lang=row["lang_source"],
                            target_lang=target_lang,
                            max_length=safe_max_len,
                            beam_size=beam_size,
                            perform_sentence_splitting=False,
                        )
                        if isinstance(new_translation, list):
                            new_translation = new_translation[0]

                        new_trg_div = len(set(new_translation.split()))
                        if new_trg_div >= src_div * div_threshold:
                            row["translation"] = new_translation
                            row["error"] = f"Retry No: {r}"
                            fixed = True
                            break
                    except Exception as e:
                        row["error"] = f"Retry No: {r}, error: {e}"
                retry_rows.append(row)
        if retry_rows:
            out = pd.concat([out.drop([r.name for r in retry_rows]), pd.DataFrame(retry_rows)], axis=0)

    # --- sort by row_id for stability ---
    out = out.sort_values("row_id").reset_index(drop=True)

    return out
