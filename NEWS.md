# easieRnmt 0.0.3.0.0

## Backend overhaul: conda → reticulate/uv

The Python backend has been migrated from conda to `uv`, managed transparently
by reticulate. This provides a significantly smoother installation experience —
no separate conda installation is required, and the Python environment is
provisioned automatically on first use.

### fasttext workaround (Windows)

EasyNMT declares `fasttext` as a hard dependency. The canonical `fasttext`
package on PyPI has no prebuilt wheels for Windows and fails to compile from
C++ source on MSVC. The workaround uses `UV_OVERRIDE` to redirect the
`fasttext` requirement to `fasttext-predict` — a prediction-only fork that
ships prebuilt binary wheels for Windows, macOS, and Linux across Python
3.9–3.13, and exposes the same `import fasttext` namespace. The override is
applied only on Windows and selects the correct wheel for the active Python
version by inspecting the uv Python directory on disk (avoiding any premature
Python initialisation). Language detection is handled in R, so the
prediction-only scope of `fasttext-predict` is not a limitation.

### GPU / CUDA setup

GPU detection and PyTorch wheel selection are now handled automatically via
`UV_TORCH_BACKEND=auto`. uv queries for NVIDIA (CUDA), AMD (ROCm), and Intel
GPU drivers and selects the correct PyTorch index without any manual
configuration. `onnxruntime-gpu` is still restricted to CUDA (NVIDIA) only;
AMD and Intel GPU users receive CPU onnxruntime alongside GPU-accelerated
PyTorch.

### Other changes

- `initialize_easynmt()` is now a no-op after the first successful call within
  a session.
- `source_python()` for the internal translation script is called after all
  `py_require()` declarations, ensuring the uv environment is fully provisioned
  before the Python module is loaded.
- `check_backend()` reports GPU status, PyTorch version, and cache directories.
