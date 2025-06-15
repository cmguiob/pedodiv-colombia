
###############################################################################
#  BOOTSTRAP UNIVERSAL  –  arregla lo que dejó el bootstrap anterior
#  · Detecta la Miniconda manual (si existe) y la usa **antes** que r-miniconda
#  · (Re)crea el entorno rgee_py con python 3.12, earthengine-api y numpy
#  · Sobrescribe EARTHENGINE_PYTHON / EARTHENGINE_ENV en ~/.Renviron
#  · No toca ningún otro archivo; después basta reiniciar R
###############################################################################

suppressPackageStartupMessages(library(reticulate))
suppressPackageStartupMessages(library(rgee))

# --- 1 · Posibles raíces de Miniconda (ajusta si instalaste en otra carpeta)
candidate_roots <- c(
  "C:/ProgramData/miniconda3",
  file.path(Sys.getenv("USERPROFILE"), "miniconda3"),              # por usuario
  file.path(Sys.getenv("USERPROFILE"), "AppData/Local/r-miniconda")# la de reticulate
)

conda_root <- NULL
for (p in candidate_roots) {
  if (file.exists(file.path(p, "Scripts/conda.exe"))) { conda_root <- p; break }
}
if (is.null(conda_root))
  stop("No encontré Miniconda. Instálala manualmente y vuelve a correr.")

conda_bin <- file.path(conda_root, "Scripts/conda.exe")

# --- 2 · Asegurarse de que exista rgee_py en ESA Miniconda
if (!"rgee_py" %in% conda_list(conda = conda_bin)$name) {
  message("→ Creando rgee_py en ", conda_root)
  conda_create("rgee_py", packages = "python=3.12", conda = conda_bin)
}
use_condaenv("rgee_py", conda = conda_bin, required = TRUE)

# --- 3 · Instalar earthengine-api + numpy si faltan
if (!py_module_available("ee"))
  py_install(c("earthengine-api==1.5.19", "numpy"),
             envname = "rgee_py", pip = TRUE)

# --- 4 · Sobrescribir variables en ~/.Renviron (esto tapa las anteriores)
ee_install_set_pyenv(
  py_path = py_config()$python,   # ruta al python.exe correcto
  py_env  = "rgee_py"
)

message("✓ Bootstrap terminado. Reinicia R antes de usar tu script de producción.")