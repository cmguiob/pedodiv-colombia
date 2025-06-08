
# ==== 1. Instalación y carga de librerías necesarias ====
if (!requireNamespace("rgee", quietly = TRUE)) install.packages("rgee")
library(rgee)

ee_install_set_pyenv(py_env = "YOUR_ENV")
ee_install_upgrade()

ee_Initialize(drive = TRUE) # Asume autenticación correcta y acceso a Google Driv
