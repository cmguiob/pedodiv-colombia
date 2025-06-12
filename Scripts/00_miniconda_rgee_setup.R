
#NOTES: Install miniconda manually before running this.

# If you intentionally switch to a different Python env for a special
#project; in that case call use_condaenv()/use_python() before any
#package touches reticulate, then revert or update
#ee_install_set_pyenv() when you’re done.

# ── ONE-TIME bootstrap for rgee_py ──────────────────────────────────────

# 1.  Detect the conda installation that holds rgee_py
# 2.  Get the python.exe path for that env
# 3.  (If the env doesn't exist yet, create it)
# 4.  Save the path so you never have to do this again

bootstrap_rgee_py <- function() {
  
  # Candidate conda roots – add more if you have them
  conda_roots <- c(
    "C:/ProgramData/miniconda3",
    file.path(Sys.getenv("USERPROFILE"), "AppData/Local/r-miniconda")
  )
  
  # Pick the first conda that exists
  conda_root <- Filter(function(p) file.exists(file.path(p, "Scripts/conda.exe")),
                       conda_roots)[1]
  stopifnot(length(conda_root) == 1)
  
  conda_bin <- file.path(conda_root, "Scripts/conda.exe")
  
  # Does rgee_py exist there?
  if (!"rgee_py" %in% reticulate::conda_list(conda = conda_bin)$name) {
    message("Creating conda env 'rgee_py' in ", conda_root)
    reticulate::conda_create("rgee_py", packages = "python=3.12",
                             conda   = conda_bin)
  }
  
  # Absolute path to python.exe for rgee_py
  py_path <- reticulate::conda_python("rgee_py", conda = conda_bin)
  
  # Make sure Earth Engine + numpy are installed (once per machine)
  reticulate::use_python(py_path, required = TRUE)
  missing <- c("earthengine-api", "numpy")[!
                                             vapply(c("ee", "numpy"),
                                                    reticulate::py_module_available, FALSE)]
  if (length(missing)) {
    reticulate::py_install(
      packages = c("earthengine-api==1.5.19", "numpy"),
      envname  = "rgee_py",
      pip      = TRUE
    )
  }
  
  # Tell rgee to remember this interpreter (adds two lines to ~/.Renviron)
  rgee::ee_install_set_pyenv(py_path = py_path, py_env = "rgee_py")
  
  message("✓ rgee_py bootstrap complete.  Restart R and use your short script.")
}

bootstrap_rgee_py()

