### RETICULATE AND RENV SHOULD BE
library(reticulate)
library(renv)

### CHECK THAT RETICULATE IS SET UP AND WORKING
python_config <- reticulate::py_config()
python_config

### ARTEMIS HAS MINIMAL PYTHON DEPENDENCIES, BUT SHOULD YOU ENCOUNTER ISSUES, CONSIDER A PYTHON VIRTUAL ENV FOR THE PROJECT
### IDEALLY, USE PYTHON 3.12

#python_bin <- python_config$python
#virtualenv_create(envname = file.path(getwd(),"pyenv"), python = python_bin)
#use_virtualenv( file.path(getwd(),"pyenv"), required = TRUE)
#virtualenv_install(file.path(getwd(),"pyenv"), packages = c("numpy","pandas"))

### ONCE RETICULATE IS SET UP CORRECTLY, ACTIVATE RENV. CHOOSE OPTION 1: Restore the project from the lockfile.
renv::init()

