## Setting up rgee for Windows users

In this repository, I share the November 2025 30-day rgee challenge scripts. You can watch the accompanying videos at https://www.youtube.com/@wycology

To get set up with rgee, for Windows users, follow this carefully and restart R and even completely restart Windows whenever instructed:

install.packages("remotes") # If you do not have it in advance

remotes::install_github("r-spatial/rgee") # Install latest version of rgee

library(rgee)

ee_clean_pyenv()

ee_install(py_env = "rgee_env") # Provide preferred name for the environment

ee_install_set_pyenv(py_env = "rgee_env")

ee_Authenticate(user = "your username", drive = TRUE, earthengine = TRUE)

ee_Initialize(drive = TRUE)

Please, follow the steps carefully, and close RStudio and open it again whenever instructed.

## END
