In this repository I share November 2025 30 days rgee challenge scripts. You can watch the accompanying videos at https://www.youtube.com/@wycology

To get set up with rgee, for Windows users, follow this:

install.packages("rgee")
library(rgee)
ee_clean_pyenv()
ee_install(py_env = "rgee_env") # Provide preferred name for the environment
ee_install_set_pyenv(py_env = "rgee_env")
ee_Authenticate(user = "your username", drive = TRUE, earthengine = TRUE)
ee_Initialize(drive = T)

Please, follow the steps carefully, and close RStudio and open it again whenever instructed.
