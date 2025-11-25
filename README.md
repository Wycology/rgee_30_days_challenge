# Setting up rgee for Windows users

In this repository, I share the November 2025 30-day rgee challenge scripts. You can watch the accompanying videos at https://www.youtube.com/@wycology

To get set up with rgee, for Windows users, I swear it is not trivial like other packages like dplyr:

# Google Earth Engine Account ----------------------------

First, you should have a Google Earth Engine Account. If you don't already, log in to the https://code.earthengine.google.com/ and register for one.
It might take a day or two to be active.


# Installing miniconda -----------------------------------

rgee relies on Python in R through reticulate to interact with Google Earth Engine. So, we would like to have this well set.
We will need to create a conda environment where the GEE Python API will be located and rgee can get it.

Follow these steps:
1. Install the miniconda3 program (https://docs.conda.io/en/latest/miniconda.html) outside R. You CANNOT say install.packages("miniconda")! Go to your browser and search for the miniconda and install it.
2. On the top right, click Download, then scroll down to Miniconda Installers and pick one for Windows with Python version indicated, and click the 64-Bit Graphical Installer and download, then install.
3. Now, after installing miniconda, hit the Windows button and type Anaconda. You should see Anaconda Prompt. Click it. A scary black and white interface pops up, in it type the following one after the other:

       a. conda create -n rgee python # This creates a conda environment called rgee with the latest Python version. Click Yes whenever prompted until the environment is set and Python installed.
       b. conda activate rgee # This activates the new rgee environment
       c. pip install google-api-python-client # This installs the API that enables communication between GEE and Python.
       d. pip install earthengine-api # This installs the earthengine-api
       e. pip install numpy # Finally, you get numpy installed. Once done, congratulations!

6. Now, your conda environment is very ready to enable rgee to communicate with Google Earth Engine. Now type the following to get the path to the environment:
   conda env list # You should be able to see the created rgee environment on the left and its path to it on the right, like `C:/...../rgee/`, copy that path accurately. You will need it in RStudio.

# Inside RStudio --------------------------------------------

  a. Check the version of your R; it should be above 3.6. When writing this, the latest version was 4.5.2
    b. Have the latest version of RStudio, or even Positron.
    c. You will need to install rtools, which is a separate program that helps to build some packages from source. Check the rtools that is compatible with your R.
    d. Now, using the pacman package, run the following:
    
    pacman::p_load(rgee, geojsonio, remotes, reticulate)
   
5. Then run the following:

       a. rgee_env_dir <- "C:\\Users\\....\\anaconda3\\envs\\rgee\\" # You should replace that path with the correct one you copied from anaconda prompt. Take note of the slashes carefully.
       b. reticulate::use_python(rgee_env_dir, required = TRUE) # Here you may restart your RStudio. In case your reticulate was using another python in your machine.
       c. rgee::ee_install_set_pyenv(py_path = rgee_env_dir, py_env = "rgee") # Follow the prompts, like restarting RStudio
       d. Sys.setenv(RETICULATE_PYTHON = rgee_env_dir)
       e. Sys.setenv(EARTHENGINE_PYTHON = rgee_env_dir)

# Initialize ------------------------------------------------

Finally, run:
  
    rgee::ee_Initialize(drive = TRUE) # This will direct you to allow rgee to use your GEE credentials, which you should accept...continue, continue, copy TOKEN if prompted and paste in RStudio accordingly.

Congratulations! Peace at last.
Please follow the steps carefully, and close RStudio and open it again whenever instructed.

## END![30_day_free](https://github.com/user-attachments/assets/df8b000e-bcd4-4530-9a69-1a5c59d38770)

