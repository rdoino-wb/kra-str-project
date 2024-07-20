# ---------------------------------------------------------------------------- #
#                   Kenyan Payment Reform: replication package                 #
#                                   World Bank - DIME                          #
# ---------------------------------------------------------------------------- #

# 0: Setting R ----

# 0.1: Prepare the workspace 
{
  
  # Clean the workspace
  rm(list = ls())
  
  # Free unused R memory
  gc()
  
  # Avoid scientific notation
  options(scipen = 9999)
  
  # Set the seed for reproducibility
  set.seed(123)
  
}

# 0.2: Loading the packages
{
  
  # List of packages needed
  packages <- c(
    "data.table",
    "readxl",
    "stringr",
    "lubridate",
    "dplyr",
    "ggplot2",
    "zoo",
    "kableExtra",
    "fixest",
    "DescTools",
    "tidyverse",
    "broom",
    "sf",
    "reshape2",
    "RStata",
    "car"
  )
  
  # Install and load pacman for package management
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(packages, character.only = TRUE, install = TRUE)
  
}

# 1: Setting Working Directories ----

# Setting path based on the user
{
  
  user <- Sys.getenv("USER")
  
  # NEED TO CHANGE THIS DIRECTORY
  if (user == "ruggerodoino") { 
    print("Ruggero has been selected")
    directory <- "/Users/ruggerodoino/Desktop/Reproducible_package_kenya"
  } else {
    stop("Unknown user. Please set the correct directory.")
  }
  
  # NEED TO CHANGE THIS DIRECTORY 
  options("RStata.StataVersion" = 15)
  options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
  
}

# Set working directories
{
 
  # Data directories
  raw_data <- file.path(directory, "Data/Raw")
  int_data <- file.path(directory, "Data/Intermediate")
  fin_data <- file.path(directory, "Data/Final")
  
  # Documentation directories
  code_doc <- file.path(directory, "Documentation/Codebooks")
  dic_doc  <- file.path(directory, "Documentation/Dictionaries")
  
  # Outputs directories
  graph_output <- file.path(directory, "Outputs/Figures")
  table_output <- file.path(directory, "Outputs/Tables")
  
  # Code directories
  code <- file.path(directory, "Code")
  function_code <- file.path(directory, "Code/Functions")
  
}

# 2: Master Script ----

# Load all the functions needed
{
  
  function_files <- list.files(function_code, full.names = TRUE)
  invisible(sapply(function_files, source, .GlobalEnv))
  
}

# 3: Run Scripts ----

{
  
  # Clean the datasets
  source(file.path(code, "1_cleaning.R"))
  
  # Create the set of descriptive graphs (no experiment yet)
  source(file.path(code, "2_graphs.R"))
  
  # Create the set of descriptive graphs (no experiment yet)
  source(file.path(code, "3_tables.R"))
  
  # Create the Pilot Sample and some Analysis
  source(file.path(code, "pilot/1_cleaning_pilot.R"))
  source(file.path(code, "pilot/2_cleaning_pilot.R"))
   stata(file.path(code, "pilot/3_pilot_randomization.do"))
  source(file.path(code, "pilot/4_pilot_sample.R"))
  
  # Randomization using STATA
  stata(file.path(code, "4_Randomization_ScaleUp.do"))
  
  # Clean and prepare the scale up dataset
  source(file.path(code, "5_cleaning_scale_up.R"))
  
  # Produce table with main results
  source(file.path(code, "7_tables_main_results.R"))
  
  # Produce the rest of the results for the experiment
  source(file.path(code, "8_other_scaleup_outputs.R"))
  
}
