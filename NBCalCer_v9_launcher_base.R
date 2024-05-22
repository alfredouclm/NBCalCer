rm(list=ls())

library(openxlsx) # Reading the config and saving the output files
library(stringr) # String manipulation

library(dplyr)
library(tidyr)
library(gtools)

# For maps
library(maptools)
library(rworldmap)

CURRENT_PATH <- "YOUR-PATH-TO-THIS-FILE-GOES-HERE"
setwd(CURRENT_PATH)

source("library/NBCalCer_v9_functions.R")
source("NBCalCer_v9_function.R")

CONFIG_FILE_BASE <- "NBCalCer_config_base-run.xlsx"

NBCalCer_run(CONFIG_FILE = CONFIG_FILE_BASE,
             RESULTS_FOLDER = "NBCalCer_SIMULATIONS_NAME/",
             CALCULATE_SONR = T,
             PROCESS_COMBINATIONS_WORLD_COUNTRIES = T)

print("Simulations completed")


