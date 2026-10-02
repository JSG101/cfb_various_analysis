# Header ----
#
# Script name: 00_00_dirs_libs_funs_vals.R
#
# Purpose of script: Determine the Directories, Libraries, Functions, and Values for XXX Project
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-09-17
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
#
#   
#

# Working Directories ----
DIR_OUTPUTS <- file.path("C:/Users/japbi/Dropbox/Data Science/00_outputs")

# Formatting and Global Options ----
conferences_p5 <- c("ACC", "Big 12", "Big Ten", "FBS Independents", "Pac-12", "SEC")
watermark_path <- data.frame(
  x = 6, y = 4, 
  image_path = "C:/Users/japbi/Dropbox/Data Science/3_proj/proj_000_practice/3_output/proj000_01_logos/logo_jgill/logo_jgill_black_white.png"
)

# Packages ----

library(tidyverse)
library(janitor)
library(cfbfastR)
library(ggpath)

# Functions ----


# Sources ----