# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/12/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Main script for cleaning, statistical analysis, and viz
# ---------------------------------------------------------------
### ADMIN ###
# Sets repo path
path='/Users/vickihurd/Documents/GitHub/UltrasoundSkillRetention'
setwd(path)
# Source library folder
files.sources = list.files('./lib/')
files.sources = paste('./lib/', files.sources, sep = "")
sapply(files.sources, source)
# Read in all pertinent libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(lme4)
library(car)
library(emmeans)

# ---------------------------------------------------------------
# CLEANING SCRIPT
# Reads in all datafiles. Outputs dfB, dfK, and dfSurveys
dfB = cleanBladder()
dfK = cleanKidney()
dfSurveys = cleanSurveys()

# ---------------------------------------------------------------
# ORGAN ACQUISITION RATE ANALYSIS
orgAcqAgg(dfB)
orgAcqT1(dfB)
orgAcqT2(dfB)
orgAcqT3(dfB)

# ---------------------------------------------------------------
# IMAGE QUALITY ANALYSIS
bladderQuality(dfB)

# ---------------------------------------------------------------
# DIAGNOSTICS ANALYSIS & VIZ
diagnostics(dfB)

# ---------------------------------------------------------------
# SURVEYS ANALYSIS & VIZ

