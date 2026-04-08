############################################################
# Project: Spring Paper 20260
# Author:  Ingri Katherine Quevedo Rocha
# Created: 2026-02-23
############################################################

############################
# Environment Setup
############################
# Set Project folder as the Working Directory
setwd("..")
getwd()

# Clear workspace
rm(list = ls())

# Set options
set.seed(123)

# Libraries
pacman::p_load(dplyr, tidyverse, ggplot2, haven, ezids, arrow, scales, tidyr, knitr, kableExtra, showtext, sysfonts)
font_add("cmunsi", "C:/Users/k_the/Downloads/computer-modern/cmunss.ttf")
showtext_auto()

#######################################
# Open EAM and BRING ELASTICITIES
#######################################

### EAM ALL
df <- read_parquet("data/proc/work_EAM.parquet")
