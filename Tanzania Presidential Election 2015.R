# loading packages -----------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(ggplot2)
library(readr)

# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_2015 <- read_csv("tanzania-election-result-2015.csv")

# Cleaning Data --------------------------------------------------------------------------------
# Renaming Variables
names(TNZ_2015)[names(TNZ_2015) == "Constituency"] <- "Electoral Constituency"
names(TNZ_2015)[names(TNZ_2015) == "X15"] <- "Regions"

# removing NO. and X16 - X34 columns
TNZ_2015 <- TNZ_2015 %>% select(-NO., -(X16:X34))