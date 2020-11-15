getwd()

library(magrittr)
library(tidyverse)
library(data.table)
# Loading required packages
pacman::p_load(VIM,mice,Hmisc,DMwR)
# Loading required packages
library(ggplot2)
library(mltools)
library(janitor)


# Importing the files

prod <- fread("product.csv")
txn <- fread("transaction_data.csv")
cmp_d <- fread("campaign_desc.csv")
cmp <- fread("campaign_table.csv")
causal <- fread("causal_data.csv")
coupon <- fread("coupon.csv")
coup_red <- fread("coupon_redempt.csv")
demog <- fread("hh_demographic.csv")


# Cleaning column names

prod <- clean_names(prod)
txn <- clean_names(txn)
cmp_d <- clean_names(cmp_d)
cmp <- clean_names(cmp)
causal <- clean_names(causal)
coupon <- clean_names(coupon)
coup_red <- clean_names(coup_red)
demog <- clean_names(demog)


# Initial Data Exploration

head(txn)
