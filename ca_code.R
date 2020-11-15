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
library(reshape2)


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

# Changing to required data-type

txn$household_key <- as.character(txn$household_key)


# Initial Data Exploration

head(txn)

str(txn)

# Week level revenue and distinct customers, orders per week

txn %>% summarise(max(week_no), min(week_no))
wk_txn <- txn %>% group_by(week_no) %>% summarise(rev = sum(sales_value-(retail_disc+coupon_match_disc)),
                                                  cust = n_distinct(household_key),
                                                  orders = n_distinct(basket_id)) %>% arrange(week_no)
head(wk_txn)
wk_txn <- as.data.frame(wk_txn)
mdf <- melt(wk_txn, id.vars="week_no")

ggplot(mdf,aes(x=week_no,y=value,color=variable,group=variable))+geom_line()+scale_color_manual(values=c("rev"="black","cust"="red","orders"="orange"))

ggplot(wk_txn,aes(week_no, cust))+geom_line()
ggplot(wk_txn,aes(week_no, orders))+geom_line()

## Seems to have some missing data points so considering only from week 16 to week 100
## Does not seem to have any outliers in chosen period
## No missing weeks also

# Distinct customers per week

?melt()
wk_cust <- txn %>% group_by(week_no) %>% summarise(rev = sum()) %>% arrange(week_no)
head(wk_txn)
ggplot(wk_txn,aes(week_no,rev))+geom_line()




