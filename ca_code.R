getwd()
install.packages("factoextra")
install.packages("tibble")

library(tibble)
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
library(factoextra)
library(car)
library(MASS)
library(caret)


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


# No of new customers per week

txn2 <- txn %>% group_by(household_key) %>% mutate(min_week = min(week_no))
wk_new_cust <- txn2 %>% mutate(new = week_no - min_week) %>% group_by(week_no) %>% summarise(new_cust = n_distinct(household_key[new == 0]))

## Since data has been provided for only a set of 2500 households, there are negligible new customers from week 16
## we can assume there are no new customers



# Get recency considerng the entire data (in terms of days - with observation point of the max day)

cust_rec <- txn2 %>% group_by(household_key) %>% summarise(lst_pur = max(day))
cust_rec <- cust_rec %>% mutate(rec = max(lst_pur)-lst_pur) %>% select(c(household_key,rec))

describe(cust_rec)

# Use k-means clustering to assign scores to customers based on recency

rec <- cust_rec$rec
rec<- as.data.frame(rec)

fviz_nbclust(rec, kmeans, method = "wss")

set.seed(234)
k2 <- kmeans(rec, centers = 4, nstart = 25)
k2
cust_rec2 <- data.frame(cust_rec, k2$cluster)
cust_rec3 <- cust_rec2 %>% mutate(rec_score = case_when( k2.cluster == 3 ~ 2L, k2.cluster == 2 ~ 3L, TRUE ~ k2.cluster)) %>% 
            select(c(household_key,rec, rec_score))

describe(cust_rec3)



# Get frequency considerng the entire data (in terms of count of visit days)


cust_freq <- txn2 %>% group_by(household_key) %>% summarise(freq = n_distinct(day))
describe(cust_freq)


# Use k-means clustering to assign scores to customers based on frequency

freq <- cust_freq$freq
freq <- as.data.frame(freq)

fviz_nbclust(freq, kmeans, method = "wss")

set.seed(234)
k3 <- kmeans(freq, centers = 4, nstart = 25)
k3
cust_freq2 <- data.frame(cust_freq, k3$cluster)
cust_freq3 <- cust_freq2 %>% mutate(freq_score = case_when( k3.cluster == 2 ~ 1L, k3.cluster == 1 ~ 2L, TRUE ~ k3.cluster)) %>% 
  select(c(household_key,freq, freq_score))

describe(cust_freq3)

# Get monetary value considerng the entire data (in terms of total sales per household)


cust_mon <- txn2 %>% group_by(household_key) %>% summarise(sls = sum(sales_value-(retail_disc+coupon_match_disc)))
describe(cust_mon)

# Use k-means clustering to assign scores to customers based on revenue

mon <- cust_mon$sls
mon <- as.data.frame(mon)

fviz_nbclust(mon, kmeans, method = "wss")

set.seed(234)
k4 <- kmeans(mon, centers = 4, nstart = 25)
k4
cust_mon2 <- data.frame(cust_mon, k4$cluster)
cust_mon3 <- cust_mon2 %>% mutate(mon_score = case_when( k4.cluster == 4 ~ 2L, k4.cluster == 2 ~ 4L, TRUE ~ k4.cluster)) %>% 
  select(c(household_key,sls, mon_score))

describe(cust_mon3)



# Overall score can be calculated by just adding up the individual scores


cust_rfm <- cust_freq3 %>% left_join(select(cust_rec3, rec, rec_score, household_key), by = c("household_key"="household_key")) %>% 
            left_join(select(cust_mon3,sls,mon_score, household_key), by = c("household_key"= "household_key"))
cust_rfm <- cust_rfm %>% mutate(overall_score = freq_score+rec_score+mon_score)


describe(cust_rfm)


# Creating the RFM segments only for weeks 1 to 50 for segmenting and then trying to predict their what CLTV segment they will belong to in the next 60 weeks


rfm_dat <- txn %>% filter(week_no <= 50)
ltv_dat <- txn %>% filter(week_no > 51)

rfm_dat %>% summarise(cust_cnt = n_distinct(household_key))
ltv_dat %>% summarise(cust_cnt = n_distinct(household_key))

## Of 2500 total households, there are 2497 households in the first 50 weeks and 2479 households from 52 to 102 weeks


# Gettinng rfm scores for the first 50 weeks

# No of new customers per week

txn3 <- rfm_dat %>% group_by(household_key) %>% mutate(min_week = min(week_no))

# Get recency considerng the entire data (in terms of days - with observation point of the max day)

cust_rec_50 <- rfm_dat %>% group_by(household_key) %>% summarise(lst_pur = max(day))
cust_rec_50 <- cust_rec_50 %>% mutate(rec = max(lst_pur)-lst_pur) %>% select(c(household_key,rec))



describe(cust_rec_50)

# Use k-means clustering to assign scores to customers based on recency

rec_50 <- cust_rec_50$rec
rec_50 <- as.data.frame(rec_50)

fviz_nbclust(rec_50, kmeans, method = "wss")

set.seed(234)
k2_50 <- kmeans(rec_50, centers = 4, nstart = 25)
k2_50
cust_rec2_50 <- data.frame(cust_rec_50, k2_50$cluster)
cust_rec3_50 <- cust_rec2_50 %>% mutate(rec_score = case_when( k2_50.cluster == 3 ~ 2L, k2_50.cluster == 2 ~ 4L,k2_50.cluster == 1 ~ 3L,k2_50.cluster == 4 ~ 1L, TRUE ~ k2_50.cluster)) %>% 
  select(c(household_key,rec, rec_score))

describe(cust_rec3_50)


# Get frequency considerng the entire data (in terms of count of visit days)


cust_freq_50 <- rfm_dat %>% group_by(household_key) %>% summarise(freq = n_distinct(day))
describe(cust_freq_50)


# Use k-means clustering to assign scores to customers based on frequency

freq_50 <- cust_freq_50$freq
freq_50 <- as.data.frame(freq_50)

fviz_nbclust(freq_50, kmeans, method = "wss")

set.seed(234)
k3_50 <- kmeans(freq_50, centers = 4, nstart = 25)
k3_50
cust_freq2_50 <- data.frame(cust_freq_50, k3_50$cluster)
cust_freq3_50 <- cust_freq2_50 %>% mutate(freq_score = case_when( k3_50.cluster == 4 ~ 1L, k3_50.cluster == 3 ~ 4L, k3_50.cluster == 2 ~ 3L, k3_50.cluster == 1 ~ 2L, TRUE ~ k3_50.cluster)) %>% 
  select(c(household_key,freq, freq_score))

describe(cust_freq3_50)


# Get monetary value considerng the entire data (in terms of total sales per household)


cust_mon_50 <- rfm_dat %>% group_by(household_key) %>% summarise(sls = sum(sales_value-(retail_disc+coupon_match_disc)))
describe(cust_mon_50)

# Use k-means clustering to assign scores to customers based on revenue

mon_50 <- cust_mon_50$sls
mon_50 <- as.data.frame(mon_50)

fviz_nbclust(mon_50, kmeans, method = "wss")

set.seed(234)
k4_50 <- kmeans(mon_50, centers = 4, nstart = 25)
k4_50
cust_mon2_50 <- data.frame(cust_mon_50, k4_50$cluster)
cust_mon3_50 <- cust_mon2_50 %>% mutate(mon_score = case_when( k4_50.cluster == 4 ~ 2L, k4_50.cluster == 3 ~ 4L, k4_50.cluster == 2 ~ 3L, TRUE ~ k4_50.cluster)) %>% 
  select(c(household_key,sls, mon_score))

describe(cust_mon3_50)


# Overall score can be calculated by just adding up the individual scores


cust_rfm_50 <- cust_freq3_50 %>% left_join(select(cust_rec3_50, rec, rec_score, household_key), by = c("household_key"="household_key")) %>% 
  left_join(select(cust_mon3_50,sls,mon_score, household_key), by = c("household_key"= "household_key"))
cust_rfm_50 <- cust_rfm_50 %>% mutate(overall_score = freq_score+rec_score+mon_score)


describe(cust_rfm_50)

# Assigning segments based on scores - High Val, Mid Val and Low Val

cust_rfm_50 <- cust_rfm_50 %>% mutate(segment = case_when(overall_score >= 9 ~ 'High val', overall_score >= 6 ~ 'Mid Val', TRUE ~ 'Low Val'))


# Calculating the CLTV for the weeks 52 to 102

cust_ltv <- ltv_dat %>% group_by(household_key) %>% summarise(ltv = sum(sales_value-(retail_disc+coupon_match_disc)))

# Use k-means clustering to label customers based on LTV

ltv <- cust_ltv$ltv
ltv <- as.data.frame(ltv)

fviz_nbclust(ltv, kmeans, method = "wss")

set.seed(234)
k4_50 <- kmeans(ltv, centers = 4, nstart = 25)
k4_50
cust_ltv2 <- data.frame(cust_ltv, k4_50$cluster)
cust_ltv3 <- cust_ltv2 %>% mutate(ltv_seg = case_when( k4_50.cluster == 4 ~ 'High LTV', k4_50.cluster == 1 ~ 'High LTV', k4_50.cluster == 2 ~ 'Low LTV', TRUE ~ 'Med LTV')) %>% 
  select(c(household_key,ltv, ltv_seg))

describe(cust_ltv3)


# Joining the LTV values (target variable) to the predictor variables which we have

ltv_rfm_dat <- cust_rfm_50 %>% left_join(select(cust_ltv3, ltv, ltv_seg, household_key), by = c("household_key" = "household_key"))

# Funtion for getting % NAs of cols in any table
get.na <- function(df)
{na.cnt <- sapply(df, function(y) round((sum(is.na(y))/NROW(y)),5)*100)
return(na.cnt)
}

# Check % of NAs present

get.na(ltv_rfm_dat)


# Replacing NAs with 0 (since the household wasn't present in next weeks he/she has contributed 0 value)

ltv_rfm_dat <- ltv_rfm_dat %>% mutate(ltv = if_else(is.na(ltv), 0, ltv),
                       ltv_seg = if_else(is.na(ltv_seg), 'Low LTV', ltv_seg))

# One hot encoding for the RFM segments

ltv_rfm_dat$segment <- as.factor(ltv_rfm_dat$segment)
ltv_rfm_dat <- data.table(ltv_rfm_dat)
ltv_rfm_dat2 <- one_hot(ltv_rfm_dat)


# Removing features from dataset

ltv_rfm_dat3 <- ltv_rfm_dat2 %>% select(c(freq_score,rec_score,mon_score, overall_score, `segment_High val`, `segment_Low Val`,ltv_seg))


# Recoding target variable

ltv_rfm_dat3$ltv_seg <- factor(ltv_rfm_dat3$ltv_seg, order = T, levels = c("Low LTV", "Med LTV", "High LTV"))


# Experiment 1 : Taking only R, F and M scores to predict LTV segment

head(ltv_rfm_dat4)
ltv_rfm_dat4 <- ltv_rfm_dat3[,c(1,2,3,7)]

# # Converting scores to ordered levels
# ltv_rfm_dat4$freq_score <- factor(ltv_rfm_dat4$freq_score, order = T, levels = c(1,2,3,4,5))
# ltv_rfm_dat4$rec_score <- factor(ltv_rfm_dat4$rec_score, order = T, levels = c(1,2,3,4,5))
# ltv_rfm_dat4$mon_score <- factor(ltv_rfm_dat4$mon_score, order = T, levels = c(1,2,3,4,5))

# Splitting to train and test

ltv_rfm_dat4$id <- 1:nrow(ltv_rfm_dat4)
train <- ltv_rfm_dat4 %>% sample_frac(.75) 
test  <- anti_join(ltv_rfm_dat4, train, by = 'id') %>% dplyr::select(-c(id))
train <- train %>% dplyr::select(-c(id))


# Multiordinal Logistic Regression


polr_mod = polr(ltv_seg ~ freq_score + rec_score + mon_score , data = train, Hess = TRUE)
summary(polr_mod)




head(test)




#Compute confusion table and misclassification error
predict_tbl = predict(polr_mod,test)
confusionMatrix(table(test$ltv_seg, predict_tbl))
mean(as.character(datatest$rpurchase) != as.character(predictrpurchase))








cor(train[,1:4])

head(train[,1:4])
?corrplot::corrplot()
 
vif(lm(ltv ~ ., data = ltv_rfm_dat[,c(3,5,7,10)]))

 ltv_rfm_dat[,c(3,5,7,10)]

head(ltv_rfm_dat2)

mod <-  lm(ltv ~ ., data =  ltv_rfm_dat2[,c(3,5,7,12)])
summary(mod) 
