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
library(randomForest)
library(xgboost)


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
cust_rec <- cust_rec %>% mutate(rec = max(lst_pur)-lst_pur) %>% dplyr::select(c(household_key,rec))

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
            dplyr::select(c(household_key,rec, rec_score))

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
  dplyr::select(c(household_key,freq, freq_score))

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
  dplyr::select(c(household_key,sls, mon_score))

describe(cust_mon3)



# Overall score can be calculated by just adding up the individual scores


cust_rfm <- cust_freq3 %>% left_join(dplyr::select(cust_rec3, rec, rec_score, household_key), by = c("household_key"="household_key")) %>% 
            left_join(dplyr::select(cust_mon3,sls,mon_score, household_key), by = c("household_key"= "household_key"))
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
cust_rec_50 <- cust_rec_50 %>% mutate(rec = max(lst_pur)-lst_pur) %>% dplyr::select(c(household_key,rec))



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
  dplyr::select(c(household_key,rec, rec_score))

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
  dplyr::select(c(household_key,freq, freq_score))

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
  dplyr::select(c(household_key,sls, mon_score))

describe(cust_mon3_50)


# Overall score can be calculated by just adding up the individual scores


cust_rfm_50 <- cust_freq3_50 %>% left_join(dplyr::select(cust_rec3_50, rec, rec_score, household_key), by = c("household_key"="household_key")) %>% 
  left_join(dplyr::select(cust_mon3_50,sls,mon_score, household_key), by = c("household_key"= "household_key"))
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
  dplyr::select(c(household_key,ltv, ltv_seg))

describe(cust_ltv3)


# Joining the LTV values (target variable) to the predictor variables which we have

ltv_rfm_dat <- cust_rfm_50 %>% left_join(dplyr::select(cust_ltv3, ltv, ltv_seg, household_key), by = c("household_key" = "household_key"))

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


###########################################   Experiment 1


# Removing features from dataset

ltv_rfm_dat3 <- ltv_rfm_dat2 %>% dplyr::select(c(freq_score,rec_score,mon_score, overall_score, `segment_High val`, `segment_Low Val`,ltv_seg))


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
set.seed(234)
polr_mod = polr(ltv_seg ~ freq_score + rec_score + mon_score , data = train, Hess = TRUE)
summary(polr_mod)



#Compute confusion table

predict_tbl = predict(polr_mod,test)
predict_tbl_train = predict(polr_mod,train)
confusionMatrix(table(test$ltv_seg, predict_tbl))
confusionMatrix(table(train$ltv_seg, predict_tbl_train))


head(predict_tbl)

test$pred <- predict_tbl

test$pred <- NULL

test_pred <- test
test_pred$pred <- predict_tbl

train_pred <- train
train_pred$pred <-predict_tbl_train

write.csv(train_pred,"train_results.csv")
write.csv()

###########################################   Experiment 2




# Removing features from dataset

ltv_rfm_dat3 <- ltv_rfm_dat2 %>% dplyr::select(c(freq_score,rec_score,mon_score, overall_score, `segment_High val`, `segment_Low Val`,ltv_seg))


# Recoding target variable

ltv_rfm_dat3$ltv_seg <- factor(ltv_rfm_dat3$ltv_seg, order = T, levels = c("Low LTV", "Med LTV", "High LTV"))


# Experiment 1 : Taking only R, F and M scores to predict LTV segment

head(ltv_rfm_dat5)
ltv_rfm_dat5 <- ltv_rfm_dat3[,c(1,2,3,7)]

# # Converting scores to ordered levels
# ltv_rfm_dat4$freq_score <- factor(ltv_rfm_dat4$freq_score, order = T, levels = c(1,2,3,4,5))
# ltv_rfm_dat4$rec_score <- factor(ltv_rfm_dat4$rec_score, order = T, levels = c(1,2,3,4,5))
# ltv_rfm_dat4$mon_score <- factor(ltv_rfm_dat4$mon_score, order = T, levels = c(1,2,3,4,5))

# Splitting to train and test

ltv_rfm_dat5$id <- 1:nrow(ltv_rfm_dat5)
train2 <- ltv_rfm_dat5 %>% sample_frac(.75) 
test2  <- anti_join(ltv_rfm_dat5, train2, by = 'id') %>% dplyr::select(-c(id))
train2 <- train2 %>% dplyr::select(-c(id))


# Random Forest
set.seed(234)
rf_mod = randomForest(ltv_seg ~ . , data = train2)
summary(rf_mod)



#Compute confusion table

predict_tbl = predict(rf_mod,test2)
predict_tbl_train = predict(rf_mod,train2)
confusionMatrix(table(test2$ltv_seg, predict_tbl))
confusionMatrix(table(train2$ltv_seg, predict_tbl_train))


###########################################   Experiment 3


xg_train_data <- train %>% dplyr::select(-c(ltv_seg))
xg_train_label <- train %>% dplyr::select(c(ltv_seg)) %>% mutate(ltv_seg2 = as.numeric((ltv_seg))) %>% dplyr::select(c(ltv_seg2))


xg_test_data <- test %>% dplyr::select(-c(ltv_seg))
xg_test_label <- test %>% dplyr::select(c(ltv_seg)) %>% mutate(ltv_seg2 = as.numeric((ltv_seg))) %>% dplyr::select(c(ltv_seg2))

# 
# xg_train_data <- xg_train_data %>% dplyr::select(c(pay_cnt,boleto_perc,credit_card_perc,avg_ship_cost,orders,same_st_del,
#                                                    avg_order_val,avg_product_price,adh_prm_ind,voucher_perc))
# xg_test_data <- xg_test_data %>% dplyr::select(c(pay_cnt,boleto_perc,credit_card_perc,avg_ship_cost,orders,same_st_del,
#                                                  avg_order_val,avg_product_price,adh_prm_ind,voucher_perc))
nrow(as.matrix(xg_train_data))

xgtrain <- xgb.DMatrix(data = as.matrix(xg_train_data), label = as.matrix(xg_train_label))

xgtest <- xgb.DMatrix(data = as.matrix(xg_test_data), label = as.matrix(xg_test_label))



xgmodel <- xgboost(data = xgtrain, # the data   
                 nround = 2) # max number of boosting iterations
                 # the objective function


#Compute confusion table



predict_tbl = predict(xgmodel,xgtest)
predict_tbl <- as.data.frame(predict_tbl)

predict_tbl_train = predict(xgmodel,xgtrain)
confusionMatrix(table(test$ltv_seg, predict_tbl))
confusionMatrix(table(train$ltv_seg, predict_tbl_train))


?xgboost()



cor(train[,1:4])

head(train[,1:4])
?corrplot::corrplot()
 
vif(lm(ltv ~ ., data = ltv_rfm_dat[,c(3,5,7,10)]))

 ltv_rfm_dat[,c(3,5,7,10)]

head(ltv_rfm_dat2)

mod <-  lm(ltv ~ ., data =  ltv_rfm_dat2[,c(3,5,7,12)])
summary(mod) 







######################################### Coupon Redemption Propoensity


head(cmp_d)

head(cmp)

head(coupon)
str(coupon)

head(coup_red)

nrow(cmp)
n_distinct(cmp$household_key)

f <- cmp %>% filter(description %in% c("TypeC","TypeB")) %>% 

s <- cmp %>% filter(description == "TypeC") %>% dplyr::select(description, campaign)
table(s)

a <- cmp %>% filter(description == 'TypeA' & campaign %in% c(8,26,30)) %>% group_by(household_key) %>% summarise(hhkey = unique(household_key)) %>% dplyr::select(c(household_key))
b <- cmp %>% filter(description == 'TypeA' & campaign %in% c(13,16)) %>% group_by(household_key) %>% summarise(hhkey = unique(household_key))



nrow(a)
nrow(b)
nrow(d)
nrow(e)
nrow(c)

c <- a %>% inner_join(dplyr::select(b,hhkey,household_key),by = c("household_key" = "household_key"))

z <- na.omit(c[,2])
z$household_key <- z$hhkey

head(coup_red)

d <- coup_red %>% inner_join(dplyr::select(r,hkey,household_key),by = c("household_key"="household_key")) %>% filter(campaign %in% c(8,26,30)) %>% group_by(household_key) %>% summarise(hhkeyd = unique(household_key)) 
e <- coup_red %>% inner_join(dplyr::select(r,hkey,household_key),by = c("household_key"="household_key"))  %>% filter(campaign %in% c(13,16)) %>% group_by(household_key) %>% summarise(hhkeye = unique(household_key))


f <- z %>% left_join(dplyr::select(d,hhkeyd,household_key),by = c("household_key" = "household_key"))



head(coup_red)

# Coupons that have been redeemed so far in Type A

c_rdmd <- coup_red %>% filter(campaign %in% c(8,26,30)) %>% group_by(coupon_upc) %>% summarise(c_upc = unique(coupon_upc))

# Coupons that are present in Type A

c_all <- coupon %>% filter(campaign %in% c(8,26,30)) %>% group_by(coupon_upc) %>% summarise(c_upc_all = unique(coupon_upc))


# Coupons that have not been redeemed at all

c_nrdmd <- c_all %>% left_join(dplyr::select(c_rdmd,c_upc,coupon_upc), by = c("coupon_upc"="coupon_upc")) %>% filter(is.na(c_upc))


head(coup_red %>% filter(campaign %in% c(8,26,30)))



length(unique(f$hhkeyd))

?unique()

n_distinct(a$household_key)
xnrow(cmp)

head(a)


head(coupon)

coupon %>% filter(campaign %in% c(17)) %>% group_by(coupon_upc) %>% summarise(coupon = unique(coupon_upc)) %>% nrow(.)








# Coupons that have been redeemed so far in Type A
 coup_red %>% filter(campaign %in% c(8))

 
 
 2496 424 10000085363        8
 2496 424 57045970076        8
 2496 424 57910070076        8
 
 # Coupon validity
 
 coupon %>% filter(campaign == 8)
 
 # Check in cmp tbl
 
 cmp %>% filter(household_key == 2496 & campaign %in% c(8))
 
 # Campaign 8 validity
 
 cmp_d %>%  filter(campaign == 8)

 head(coupon)
 
 # check in txn table
 
 txn %>% filter(household_key == 2496 & day %in% (412:460))
 
 
 
 # Checking how many of 924 have cust info
 demog$hkey <- demog$household_key
 
 r <- c %>% inner_join(dplyr::select(demog,hkey,household_key),by = c("household_key" = "household_key"))
 
 
 
 head(cmp)


 # Getting the performance base and observation base customers
 
observ_base <- cmp %>%  inner_join(dplyr::select(c,hhkey,household_key),by=c("household_key"="household_key")) %>% filter(campaign %in% c(8,26,30)) %>%
               group_by(household_key) %>% summarise(cmps = n_distinct(campaign),
                                                     cpns = 16*n_distinct(campaign))
 
perf_base <- cmp %>% inner_join(dplyr::select(c,hhkey,household_key),by=c("household_key"="household_key")) %>% filter(campaign %in% c(13,16))%>%
             dplyr::select(c(household_key)) %>% group_by(household_key) %>% summarise(hhkey = n_distinct(household_key))


 # Adding coupon redemption to customers in the performance period

head(coup_red)

redempt <- coup_red %>% filter(campaign %in% c(13,16)) %>% group_by(household_key) %>% summarise(h_key = n_distinct(household_key)) %>% mutate(redeem = 1)


perf_base <- perf_base %>% left_join(dplyr::select(redempt,redeem,household_key), by = c("household_key"="household_key")) %>% 
              dplyr::select(c(household_key, redeem)) %>% mutate(redeem = if_else(is.na(redeem), 0, redeem))



# Adding coupon redemtion data at customer level (#coupons redeemed, #campaignsinvolved)


obs_redempt <- coup_red %>% filter(campaign %in% c(8,26,30)) %>% group_by(household_key) %>% summarise(rdms = n(),
                                                                                        cmps_rdm = n_distinct(campaign)) 
observ_base2 <- observ_base %>% left_join(dplyr::select(obs_redempt,rdms,cmps_rdm,household_key), by = c("household_key"="household_key")) %>% 
              mutate(rdms =  if_else(is.na(rdms), 0L, rdms),
                     cmps_rdm =  if_else(is.na(cmps_rdm), 0L, cmps_rdm)) %>% mutate(cmps_rate = cmps_rdm/cmps*100,
                                                                                    rdms_rate = rdms/cpns*100) %>% 
                dplyr::select(c(household_key,cmps_rate,rdms_rate))

head(observ_base2)

# Filtering only for observation txns

txn$hhkey <- as.integer(txn$household_key)
obs_txn <- txn %>% inner_join(dplyr::select(observ_base2,household_key), by = c("hhkey"= "household_key")) %>% filter(day %in% c(224:264,323:369,412:460))

nrow(obs_txn)
# Adding product info
head(prod)
obs_txn_pr <- obs_txn %>% inner_join(dplyr::select(prod,manufacturer,department,brand,commodity_desc,sub_commodity_desc,curr_size_of_product,product_id), by = ("product_id"="product_id"))


# Adding causal info

head(causal)

obs_txn_pr_ad <- obs_txn_pr %>% left_join(dplyr::select(causal,display,mailer,product_id,store_id,week_no),by = c("product_id"="product_id","store_id"="store_id","week_no"="week_no"))
obs_txn_pr_ad2 <- obs_txn_pr_ad %>% mutate(display = if_else(is.na(display),'0', display),
                                          mailer = if_else(is.na(mailer),'0', mailer))                                       
               
# Creating features


obs_txn_pr_ad3 <- obs_txn_pr_ad2  %>% mutate(
                                                     pvt_brnd = case_when(brand == 'Private' ~ 1, TRUE ~ 0),
                                                     display = case_when(display == '0' ~ 0, TRUE ~ 1),
                                                     mailer = case_when(mailer == '0' ~ 0, TRUE ~ 1)) %>% 
                    group_by(household_key) %>% summarise(str_cnt = n_distinct(store_id),
                                                          mnf_cnt = n_distinct(manufacturer),
                                                          dpt_cnt = n_distinct(department),
                                                          pvt_brnd = sum(pvt_brnd)/nrow(.)*10000,
                                                          display = sum(display)/n_distinct(day),
                                                          mailer = sum(mailer)/n_distinct(day))
head(obs_txn_pr_ad2)

# # Adding demographic data
# obs_txn_pr_ad3$hhkey <- as.integer(obs_txn_pr_ad3$household_key)
# str(obs_txn_pr_ad3)
# obs_txn_pr_ad4 <- obs_txn_pr_ad3 %>% left_join(dplyr::select(demog,age_desc,marital_status_code,income_desc,homeowner_desc,hh_comp_desc,household_size_desc,kid_category_desc,hkey), by = c("hhkey"="hkey"))



# Adding target variable

obs_txn_pr_ad3$hhkey <- as.integer(obs_txn_pr_ad3$household_key)
obs_txn_pr_ad5 <- obs_txn_pr_ad3 %>% left_join(dplyr::select(perf_base,redeem,household_key), by = c("hhkey"="household_key"))



head(obs_txn_pr_ad6)

# Combinnin with obs base

observ_base3 <- obs_txn_pr_ad5 %>% inner_join(dplyr::select(observ_base2,cmps_rate,rdms_rate,household_key), by = c("hhkey"= "household_key"))
obs_txn_pr_ad6 <- observ_base3 %>% dplyr::select(-c(household_key,hhkey))


# Taking demographic info

# demog2 <- fread("hh_demographic2.csv")
# 
# head(demog2)
# str(demog2)
# demog3 <- clean_names(demog2)
# 
# demog3$age_group <- as.factor(demog3$age_group)
# demog3$homeowner_desc <- as.factor(demog3$homeowner_desc)
# demog4 <- one_hot(demog3,cols = "auto")
# 
# demog5 <- demog4 %>% mutate(kid_category_desc = case_when(kid_category_desc == 0 ~ 0, TRUE ~ 1))
# ?one_hot()
# 
# 
# cor(demog5)
# 
# demog6 <- demog5 %>% dplyr::select(-c(age_group_Youth,homeowner_desc_Renter,adults,kid_category_desc))
# 
# 
# 
# observ_base4 <- observ_base3 %>% left_join(demog6, by = c("hhkey" = "household_key"))
# 
# observ_base5 <- observ_base4 %>% dplyr::select(-c(household_key,hhkey))

# Quick Logistic model with existing imbalance

# Splitting to train and test
set.seed(234)
observ_base5 <- obs_txn_pr_ad6
obs_txn_pr_ad7 <- observ_base5
obs_txn_pr_ad7$redeem <- as.factor(obs_txn_pr_ad7$redeem)
obs_txn_pr_ad7$id <- 1:nrow(obs_txn_pr_ad7)
train2 <- obs_txn_pr_ad7 %>% sample_frac(.75) 
test2  <- anti_join(obs_txn_pr_ad7, train2, by = 'id') %>% dplyr::select(-c(id))
train2 <- train2 %>% dplyr::select(-c(id))



train2 <- as.data.frame(train2)

## Loading DMwr to balance the unbalanced class
library(DMwR)

?SMOTE()
str(train2)
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(redeem ~ ., train2, perc.over = 400, k = 8, perc.under = 100)

table(train2$redeem)
table(balanced.data$redeem)
str(train2)

str(obs_txn_pr_ad7)
set.seed(234)
logitmod <- glm(redeem ~ ., family = "binomial", data=balanced.data) 

head(t)


summary(logitmod)

#Compute confusion table

predict <- predict(logitmod, test2, type = 'response')

table(test2$redeem, predict > 0.5)







pred.logit1 <- predict(logitmod, newdata = test2, type = "response")
pred.logit <- ifelse(pred.logit1 > 0.5, 1 ,0)

smote_op_4008100 <- test2 %>% cbind(pred.logit1)

write.csv(smote_op_4008100,"smote_op_4008100.csv")

mdl.op.lg <- table(test2$redeem, pred.logit)
mdl.op.lg

confusionMatrix(mdl.op.lg,,positive = '1')


table(test2$redeem)

table(train2$redeem, predict > 0.5)

predict_tbl = predict(logitmod,test2)
predict_tbl_train = predict(logitmod,train2)
confusionMatrix(table(test2$redeem, predict_tbl))
confusionMatrix(table(train2$redeem, predict_tbl_train))

str(train2)

?confusionMatrix

head(obs_txn_pr_ad5)                           
