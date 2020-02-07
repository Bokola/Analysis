
## packages
rm(list = ls(all = T))
setwd('C:/Users/bokola/Google drive/Data/final exam')
list.files()

ipk <- function(pkg){
  new.pkg <- list.of.pkg[!(list.of.pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
  
}
list.of.pkg <- c("tidyverse", "repr", "magrittr", "gridExtra", "caret", "ROCR", "pROC")
ipk(list.of.pkg)
options(repr.plot.width = 4, repr.plot.height = 3.5)

## import data

 # aw_avemonthspend <- read.csv("AW_AveMonthSpend.csv", header = T)
 # names(aw_avemonthspend) <- tolower(names(aw_avemonthspend))
 # 
 # aw_bikebuyer <- read.csv("AW_BikeBuyer.csv", header = T)
 # names(aw_bikebuyer) <- tolower(names(aw_bikebuyer))
 # 
 # advworkscusts <- read.csv("AdvWorksCusts.csv", header = T)
 # names(advworkscusts) <- tolower(names(advworkscusts))


temp = list.files(pattern="*.csv")[grep("^a", list.files(pattern="*.csv"), ignore.case = T) ]
list2env(
  lapply(setNames(temp, tolower(make.names(gsub("\\..*", "", temp)))), function(x){
    x =read.csv(x)
    names(x) <- tolower(names(x))

    return(x)}),

  envir = .GlobalEnv)



# 2. removing duplicated ids

dim(aw_avemonthspend) # 16519 obs
aw_avemonthspend <- aw_avemonthspend[!duplicated(aw_avemonthspend$customerid),]
aw_bikebuyer <- aw_bikebuyer[!duplicated(aw_bikebuyer$customerid, fromLast = T),]
advworkscusts <- advworkscusts[!duplicated(advworkscusts$customerid, fromLast = T),]


dim(aw_avemonthspend) # 16404, 2
dim(aw_bikebuyer) #16404, 2
dim(advworkscusts) # 16404, 23

#aw_avemonthspend %>%
  #inner_join(aw_bikebuyer) -> semi_data

aw_avemonthspend %>%
  summarise(mean = mean(avemonthspend, na.rm = T), sd = sd(avemonthspend, na.rm = T), sample = n(), min = min(avemonthspend, na.rm = T), max = max(avemonthspend, na.rm = T), median = median(avemonthspend))
names(aw_bikebuyer)
ggplot(aw_bikebuyer) + 
  geom_histogram(aes(x = bikebuyer))
names(advworkscusts)

aggregate(yearlyincome~occupation,advworkscusts, median) %>%
  arrange(yearlyincome)

data <- merge(aw_avemonthspend, advworkscusts, by = "customerid")
mutate(data, age = ifelse(difftime(as.Date("1998-01-01"), as.Date(as.character(birthdate)), units = 'days')/365 <25,'25',
      ifelse(difftime(as.Date("1998-01-01"), as.Date(as.character(birthdate)), units = 'days')/365<=45,'25-45',
        ifelse(difftime(as.Date("1998-01-01"), as.Date(as.character(birthdate)), units = 'days')/365<=55,'45-55','55')))) %>%
  aggregate(avemonthspend~gender+age, ., sum) %>% arrange(avemonthspend)
data %>%
  aggregate(avemonthspend~maritalstatus, ., median) %>%
  arrange(avemonthspend)
data %>%
  aggregate(avemonthspend~numbercarsowned, ., median) %>%
  arrange(avemonthspend)
data %>%
  aggregate(avemonthspend~gender, ., median) %>%
  arrange(avemonthspend)
data %>%
  aggregate(avemonthspend~gender, ., IQR) %>%
  arrange(avemonthspend)
data_final <- merge(data, aw_bikebuyer, by = "customerid")
data_final %>%
  aggregate(yearlyincome ~ bikebuyer, ., median) %>%
  arrange(yearlyincome)
data_final %>%
  aggregate(numbercarsowned ~ bikebuyer, ., median) %>%
  arrange(numbercarsowned)
data_final %>%
 
  aggregate(bikebuyer ~ occupation, ., sum)
data_final %>%
  aggregate(bikebuyer ~gender, ., sum)
write.csv(data_final, file = "data_final.csv", row.names = F )

# classiffication
#1. Data exploration
names(data_final)
data_final %>%
  aggregate(avemonthspend ~ bikebuyer, ., sum)
plot_bars <- function(df){
  options(repr.plot.width = 4, repr.plot.height = 3.5)
  for(col in col_char) {
  #for (col in colnames(df)) {
    #if(is.factor(df[, col])) {
      p <- ggplot(df, aes_string(col)) +
        geom_bar(alpha = 0.6) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1 ))
      print(p)
    #}
    
  }
}
col_char <- c("occupation", "maritalstatus", "education", "gender")
plot_bars(data_final)
 # no much differences in gender - could be excluded.


plot_box <- function(df, cols, col_x = 'bad_credit'){
  options(repr.plot.width = 4, repr.plot.height = 3.5)
  for (col in cols){
    p <- ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
    
  }
}

num_cols = c('loan_duration_mo', 'loan_amount', 'payment_pcnt_income', 'age_yrs', 'number_loans', 'dependents')
plot_box(credit, num_cols)

num_cols = c('loan_duration_mo', 'loan_amount', 'payment_pcnt_income', 'age_yrs')
preProcValues <- preProcess(training[,num_cols], method = c("center", "scale"))
# preprocessing numeric cols
training[,num_cols] = predict(preProcValues, training[,num_cols])
test[,num_cols] = predict(preProcValues, test[,num_cols])
head(training[,num_cols])
# modelling
set.seed(5566)
logistic_mod <- glm(bad_credit ~ loan_duration_mo + loan_amount + payment_pcnt_income + age_yrs +checking_account_status + credit_history + purpose + gender_status + time_in_residence + property, family = binomial, data = training)

#scoring
test$probs <- predict(logistic_mod, newdata = test, type = 'response')
test[1:20, c('bad_credit', 'probs')]

score_model <- function(df, threshold) {
  df$score <- ifelse(df$probs < threshold, 'bad', 'good')
  df
}
test <- score_model(test, 0.5)
test[1:20, c('bad_credit', 'probs', 'score')]

logistic.eval <- function(df){ 
  # First step is to find the TP, FP, TN, FN cases
  df$conf = ifelse(df$bad_credit == 'bad' & df$score == 'bad', 'TP',
                   ifelse(df$bad_credit == 'bad' & df$score == 'good', 'FN',
                          ifelse(df$bad_credit == 'good' & df$score == 'good', 'TN', 'FP')))
  
  # Elements of the confusion matrix
  TP = length(df[df$conf == 'TP', 'conf'])
  FP = length(df[df$conf == 'FP', 'conf'])
  TN = length(df[df$conf == 'TN', 'conf'])
  FN = length(df[df$conf == 'FN', 'conf'])
  
  ## Confusion matrix as data frame
  out = data.frame(Negative = c(TN, FN), Positive = c(FP, TP))
  row.names(out) = c('Actual Negative', 'Actual Positive')
  print(out)  
  
  # Compute and print metrics
  P = TP/(TP + FP)
  R = TP/(TP + FN)  
  F1 = 2*P*R/(P+R)  
  cat('\n')
  cat(paste('accuracy  =', as.character(round((TP + TN)/(TP + TN + FP + FN), 3)), '\n'))      
  cat(paste('precision =', as.character(round(P, 3)), '\n'))     
  cat(paste('recall    =', as.character(round(R, 3)), '\n'))
  cat(paste('F1        =', as.character(round(F1,3)),'\n'))
  
  roc_obj <- roc(df$bad_credit, df$probs)
  cat(paste('AUC       =', as.character(round(auc(roc_obj),3)),'\n'))
}
logistic.eval(test)

#or
confusionMatrix(factor(test$score), test$bad_credit)


ROC_AUC = function(df){
  options(repr.plot.width=5, repr.plot.height=5)
  pred_obj = prediction(df$probs, df$bad_credit)
  perf_obj <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  AUC = performance(pred_obj,"auc")@y.values[[1]] # Access the AUC from the slot of the S4 object
  plot(perf_obj)
  abline(a=0, b= 1, col = 'red')
  text(0.8, 0.2, paste('AUC = ', as.character(round(AUC, 3))))
}

ROC_AUC(test)

test$probs = rep(0, length.out = nrow(test))
test$score = rep(0, length.out = nrow(test))
logistic.eval(test)
ROC_AUC(test)
