## R Packages Required for Day 4 and 5 +++ -----------
install.packages("tidyverse")
install.packages("DBI")
install.packages("RMySQL")
install.packages("lubridate")

# NOTE: If you already installed the packages, you can skip the commands above

## loading the packages once installed
library(tidyverse)
library(DBI)
library(RMySQL)
library(lubridate)


#--++++++++++++++++++++++++++++++++++++++++++++ ----
## Training Day 4; Data extraction/pulling from MySQL
#--+++++++++++++++++++++++++++++++++++++++++++++ ----
# load required packages
library(DBI)
library(RMySQL)
library(tidyverse)

# Create database connection
con <- dbConnect(MySQL(),
                 host = 'keklf-mysqluat',
                 user = 'tiba_usr',
                 password = 'tiba2019',
                 dbname = 'tiba')

# list number of tables
dbListTables(con)

# List fields of 2 tables
dbListFields(con, "person") # sex, dob for age calculation

dbListFields(con, ) # date_admn, admin_ward, diagnosis_1_disch,diagnosis_1_category

## ++ Approach 1 (write an SQL code from within R)
adult_admission <- dbGetQuery(con, "select * from adult_admission")

haematology_table <-dbGetQuery(con, 
                               "select fk_study, adm_sample, fk_specimen from haematology_test") 

## compute age from dob and date_admn

## convert dob and date_adm to dates


## calculate age


## ++ Approach 2 using R direct (export the 2 tables and use R to merge/select variables)
library()

adult_adm <- dbGetQuery() # export admissions table

khdss_table <- dbGetQuery() # extract person table

## merge and select variables
tiba_demo_data <-   ## filter all female gender
  

## calculate age
tiba_demo_data <-  # divide days by 365.25
 

## further participant exercise
# calculate BMI 



#--++++++++++++++++++++++++++++++++++++++++++++ ----
## TIBA Workshop practice script ---Day 5-- +++ -----
#--+++++++++++++++++++++++++++++++++++++++++++++ ----

## Getting data into R --------------------------------------------------

# reading from flat files ----

# load packages
library()  # for reading .xlsx data (excel datasets)
library()   # read a variety of formats (.csv, tab delimeted)


## read various formats of data
dir()  # list items on the datasets folder

# read .xlsx (excel data)
adult_adm_excel <- 

# read .csv (comma separated files)
adult_adm_csv <- 

# read .txt(tab delimeted data)
adult_adm_txt <- 


# Connecting to web-based database (REDCap) ----

library() # load required package

# create a connection
con <- redcapConnection(url='redcap_link here',token = 'token here')

# export data
redcap_all_data<-exportRecords('conection_here', fields = NULL, forms = NULL , 
                               records = NULL, events = NULL ,labels = TRUE, 
                               dates = TRUE, survey = FALSE, factors=F, dag = T, 
                               checkboxLabels = TRUE)

?redcapAPI # get more help

# how many records for each visits



## You can also export events (timepoints)
total_events <- 

## you can export instruments (CRFs)
CRFs_name <- 

## You can export users and their rights
all_users <- 

### +++++++++++ --- AND MANY MORE -----++++++

# Connecting to web-based database (MySQL) ----
# load required packages

library()
library()

con <- dbConnect()   # password associated with username

# list tables available


# list fields/variables in a specific table
  ## list fields for the  table


# extract/pull data from the database



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN messy datasets +++++++++++++++++++++++++++++++++++++++++++++++++
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
adult_admission <- 

lab_results <- 


# Exploring raw data ------------------------------------------------------

dim()
str()
glimpse()  # better version of str()

head()
tail()

summary() # quick summary of all variables


# General housekeeping (maintaining the quality) ---------------------------------
# drop un-necessary columns



## identify duplicates (on all variables)
dups_1 <-   ## duplicated rows


## remove duplicated data
adult_admission <-  # remove duplicated data

# also adult_admission[!duplicated(adult_admission), ] or unique(adult_admission)


## checking duplicates on a variable
dups_2 <- 

## remove duplicates on a variable
adult_admission <-  # remove duplicated data on id



#some columns have high missing rate, probably not needed
miss_perc <- 
View(miss_perc)

# exclude columns where 100% data is missing
adult_admission <- 


## missing values
# columns with almost 100% missing values report
miss_perc <-  # proportion of missing
 
View(miss_perc)

## filter specific missing data for variables
miss_height <-   # missing height

miss_hgt_wght <-  # missing either height or weight

miss_vitals <-   # missing both height and weight


## suspicious/abnormal values (outliers for numeric variable)
# visualize (histogram, boxplots and scatterplots) 
# focus on vital status and identify any anormalies

# scatter plot
# histogram
# boxplot

## you can also use summary()
summary()


## check for dates (admission vs discharge)
# was anyone discharged before being enrolled?? 

                # Very important you convert to date format

 # patients discharged before they were enrolled

## creating variables (discretizing continous variables)
# length_hosp_stay


# calculate age and discretize



# BMI



# Tidying/Reshaping data --------------------------------------------------
# gather
 # use results, gather cbc tests into 1 column test_type and result



# spread
 # undo the above (spread)




# separate
 # separate admission date into day, month and year (by default, original variable is droped)



# unite
 # join date_admn and time_admn :: datetime_admn



## merging datasets
# adult_admission and results data

# Preparing data for analysis ---------------------------------------------
## majorly type conversion




# Exporting and saving datasets -------------------------------------------



