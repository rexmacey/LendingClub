#Take raw CSV files combine into large rdata file. No dependency
library(stringr)
library(lubridate)
library(zoo)
setwd(Sys.getenv("LENDINGCLUB_DIR"))  # this uses a variable defined in the .rProfile file

EmpLen2Num<-function(x){
    switch(x,
           "< 1 year" = 0,
           "1 year" = 1,
           "2 years" = 2,
           "3 years" = 3,
           "4 years" = 4,
           "5 years" = 5,
           "6 years" = 6,
           "7 years" = 7,
           "8 years" = 8,
           "9 years" =9,
           "10+ years" = 10,
           NA)
}


dfa <- read.csv("LoanStats3a.csv", h=T, stringsAsFactors=F, skip=1)
dfb <- read.csv("LoanStats3b.csv", h=T, stringsAsFactors=F,skip=1)
dfc <- read.csv("LoanStats3c.csv", h=T, stringsAsFactors=F, skip=1)
dfd <- read.csv("LoanStats3d.csv", h=T, stringsAsFactors=F, skip=1)
df<-rbind(dfa,dfb,dfc,dfd)
rm(dfa,dfb,dfc,dfd)

df$issue_d <- as.yearmon(df$issue_d,"%b-%Y")
df$year_issued <- year(df$issue_d)
df$month_issued <- month(df$issue_d)
df$earliest_cr_line <- as.yearmon(df$earliest_cr_line,"%b-%Y")
df$revol_util <- str_replace_all(df$revol_util, "[%]", "")
df$revol_util <- as.numeric(df$revol_util)
df$funded_amnt_inv<-as.numeric(df$funded_amnt_inv) 
df$term<-as.factor(df$term) 
df$int_rate <- str_replace_all(df$int_rate, "[%]","") 
df$int_rate <- as.numeric(df$int_rate) 
df$grade <- ordered(df$grade,levels=c("A","B","C","D","E","F","G")) 
df$sub_grade <- ordered(df$sub_grade,levels=c(
    "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "C1", "C2", "C3", "C4", "C5", "D1", "D2", "D3", "D4", "D5",
    "E1", "E2", "E3", "E4", "E5", "F1", "F2", "F3", "F4", "F5", "G1", "G2", "G3", "G4", "G5")) 
df$emp_length <- sapply(df$emp_length,EmpLen2Num)
df$home_ownership <- as.factor(df$home_ownership) 
df$pymnt_plan <- as.factor(df$pymnt_plan) 
df$purpose <- as.factor(df$purpose) 
df$addr_state <- as.factor(df$addr_state) 
df$total_pymnt <- as.numeric(df$total_pymnt) 
df$last_pymnt_d <- as.yearmon(df$last_pymnt_d,"%b-%Y") 
df$verification_status <- as.factor(df$verification_status)

df$loan_status <- as.factor(df$loan_status)
df$initial_list_status <- as.factor(df$initial_list_status)
df$recoveries <- as.numeric(df$recoveries)
df$next_pymnt_d <- as.yearmon(df$next_pymnt_d,"%b-%Y")
df$last_credit_pull_d <- as.yearmon(df$last_credit_pull_d,"%b-%Y")
df$last_pymnt_amnt <- as.numeric(df$last_pymnt_amnt)
df$zip_code <- as.factor(df$zip_code)
df$application_type <- as.factor(df$application_type)
saveRDS(df,file="LoanStatsAll.rds")
