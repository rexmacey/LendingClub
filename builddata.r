# version 2: more variables, remove loan_status "Does not meet the credit policy"
library(stringr)
library(plyr)
library(lubridate)
library(randomForest)
library(reshape2)
library(formattable)
library(tseries)
library(zoo)
source("irr.r")
setwd(Sys.getenv("LENDINGCLUB_DIR"))  # this uses a variable defined in the .rProfile file
df<-readRDS("LoanStatsAll.rds")

# calculated variables.
credit_line_years<-function(issue_d,earliest_cr_line){log(issue_d - earliest_cr_line)}
revol_bal_per_income<-function(revol_bal,annual_inc){1+revol_bal/annual_inc}
revol_bal_per_loan_amt<-function(revol_bal,loan_amnt){1+revol_bal/loan_amnt}

status_is_closed<-levels(df$loan_status)[c(2,8)] # Charged Off or Fully Paid
df<-df[df$loan_status %in% status_is_closed,]  # only fully paid or charged off loans
# Restrict loans to charged off and fully paid loans that have had time to pay
max_issue_d<-max(df$issue_d,na.rm=TRUE)
idx<- (df$issue_d < (max_issue_d - 3) & df$term == " 36 months") | (df$issue_d < (max_issue_d - 5) & df$term == " 60 months")
df<-df[idx,]
#remove loans issued before 2008
df<-df[df$issue_d>=2008,]
#remove records with pub_rec == NA.  This clears up a bunch of NA problems
df<-df[!is.na(df$pub_rec),]

# New variables
df$cr_line_yrs<-credit_line_years(df$issue_d,df$earliest_cr_line)
df$revol_bal_per_income<-revol_bal_per_income(df$revol_bal,df$annual_inc)
df$revol_bal_per_loan_amt<-revol_bal_per_loan_amt(df$revol_bal,df$loan_amnt)
# repl emp_length==NA with -1
df[is.na(df$emp_length),"emp_length"] <- -1
df$unemployed<-is.na(df$emp_length)
df$NA_delinq<-is.na(df$mths_since_last_delinq)
df$NA_major_derog<-is.na(df$mths_since_last_major_derog)
df$NA_last_record<-is.na(df$mths_since_last_record)
df$NA_emp_length<-is.na(df$emp_length)
# repl revol_util==NA with max
df[is.na(df$revol_util),"revol_util"] <- max(df$revol_util,na.rm = TRUE)

xvar<-c("loan_amnt","term","int_rate","installment","grade","sub_grade","emp_length","home_ownership","annual_inc",
        "verification_status",
        "purpose","addr_state","acc_now_delinq","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_util",
        "total_acc","month_issued", 
        "cr_line_yrs","revol_bal_per_income","revol_bal_per_loan_amt","initial_list_status",
        "unemployed","NA_delinq","NA_major_derog","NA_last_record","NA_emp_length")

temp<-df[,c("total_pymnt","total_rec_late_fee","collection_recovery_fee","installment","funded_amnt","issue_d")]
temp$issue_d<-as.numeric(temp$issue_d)
str(temp)
tempirr<-apply(temp,1,function(x) IRR(x[1],x[2],x[3],x[4],x[5],x[6]))
df<-df[,xvar]
df$irr<-tempirr
summary(df)
# 2/16/2016: 53426 loans at this point.  
# Only  1375 are for 60 months, so we are removing those
df<-df[df$term==" 36 months",]
# No variance in the follwing, so they removed
df$acc_now_delinq<-NULL
df$initial_list_status<-NULL
df$unemployed<-NULL
saveRDS(df,"CompletedLoansV2.rds")


