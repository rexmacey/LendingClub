library(stringr)
library(plyr)
library(lubridate)
library(randomForest)
library(reshape2)
library(formattable)
library(zoo)
source("irr.r")
setwd(Sys.getenv("LENDINGCLUB_DIR"))  # this uses a variable defined in the .rProfile file
df<-readRDS("LoanStatsAll.rds")
df2<-df # used for all loans
#number of loans (rows of data)
nrow(df)
credit_line_years<-function(issue_d,earliest_cr_line){(issue_d - earliest_cr_line)/365.25}
revol_bal_per_income<-function(revol_bal,annual_inc){revol_bal/annual_inc}
revol_bal_per_loan_amt<-function(revol_bal,loan_amnt){revol_bal/loan_amnt}

# Restrict loans to charged off and fully paid loans that have had time to pay
max_issue_d<-max(df$issue_d,na.rm=TRUE)
idx<- (df$issue_d < (max_issue_d - 3) & df$term == " 36 months") | (df$issue_d < (max_issue_d - 5) & df$term == " 60 months")
df<-df[idx,]

levels(df$loan_status)
status_is_closed<-levels(df$loan_status)[c(2,4,5,7,8)]
df<-df[df$loan_status %in% status_is_closed,]  # only fully paid or charged off loans

# New variables
df$cr_line_yrs<-credit_line_years(df$issue_d,df$earliest_cr_line)
df$revol_bal_per_income<-revol_bal_per_income(df$revol_bal,df$annual_inc)
df$revol_bal_per_loan_amt<-revol_bal_per_loan_amt(df$revol_bal,df$loan_amnt)

xvar<-c("loan_amnt","term","int_rate","installment","grade","sub_grade","emp_length","home_ownership","annual_inc",
        "verification_status",
        "purpose","addr_state","acc_now_delinq","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_util",
        "total_acc","month_issued", 
        "cr_line_yrs","revol_bal_per_income","revol_bal_per_loan_amt")


#remove loans issued before 2008
df<-df[df$issue_d>=2008,]
#remove records with pub_rec == NA.  This clears up a bunch of NA problems
df<-df[!is.na(df$pub_rec),]
# repl emp_length==NA with -1
df[is.na(df$emp_length),"emp_length"] <- -1
# repl revol_util==NA with 100
df[is.na(df$revol_util),"revol_util"] <- 100

temp<-df[,c("total_pymnt","total_rec_late_fee","collection_recovery_fee","installment","funded_amnt","issue_d")]
tempirr<-apply(temp,1,function(x) IRR(x[1],x[2],x[3],x[4],x[5],x[6]))
df<-df[,xvar]
df$irr<-tempirr
saveRDS(df,"CompletedLoans.rds")


