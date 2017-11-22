library(ipred)
library(rpart)
library(randomForest)

setwd("C:/Users/yogas/Downloads/hackathon")
rawData<-read.csv('loan.csv')

relevantRows<-ifelse(rawData$loan_status=='Fully Paid' | rawData$loan_status=='Charged Off' | rawData$loan_status=='Late (31-120 days)', TRUE, FALSE)
#relevantRows<-ifelse(rawData$loan_status=='Current', FALSE, TRUE)
data<-rawData[relevantRows,]

# Gets summary statistics for relevant columns
relevantColumns<-data[,c('loan_amnt','term','emp_title','emp_length','home_ownership','annual_inc','purpose','loan_status')]


loanAmtModerate<-ifelse(10000 <= data$loan_amnt & data$loan_amnt < 20000, 1, 0)
loanAmtHigh<-ifelse(data$loan_amnt > 20000, 1, 0)

loanLongTerm<-ifelse(data$term ==' 60 months', 1, 0)

empLengthModerate<-ifelse(data$emp_length == '3 years' | data$emp_length == '5 years', 1, 0)
empLengthLong<-ifelse(data$emp_length == '10+ years', 1, 0)

homeOwnerShip<-ifelse(data$home_ownership == 'MORTGAGE' | data$home_ownership == 'OWN', 1, 0)

incomeModerate<-ifelse(data$annual_inc > 45000 & data$annual_inc < 75000, 1, 0)
incomeHigh<-ifelse(data$annual_inc > 75000, 1, 0)

purposeDebt<-ifelse(data$purpose == 'debt_consolidation' | data$purpose == 'credit_card', 1, 0)

loanFullyPaid<-ifelse(data$loan_status=='Fully Paid', 1, 0)
loanChargedOff<-ifelse(data$loan_status=='Charged Off', 1, 0)
loanLate<-ifelse(data$loan_status=='Late (31-120 days)', 1, 0)

outcome<-as.character(data$loan_status)

annualIncome<-data$annual_inc

trainDataSet<-cbind(loanAmtModerate,loanAmtHigh,loanLongTerm,empLengthModerate,empLengthLong,
                    homeOwnerShip, annualIncome, purposeDebt, loanFullyPaid)
trainDataSet<-as.data.frame(trainDataSet)

model<-rpart(loanFullyPaid~., trainDataSet, control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
plot(model)
text(model)

# Not needed
#data2$loan_amnt=as.numeric(data2$loan_amnt)
#data2$term=as.character(data2$term)
#data2$emp_title=as.character(data2$emp_title)
#data2$emp_title=as.character(data2$emp_length)
#data2$home_ownership=as.character(data2$home_ownership)
#data2$annual_inc=as.numeric(data2$annual_inc)
#data2$purpose=as.character(data2$purpose)
#data2$loan_status=as.character(data2$loan_status)

