

rm(list = ls())
dev.off()

require(dplyr)
require(ggplot2)
require(ggpubr)
require(dplyr)
require(mlr)
getwd()
setwd("E:/R/Analytics vidhya/loan predict")

loan_train<-read.csv('train_ctrUa4K.csv',na.strings = c(""," ",NA ))

loan_train$Loan_Status<- if_else(loan_train$Loan_Status=='Y','1','0')   #for dummy variable

loan_test<- read.csv('test_lAUu6dG.csv',na.strings = c(""," ",NA ))


#master data
master<- bind_rows(loan_train,loan_test)
master$Married= as.factor(master$Married)
# Let's take a peek at dataset
str(master)     
glimpse(master)

summarizeLevels(master[,-1])
summary(master)

#case matching

master<- mutate_if(master,is.character,tolower)

sapply(master,n_distinct)

#check duplicate

sum(duplicated(master[,-1]))
master[which(duplicated(master[,-1]) | duplicated(master[,-1], fromLast = T)),]

sum(is.na(master[,-13]))

colSums(is.na(master))

#na's
#loan amount - 27
#loan_amount_term - 20
#credit history - 79
summary(master$LoanAmount)
summary(master$Loan_Amount_Term)
summary(master$Credit_History)
unique(master$Credit_History)

#how to treat NA 

#blank
(colSums(master=="",na.rm = T))

colSums(master==" ",na.rm = T)

#gender 24
#married 3
#dependents 25
#self employed 55


####EDA begin##################

#univariate analysis & bi
require(ggplot2)
loan_plot<-ggplot(master[1:614,], aes(fill = as.factor(Loan_Status)))


#1 loan status

loan_plot + geom_bar(aes(x=Loan_Status))
table(master$Loan_Status)
prop.table(table(master$Loan_Status))*100
#2 property_area

loan_plot + geom_bar(aes(x=Property_Area ),position = 'fill')

#3 credit history
str(master)

loan_plot + geom_bar(aes(x = Credit_History))

loan_plot + geom_bar(aes(x = Credit_History), position = 'fill')

master$Credit_History <- as.factor(master$Credit_History)

str(master$Credit_History)

#Treatig Credit_History

sum(is.na(master$Credit_History))
#Since there are 79 NA and credit history dont depend on data provided
#Converting Credit History to Character
master$Credit_History <- as.character(master$Credit_History)
#Taking Na as "Unavailabel"
master$Credit_History[is.na(master$Credit_History )] <- "2"
#Converting Credit History to Factor
master$Credit_History <- as.factor(master$Credit_History)

#4 Loan_Amount_Term

loan_plot + geom_histogram(aes(Loan_Amount_Term), bins = 5, col = 'black')
loan_plot + geom_histogram(aes(Loan_Amount_Term), bins = 10, col = 'black', position = 'fill')


#treatment of NA  
ggplot(data=master,aes(x=master$Loan_Amount_Term)) + geom_bar(na.rm = T)

summary(master$Loan_Amount_Term)
sum(is.na(master$Loan_Amount_Term))
20/981*100
#change it by mode

master$Loan_Amount_Term[is.na(master$Loan_Amount_Term) ] <- 360 

#5 LoanAmount 

loan_plot + geom_histogram(aes(LoanAmount), bins = 5, col = 'black')
loan_plot + geom_histogram(aes(LoanAmount), bins = 10, col = 'black', position = 'fill')

# Check outliers
plot(quantile(master$LoanAmount, seq(0,1,0.01,),na.rm = T))
quantile(master$LoanAmount, seq(0,1,0.01),na.rm = T)

master$LoanAmount[which(master$LoanAmount >379.76)] <- 379.76

#treatment of NA
summary(master$LoanAmount)

#here we can use mice function
# See what happens when imputing with mean.
la <- master$LoanAmount

la <- ifelse(is.na(la),
             median(la, na.rm = T), 
             la)


par(mfrow = c(1,2))

hist(master$LoanAmount, col = 'grey', main = 'Original Age')
hist(la, col = 'skyblue', main = 'Imputed Age')

# Skewed imputation. Does not seem right.

# NA imputation with mice.
require(randomForest)
require(mice)
str(master)
mice_df <- master[,!colnames(master) %in% 
                    c('Loan_ID','Property_Area','Loan_Status','Loan_Amount_Term')]

set.seed(1)
mice_model <- mice(mice_df, method = 'rf')

mice_data <- complete(mice_model)

# Check imputation 
hist(master$LoanAmount, col = 'grey', main = 'Original Age')
hist(mice_data$LoanAmount, col = 'skyblue', main = 'Imputed Age')

# Seems good to impute.
par(mfrow = c(1,1))

master$LoanAmount <- mice_data$LoanAmount

#6. apllicant & coaplicant income
sum(is.na(master$ApplicantIncome))
sum(is.na(master$CoapplicantIncome))

#treating outlier of applicant

plot(quantile(master$ApplicantIncome, seq(0,1,0.01,)))
quantile(master$ApplicantIncome, seq(0,1,0.01),na.rm = T)
master$ApplicantIncome[which(master$ApplicantIncome > 18232.2)] <- 18232.2

#treating outlier of coaplicant 
plot(quantile(master$CoapplicantIncome, seq(0,1,0.01,)))
quantile(master$CoapplicantIncome, seq(0,1,0.01),na.rm = T)
master$CoapplicantIncome[which(master$CoapplicantIncome > 11034.4)] <- 11034.4

#creat new variable total income cause we have related applicant and coaplicant income variable
#Calculating Total Income by creaing new column

#master <- mutate(master,Total_Income=ApplicantIncome+CoapplicantIncome)

#no NA



#7 self employed

sum(is.na(master$Self_Employed))
#Converting Credit History to Character
master$Self_Employed <- as.character(master$Self_Employed)
#Taking Na as "Unavailabel"
master$Self_Employed[is.na(master$Self_Employed )] <- "unknown"
#Converting Credit History to Factor
master$Self_Employed<- as.factor(master$Self_Employed)

#8 education

sum(is.na(master$Education))

loan_plot + geom_bar(aes(x=Education))

#education dependent variable

#9 married

sum(is.na(master$Married))
#3 NA available

24/981*100

#Treating Married
master[which(is.na(master$Married)),]
#For 229 & 436 Coapplicant Income is 0 and for 105 is 754

ggplot(data=master[master$ApplicantIncome<12000
                   &master$Education=='Graduate'
                   &master$Self_Employed=='No'
                   &master$Credit_History==1,],
       aes(x=ApplicantIncome,fill=Gender))+
  geom_histogram(col='black')+
  facet_grid(Married~.)
#436 is unmarried
#105 and 229 is married

ggplot(data=master[master$CoapplicantIncome<5000
                   &master$Education=='Graduate'
                   &master$Self_Employed=='No'
                   &master$Credit_History==1,],
       aes(x=CoapplicantIncome,fill=Gender))+
  geom_histogram(col='black',position = 'dodge',binwidth = 200)+
  facet_grid(Married~.)
#436 is unmarried
#105 and 229 is married

master$Married[c(105,229)] <- 'Yes'
master$Married[436] <- 'No'
sum(is.na(master$Married))
prop.table(table(master$Married))

#10 gender
sum(is.na(master$Gender))
#Treating Gender
View(master[which(is.na(master$Gender)),]%>%filter(ApplicantIncome<5000))
summary(master[which(is.na(master$Gender)),]%>%filter(ApplicantIncome<5000))


G1 <- ggplot(data=master[master$ApplicantIncome<20000,],
             aes(x=ApplicantIncome,fill=Married))+
  geom_histogram(col='black',position = 'dodge')+
  facet_grid(Gender~.)

G2 <- ggplot(data=master[master$ApplicantIncome<20000,],
             aes(x=CoapplicantIncome,fill=Married))+
  geom_histogram(col='black',position = 'dodge')+
  facet_grid(Gender~.)

G3 <- ggplot(data=master[master$ApplicantIncome<20000,],
             aes(x=Total_Income,fill=Married))+
  geom_histogram(col='black',position = 'dodge')+
  facet_grid(Gender~.)

ggarrange(G1,G2,G3, ncol = 2, nrow = 2)
require(ggplot2)
#it is observed that males are married in comparision to females
#hence trying to treat the data as married are males and unmarried are females
#on the behalf of Applicant Income and Coapplicant Income

master$Gender[master$Married=='Yes'& is.na(master$Gender)] <- 'Male'
master$Gender[master$Married=='No'& is.na(master$Gender)] <- 'Female'

sum(is.na(master$Gender))
prop.table(table(master$Gender))
summary(master$Gender)

levels(master$Gender)


#11 Dependents

sum(is.na(master$Dependents))
summary(master$Dependents)
prop.table(table(master$Dependents))*100
25/981*100

loan_plot + geom_bar(aes(x= Dependents))

master[is.na(master$Dependents),]

#2% value  so we will replace NA with mode(0) 

master$Dependents[is.na(master$Dependents)] <- 0

############## eda complete ##############

trainload<- master[1:614, ]

testload <- master[615:981, ]

######### Random forest ####################

require(randomForest)

str(trainload)

trainload$Loan_Status <- as.factor(trainload$Loan_Status) 

rf_model1<- randomForest(Loan_Status ~ ., data = trainload[,-1], importance = T,
                         ntree = 1000, do.trace = 10)


varImpPlot(rf_model)

79/981*100



################# test data predict ##################33
testload <- master[615:981,]
str(testload)
pred_hd <- predict(rf_model, newdata = testload,type= 'class')

pred_hd[1:300]

testload$Loan_Status<- pred_hd 

prop.table(table(pred_hd))*100


####################################

testload$Loan_Status <- as.character(pred_hd)

testload$Loan_Status <- ifelse(testload$Loan_Status==1,"Y","N")

str(testload)

write.csv(testload[,c(1,13)],"solution1.csv",row.names = F)

result<- read.csv('solution1.csv')

prop.table(table(result$Loan_Status))*100

#      N        Y 
#  17.43869 82.56131 


