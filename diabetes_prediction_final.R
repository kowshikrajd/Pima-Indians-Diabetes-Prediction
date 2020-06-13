#Importing the dataset
diabetes.df <- read.csv("diabetes.csv")

#data selecton
pregnancyRange <- 
  ifelse(diabetes.df$Age < 21, "<21", 
         ifelse((diabetes.df$Age>=21) & (diabetes.df$Age<=25), "21-25", 
                ifelse((diabetes.df$Age>25) & (diabetes.df$Age<=30), "25-30",
                       ifelse((diabetes.df$Age>30) & (diabetes.df$Age<=35), "30-35",
                              ifelse((diabetes.df$Age>35) & (diabetes.df$Age<=40), "35-40",
                                     ifelse((diabetes.df$Age>40) & (diabetes.df$Age<=50), "40-50",
                                            ifelse((diabetes.df$Age>50) & (diabetes.df$Age<=60), "50-60",
                                                   ifelse((diabetes.df$Age>60), "60+","<21")
                                            )
                                     )
                              )
                       )
                )
         )
  )
table(pregnancyRange)

x.sub0 <- diabetes.df[(diabetes.df$Age >= 21 & diabetes.df$Age<=25),]
x.sub1 <- diabetes.df[diabetes.df$Age > 25 & diabetes.df$Age<=30, ]
x.sub2 <- diabetes.df[diabetes.df$Age > 30 & diabetes.df$Age<=35, ]
x.sub3 <- diabetes.df[diabetes.df$Age > 35 & diabetes.df$Age<=40, ]
x.sub4 <- diabetes.df[diabetes.df$Age > 40 & diabetes.df$Age<=50, ]
x.sub5 <- diabetes.df[diabetes.df$Age > 50 & diabetes.df$Age<=60, ]
x.sub6 <- diabetes.df[diabetes.df$Age >60, ]


r0 <- as.integer((sum(x.sub0$Outcome == 1) /  nrow(x.sub0)) * 100)
r1 <- as.integer((sum(x.sub1$Outcome == 1) /  nrow(x.sub1)) * 100)
r2 <- as.integer((sum(x.sub2$Outcome == 1) /  nrow(x.sub2)) * 100)
r3 <- as.integer((sum(x.sub3$Outcome == 1) /  nrow(x.sub3)) * 100)
r4 <- as.integer((sum(x.sub4$Outcome == 1) /  nrow(x.sub4)) * 100)
r5 <- as.integer((sum(x.sub5$Outcome == 1) /  nrow(x.sub5)) * 100)
r6 <- as.integer((sum(x.sub6$Outcome == 1) /  nrow(x.sub6)) * 100)

womenAgeOutcome<- c(r0, r1,r2,r3,r4,r5,r6)
womenAgeRange <- c("21-25","25-30","30-35","35-40","40-50","50-60","60+")
barplot(womenAgeOutcome,names.arg=womenAgeRange,xlab="Age Range",ylab="Percentage",col="#345234",main="Diabetes Age-Wise   Chart" ,ylim=c(0,100))


#Summary statistics of the dataset
summary(diabetes.df)
cor(diabetes.df[,sapply(diabetes.df,is.numeric)],use = "complete.obs")
pairs(~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,data=diabetes.df)

#Histogram plots for all numerical variables
install.packages("psych")
library(psych)
multi.hist(diabetes.df)

#Fix the zeroes in SkinThickness, BMI, BloodPressure, Glucose, Insulin for DECISION TREE
diabetes.df[,"SkinThickness"][diabetes.df[,"SkinThickness"] == 0] <- NA
diabetes.df[,"BMI"][diabetes.df[,"BMI"] == 0] <- NA
diabetes.df[,"BloodPressure"][diabetes.df[,"BloodPressure"] == 0] <- NA
diabetes.df[,"Glucose"][diabetes.df[,"Glucose"] == 0] <- NA
diabetes.df[,"Insulin"][diabetes.df[,"Insulin"] == 0] <- NA

#Decision Tree & Confusion Matrix for Training data
set.seed(1)
train.index <- sample(c(1:dim(diabetes.df)[1]),dim(diabetes.df)[1]*0.6)
train.df <- diabetes.df[train.index, ]
valid.df <- diabetes.df[-train.index, ]

library(rpart)
library(rpart.plot)
train.ct <- rpart(Outcome ~ ., data = train.df, method = "class",maxdepth=7)
prp(train.ct, type = 1, extra = 1, under = TRUE)

library(caret)
train.ct.pred <- predict(train.ct, train.df, type = "class")
confusionMatrix(train.ct.pred, as.factor(train.df$Outcome))

#Decision tree and Confusion Matrix for validation data
valid.ct <- rpart(Outcome ~ ., data = valid.df, method = "class",maxdepth=7)
prp(valid.ct, type = 1, extra = 1, under = TRUE)

valid.ct.pred <- predict(valid.ct, valid.df, type = "class")
confusionMatrix(valid.ct.pred, as.factor(valid.df$Outcome))

#AUC for training and validation data for DECISION TREE
library(pROC)

train.ct.pred.roc <- predict(train.ct, train.df, type = "prob")
rt.train <- roc(train.df$Outcome, train.ct.pred.roc[,2])
plot.roc(rt.train)
auc(rt.train)

valid.ct.pred.roc <- predict(valid.ct, valid.df, type = "prob")
rt.valid <- roc(valid.df$Outcome, valid.ct.pred.roc[,2])
plot.roc(rt.valid)
auc(rt.valid)

##
##Logistic Regression - The data is re-read, imputed and then processed

diabetes.df$SkinThickness[is.na(diabetes.df$SkinThickness)] <- mean(diabetes.df$SkinThickness,na.rm = TRUE)
diabetes.df$BMI[is.na(diabetes.df$BMI)] <- mean(diabetes.df$BMI,na.rm = TRUE)
diabetes.df$BloodPressure[is.na(diabetes.df$BloodPressure)] <- mean(diabetes.df$BloodPressure,na.rm = TRUE)
diabetes.df$Glucose[is.na(diabetes.df$Glucose)] <- mean(diabetes.df$Glucose,na.rm = TRUE)
diabetes.df$Insulin[is.na(diabetes.df$Insulin)] <- mean(diabetes.df$Insulin,na.rm = TRUE)

logit.train.df <- diabetes.df[train.index, ]
logit.valid.df <- diabetes.df[-train.index, ]

#Logit for training data
logit.train.reg <- glm(Outcome ~.,data = logit.train.df, family = "binomial")
summary(logit.train.reg)

#Logit for Validation data
logit.valid.reg <- glm(Outcome ~.,data = logit.valid.df, family = "binomial")
summary(logit.valid.reg)

#odds ratio
exp(coef(logit.train.reg))
round(data.frame(summary(logit.train.reg)$coefficient,odds.ratio=exp(coef(logit.train.reg))),5)

exp(coef(logit.valid.reg))
round(data.frame(summary(logit.valid.reg)$coefficient,odds.ratio=exp(coef(logit.valid.reg))),5)

#Confusion matrix for training and validation data set
logit.reg.pred.train <- predict(logit.train.reg, logit.train.df)
confusionMatrix(as.factor(ifelse(logit.reg.pred.train > 0.5, 1, 0)), as.factor(logit.train.df$Outcome))

logit.reg.pred.valid <- predict(logit.valid.reg, logit.valid.df)
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid > 0.5, 1, 0)), as.factor(logit.valid.df$Outcome))

#ROC for train and validation data set
rl.train <- roc(train.df$Outcome,logit.reg.pred.train)
plot.roc(rl.train)
auc(rl.train)

rl.valid <- roc(valid.df$Outcome,logit.reg.pred.valid)
plot.roc(rl.valid)
auc(rl.valid)
