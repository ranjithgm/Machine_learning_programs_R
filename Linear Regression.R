customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

library(caTools)

sample.split(customer_churn$MonthlyCharges,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

nrow(train)
nrow(test)

lm(MonthlyCharges~tenure, data=train)-> model1
predict(model1, newdata=test)->predicted_values

cbind(Actual=test$MonthlyCharges,Predicted=predicted_values)->final_data
head(final_data)

class(final_data)

as.data.frame(final_data)->final_data
class(final_data)

final_data$Actual - final_data$Predicted ->error
head(error)

cbind(final_data,error)-> final_data
head(final_data)

sqrt(mean((final_data$error)^2))->rmse1
#---

lm(MonthlyCharges~InternetService,data=train)-> model2
predict(model2,newdata=test)-> result
cbind(Actual=test$MonthlyCharges,Predicted=result)->final_data2
head(final_data2)

as.data.frame(final_data2)-> final_data2
(final_data2$Actual-final_data2$Predicted)->error2

cbind(final_data2,error2) -> final_data2
sqrt(mean((final_data2$error2)^2))-> rmse2
#-----------------------------------------------------------------------------------------------
#Multiple Linear Regression
  
#library(caTools)  

sample.split(customer_churn$tenure,SplitRatio = 0.65)-> split_model
subset(customer_churn, split_model==T)->train
subset(customer_churn, split_model==F)->test

nrow(train)
nrow(test)

lm(tenure~MonthlyCharges+gender+InternetService+Contract, data=train)-> mod1
predict(mod1,test)->result1

cbind(Actual=test$tenure,Predicted=result1)->final_data1
head(final_data1)
class(final_data1)

as.data.frame(final_data1)->final_data1
class(final_data1)
final_data1$Actual - final_data1$Predicted ->error1
head(error1)
cbind(final_data1,error1)-> final_data1
head(final_data1)

sqrt(mean((final_data1$error1)^2))->rmse1
---
lm(tenure~Partner+PhoneService+TotalCharges+PaymentMethod,data=train)-> mod2
predict(mod2,test)-> result2
cbind(Actual=test$tenure,Predicted=result2)->final_data2
head(final_data2)

as.data.frame(final_data2)-> final_data2
(final_data2$Actual-final_data2$Predicted)->error2
head(error2)

cbind(final_data2,error2) -> final_data2
sqrt(mean((final_data2$error2)^2,na.rm=T))->rmse2

#---------------------------------------------------
#Assumptions
  
library(ggplot2)  

ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()
ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()+geom_smooth(method = "lm")

lm(TotalCharges~tenure, data = customer_churn)->mod1
predict(mod1,data=customer_churn)-> result1
cbind(Actual=customer_churn$TotalCharges, Predicted=result1)-> final_data1

head(final_data1)
as.data.frame(final_data1)->final_data1
final_data1$Actual -final_data1$Predicted  -> error1
cbind(final_data1,error1)-> final_data1

ggplot(data= final_data1, aes(x=Predicted, y=error1)) + geom_point()

qqnorm(final_data1$error1)







