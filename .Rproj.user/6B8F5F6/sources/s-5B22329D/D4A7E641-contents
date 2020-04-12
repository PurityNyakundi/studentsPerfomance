#load the dataset

student <- read.csv("StudentsPerformance.csv",stringsAsFactors = FALSE)
student
#load the necessary libraries

library(dplyr)
library(tidyverse)
library(ggplot2)

str(student)
student
#check for NA
colSums(is.na(student))
#no missing values

#change some variables to factor 
student$race.ethnicity<-as.factor(student$race.ethnicity)
str(student)

#lets introduce a new variable
#mutate(student,totat_score = sum(student[6:8]))

head(student)

str(student)

st<-student%>%
  group_by(math.score,reading.score, writing.score)%>%
  mutate(total_score = math.score+reading.score+ writing.score,
         mean_score = (math.score+reading.score+ writing.score)/3)
   # select(total.score,math.score,reading.score,writing.score)
  

str(st)
glimpse(st)

#change the male and female
view(st)
st$gender<-ifelse(st$gender=="female",1,0)
glimpse(st)
st$gender<-as.factor(st$gender)
glimpse(st)
#EDA
#check the gender balance in this class and the dominating race in the class
ggplot(st,mapping = aes(gender,fill = race.ethnicity))+
  geom_histogram(stat = "count",position = "dodge")+
  ggtitle("Gender Dominating in class")
  
#girls are more than boys
#The most dominating gender is group C

#check the most perfoming gender
st%>%
  group_by(gender)%>%
  arrange(desc(mean_score))%>%
  select(gender,mean_score)%>%
  top_n(10,wt = mean_score)
#ladies seem to be on the top,hence best performing
#lets check the test preparation course
#change it to factor

st$test.preparation.course<-as.factor(st$test.preparation.course)
glimpse(st)
str(st)

ggplot(st,aes(test.preparation.course,fill = gender))+
  geom_histogram(stat = "count")+
  
  
#check maths perfomance
  st%>%
  group_by(gender)%>%
  arrange(desc(math.score))%>%
  select(gender,math.score)%>%
  top_n(10,math.score)
  
ggplot(st,mapping = aes(math.score))+
  geom_bar(stat = "count",position = "dodge")+
  facet_wrap(~gender)
  #theme(element_rect(fill = "WHITE", color = "black"))

#girls are not perfoming very well 
#It can be seen one is getting as low as 0

#check the summaries 
summary(st)

#We now try to model our data
#our aim is to predict the mean.score 
#also can predict either mail or female

train<-st[1:500,c(-2,-3,-4,-5)]
view(train)
test<-st[501:1000,c(-2,-3,-4,-5)]
view(test)

#normalize for knn

normalize<-function()

md<-glm(gender~math.score+reading.score+writing.score+total_score+mean_score,data = train,family = "binomial")
pr<-predict(md,test,method= "response")
pred<-ifelse(pr>0.56,1,0)
y_predq<-factor(pred,levels = c(0,1))
table(y_predq,test$gender)
mean(y_predq==test$gender)

summary(md)

mean(test$gender==pred)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


data_nom<-as.data.frame(lapply(st[6:10],normalize))
#independent variables
data_nom_train<-data_nom[1:500,]
view(data_nom_train)
data_nom_test<-data_nom[501:1000,]
view(data_nom_test)
#labels

label_train<-st[1:500,1]
view(label_train)
label_test<-st[501:1000,1]

summary(data_nom)
#our data is now analyzed

#now  we train our model
library(class)
kmodel<-knn(data_nom_train,data_nom_test,cl = label_train)
#an error but can't understand 
rtrain<-cbind(data_nom_train,label_train)
rtest<-cbind(data_nom_test,label_test)
library(ranger)
ramodel<-ranger(mean_score~.,data = rtrain,num.trees = 500,respect.unordered.factors = "order")
summary(ramodel)
ml<-lm(mean_score~.,data_nom_train)
summary(ml)
rpred<-predict(ramodel,rtest)
rpred
#table(rpred$predictions,test$gender)
mean(rpred$predictions==test$gender)
table(rpred$predictions)

#still in progress