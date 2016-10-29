# Matching submitted outputs

setwd('C:/Users/HP-PC/Desktop/Knocktober/Train')
setwd("C:\\Users\\SatyakiBh\\Desktop\\AV-Knoctober\\backup")
camp_1 <- read.csv("First_Health_Camp_Attended.csv")
camp_1$X = NULL
camp_2 <- read.csv("Second_Health_Camp_Attended.csv")
camp_3 <- read.csv("Third_Health_Camp_Attended.csv")
camps <- read.csv("Health_Camp_Detail.csv")
patients <- read.csv("Patient_Profile.csv")
data_init <- read.csv("Train.csv")
sub_init <- read.csv("Test.csv")

data = merge(data_init,camps,'Health_Camp_ID')
data = merge(data,patients,'Patient_ID')
data = merge(data,camp_1,c('Health_Camp_ID','Patient_ID'),all.x = T)
data = merge(data,camp_2,c('Health_Camp_ID','Patient_ID'),all.x = T)
data = merge(data,camp_3,c('Health_Camp_ID','Patient_ID'),all.x = T)
data$Registration_Date = as.numeric(as.Date(data$Registration_Date,format='%d-%B-%y'))
data$Camp_Start_Date = as.numeric(as.Date(data$Camp_Start_Date,format='%d-%B-%y'))
data$Camp_End_Date = as.numeric(as.Date(data$Camp_End_Date,format='%d-%B-%y'))
data$First_Interaction = as.numeric(as.Date(data$First_Interaction,format='%d-%B-%y'))
data$Donation = NULL
data$Last_Stall_Visited_Number = NULL

data$How_old<- as.numeric(as.Date("10/22/2016",format="%m/%d/%Y")) - data$Registration_Date
data$Income <- as.numeric(data$Income)
data$CampDuration<- data$Camp_End_Date - data$Camp_Start_Date
data$How_old_interaction <- as.numeric(as.Date("10/22/2016",format="%m/%d/%Y")) - data$First_Interaction
data$Online_Follower <- as.factor(data$Online_Follower)
data$LinkedIn_Shared <- as.factor(data$LinkedIn_Shared)
data$Twitter_Shared <- as.factor(data$Twitter_Shared)
data$Facebook_Shared <- as.factor(data$Facebook_Shared)
data$Category3<-as.factor(data$Category3)
data$days_between_registration_CampStart <- data$Camp_Start_Date - data$Registration_Date
data$RegisteredDuringCamp <- ifelse((data$days_between_registration_CampStart==0),0,1)
data$RegisteredDuringCamp <- as.factor(data$RegisteredDuringCamp)

library(sqldf)
tb<-sqldf("select Patient_ID,count(Patient_ID) as cnt from data group by Patient_ID")
#tb<-sqldf("select Patient_ID,count(Patient_ID) as cnt from data group by Patient_ID")

new_data<-data
new_data$Health_Score[!is.na(new_data$Health_Score)]=1
new_data$Health_Score[is.na(new_data$Health_Score)]=0
#tb2<-sqldf("select Patient_ID, count(Health_Score) as ev from new_data group by Patient_ID")
#tb1<-sqldf("select Patient_ID, count(Health_Score) as eve from (select * from new_data where to_char(Health_Score)=1) group by Patient_ID")
#tbz<-sqldf("select Patient_ID, Health_Score, count(Health_Score) from new_data group by Patient_ID,Health_Score order by Patient_Id")
tbz1<-sqldf("select Patient_ID, count(Health_Score) as event from (select * from new_data where Health_Score=1) group by Patient_ID order by Patient_Id")
data1 = merge(x=tb,y=tbz1,by.x='Patient_ID',all.x = T)
data1$event[is.na(data1$event)]=0
data1$prob=data1$event*1.0/data1$cnt
data1$event=NULL
data1$cnt=NULL
data = merge(x=data,y=data1,by.x='Patient_ID',all.x=T)

new_data<-data
new_data$Health.Score[!is.na(new_data$Health.Score)]=1
new_data$Health.Score[is.na(new_data$Health.Score)]=0
new_data$HS2=new_data$Health.Score
#sqldf("select * from new_data where HS2=1")
tbz1<-sqldf("select Patient_ID, count(HS2) as event from (select * from new_data where HS2=1) group by Patient_ID order by Patient_ID")
data1 = merge(x=tb,y=tbz1,by.x='Patient_ID',all.x = T)
data1$event[is.na(data1$event)]=0
data1$prob_2=data1$event*1.0/data1$cnt
data1$event=NULL
data1$cnt=NULL
data = merge(x=data,y=data1,by.x='Patient_ID',all.x=T)

new_data<-data
new_data$Number_of_stall_visited[!is.na(new_data$Number_of_stall_visited)]=1
new_data$Number_of_stall_visited[is.na(new_data$Number_of_stall_visited)]=0
#sqldf("select * from new_data where HS2=1")
tbz1<-sqldf("select Patient_ID, count(Number_of_stall_visited) as event from (select * from new_data where Number_of_stall_visited=1) group by Patient_ID order by Patient_ID")
data1 = merge(x=tb,y=tbz1,by.x='Patient_ID',all.x = T)
data1$event[is.na(data1$event)]=0
data1$prob_3=data1$event*1.0/data1$cnt
data1$event=NULL
data1$cnt=NULL
data = merge(x=data,y=data1,by.x='Patient_ID',all.x=T,all.y = F)

for_sub_data=sqldf("select Patient_ID, prob, prob_2, prob_3 from data")

sub_data = merge(sub_init,camps,'Health_Camp_ID')
sub_data = merge(sub_data,patients,'Patient_ID')
sub_data = merge(sub_data,camp_1,c('Health_Camp_ID','Patient_ID'),all.x = T)
sub_data = merge(sub_data,camp_2,c('Health_Camp_ID','Patient_ID'),all.x = T)
sub_data = merge(sub_data,camp_3,c('Health_Camp_ID','Patient_ID'),all.x = T)
sub_data$Registration_Date = as.numeric(as.Date(sub_data$Registration_Date,format='%d-%B-%y'))
sub_data$Camp_Start_Date = as.numeric(as.Date(sub_data$Camp_Start_Date,format='%d-%B-%y'))
sub_data$Camp_End_Date = as.numeric(as.Date(sub_data$Camp_End_Date,format='%d-%B-%y'))
sub_data$First_Interaction = as.numeric(as.Date(sub_data$First_Interaction,format='%d-%B-%y'))
sub_data$Donation = NULL
sub_data$Last_Stall_Visited_Number = NULL

sub_data$How_old<- as.numeric(as.Date("10/22/2016",format="%m/%d/%Y")) - sub_data$Registration_Date
sub_data$Income <- as.numeric(sub_data$Income)
sub_data$CampDuration<- sub_data$Camp_End_Date - sub_data$Camp_Start_Date
sub_data$How_old_interaction <- as.numeric(as.Date("10/22/2016",format="%m/%d/%Y")) - sub_data$First_Interaction
sub_data$Online_Follower <- as.factor(sub_data$Online_Follower)
sub_data$LinkedIn_Shared <- as.factor(sub_data$LinkedIn_Shared)
sub_data$Twitter_Shared <- as.factor(sub_data$Twitter_Shared)
sub_data$Facebook_Shared <- as.factor(sub_data$Facebook_Shared)
sub_data$Category3<-as.factor(sub_data$Category3)
sub_data$days_between_registration_CampStart <- sub_data$Camp_Start_Date - sub_data$Registration_Date
sub_data$RegisteredDuringCamp <- ifelse((sub_data$days_between_registration_CampStart==0),0,1)
sub_data$RegisteredDuringCamp <- as.factor(sub_data$RegisteredDuringCamp)


sub_data = merge(x=sub_data,y=patients,by='Patient_ID',all.x=T,all.y = F)

sub_data = merge(x=sub_data,y=for_sub_data,by='Patient_ID',all.x=T,all.y=F)

tr_score1 = as.numeric(!is.na(data$Health_Score))
tr_score2 = as.numeric(!is.na(data$Health.Score))

inp<-data
inp$Patient_ID=NULL
inp$Health_Camp_ID=NULL
inp$Registration_Date=NULL
inp$Camp_Start_Date=NULL
inp$Camp_End_Date=NULL
inp$First_Interaction=NULL
inp$Health_Score=NULL
inp$Health.Score=NULL
inp$Number_of_stall_visited=NULL

inp$Education_Score=NULL
inp$Age=NULL
inp$City_Type=NULL
inp$Employer_Category=NULL
inp$days_between_registration_CampStart=NULL
#cc<-cbind(inp,tr_score1)
#aa<-rfImpute(inp,tr_score1)
#table(is.na(inp[,1]))
inp[,14][is.na(inp[,14])]=mean(inp[,14],na.rm = T)
inp[,17][is.na(inp[,17])]=1

library(FSelector)
tr_n<-cbind(inp,tr_score1)
weights <- information.gain(tr_score1~., data=tr_n)
print(weights)
subset <- cutoff.k(weights, 2)
print(subset)

rf1<-randomForest(inp,tr_score1)

inp<-sub_data
inp$Patient_ID=NULL
inp$Health_Camp_ID=NULL
inp$Registration_Date=NULL
inp$Camp_Start_Date=NULL
inp$Camp_End_Date=NULL
inp$First_Interaction=NULL
inp$Health_Score=NULL
inp$Health.Score=NULL
inp$Number_of_stall_visited=NULL

inp$Education_Score=NULL
inp$Age=NULL
inp$City_Type=NULL
inp$Employer_Category=NULL
inp$days_between_registration_CampStart=NULL
#cc<-cbind(inp,tr_score1)
#aa<-rfImpute(inp,tr_score1)
#table(is.na(inp[,1]))
inp[,14][is.na(inp[,14])]=mean(inp[,14],na.rm = T)
inp[,17][is.na(inp[,17])]=1

pr<-predict(rf,inp) #Note now inp is test set


library(caTools)
set.seed(101) 
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

train_score1 = as.numeric(!is.na(train$Health_Score))
train_score2 = as.numeric(!is.na(train$Health.Score))
# train_score1 = as.numeric(is.na(train$Health_Score))
# train_score2 = as.numeric(is.na(train$Health.Score))
train_score3 = train$Number_of_stall_visited
train_score3[is.na(train_score3)] = 0
train_score3 = as.numeric(train_score3!=0)

train$Health_Score=NULL
train$Health.Score=NULL
train$Number_of_stall_visited=NULL



test_score1 = as.numeric(!is.na(test$Health_Score))
test_score2 = as.numeric(!is.na(test$Health.Score))
# test_score1 = as.numeric(is.na(test$Health_Score))
# test_score2 = as.numeric(is.na(test$Health.Score))
test_score3 = test$Number_of_stall_visited
test_score3[is.na(test_score3)] = 0
test_score3 = as.numeric(test_score3!=0)


test$Health_Score=NULL
test$Health_Score=NULL
test$Number_of_stall_visited=NULL

rf<-randomForest(train,train_score1)

library(unbalanced)
train_smote = ubSMOTE(train, factor(train_score1), k=5, perc.over = 100, perc.under=200)
train_score1 = train_smote$Y
train_1 = train_smote$X

'''
library(FSelector)
tr_n<-train_smote
weights <- information.gain(Y~., data=tr_n)
print(weights)
subset <- cutoff.k(weights, 2)
print(subset)
'''

train_smote = ubSMOTE(train, factor(train_score2), k=5, perc.over = 100, perc.under=200)
train_score2 = train_smote$Y
train_2 = train_smote$X

train_smote = ubSMOTE(train, factor(train_score3), k=5, perc.over = 100, perc.under=200)
train_score3 = train_smote$Y
train_3 = train_smote$X


library(randomForest)
tr1<-na.omit(train_smote)
rf<-randomForest(tr1$X,tr1$Y)




library(h2o)
library(data.table)
library(dplyr)



h2o.server <- h2o.init( nthreads= -1)

## Preprocessing the training data

#Converting all columns to factors
selCols = names(train_1)
train_1 = train_1[,(selCols) := lapply(.SD, as.factor), .SDcols = selCols]

subHex = as.h2o(sub_data)

train_1 = cbind(train_1,Y=train_score1)
test_1 = cbind(test,Y=test_score1)
#Converting to H2o Data frame & splitting
train.hex1 = as.h2o(train_1)
testHex1 = as.h2o(test)
features=names(train.hex1)[-ncol(train.hex1)]


gbmF_model_1 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =testHex1 ,
                        max_depth = 3,
                        distribution = "bernoulli",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =testHex1 ,
                        max_depth = 3,
                        distribution = "bernoulli",
                        ntrees =430,
                        learn_rate = 0.04,
                        nbins_cats = 5891
)




dl_model_1 = h2o.deeplearning( x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)


dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
)


dl_model_3 = h2o.deeplearning( x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=120,
                               adaptive_rate =F
)

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = testHex1,type="") )
pred1_1 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = subHex,type="") )
sub1_1 = sub_pred_score1$p1
pred = pred1_1
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_2, newdata = testHex1,type="") )
pred1_2 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_2, newdata = subHex,type="") )
sub1_2 = sub_pred_score1$p1
pred = pred1_2
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = testHex1,type="") )
pred1_3 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = subHex,type="") )
sub1_3 = sub_pred_score1$p1
pred = pred1_3 
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata = testHex1,type="") )
pred1_4 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata = subHex,type="") )
sub1_4 = sub_pred_score1$p1
pred = pred1_4
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_3, newdata = testHex1,type="") )
pred1_5 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_3, newdata = subHex,type="") )
sub1_5 = sub_pred_score1$p1
pred = pred1_5
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

final1=rowMeans(data.frame(pred1_1,pred1_2,pred1_3,pred1_4,pred1_5))
pred = final1
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

sub1=rowMeans(data.frame(sub1_1,sub1_2,sub1_3,sub1_4,sub1_5))


###############################################

train_1 = cbind(train_2,Y=train_score2)
test_1 = cbind(test,Y=test_score2)
#Converting to H2o Data frame & splitting
train.hex1 = as.h2o(train_1)
testHex1 = as.h2o(test_1)
features=names(train.hex1)[-ncol(train.hex1)]


gbmF_model_1 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =testHex1 ,
                        max_depth = 3,
                        distribution = "bernoulli",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =testHex1 ,
                        max_depth = 3,
                        distribution = "bernoulli",
                        ntrees =430,
                        learn_rate = 0.04,
                        nbins_cats = 5891
)




dl_model_1 = h2o.deeplearning( x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)


dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
)


dl_model_3 = h2o.deeplearning( x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=120,
                               adaptive_rate =F
)

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = testHex1,type="") )
pred1_1 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = subHex,type="") )
sub1_1 = sub_pred_score1$p1
pred = pred1_1
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_2, newdata = testHex1,type="") )
pred1_2 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_2, newdata = subHex,type="") )
sub1_2 = sub_pred_score1$p1
pred = pred1_2
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = testHex1,type="") )
pred1_3 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = subHex,type="") )
sub1_3 = sub_pred_score1$p1
pred = pred1_3 
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata = testHex1,type="") )
pred1_4 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata = subHex,type="") )
sub1_4 = sub_pred_score1$p1
pred = pred1_4
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_3, newdata = testHex1,type="") )
pred1_5 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_5, newdata = subHex,type="") )
sub1_5 = sub_pred_score1$p1
pred = pred1_5
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

final2=rowMeans(data.frame(pred1_1,pred1_2,pred1_3,pred1_4,pred1_5))
pred = final2
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

sub2=rowMeans(data.frame(sub1_1,sub1_2,sub1_3,sub1_4,sub1_5))

###################################333


train_1 = cbind(train_3,Y=train_score3)
test_1 = cbind(test,Y=test_score3)
#Converting to H2o Data frame & splitting
train.hex1 = as.h2o(train_1)
testHex1 = as.h2o(test_1)
features=names(train.hex1)[-ncol(train.hex1)]


gbmF_model_1 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =testHex1 ,
                        max_depth = 3,
                        distribution = "bernoulli",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =testHex1 ,
                        max_depth = 3,
                        distribution = "bernoulli",
                        ntrees =430,
                        learn_rate = 0.04,
                        nbins_cats = 5891
)




dl_model_1 = h2o.deeplearning( x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)


dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
)


dl_model_3 = h2o.deeplearning( x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =testHex1 ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=120,
                               adaptive_rate =F
)

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = testHex1,type="") )
pred1_1 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = subHex,type="") )
sub1_1 = sub_pred_score1$p1
pred = pred1_1
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_2, newdata = testHex1,type="") )
pred1_2 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_2, newdata = subHex,type="") )
sub1_2 = sub_pred_score1$p1
pred = pred1_2
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = testHex1,type="") )
pred1_3 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = subHex,type="") )
sub1_3 = sub_pred_score1$p1
pred = pred1_3 
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata = testHex1,type="") )
pred1_4 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata = subHex,type="") )
sub1_4 = sub_pred_score1$p1
pred = pred1_4
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_3, newdata = testHex1,type="") )
pred1_5 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_3, newdata = subHex,type="") )
sub1_5 = sub_pred_score1$p1
pred = pred1_5
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

final3=rowMeans(data.frame(pred1_1,pred1_2,pred1_3,pred1_4,pred1_5))
pred = final3
table(pred>mean(pred),as.vector(testHex1$Y>0))
auc(roc(pred,factor(as.vector(testHex1$Y>0))))

sub3=rowMeans(data.frame(sub1_1,sub1_2,sub1_3,sub1_4,sub1_5))


#########################################################
pred_final=apply(cbind(final1,final2,final3),1,max)
labels = (test_score1+test_score2+test_score3)>0
table(pred_final>mean(pred_final),labels)
auc(roc(pred_final,factor(labels)))

pred_final=apply(cbind(final1,final2,final3),1,min)
labels = (test_score1+test_score2+test_score3)>0
table(pred_final>mean(pred_final),labels)
auc(roc(pred_final,factor(labels)))

pred_final=apply(cbind(final1,final2,final3),1,mean)
labels = (test_score1+test_score2+test_score3)>0
table(pred_final>mean(pred_final),labels)
auc(roc(pred_final,factor(labels)))

pred_max = apply(cbind(final1,final2,final3),1,max)
pred_min = apply(cbind(final1,final2,final3),1,min)
pred_final = ifelse(pred_max>0.5,pred_max,pred_min)
table(pred_final>mean(pred_final),labels)
auc(roc(pred_final,factor(labels)))

sub_max=apply(cbind(sub1,sub2,sub3),1,max)
sub_min = apply(cbind(sub1,sub2,sub3),1,min)
sub_final = ifelse(sub_max>0.5,sub_max,sub_min)
submit=data.frame(Patient_ID=sub_data$Patient_ID,Health_Camp_ID=sub_data$Health_Camp_ID,Outcome=sub_max)
write.csv(submit,'sub_Max_min_h2o.csv',row.names = F)
