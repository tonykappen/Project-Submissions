library(corrplot)
library(pracma)
library(caret)
library(caTools)
library(kernlab)
library(ggplot2)
library(party)
library(rpart)
library(rpart.plot)
library(rattle)


setwd("E:/AML - BUAN 6341")
colNames = c ("age", "workclass", "fnlwgt", "education", 
              "educationnum", "maritalstatus", "occupation",
              "relationship", "race", "sex", "capitalgain",
              "capitalloss", "hoursperweek", "nativecountry",
              "class")
data<-read.csv('adult_train.csv',header = FALSE, sep=",",strip.white=TRUE, 
                      col.names=colNames,na.strings = "?", stringsAsFactors = TRUE)
#head(adult_train[13])
#is.factor(adult_train[13])
head(data)
str(data)

#Clean Data
table (complete.cases (data))

# Summarize all data sets with NAs only
summary  (data[!complete.cases(data),])

# Distribution of the income level factor(dependent variable) in the entire training data set.
table (data$class)
print(prop.table(table(data$class)))

#Removing Datasets with NA from train and test
data = data [!is.na (data$workclass) & !is.na (data$occupation), ]
data = data [!is.na (data$nativecountry), ]


table (complete.cases (data))

table (data$class)
str(data)

#Setting fnlwgt and educationnum to null as they dont seem to contribute much to the prediction
data$fnlwgt = NULL
data$educationnum = NULL


summary(data)
str(data)
data$workclass = as.character(data$workclass)
data$occupation = as.character(data$occupation)
data$nativecountry = as.character(data$nativecountry)
data$education = as.character(data$education)
data$race = as.character(data$race)
data$marital = as.character(data$marital)

#Reducing num of levels for Categorical variables
data$workclass = gsub("^Federal-gov","Federal-Govt",data$workclass)
data$workclass = gsub("^Local-gov","Other-Govt",data$workclass)
data$workclass = gsub("^State-gov","Other-Govt",data$workclass)
data$workclass = gsub("^Private","Private",data$workclass)
data$workclass = gsub("^Self-emp-inc","Self-Employed",data$workclass)
data$workclass = gsub("^Self-emp-not-inc","Self-Employed",data$workclass)
data$workclass = gsub("^Without-pay","Not-Working",data$workclass)
data$workclass = gsub("^Never-worked","Not-Working",data$workclass)

data$occupation = gsub("^Adm-clerical","Admin",data$occupation)
data$occupation = gsub("^Armed-Forces","Military",data$occupation)
data$occupation = gsub("^Craft-repair","Blue-Collar",data$occupation)
data$occupation = gsub("^Exec-managerial","White-Collar",data$occupation)
data$occupation = gsub("^Farming-fishing","Blue-Collar",data$occupation)
data$occupation = gsub("^Handlers-cleaners","Blue-Collar",data$occupation)
data$occupation = gsub("^Machine-op-inspct","Blue-Collar",data$occupation)
data$occupation = gsub("^Other-service","Service",data$occupation)
data$occupation = gsub("^Priv-house-serv","Service",data$occupation)
data$occupation = gsub("^Prof-specialty","Professional",data$occupation)
data$occupation = gsub("^Protective-serv","Other-Occupations",data$occupation)
data$occupation = gsub("^Sales","Sales",data$occupation)
data$occupation = gsub("^Tech-support","Other-Occupations",data$occupation)
data$occupation = gsub("^Transport-moving","Blue-Collar",data$occupation)

data$nativecountry[data$nativecountry=="Cambodia"] = "Asia"
data$nativecountry[data$nativecountry=="Canada"] = "North-America"    
data$nativecountry[data$nativecountry=="China"] = "Asia"     
data$nativecountry[data$nativecountry=="Columbia"] = "South-America"    
data$nativecountry[data$nativecountry=="Cuba"] = "North-America"      
data$nativecountry[data$nativecountry=="Dominican-Republic"] = "North-America"
data$nativecountry[data$nativecountry=="Ecuador"] = "South-America"     
data$nativecountry[data$nativecountry=="El-Salvador"] = "South-America" 
data$nativecountry[data$nativecountry=="England"] ="Europe"
data$nativecountry[data$nativecountry=="France"] = "Europe"
data$nativecountry[data$nativecountry=="Germany"] = "Europe"
data$nativecountry[data$nativecountry=="Greece"] = "Europe"
data$nativecountry[data$nativecountry=="Guatemala"] ="North-America"
data$nativecountry[data$nativecountry=="Haiti"] = "North-America"
data$nativecountry[data$nativecountry=="Holand-Netherlands"] = "Europe"
data$nativecountry[data$nativecountry=="Honduras"] = "North-America"
data$nativecountry[data$nativecountry=="Hong"] = "Asia"
data$nativecountry[data$nativecountry=="Hungary"] = "Europe"
data$nativecountry[data$nativecountry=="India"] = "Asia"
data$nativecountry[data$nativecountry=="Iran"] = "Asia"
data$nativecountry[data$nativecountry=="Ireland"] = "Europe"
data$nativecountry[data$nativecountry=="Italy"] = "Europe"
data$nativecountry[data$nativecountry=="Jamaica"] = "North-America"
data$nativecountry[data$nativecountry=="Japan"] = "Asia"
data$nativecountry[data$nativecountry=="Laos"] = "Asia"
data$nativecountry[data$nativecountry=="Mexico"] = "North-America"
data$nativecountry[data$nativecountry=="Nicaragua"] = "North-America"
data$nativecountry[data$nativecountry=="Outlying-US(Guam-USVI-etc)"] = "North-America"
data$nativecountry[data$nativecountry=="Peru"] = "South-America"
data$nativecountry[data$nativecountry=="Philippines"] = "Asia"
data$nativecountry[data$nativecountry=="Poland"] = "Europe"
data$nativecountry[data$nativecountry=="Portugal"] = "Europe"
data$nativecountry[data$nativecountry=="Puerto-Rico"] = "North-America"
data$nativecountry[data$nativecountry=="Scotland"] = "Europe"
data$nativecountry[data$nativecountry=="South"] = "Europe"
data$nativecountry[data$nativecountry=="Taiwan"] = "Asia"
data$nativecountry[data$nativecountry=="Thailand"] = "Asia"
data$nativecountry[data$nativecountry=="Trinadad&Tobago"] = "North-America"
data$nativecountry[data$nativecountry=="United-States"] ="North-America"
data$nativecountry[data$nativecountry=="Vietnam"] = "Asia"
data$nativecountry[data$nativecountry=="Yugoslavia"] = "Europe"

data$education = gsub("^10th","Dropout",data$education)
data$education = gsub("^11th","Dropout",data$education)
data$education = gsub("^12th","Dropout",data$education)
data$education = gsub("^1st-4th","Dropout",data$education)
data$education = gsub("^5th-6th","Dropout",data$education)
data$education = gsub("^7th-8th","Dropout",data$education)
data$education = gsub("^9th","Dropout",data$education)
data$education = gsub("^Assoc-acdm","Associates",data$education)
data$education = gsub("^Assoc-voc","Associates",data$education)
data$education = gsub("^Bachelors","Bachelors",data$education)
data$education = gsub("^Doctorate","Doctorate",data$education)
data$education = gsub("^HS-Grad","HS-Graduate",data$education)
data$education = gsub("^Masters","Masters",data$education)
data$education = gsub("^Preschool","Dropout",data$education)
data$education = gsub("^Prof-school","Prof-School",data$education)
data$education = gsub("^Some-college","HS-Graduate",data$education)

data$marital[data$marital=="Married-AF-spouse"] = "Married"
data$marital[data$marital=="Married-civ-spouse"] = "Married"
data$marital[data$marital=="Married-spouse-absent"] = "Not-Married"
data$marital[data$marital=="Separated"] = "Not-Married"
data$marital[data$marital=="Divorced"] = "Not-Married"

data$race[data$race=="Amer-Indian-Eskimo"] = "American-Indian"
data$race[data$race=="Asian-Pac-Islander"] = "Asian"

is.na(data) = data=='?'
is.na(data) = data==' ?'
data = na.omit(data)

data$marital = factor(data$marital)
data$education = factor(data$education)
data$nativecountry = factor(data$nativecountry)
data$workclass = factor(data$workclass)
data$occupation = factor(data$occupation)
data$race = factor(data$race)
data$sex = factor(data$sex)
data$relationship = factor(data$relationship)
data$class<-as.factor(ifelse(data$class==data$class[1],0,1))
is.factor(data$class)
summary(data)


#Split Data into train and test sets
library(caTools)
set.seed(1)
split = sample.split(data$class, SplitRatio = 0.70)
adult_train = subset(data, split == TRUE)
adult_train<-adult_train[sample(1:nrow(adult_train)),]
print(prop.table(table(adult_train$class)))
adult_test = subset(data, split == FALSE)
head(adult_test)
is.na(adult_test) = adult_test=='?'
is.na(adult_test) = adult_test==' ?'
adult_test = na.omit(adult_test)
is.factor(adult_train$class)


#SVM
library(kernlab)
library(ROCR)
#Scaling
scaled_adult_train<-adult_train
scaled_adult_test<-adult_test
#Scaling Numeric Variables for SVM
scaled_adult_train[,c(1,9,10,11)]<-scale(scaled_adult_train[,c(1,9,10,11)])
scaled_adult_test[,c(1,9,10,11)]<-scale(scaled_adult_test[,c(1,9,10,11)])
pred<-data.frame()

#SVM
#SVM Function
set.seed(1)
svm_classifier<-function(train_data,test_data,k){
  time_train<-system.time(svm<-ksvm(class~.,data=train_data,type="C-svc",kernel=k,prob.model=TRUE,C=1))
  time_test<-system.time(svm_pred<-predict(svm,newdata=test_data[-13],type='prob'))
  svm_pred_con<-predict(svm,newdata=test_data[-13],type='response')
  pred<-prediction(svm_pred[,2],test_data$class)  
  performance<-performance(pred,"tpr","fpr")
  dd<-data.frame(FP = performance@x.values[[1]], TP = performance@y.values[[1]])
  confusion<-confusionMatrix(svm_pred_con,test_data$class)
  auc<-performance(pred, measure = 'auc')@y.values[[1]]
  results<-list(Confustion_Matrix=confusion,AUC=auc,plotdata=dd,Accuracy=confusion$overall[1],time_train,time_test)
  return(results)
}

svm_train_radial<-svm_classifier(train_data = scaled_adult_train,test_data = scaled_adult_train,k = "rbfdot")
svm_test_radial<-svm_classifier(train_data = scaled_adult_train,test_data = scaled_adult_test,k = "rbfdot")
svm_train_linear<-svm_classifier(train_data = scaled_adult_train,test_data = scaled_adult_train,k = "vanilladot")
svm_test_linear<-svm_classifier(train_data = scaled_adult_train,test_data = scaled_adult_test,k = "vanilladot")
svm_train_laplace<-svm_classifier(train_data = scaled_adult_train,test_data = scaled_adult_train,k = "laplacedot")
svm_test_laplace<-svm_classifier(train_data = scaled_adult_train,test_data = scaled_adult_test,k = "laplacedot")

#Train and Test Accuracies and Confusion Matrices of All 3 kernels
svm_test_linear$Accuracy
svm_test_radial$Accuracy
svm_test_laplace$Accuracy
svm_train_linear$Accuracy
svm_train_radial$Accuracy
svm_train_laplace$Accuracy

svm_test_linear$AUC
svm_test_radial$AUC
svm_test_laplace$AUC
svm_train_linear$AUC
svm_train_radial$AUC
svm_train_laplace$AUC

svm_train_linear$Confustion_Matrix
svm_train_radial$Confustion_Matrix
svm_train_laplace$Confustion_Matrix
svm_test_linear$Confustion_Matrix
svm_test_radial$Confustion_Matrix
svm_test_laplace$Confustion_Matrix

#Train and Test ROCs for all 3 kernels
roc_radial_train <-geom_line(data =svm_train_radial$plotdata, aes(x = FP, y = TP, color = 'Radial_SVM_Train'))
roc_radial_test <-geom_line(data =svm_test_radial$plotdata, aes(x = FP, y = TP, color = 'Radial_SVM_Test'))
roc_linear_train<-geom_line(data =svm_train_linear$plotdata, aes(x = FP, y = TP, color = 'linear_SVM_Train'))
roc_linear_test<- geom_line(data =svm_test_linear$plotdata, aes(x = FP, y = TP, color = 'linear_SVM_Test'))
roc_laplace_train<-geom_line(data =svm_train_laplace$plotdata, aes(x = FP, y = TP, color = 'laplace_SVM_Train'))
roc_laplace_test<-geom_line(data =svm_test_laplace$plotdata, aes(x = FP, y = TP, color = 'laplace_SVM_Test'))


#plotting ROC for test
ggplot()+roc_radial_test+
  roc_linear_test+
  roc_laplace_test+
  xlab('False Positive')+
  ylab('True Positive')+
  annotate("text", x=0.50, y=0.30, hjust=0, vjust=0, size=5,
           label=paste("AUC(linear) =", round(svm_test_linear$AUC, 3)))+
  annotate("text", x=0.50, y=0.15, hjust=0, vjust=0, size=5,
           label=paste("AUC(radial) =", round(svm_test_radial$AUC, 3)))+
  annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC(laplace) =", round(svm_test_laplace$AUC, 3)))+
  scale_colour_manual(name="Legend",
                      values=c(Radial_SVM_Test="red",laplace_SVM_Test="blue",linear_SVM_Test='green'))



#Learning Curves
#Error vs Train Size and Train and Test Time

svm_error<-function(train_data,test_data,k){
  i<-1
  j<-1
  predict_train<-c()
  predict_test<-c()
  train_error<-c()
  test_error<-c()
  train_time<-c()
  test_time<-c()
  range<-c(100,1000,5000,8000)
  for(i in range){
    predict_train[[j]]<-svm_classifier(train_data = train_data[1:i,],test_data = train_data[2:i,],k=k)
    predict_test[[j]]<-svm_classifier(train_data = train_data[1:i,],test_data = test_data[2:i,],k=k)
    train_error[j]<-1-as.numeric(predict_train[[j]][4])
    test_error[j]<-1-as.numeric(predict_test[[j]][4])
    train_time[j]<-predict_test[[j]][[5]][[3]]
    test_time[j]<-predict_test[[j]][[6]][[3]]
    j<-j+1
  }
  results<-list(Size=range,train_error=train_error,test_error=test_error,train_time=train_time,test_time=test_time)
}

linear_error<-svm_error(train_data = scaled_adult_train,test_data = scaled_adult_test,k = "vanilladot")
radial_error<-svm_error(train_data = scaled_adult_train,test_data = scaled_adult_test,k = "rbfdot")
laplace_error<-svm_error(train_data = scaled_adult_train,test_data = scaled_adult_test,k = "laplacedot")

radial_error

#Learning Curve Linear SVM Error vs Train size
ggplot()+
  geom_point(aes(x=linear_error$Size,y=linear_error$train_error,color='linear_Train'))+
  geom_line(aes(x=linear_error$Size,y=linear_error$train_error,color='linear_Train'))+
  geom_line(aes(x=linear_error$Size,y=linear_error$test_error,color='linear_Test'))+
  geom_point(aes(x=linear_error$Size,y=linear_error$test_error,color='linear_Test'))+
  ggtitle("Learning Curve linear SVM")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(linear_Train="red",linear_Test="blue"))
#Learning Curve Radial SVM Error vs Train size
ggplot()+
  geom_point(aes(x=radial_error$Size,y=radial_error$train_error,color='radial_Train'))+
  geom_line(aes(x=radial_error$Size,y=radial_error$train_error,color='radial_Train'))+
  geom_line(aes(x=radial_error$Size,y=radial_error$test_error,color='radial_Test'))+
  geom_point(aes(x=radial_error$Size,y=radial_error$test_error,color='radial_Test'))+
  ggtitle("Learning Curve radial SVM")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(radial_Train="red",radial_Test="blue"))

#Learning Curve Laplace SVM Error vs Train size
ggplot()+
  geom_point(aes(x=laplace_error$Size,y=laplace_error$train_error,color='laplace_Train'))+
  geom_line(aes(x=laplace_error$Size,y=laplace_error$train_error,color='laplace_Train'))+
  geom_line(aes(x=laplace_error$Size,y=laplace_error$test_error,color='laplace_Test'))+
  geom_point(aes(x=laplace_error$Size,y=laplace_error$test_error,color='laplace_Test'))+
  ggtitle("Learning Curve laplace SVM")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(laplace_Train="red",laplace_Test="blue"))




#Error vs Train and Test Time
#Learning Curve 3 kernels against Error vs Train Time
ggplot()+
  geom_point(aes(x=linear_error$train_time,y=linear_error$train_error,color='linear_Train'))+
  geom_line(aes(x=linear_error$train_time,y=linear_error$train_error,color='linear_Train'))+
  geom_point(aes(x=radial_error$train_time,y=radial_error$train_error,color='radial_Train'))+
  geom_line(aes(x=radial_error$train_time,y=radial_error$train_error,color='radial_Train'))+
  geom_point(aes(x=laplace_error$train_time,y=laplace_error$train_error,color='laplace_Train'))+
  geom_line(aes(x=laplace_error$train_time,y=laplace_error$train_error,color='laplace_Train'))+
  ggtitle("Learning Curve 3 kernels Train data")+
  xlab('Train time(sec)')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(linear_Train="red",radial_Train="blue",laplace_Train="green"))

#Learning Curve 3 kernels against Error vs Test Time
ggplot()+
  geom_point(aes(x=linear_error$test_time,y=linear_error$test_error,color='linear_test'))+
  geom_line(aes(x=linear_error$test_time,y=linear_error$test_error,color='linear_test'))+
  geom_point(aes(x=radial_error$test_time,y=radial_error$test_error,color='radial_test'))+
  geom_line(aes(x=radial_error$test_time,y=radial_error$test_error,color='radial_test'))+
  geom_point(aes(x=laplace_error$test_time,y=laplace_error$test_error,color='laplace_test'))+
  geom_line(aes(x=laplace_error$test_time,y=laplace_error$test_error,color='laplace_test'))+
  ggtitle("Learning Curve 3 kernels test data")+
  xlab('test time(sec)')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(linear_test="red",radial_test="blue",laplace_test="green"))



#cross validation with radial SVM
folds_svm = createFolds(scaled_adult_train$class, k = 10)
cv_svm = lapply(folds_svm, function(x) {
  training_fold_svm= scaled_adult_train[-x, ]
  test_fold_svm= scaled_adult_train[x, ]
  classifier_svm<-ksvm(class~.,data=training_fold_svm,type="C-svc",kernel="laplacedot",prob.model=TRUE,C=1)
  y_pred_svm= predict(classifier_svm, newdata = test_fold_svm[-13])
  cm_svm = table(test_fold_svm[, 13], y_pred_svm)
  accuracy = (cm_svm[1,1] + cm_svm[2,2]) / (cm_svm[1,1] + cm_svm[2,2] + cm_svm[1,2] + cm_svm[2,1])
  return(accuracy)
})
accuracy_svm = mean(as.numeric(cv_svm))#accuracy reduced to 0.8420
accuracy_svm

#Decision Tree
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)

#Decision Tree Function
set.seed(1)
tree_classifier<-function(train.data,test.data){
  
  
  time_Train_tree<-system.time(tree<-rpart(class~.,train.data,parms = list(split='information'),
                                           method = 'class',cp=-1,maxdepth=10))
  time_Test_tree<-system.time(tree_pred<-predict(tree, newdata = test.data[-13], type = 'class'))
  tree_pred_roc<-predict(tree, newdata = test.data[-13], type = 'prob')
  pred_tree<-prediction(tree_pred_roc[,2],test.data$class)  
  performance_tree<-performance(pred_tree,"tpr","fpr")
  dd_tree<-data.frame(FP = performance_tree@x.values[[1]], TP = performance_tree@y.values[[1]])
  confusion_tree<-confusionMatrix(tree_pred,test.data$class)
  auc_tree<-performance(pred_tree, measure = 'auc')@y.values[[1]]
  ptree<-prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"],maxdepth=10)
  tree_pred_prune = predict(ptree, newdata = test.data[-13], type = 'class')
  tree_pred_prune_roc = predict(ptree, newdata = test.data[-13], type = 'prob')
  pred_tree_prune<-prediction(tree_pred_prune_roc[,2],test.data$class)  
  performance_tree_prune<-performance(pred_tree_prune,"tpr","fpr")
  dd_tree_prune<-data.frame(FP = performance_tree_prune@x.values[[1]], TP = performance_tree_prune@y.values[[1]])
  confusion_tree_prune<-confusionMatrix(tree_pred_prune,test.data$class)
  auc_tree_prune<-performance(pred_tree_prune, measure = 'auc')@y.values[[1]]
  results_tree<-list(Decision_Tree=tree,Confustion_Matrix=confusion_tree,AUC=auc_tree,plotdata=dd_tree,Accuracy=confusion_tree$overall[1],
                     Decision_Tree_prune=ptree,Confustion_Matrix_prune=confusion_tree_prune,AUC_prune=auc_tree_prune,plotdata_prune=dd_tree_prune,
                     Accuracy_prune=confusion_tree_prune$overall[1],
                     time_Train_tree=time_Train_tree,time_Test_tree=time_Test_tree)
}

tree_train<-tree_classifier(train.data = adult_train,test.data = adult_train)
tree_test<-tree_classifier(train.data =adult_train,test.data = adult_test)

#AUC and Accuracy for Train and Test sets
tree_train$AUC
tree_test$AUC
tree_train$AUC_prune
tree_test$AUC_prune
tree_train$Accuracy
tree_test$Accuracy
tree_train$Accuracy_prune
tree_test$Accuracy_prune

#Confusion Matrix
tree_test$Confustion_Matrix
tree_test$Confustion_Matrix_prune

#Roc for train and test sets
roc_tree_train <-geom_line(data =tree_train$plotdata, aes(x = FP, y = TP, color = 'Decision_Tree_Train'))
roc_tree_test <-geom_line(data =tree_test$plotdata, aes(x = FP, y = TP, color = 'Decision_Tree_Test'))
roc_ptree_train<-geom_line(data =tree_train$plotdata_prune, aes(x = FP, y = TP, color = 'Prune_Tree_Train'))
roc_ptree_test <-geom_line(data =tree_test$plotdata_prune, aes(x = FP, y = TP, color = 'Prune_Tree_Test'))

#plotting ROC for tree
ggplot()+roc_tree_test+roc_ptree_test+
  xlab('False Positive')+
  ylab('True Positive')+
  scale_colour_manual(name="Legend",
                      values=c(Decision_Tree_Test="red",Prune_Tree_Test="blue"))+
  annotate("text", x=0.45, y=0.15, hjust=0, vjust=0, size=5,
         label=paste("AUC(Tree) =", round(tree_test$AUC, 4)))+
  annotate("text", x=0.45, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC(pruned) =", round(tree_test$AUC_prune, 4)))



#Train Size vs Error
set.seed(1)
i=1
j=1
dec_lc<-list()
dec_error_test<-c()
dec_error_train<-c()
dec_predict_test<-list()
dec_prune_error_train<-c()
dec_prune_error_test<-c()
dec_time_Train<-c()
dec_time_Test<-c()
dec_prune_train_time<-c()
dec_prune_test_time<-c()
range<-c(100,1000,5000,8000)
for(i in range){
  dec_lc[[j]]<-tree_classifier(train.data = adult_train[1:i,],test.data = adult_train[1:i,])
  dec_predict_test[[j]]<-tree_classifier(train.data = adult_train[1:i,],test.data = adult_test[1:i,])
  dec_error_train[j]<-1-as.numeric(dec_lc[[j]][5])
  dec_error_test[j]<-1-as.numeric(dec_predict_test[[j]][5])
  dec_prune_error_train[j]<-1-as.numeric(dec_lc[[j]][10])
  dec_prune_error_test[j]<-1-as.numeric(dec_predict_test[[j]][10])
  dec_time_Train[j]<-dec_lc[[j]][[11]][[3]]
  dec_time_Test[j]<-dec_lc[[j]][[12]][[3]]
  j<-j+1
}

#Error vs Train Size
ggplot()+
  geom_point(aes(x=range,y=dec_error_train,color='Tree_Train'))+
  geom_line(aes(x=range,y=dec_error_train,color='Tree_Train'))+
  geom_line(aes(x=range,y=dec_error_test,color='Tree_Test'))+
  geom_point(aes(x=range,y=dec_error_test,color='Tree_Test'))+
  ggtitle("Learning Curve for Decision Tree")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(Tree_Train="red",Tree_Test="blue"))

ggplot()+
  geom_point(aes(x=range,y=dec_prune_error_train,color='Tree_Train_Pruned'))+
  geom_line(aes(x=range,y=dec_prune_error_train,color='Tree_Train_Pruned'))+
  geom_line(aes(x=range,y=dec_prune_error_test,color='Tree_Test_Pruned'))+
  geom_point(aes(x=range,y=dec_prune_error_test,color='Tree_Test_Pruned'))+
  ggtitle("Learning Curve for Pruned Decision Tree")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(Tree_Train_Pruned="red",Tree_Test_Pruned="blue"))

#Train Error vs Train and Test Time
#Learning Curve  Error vs Train Time
ggplot()+
  geom_point(aes(x=dec_time_Train,y=dec_error_train,color='Tree_Train'))+
  geom_line(aes(x=dec_time_Train,y=dec_error_train,color='Tree_Train'))+
  
  ggtitle("Learning Curve Error vs Train Time")+
  xlab('Train time(sec)')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(Tree_Train="red"))

#Learning Curve  Error vs Test Time
ggplot()+
  geom_point(aes(x=dec_time_Test,y=dec_error_test,color='Tree_Test'))+
  geom_line(aes(x=dec_time_Test,y=dec_error_test,color='Tree_Test'))+
  ggtitle("Learning Curve vs Test Time")+
  xlab('test time(sec)')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(Tree_Test="blue"))


#Cross validation
folds_tree = createFolds(adult_train$class, k = 10)
cv_tree = lapply(folds_tree, function(x) {
  training_fold_tree= adult_train[-x, ]
  test_fold_tree= adult_train[x, ]
  classifier_tree=rpart(class~.,adult_train,parms = list(split='information'),
                        method = 'class')
  y_pred_tree= predict(classifier_tree, newdata = test_fold_tree[,-13],type="class")
  cm_tree = table(test_fold_tree[, 13], y_pred_tree)
  accuracy = (cm_tree[1,1] + cm_tree[2,2]) / (cm_tree[1,1] + cm_tree[2,2] + cm_tree[1,2] + cm_tree[2,1])
  return(accuracy)
})
accuracy_tree = mean(as.numeric(cv_tree))#accuracy = 0.842
accuracy_tree

#Boosting

library(ada)
set.seed(1)
ada_boost_classifier<-function(train.data,test.data){
  
  
  tree<-ada(class~.,train.data,iter=50, loss="logistic")
  tree_pred<-predict(tree, newdata = test.data[-13])
  tree_pred_roc<-predict(tree, newdata = test.data[-13], type = 'prob')
  pred_tree<-prediction(tree_pred_roc[,2],test.data$class)  
  performance_tree<-performance(pred_tree,"tpr","fpr")
  dd_tree<-data.frame(FP = performance_tree@x.values[[1]], TP = performance_tree@y.values[[1]])
  confusion_tree<-confusionMatrix(tree_pred,test.data$class)
  auc_tree<-performance(pred_tree, measure = 'auc')@y.values[[1]]
  
  results_tree<-list(Decision_Tree=tree,Confustion_Matrix=confusion_tree,AUC=auc_tree,plotdata=dd_tree,Accuracy=confusion_tree$overall[1])
}

ada_boost_prune<-function(train.data,test.data){
  
  tree<-ada(class~.,train.data,control=rpart::rpart.control(maxdepth=30,
                                                            cp=0.010000,
                                                            minsplit=20,
                                                            maxsplit=10,
                                                            xval=10),iter=50, loss="logistic")
  tree_pred<-predict(tree, newdata = test.data[-13])
  tree_pred_roc<-predict(tree, newdata = test.data[-13], type = 'prob')
  pred_tree<-prediction(tree_pred_roc[,2],test.data$class)  
  performance_tree<-performance(pred_tree,"tpr","fpr")
  dd_tree<-data.frame(FP = performance_tree@x.values[[1]], TP = performance_tree@y.values[[1]])
  confusion_tree<-confusionMatrix(tree_pred,test.data$class)
  auc_tree<-performance(pred_tree, measure = 'auc')@y.values[[1]]
  
  results_tree<-list(Decision_Tree=tree,Confustion_Matrix=confusion_tree,AUC=auc_tree,plotdata=dd_tree,Accuracy=confusion_tree$overall[1])
}


#implementing the ada boost
ada_boost_train <- ada_boost_classifier(train.data=adult_train,test.data= adult_train)
ada_boost_test <- ada_boost_classifier(train.data=adult_train, test.data=adult_test)

#Implementing adaboost prune
ada_train_prune <- ada_boost_prune(train.data=adult_train,test.data= adult_train)
ada_test_prune <- ada_boost_prune(train.data=adult_train, test.data=adult_test)

#AUC and Accuracy for Train and Test sets
ada_boost_train$AUC
ada_boost_test$AUC
ada_train_prune$AUC
ada_test_prune$AUC
ada_boost_train$Accuracy
ada_boost_test$Accuracy
ada_train_prune$Accuracy
ada_test_prune$Accuracy

ada_boost_test$Confustion_Matrix
ada_test_prune$Confustion_Matrix

#Roc for train and test sets
roc_tree_train <-geom_line(data =ada_boost_train$plotdata, aes(x = FP, y = TP, color = 'Boosted_Decision_Tree_Train'))
roc_tree_test <-geom_line(data =ada_boost_test$plotdata, aes(x = FP, y = TP, color = 'Boosted_Decision_Tree_Test'))
roc_ptree_train<-geom_line(data =ada_train_prune$plotdata, aes(x = FP, y = TP, color = 'Prune_Boosted_Tree_Train'))
roc_ptree_test <-geom_line(data =ada_test_prune$plotdata, aes(x = FP, y = TP, color = 'Prune_Boosted_Tree_Test'))

#plotting ROC for tree
ggplot()+roc_tree_test+roc_ptree_test+
  xlab('False Positive')+
  ylab('True Positive')+
  scale_colour_manual(name="Legend",
                      values=c(Boosted_Decision_Tree_Test="red",Prune_Boosted_Tree_Test="blue"))+
  annotate("text", x=0.50, y=0.15, hjust=0, vjust=0, size=5,
           label=paste("AUC(Tree) =", round(ada_boost_test$AUC, 3)))+
  annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC(pruned) =", round(ada_test_prune$AUC, 3)))

#Learning Curve Against Train Size 
i=1
j=1
dec_boost<-list()
dec_prune<-list()
dec_prune_predict_test<-list()
dec_error_test<-c()
dec_error_train<-c()
dec_predict_test<-list()
dec_prune_boost<-c()
dec_prune_error_train<-c()
dec_prune_error_test<-c()

range<-c(100,1000,5000,8000)
for(i in range){
  dec_boost[[j]]<-ada_boost_classifier(train.data = adult_train[1:i,],test.data = adult_train[1:i,])
  dec_predict_test[[j]]<-ada_boost_classifier(train.data = adult_train[1:i,],test.data = adult_test[1:i,])
  dec_error_train[j]<-1-as.numeric(dec_boost[[j]][5])
  dec_error_test[j]<-1-as.numeric(dec_predict_test[[j]][5])
  dec_prune[[j]]<-ada_boost_classifier(train.data = adult_train[1:i,],test.data = adult_train[1:i,])
  dec_prune_predict_test[[j]]<-ada_boost_classifier(train.data = adult_train[1:i,],test.data = adult_test[1:i,])
  dec_prune_error_train[j]<-1-as.numeric(dec_prune[[j]][5])
  dec_prune_error_test[j]<-1-as.numeric(dec_prune_predict_test[[j]][5])
  j<-j+1
}

ggplot()+
  geom_point(aes(x=range,y=dec_error_train,color='Tree_Train'))+
  geom_line(aes(x=range,y=dec_error_train,color='Tree_Train'))+
  geom_line(aes(x=range,y=dec_error_test,color='Tree_Test'))+
  geom_point(aes(x=range,y=dec_error_test,color='Tree_Test'))+
  ggtitle("Learning Curve for boosted Decision Tree")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(Tree_Train="red",Tree_Test="blue"))

ggplot()+
  geom_point(aes(x=range,y=dec_prune_error_train,color='Tree_Train_Pruned'))+
  geom_line(aes(x=range,y=dec_prune_error_train,color='Tree_Train_Pruned'))+
  geom_line(aes(x=range,y=dec_prune_error_test,color='Tree_Test_Pruned'))+
  geom_point(aes(x=range,y=dec_prune_error_test,color='Tree_Test_Pruned'))+
  ggtitle("Learning Curve for Pruned boosted Decision Tree")+
  xlab('Train Set Size')+
  ylab('Error')+
  scale_colour_manual(name="Legend",
                      values=c(Tree_Train_Pruned="red",Tree_Test_Pruned="blue"))





