library("dplyr")
library("readr")
setwd("D:/Spring2018/Pipeline/project/project_code")

test<- read_csv("test.csv")
train<- read_csv("train.csv")

#test <- read_csv("test_withoutUM.csv")
#train <- read_csv("train_withoutUM.csv")

#only urine
#train <- train[,28:41] %>% cbind(train$eGFR)
#names(train)[15]<-"eGFR"
#test <- test[,28:41] %>% cbind(test$eGFR)
#names(test)[15]<-"eGFR"

#only questionarie
#train <- train[,42:ncol(train)]
#test <- test[,42:ncol(test)]

#only blod
#train <- train[,1:27] %>% cbind(train$eGFR)
#names(train)[28]<-"eGFR"
#test <- test[,1:27] %>% cbind(test$eGFR)
#names(test)[28]<-"eGFR"

#without blood lab data
#train <- train[,28:ncol(train)]
#test <- test[,28:ncol(test)]

#without non-lab
#train <- train[,1:41]%>% cbind(train$eGFR)
#test <- test[,1:41]%>% cbind(test$eGFR)
#names(train)[42]<-"eGFR"
#names(test)[42]<-"eGFR"

#without urine
train <- train[,-c(28:41)]
test <- test[,-c(28:41)]

(train %>% filter(eGFR == 1) %>% nrow())/(train %>%nrow())


col <- c("eGFR","TroubleSleep", "Depressed", "Smoke100", "Education19", "HHIncome" )
#col <- c('eGFR')
train[col] <- lapply(train[col], factor)
test[col] <- lapply(test[col], factor)


common <- intersect(names(train), names(test)) 
for (p in common) { 
  if (class(train[[p]]) == "factor") { 
    levels(test[[p]]) <- levels(train[[p]]) 
  } 
}

train <- train %>% select(-SEQN)
test <- test %>% select(-SEQN)

test_result <- data.frame("true"=test$eGFR)
####################logistics regression with regularization
library("glmnet")
lr <- cv.glmnet(x = data.matrix(train%>%select(-eGFR)), 
                y = data.matrix(train%>%select(eGFR)%>%as.matrix()%>%as.factor()), 
                family = c("binomial"))
plot(lr)
lr$lambda.min
coef(lr, s = "lambda.min")
temp <- predict(lr,data.matrix(test%>%select(-eGFR)),
                s=lr$lambda.min,type = "class")
test_result["lr"] <- temp
sum(test_result$lr == test_result$true)/nrow(test)
#for variable importance
lr2 <- glmnet(x = data.matrix(train%>%select(-eGFR)),
              y = data.matrix(train%>%select(eGFR)%>%as.matrix()%>%as.factor()),
              family = c("binomial"),
              lambda = lr$lambda.min)
library(caret)
varImp(lr2, lambda = lr$lambda.min, scale = FALSE)

####################ensemble decsion trees
# random forest
library(randomForest)
rf <- randomForest(formula = eGFR ~ ., data=train)
#sapply(train, class)
#sapply(test, class)

test_result["rf"] <- predict(rf, test)
sum(test_result$rf == test_result$true)/nrow(test)
varImpPlot(rf, 
           sort = T,
           n.var=20,
           main="Top 10 - Variable Importance")

# Example adaboost
library(ada)
adaforest <- ada(formula = eGFR ~ ., data=train)
test_result["ada"] <- predict(adaforest, test)
sum(test_result$ada == test_result$true)/nrow(test)
varplot(adaforest)

# Example gradient boosted forest
library(gbm)
gforest <-  gbm(formula = (eGFR%>%as.character()) =="1" ~ ., distribution = "bernoulli", data=train)
test_gbm <- test
test_gbm$eGFR <- as.character(test_gbm$eGFR)
test_result["gbf"] <- predict(gforest, test_gbm, n.trees=gforest$n.trees)
test_result["gbf"] <- ifelse(test_result["gbf"]>0.5, 1, 0)
sum(test_result$gbf == test_result$true)/nrow(test)



####################kernel svm
library(e1071)
svm_tune <- tune(svm, train.x=eGFR ~ . , data=train, kernel="linear", 
                 ranges=list(cost=10^(-1:2)))
print(svm_tune)
svmModel <- svm(eGFR ~ . , train, cost=1, kernel="linear")
test_result["svm"] <- predict(svmModel, test)
sum(test_result$svm == test_result$true)/nrow(test)

library(kernlab)
svp <- ksvm(eGFR ~ . , train, type = "C-svc", C = 2, kernel = "polydot")
sum(predict(svp, test) == test_result$true)/nrow(test)
varImp(svp)


#####################Neural Network


#feature importance for NN:
#(1) lesion analysis --> how much worse is the prediction if you 
#remove the variable from the data set. (2) how much does the loss change
#when you change the variable's value fixing other variables in your data.
#Xtran, Ytran, Xtest, Ytest

#train = train[,28:ncol(train)]
#test = test[,28:ncol(test)]
Xtrain = train %>% select(-eGFR,-SEQN,-Diastolic_mmHg) %>% as.matrix()
Ytrain = train %>% select(eGFR) %>% as.matrix()

Xtest = test %>% select(-eGFR,-SEQN,-Diastolic_mmHg) %>% as.matrix()
Ytest = test %>% select(eGFR) %>% as.matrix()

Xwoumtrain = train_withoutUM %>% select(-eGFR,-SEQN,-Caffeine_mg) %>% as.matrix()
Ywoumtrain = train_withoutUM %>% select(eGFR) %>% as.matrix()

Xwoumtest = test_withoutUM %>% select(-eGFR,-SEQN,-Caffeine_mg) %>% as.matrix()
Ywoumtest = test_withoutUM %>% select(eGFR) %>% as.matrix()


### Specify the architecture
modelnn = keras_model_sequential() 
modelnn %>%
  layer_dense(units = 100, activation = 'tanh', input_shape = ncol(Xtrain))%>%
  #layer_dense(units = 100, activation = 'linear')%>%
  layer_dense(units = 100, activation = 'relu')%>%
  #layer_dropout(rate = 0.5)%>%
  layer_dense(units = 50, kernel_regularizer = regularizer_l2(l=0.001))%>%
  layer_dense(units = 1, activation = 'sigmoid')


summary(modelnn)


###############################################################################3
# Specify loss, batch size, optimizer, extra performance measures
modelnn %>% compile(
  loss = 'binary_crossentropy', #for binary predictions
  optimizer = optimizer_nadam(clipnorm = 10),
  metrics = c('accuracy' )
)


### Run model to learn weights
historynn = 
  modelnn %>% fit(Xtrain, Ytrain,
                  epochs = 30,
                  batch_size = 20,
                  validation_split = 0.2, shuffle=T
  )

plot(historynn)
###########################################################################

#evaluate accuracy
modelnn %>% evaluate(Xtest, Ytest)

#do prediction on Xtest
prediction1 = modelnn %>% predict(Xtest)

#get weights 
modelnn %>% get_weights()


#plot truth(blue) + prediction(red)
plot(to_binary,col="red")
points(Ytest,col="blue")

NN_pred = ROCR::prediction(prediction1,as.numeric(Ytest))
NN_rates = ROCR::performance(NN_pred,"tpr","fpr")
NN_auc = ROCR::performance(NN_pred, measure = "auc")
NN_avg_auc = mean(unlist(NN_auc@y.values))


#0.71
plot(NN_rates, avg="vertical",pin=c(10,10))

#0.75
NN_avg_auc





##########################################################

modelwoum = keras_model_sequential() 
modelwoum %>%
  layer_dense(units = 100, activation = 'tanh', input_shape = ncol(Xwoumtrain))%>%
  #layer_dense(units = 100, activation = 'linear')%>%
  layer_dense(units = 50, activation = 'relu')%>%
  #layer_dropout(rate = 0.5)%>%
  layer_dense(units = 50, kernel_regularizer = regularizer_l2(l=0.001))%>%
  layer_dense(units = 1, activation = 'sigmoid')


summary(modelwoum)

###############################################################################3
# Specify loss, batch size, optimizer, extra performance measures
modelwoum %>% compile(
  loss = 'binary_crossentropy', #for binary predictions
  optimizer = optimizer_nadam(clipnorm = 10),
  metrics = c('accuracy' )
)


### Run model to learn weights
historywoum = 
  modelwoum %>% fit(Xwoumtrain, Ywoumtrain,
                    epochs = 30,
                    batch_size = 10,
                    validation_split = 0.2, shuffle=T
  )
plot(historywoum)



###########################################################################

#evaluate accuracy
modelwoum %>% evaluate(Xwoumtest, Ywoumtest)

#do prediction on Xtest
prediction2 = modelwoum %>% predict(Xwoumtest)

#get weights 
modelnn %>% get_weights()


#plot truth(blue) + prediction(red)
plot(prediction2,col="red")
points(Ywoumtest,col="blue")

NN_pred2 = ROCR::prediction(prediction2,as.numeric(Ywoumtest))
NN_rates2 = ROCR::performance(NN_pred2,"tpr","fpr")
NN_auc2 = ROCR::performance(NN_pred2, measure = "auc")
NN_avg_auc2 = mean(unlist(NN_auc2@y.values))

plot(NN_rates2, avg="vertical",pin=c(10,10))

#0.7978
NN_avg_auc2
