setwd("C:\\Users\\jyoti\\OneDrive\\Documents\\PGPBABI\\Data Mining")
loandata = read.csv("loandata.csv")
str(loandata)
names(loandata)
prop.table(table(loandata$delinquent))

loandata$Sdelinquent = as.factor(loandata$Sdelinquent)

library(caTools)

set.seed(123)
split = sample.split(loandata$delinquent, SplitRatio = 0.70)
traindata = subset(loandata, split == TRUE)
testdata = subset(loandata, split == FALSE)
View(traindata)
View(testdata)

prop.table(table(traindata$delinquent))
prop.table(table(testdata$delinquent))


library(rpart)
library(rpart.plot)
set.seed(420)

tree_full = rpart(formula = delinquent~., data = traindata, minsplit=5, minbucket=1)
tree_full
rpart.plot(tree_full, cex=0.4)
printcp(tree_full)

traindata$predict.class = predict(tree_full,traindata,type="class")
traindata$predict.score=predict(tree_full,traindata)
View(traindata)
tabtrain=with(traindata,table(delinquent,predict.class))
tabtrain

TN_train = tabtrain[1,1]
TP_train = tabtrain[2,2]
FN_train = tabtrain[2,1]
FP_train = tabtrain[1,2]

train_acc = (TN_train+TP_train)/(TN_train+TP_train+FN_train+FP_train)
train_acc

train_sens = TP_train/(TP_train+FN_train)
train_sens


train_spec = TN_train/(TN_train+FP_train)
train_spec



## Predict using the CART model
testdata$predict.class=predict(tree_full,testdata,type="class")
testdata$predict.score=predict(tree_full,testdata)

## Creating the confusion matrix
tabtest=with(testdata,table(delinquent,predict.class))
tabtest

TN_test = tabtest[1,1]
TP_test = tabtest[2,2]
FN_test = tabtest[2,1]
FP_test = tabtest[1,2]

test_acc = (TN_test+TP_test)/(TN_test+TP_test+FN_test+FP_test)
test_acc

test_sens = TP_test/(TP_test+FN_test)
test_sens


test_spec = TN_test/(TN_test+FP_test)
test_spec

df_results_train = data.frame(train_acc, train_sens, train_spec)
names(df_results_train) = c("ACC", "SENS", "SPEC")
df_results_test = data.frame(test_acc, test_sens, test_spec)
names(df_results_test) = c("ACC", "SENS", "SPEC")

?rbind
df_fin =rbind(df_results_train, df_results_test)
row.names(df_fin) = c('tree_full_train', 'tree_full_test')
df_fin


traindata$predict.class = NULL
traindata$predict.score = NULL
testdata$predict.class = NULL
testdata$predict.score = NULL


set.seed(123)
?rpart

tree_manual_prune=rpart(formula = delinquent ~ ., data = traindata, method="class",control = rpart.control(minsplit = 50,  minbucket = 15))



rpart.plot(tree_manual_prune, cex=0.8)
print(tree_manual_prune)


printcp(tree_full)
plotcp(tree_full)

bestcp=tree_full$cptable[which.min(tree_full$cptable[,"xerror"]),"CP"]
bestcp


ptree=prune(tree_full,cp=bestcp)
print(ptree)
rpart.plot(ptree, cex = 0.8)
ptree
