setwd ("D:\\Business Analytics\\Project 4 (HR)\\")

hr_train = read.csv("D:\\Business Analytics\\Project 4 (HR)\\hr_train.csv",stringsAsFactors = F)
hr_test = read.csv("D:\\Business Analytics\\Project 4 (HR)\\hr_test.csv",stringsAsFactors = F)

library(dplyr)
glimpse(hr_train)

hr_test$left = NA
hr_test$data = "test"
hr_train$data = "train"

hr_all = rbind(hr_train,hr_test)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

hr_all = CreateDummies(hr_all,"sales",100)
hr_all = CreateDummies(hr_all,"salary",100)

glimpse(hr_all)

for(col in names(hr_all)){
  if(sum(is.na(hr_all[,col]))>0 & !(col %in% c("data","left"))){
    hr_all[is.na(hr_all[,col]),col]=mean(hr_all[hr_all$data=='train',col],na.rm=T)
  }
}



# Seprate train and test

hr_train = hr_all %>% 
  filter(data == "train") %>% 
  select(-data)

hr_test = hr_all %>% 
  filter(data == "test") %>% 
  select(-data,-left)

# for validation divide train data into two

set.seed(2)
s=sample(1:nrow(hr_train),0.8*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

library(car)
lin_fit = lm(left~.,data=hr_train1)
sort(vif(log_fit),decreasing = T)[1:3]

log_fit = glm(left~.,data=hr_train1,family = "binomial")
log_fit = step(log_fit)

formula(log_fit)

log_fit = glm(left ~ satisfaction_level + last_evaluation + number_project + 
                average_montly_hours + time_spend_company + Work_accident + 
                promotion_last_5years + sales_accounting + 
                salary_medium + salary_low,data = hr_train1,family = "binomial")
summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = hr_train2,type='response')

auc(roc(hr_train2$left,val.score))

# on entire training data

log_fit_final = glm(left ~ satisfaction_level + last_evaluation + number_project + 
                      average_montly_hours + time_spend_company + Work_accident + 
                      promotion_last_5years +  
                      salary_medium + salary_low,data = hr_train,family = "binomial")
summary(log_fit_final)

test.prob.score= predict(log_fit_final,newdata = hr_test,type='response')
write.table(test.prob.score,"Amar_Shinde_P4_part2.csv",row.names = F,sep = ',',col.names = c("left"))


# trees

setwd ("D:\\Business Analytics\\Project 4 (HR)\\")

hr_train = read.csv("D:\\Business Analytics\\Project 4 (HR)\\hr_train.csv",stringsAsFactors = F)
hr_test = read.csv("D:\\Business Analytics\\Project 4 (HR)\\hr_test.csv",stringsAsFactors = F)

library(dplyr)
library(cvTools)
library(randomForest)
glimpse(hr_train)

hr_test$left = NA
hr_test$data = "test"
hr_train$data = "train"

hr_all = rbind(hr_train,hr_test)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


cat_cols = names(hr_all)[sapply(hr_all,is.character)]
unique(cat_cols)

cat_cols=cat_cols[!(cat_cols %in% c('data'))]
cat_cols

for (i in cat_cols){
  hr_all = CreateDummies(hr_all,i,50)
}


for(col in names(hr_all)){
  if(sum(is.na(hr_all[,col]))>0 & !(col %in% c("data","left"))){
    hr_all[is.na(hr_all[,col]),col]=mean(hr_all[hr_all$data=='train',col],na.rm=T)
  }
}


# Seprate train and test

hr_train = hr_all %>% 
  filter(data == "train") %>% 
  select(-data)

hr_test = hr_all %>% 
  filter(data == "test") %>% 
  select(-data,-left)

# for validation divide train data into two

set.seed(2)
s=sample(1:nrow(hr_train),0.8*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

hr_train$left = as.factor(hr_train$left)

param=list(mtry=c(5,10,15,20),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~., 
             data =hr_train,
             tuning =params,
             folds = cvFolds(nrow(hr_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

myauc
#[1] 0.8401437  for mtry ntree maxnodes nodesize
#                    35   700      100        5

best_params

best_params=data.frame(mtry=15,
                       ntree=700,
                       maxnodes=100,
                       nodesize=10)

retail.rf.final=randomForest(left~.,
                             mtry=best_params$mtry,
                             ntree=best_params$ntree,
                             maxnodes=best_params$maxnodes,
                             nodesize=best_params$nodesize,
                             data=hr_train
)

test.score=predict(retail.rf.final,newdata = hr_test,type='prob')[,1]
write.table(test.score,"Amar_Shinde_P4_part2.csv",row.names = F,sep = ',',col.names = c("left"))



##---------------------Starter Script-------------------

setwd ("D:\\Business Analytics with R and SAS\\Project 4 (HR)\\")

hr_train = read.csv("D:\\Business Analytics with R and SAS\\Project 4 (HR)\\hr_train.csv")
hr_test = read.csv("D:\\Business Analytics with R and SAS\\Project 4 (HR)\\hr_test.csv")

library(dplyr)
library(car)


hr_test$left= NA

hr_train$data = 'train'
hr_test$data = 'test'

all= rbind(hr_train,hr_test)

apply(all,2,function(x) length(unique(x)))
glimpse(all)

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

all$promotion_last_5years = as.factor(all$promotion_last_5years)
all$Work_accident = as.factor(all$Work_accident)

# Impute the NA values with Mean in the dataset

hr_train = all %>% filter(data == 'train') %>% select(-data) 
hr_test= all %>% filter(data == 'test') %>% select(-left, -data) 
hr_train$left = as.factor(hr_train$left)
#-----------------------------------------------------------------------
library(cvTools)
library(randomForest)

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~., 
             data =hr_train,
             tuning =params,
             folds = cvFolds(nrow(hr_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}
myauc
best_params

best_params=data.frame(mtry=25,
                       ntree=500,
                       maxnodes=100,
                       nodesize=10)

ld.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=hr_train)
#-------------------------------------
for_vif = lm(left ~., data = hr_train) 
vif(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

library(randomForest)

fit_hr= randomForest(as.factor(left)~.,data=hr_train)
fit_hr

response=predict(ld.rf.final, newdata = hr_test,type = "prob")[,2]
write.table(response,"Amar_Shinde_P4_part2.csv",row.names = F,sep = ',',col.names = c("left"))