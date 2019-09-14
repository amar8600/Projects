library(dplyr)

setwd("D:\\Business Analytics\\Project 2(Retail)\\")
getwd()

retail_train = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_train.csv",stringsAsFactors = F)
retail_test = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_test.csv",stringsAsFactors = F)

retail_test$store = NA
glimpse(retail_train)


retail_train$data = "train"
retail_test$data = "test"

retail_all = rbind(retail_train,retail_test)

glimpse(retail_all)

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

retail_all = CreateDummies(retail_all,"store_Type",50)
retail_all = CreateDummies(retail_all,"state_alpha",50)
retail_all$countyname = NULL
retail_all$Areaname = NULL
retail_all$countytownname = NULL
retail_all$storecode = NULL


# separte train and test

retail_train = retail_all %>% 
  filter(data == "train") %>% 
  select(-data)

retail_test = retail_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)

# for validation divide train data into two

set.seed(2)
s=sample(1:nrow(retail_train),0.8*nrow(retail_train))
retail_train1=retail_train[s,]
retail_train2=retail_train[-s,]

library(car)

for_vif=lm(store~.-Id,data=retail_train1)

sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-Id-sales0-sales2,data=retail_train1)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif = step(for_vif)

formula(for_vif)
for_vif= lm(store ~ sales1 + sales3 + sales4 + State + CouSub +  
                state_alpha_LA + state_alpha_AL + 
               state_alpha_AR + state_alpha_PR +  
               state_alpha_OH + state_alpha_IN + state_alpha_TN + 
               state_alpha_IL +  state_alpha_KY + 
              state_alpha_GA + state_alpha_VT + state_alpha_NH + state_alpha_MA,data=retail_train1)
summary(for_vif)


## logistic regression

library(dplyr)

setwd("D:\\Business Analytics\\Project 2(Retail)\\")
getwd()

retail_train = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_train.csv",stringsAsFactors = F)
retail_test = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_test.csv",stringsAsFactors = F)

retail_test$store = NA
glimpse(retail_train)


retail_train$data = "train"
retail_test$data = "test"

retail_all = rbind(retail_train,retail_test)

retail_all$Areaname = NULL

glimpse(retail_all)

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

cat_cols = names(retail_all)[sapply(retail_all,is.character)]
unique(cat_cols)

cat_cols=cat_cols[!(cat_cols %in% c('data'))]
cat_cols

for (i in cat_cols){
  retail_all = CreateDummies(retail_all,i,50)
}

for(col in names(retail_all)){
  if(sum(is.na(retail_all[,col]))>0 & !(col %in% c("data","store"))){
    retail_all[is.na(retail_all[,col]),col]=mean(retail_all[retail_all$data=='train',col],na.rm=T)
  }
}

glimpse(retail_all)


# separte train and test

retail_train = retail_all %>% 
  filter(data == "train") %>% 
  select(-data)

retail_test = retail_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)

# for validation divide train data into two

set.seed(2)
s=sample(1:nrow(retail_train),0.8*nrow(retail_train))
retail_train1=retail_train[s,]
retail_train2=retail_train[-s,]

library(car)


log_fit= glm(store~.-Id-storecode_NCNTY23003N23003-sales0-sales2-sales3,data=retail_train1,family = 'binomial')

summary(log_fit)
sort(vif(log_fit),decreasing = T)[1:3]

log_fit=step(log_fit)
summary(log_fit)

formula(log_fit)

log_fit = glm(store ~ sales1 + sales4 + State + population + countyname_PenobscotCounty + 
                storecode_METRO14460MM1120 + 
                state_alpha_LA + state_alpha_AL +   
                 state_alpha_AR +  state_alpha_PR + 
                state_alpha_MS +  state_alpha_OH + state_alpha_IN + 
                 state_alpha_TN + state_alpha_IA + state_alpha_IL + 
                 state_alpha_MO + state_alpha_KY + state_alpha_GA + 
                 state_alpha_VT + state_alpha_NH + state_alpha_MA,data = retail_train1,family = 'binomial')
library(pROC)

val.score=predict(log_fit,newdata = retail_train2,type='response')

auc(roc(retail_train2$store,val.score))

## on training data
log_fit_final = glm(store ~ sales1 +  population + countyname_PenobscotCounty + 
                storecode_METRO14460MM1120 + 
                state_alpha_LA +   
                state_alpha_PR + 
                  state_alpha_IN + 
                state_alpha_TN +  state_alpha_IL + 
                state_alpha_VT + state_alpha_NH + state_alpha_MA,data = retail_train,family = 'binomial')


sort(vif(log_fit_final),decreasing = T)[1:3]

log_fit_final=step(log_fit_final)
summary(log_fit_final)

test.prob.score= predict(log_fit_final,newdata = retail_test,type='response')
write.table(test.prob.score,"Amar_Shinde_P2_part2.csv",row.names = F,sep = ',',col.names = c("store"))


## trees

library(tree)
library(randomForest)
library(car)
library(dplyr)

setwd("D:\\Business Analytics\\Project 2(Retail)\\")
getwd()

retail_train = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_train.csv",stringsAsFactors = F)
retail_test = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_test.csv",stringsAsFactors = F)

retail_test$store = NA
glimpse(retail_train)


retail_train$data = "train"
retail_test$data = "test"

retail_all = rbind(retail_train,retail_test)

glimpse(retail_all)

retail_all$store = as.factor(retail_all$store)

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

retail_all = CreateDummies(retail_all,"store_Type",50)
retail_all = CreateDummies(retail_all,"state_alpha",50)
retail_all$countyname = NULL
retail_all$Areaname = NULL
retail_all$countytownname = NULL
retail_all$storecode = NULL

# separte train and test

retail_train = retail_all %>% 
  filter(data == "train") %>% 
  select(-data)

retail_test = retail_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)

# for validation divide train data into two

set.seed(2)
s=sample(1:nrow(retail_train),0.8*nrow(retail_train))
retail_train1=retail_train[s,]
retail_train2=retail_train[-s,]

# building tree

retail.tree = tree(store ~  sales4 + State +   
                     state_alpha_LA + state_alpha_AL + 
                     state_alpha_PR +  
                     state_alpha_OH + state_alpha_IN + state_alpha_TN + 
                     state_alpha_IL +  state_alpha_KY + 
                     state_alpha_GA + state_alpha_VT + state_alpha_NH + state_alpha_MA,data=retail_train1)


## Performance on validation set

val.score=predict(retail.tree,newdata = retail_train2,type='vector')[,1]
pROC::roc(retail_train2$store,val.score)$auc


## build model on entire data

retail.tree.final=tree(store ~  sales4 + State +   
                         state_alpha_LA + state_alpha_AL + 
                         state_alpha_PR +  
                         state_alpha_OH + state_alpha_IN + state_alpha_TN + 
                         state_alpha_IL +  state_alpha_KY + 
                         state_alpha_GA + state_alpha_VT + state_alpha_NH + state_alpha_MA,data=retail_train)

## Probability score prediciton on test/production data

test.score=predict(retail.tree.final,newdata=retail_test,type='vector')[,1]
write.table(test.score,"Amar_Shinde_P2_part2.csv",row.names = F,sep = ',',col.names = c("store"))



## random forest

library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(tidyr)
library(cvTools)

setwd("D:\\Business Analytics\\Project 2(Retail)\\")
getwd()

retail_train = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_train.csv",stringsAsFactors = F)
retail_test = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_test.csv",stringsAsFactors = F)

retail_test$store = NA
glimpse(retail_train)


retail_train$data = "train"
retail_test$data = "test"

retail_all = rbind(retail_train,retail_test)

glimpse(retail_all)

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

retail_all = CreateDummies(retail_all,"store_Type",50)
retail_all = CreateDummies(retail_all,"state_alpha",50)
retail_all$countyname = NULL
retail_all$Areaname = NULL
retail_all$countytownname = NULL
retail_all$storecode = NULL


# separte train and test

retail_train = retail_all %>% 
  filter(data == "train") %>% 
  select(-data)

library("imputeTS")
retail_train = na.replace(retail_train, 0)


retail_test = retail_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)
retail_test = na.replace(retail_test, 0)


retail_train$store = as.factor(retail_train$store)
glimpse(retail_train)

#######

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
  
  k=cvTuning(randomForest,store~.-Id, 
             data =retail_train,
             tuning =params,
             folds = cvFolds(nrow(retail_train), K=10, type ="random"),
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
#[1] 0.8059414  for mtry ntree maxnodes nodesize
#                     5   700      100       10
#[1] 0.808013  for mtry ntree maxnodes nodesize
#                    20   500      100       10

#   0.8075532      mtry ntree maxnodes nodesize
#                    15   700      100        5
best_params


## Model on the entire training data


best_params=data.frame(mtry=20,
                       ntree=500,
                       maxnodes=100,
                       nodesize=10)

retail.rf.final=randomForest(store~.-Id,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=retail_train
)

test.score=predict(retail.rf.final,newdata = retail_test,type='prob')[,1]
write.table(test.score,"Amar_Shinde_P2_part2.csv",row.names = F,sep = ',',col.names = c("store"))



# boosting Machine

library(dplyr)
library(cvTools)
library(randomForest)
library(gbm)

setwd("D:\\Business Analytics\\Project 2(Retail)\\")
getwd()

retail_train = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_train.csv",stringsAsFactors = F)
retail_test = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_test.csv",stringsAsFactors = F)

retail_test$store = NA
glimpse(retail_train)


retail_train$data = "train"
retail_test$data = "test"

retail_all = rbind(retail_train,retail_test)

retail_all$Areaname = NULL

# retail_all$countyname = NULL
# retail_all$Areaname = NULL
# retail_all$countytownname = NULL
# retail_all$storecode = NULL

glimpse(retail_all)

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

# retail_all = CreateDummies(retail_all,"store_Type",50)
# retail_all = CreateDummies(retail_all,"state_alpha",50)

############################################

cat_cols = names(retail_all)[sapply(retail_all,is.character)]
unique(cat_cols)

cat_cols=cat_cols[!(cat_cols %in% c('data'))]
cat_cols

for (i in cat_cols){
  retail_all = CreateDummies(retail_all,i,50)
}

############################################

for(col in names(retail_all)){
  if(sum(is.na(retail_all[,col]))>0 & !(col %in% c("data","store"))){
    retail_all[is.na(retail_all[,col]),col]=mean(retail_all[retail_all$data=='train',col],na.rm=T)
  }
}

glimpse(retail_all)


# separte train and test

retail_train = retail_all %>% 
  filter(data == "train") %>% 
  select(-data)

retail_test = retail_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)



subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

param=list(interaction.depth=c(7,11,12,13,17,23),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(5,10,15,20))

num_trials=20
my_params=subset_paras(param,num_trials)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}


myauc = 0

for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(gbm,store~.-Id,
             data =retail_train,
             tuning =params,
             args=list(distribution="bernoulli"),
             folds = cvFolds(nrow(retail_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response",n.trees=params$n.trees)
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


ci.gbm.final=gbm(store~.-Id,data=retail_train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "bernoulli")

test.score=predict(ci.gbm.final,newdata=retail_test,type='response',n.trees = best_params$n.trees)
write.table(test.score,"Amar_Shinde_P2_part2.csv",row.names = F,sep = ',',col.names = c("store"))

# stacking

library(dplyr)
library(cvTools)
library(randomForest)
library(gbm)

setwd("D:\\Business Analytics\\Project 2(Retail)\\")
getwd()

retail_train = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_train.csv",stringsAsFactors = F)
retail_test = read.csv("D:\\Business Analytics\\Project 2(Retail)\\store_test.csv",stringsAsFactors = F)

retail_test$store = NA
glimpse(retail_train)


retail_train$data = "train"
retail_test$data = "test"

retail_all = rbind(retail_train,retail_test)

retail_all$Areaname = NULL

# retail_all$countyname = NULL
# retail_all$Areaname = NULL
# retail_all$countytownname = NULL
# retail_all$storecode = NULL

glimpse(retail_all)

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

# retail_all = CreateDummies(retail_all,"store_Type",50)
# retail_all = CreateDummies(retail_all,"state_alpha",50)

############################################

cat_cols = names(retail_all)[sapply(retail_all,is.character)]
unique(cat_cols)

cat_cols=cat_cols[!(cat_cols %in% c('data'))]
cat_cols

for (i in cat_cols){
  retail_all = CreateDummies(retail_all,i,50)
}

############################################

for(col in names(retail_all)){
  if(sum(is.na(retail_all[,col]))>0 & !(col %in% c("data","store"))){
    retail_all[is.na(retail_all[,col]),col]=mean(retail_all[retail_all$data=='train',col],na.rm=T)
  }
}

glimpse(retail_all)


# separte train and test

retail_train = retail_all %>% 
  filter(data == "train") %>% 
  select(-data)

retail_test = retail_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)

mykfolds=function(nobs,nfold=5){
  
  t=cvFolds(nobs,K=nfold,type='random')
  
  folds=list()
  
  for(i in 1:nfold){
    
    test=t$subsets[t$which==i]
    train=t$subsets[t$which!=i]
    
    folds[[i]]=list('train'=train,'test'=test)
  }
  
  return(folds)
}

#------------------------
myfolds=mykfolds(nrow(retail_train),10)
#------------------------

retail_train_layer=data.frame(rf_var=numeric(nrow(retail_train)),
                          gbm_var=numeric(nrow(retail_train)))

library(randomForest)
for(i in 1:10){
  print(c(i))
  fold=myfolds[[i]]
  
  train_data=retail_train[fold$train,]
  # for this iteration model will be built on this chunk of the data
  test_data=retail_train[fold$test,]
  # predicitons will be made on this chunk which is not being
  # used in the modeling process
  
  print('rf')
  
  rf.fit=randomForest(factor(store)~.-Id,
                      mtry=10,
                      ntree=500,
                      maxnodes=100,
                      nodesize=10,
                      data=train_data
  )
  ## these value of parameters have been chosen randomly here
  ## but separately tuned parameter choices will be better
  rf_score=predict(rf.fit,newdata=test_data,type='prob')[,1]
  
  print('gbm')
  gbm.fit=gbm(store~.-Id,data=train_data,
              interaction.depth=7,
              n.trees=700,
              shrinkage=0.01,
              n.minobsinnode=5,
              distribution = "bernoulli")
  ## these value of parameters have been chosen randomly here
  ## but separately tuned parameter choices will be better
  gbm_score=predict(gbm.fit,newdata=test_data,
                    n.trees=700,type='response')
  
  retail_train_layer$rf_var[fold$test]=rf_score
  
  retail_train_layer$gbm_var[fold$test]=gbm_score
}


#------------------

retail_test_layer=data.frame(rf_var=numeric(nrow(retail_test)),
                         gbm_var=numeric(nrow(retail_test)))

full.rf=randomForest(factor(store)~.-Id,
                     mtry=10,
                     ntree=500,
                     maxnodes=100,
                     nodesize=10,
                     data=retail_train
)
full.gbm=gbm(store~.-Id,data=retail_train,
             interaction.depth=7,
             n.trees=700,
             shrinkage=0.01,
             n.minobsinnode=5,
             distribution = "bernoulli")
# note that paramater choices are exactly same as earlier

retail_test_layer$rf_var=predict(full.rf,newdata=retail_test,type='prob')[,1]
retail_test_layer$gbm_var=predict(full.gbm,newdata=retail_test,
                              n.trees=700,type='response')

## ------------------------------------------------------------------------
retail_train_layer$store=retail_train$store

log.mod=glm(store~.,data=retail_train_layer,family = "binomial")


## ----use this for prediction and submission on test--------------------------------------------------------------
test.score=predict(log.mod,newdata=retail_test_layer,type='response')
write.table(test.score,"Amar_Shinde_P2_part2.csv",row.names = F,sep = ',',col.names = c("store"))