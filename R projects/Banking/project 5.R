setwd ("D:\\Business Analytics with R and SAS\\Project 5(Banking)\\")
getwd()

bank_train = read.csv("D:\\Business Analytics with R and SAS\\Project 5(Banking)\\bank-full_train.csv",stringsAsFactors = F)
bank_test = read.csv("D:\\Business Analytics with R and SAS\\Project 5(Banking)\\bank-full_test.csv",stringsAsFactors = F)

library(dplyr)
glimpse(bank_train)

bank_test$y = NA
bank_test$data = 'test'
bank_train$data = 'train'

bank_all = rbind(bank_train,bank_test)

glimpse(bank_all)

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

names(bank_all)[sapply(bank_all,function(x) is.character(x))]

#-----------------------
bank_all=bank_all %>% 
  select(-ID,-poutcome)

colnames(bank_all)

for_dummy_vars=c("job","marital","education","contact","month")


for(col in for_dummy_vars){
  bank_all=CreateDummies(bank_all,col,100)
}

for(col in names(bank_all)){
  
  if(sum(is.na(bank_all[,col]))>0 & !(col %in% c("data","y"))){
    
    bank_all[is.na(bank_all[,col]),col]=mean(all[bank_all$data=='train',col],na.rm=T)
  }
  
}

head(all)

bank_train = bank_all %>% filter(data == 'train') %>% select(-data) 
bank_test= bank_all %>% filter(data == 'test') %>% select(-y, -data) 

any(is.na(bank_train))
any(is.na(bank_test))

#------------------------

##---------separate train and test

bank_train$y = as.numeric(bank_train$y == 'yes')

glimpse(bank_train)


library(car)

for_vif=lm(y~.-month_may-job_blue_collar,data=bank_train)

sort(vif(for_vif),decreasing = T)[1:3]


log_fit=glm(y~.-month_may-job_blue_collar,data=bank_train,family = "binomial")

log_fit=step(log_fit)

formula(log_fit)

log_fit = glm(y ~  balance + housing + loan + day + duration + campaign + 
                pdays + previous + job_student + job_housemaid + job_retired + 
                job_admin. + job_technician + job_management + marital_single + 
                marital_married + education_primary + education_tertiary + 
                contact_unknown + contact_cellular + month_mar + month_sep + 
                month_oct + month_jan + month_feb + month_apr + month_nov + 
                month_jun + month_aug + month_jul,data = bank_train,family = 
                "binomial")
summary(log_fit)

train.score=predict(log_fit,newdata = bank_train,type='response')
real=bank_train$y
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff



test.prob.score= predict(log_fit,newdata = bank_test,type='response')

cust.subscribtion = ifelse( test.prob.score >my_cutoff,'yes','no')


write.table(cust.subscribtion ,"Amar_Shinde_P5_part2.csv",row.names = F,sep = ',',col.names = c("y"))


# --stater script-----


setwd ("D:\\Business Analytics with R and SAS\\Project 5(Banking)\\")
getwd()


bf_train=read.csv("bank-full_train.csv",sep=",",header=T)
bf_test=read.csv("bank-full_test.csv",sep=",",header=T)
library(dplyr)
glimpse(bf_train)

bf_test$y= NA
bf_train$data = 'train'
bf_test$data = 'test'

all= rbind(bf_train,bf_test)

apply(all,2,function(x) length(unique(x)))

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

all=all %>% 
  select(-ID,-poutcome)

head(all)
for_dummy_vars=c('job','martial','education','contact','month')
for_dummy_vars
for(var in for_dummy_vars){
  all=CreateDummies(all,var,100)
}

for(col in names(all)){
  
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","y"))){
    
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
  
}

head(all)

bf_train = all %>% filter(data == 'train') %>% select(-data) 
bf_test= all %>% filter(data == 'test') %>% select(-y, -data) 

any(is.na(bf_train))
any(is.na(bf_test))

library(randomForest)
fit = randomForest(as.factor(y )~ ., data = bf_train) 

train.predictions = predict(fit, newdata =bf_test)

### Make predictions on test and submit 

test.predictions = predict(fit, newdata = bf_test)

write.csv(test.predictions,file = "file_name.csv", row.names = F)








