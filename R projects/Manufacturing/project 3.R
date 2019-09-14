setwd ("D:\\Business Analytics with R and SAS\\Project 3 (Manufacturing)\\")
getwd()

library(dplyr)
product_train=read.csv("D:\\Business Analytics with R and SAS\\Project 3 (Manufacturing)\\product_train.csv",stringsAsFactors = F)
product_test=read.csv("D:\\Business Analytics with R and SAS\\Project 3 (Manufacturing)\\product_test.csv",stringsAsFactors = F)

glimpse(product_train)

product_test$went_on_backorder = NA
product_test$data = "test"
product_train$data = "train"

product_all = rbind(product_train,product_test)
glimpse(product_all)

product_all=product_all %>% 
  select(-ppap_risk,-oe_constraint,-deck_risk,-stop_auto_buy,-rev_stop)


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

#--------------------
char_logical=sapply(product_all,is.character)
cat_cols=names(product_all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','went_on_backorder'))]
cat_cols

for(col in cat_cols){
  product_all=CreateDummies(product_all,col,50)
}

glimpse(product_all)

product_all=product_all[!(is.na(product_all$went_on_backorder) & product_all$data=="train"),]
for(col in names(product_all)){
  if(sum(is.na(product_all[,col]))>0 & !(col %in% c("data","went_on_backorder"))){
    product_all[is.na(product_all[,col]),col]=mean(product_all[product_all$data=='train',col],na.rm=T)
  }
}

product_all$went_on_backorder=ifelse(product_all$went_on_backorder=="No",0,1)
product_all$went_on_backorder=as.numeric(product_all$went_on_backorder)

glimpse(product_all)

##---------separate train and test

product_train=product_all %>% 
  filter(data=="train") %>% 
  select(-data)

product_test=product_all %>% 
  filter(data=="test") %>% 
  select(-data)

glimpse(product_train)


#------------------

library(car)

for_vif= lm(went_on_backorder~.-forecast_6_month-sales_6_month-sales_9_month
            -forecast_9_month-sales_1_month,data=product_train)

sort(vif(for_vif),decreasing = T)[1:3]


product_train$went_on_backorder = as.factor(product_train$went_on_backorder)

log_fit = glm(went_on_backorder~.-forecast_6_month-sales_6_month-sales_9_month-
                forecast_9_month-sales_1_month,data=product_train,family = 'binomial')

log_fit=step(log_fit)

formula(log_fit)

log_fit=glm(went_on_backorder ~ sku + lead_time + in_transit_qty + forecast_3_month + 
               perf_6_month_avg + local_bo_qty + potential_issue_No
            ,data=product_train,family='binomial')
summary(log_fit)

#---------------- determining cutoff

train.score=predict(log_fit,newdata = product_train,type='response')
real=product_train$went_on_backorder
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
test.prob.score= predict(log_fit,newdata = product_test,type='response')

test.hardclasses = ifelse( test.prob.score > my_cutoff,'yes','no')
# now that we have our cutoff we can convert score to hard classes

write.table(test.hardclasses,"Amar_Shinde_P3_part2.csv",row.names = F,sep = ',',col.names = c("went_on_backorder"))



#----------quiz-----------------

product_test$deck_risk = as.numeric(product_test$deck_risk == 'Yes')
table(product_test$deck_risk)
chisq = chisq.test(product_test$deck_risk)
chisq

cor(product_test$forecast_9_month,product_test$sales_9_month)




#-------starter script--------------
setwd ("D:\\Business Analytics with R and SAS\\Project 3 (Manufacturing)\\")
getwd()

p_train=read.csv("D:\\Business Analytics with R and SAS\\Project 3 (Manufacturing)\\product_train.csv")
p_test=read.csv("D:\\Business Analytics with R and SAS\\Project 3 (Manufacturing)\\product_test.csv")




library(dplyr)
library(car)



p_test$went_on_backorder=NA

p_train$data="train"
p_test$data="test"
p_all=rbind(p_train,p_test)

glimpse(p_all)

p_all=p_all %>% 
  select(-ppap_risk,-oe_constraint,-deck_risk,-stop_auto_buy,-rev_stop)

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

char_logical=sapply(p_all,is.character)
cat_cols=names(p_all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','went_on_backorder'))]
cat_cols

for(col in cat_cols){
  p_all=CreateDummies(p_all,col,50)
}

glimpse(p_all)

p_all=p_all[!(is.na(p_all$went_on_backorder) & p_all$data=="train"),]
for(col in names(p_all)){
  if(sum(is.na(p_all[,col]))>0 & !(col %in% c("data","went_on_backorder"))){
    p_all[is.na(p_all[,col]),col]=mean(p_all[p_all$data=='train',col],na.rm=T)
  }
}

p_all$went_on_backorder=ifelse(p_all$went_on_backorder=="No",0,1)
p_all$went_on_backorder=as.numeric(p_all$went_on_backorder)

glimpse(p_all)
p_train=p_all %>% 
  filter(data=="train") %>% 
  select(-data)

p_test=p_all %>% 
  filter(data=="test") %>% 
  select(-data)

glimpse(p_train)

fit=lm(went_on_backorder~.,data=p_train)
sort(vif(fit),decreasing=T)[1:3]

fit=lm(went_on_backorder~. -forecast_6_month,data=p_train)
sort(vif(fit),decreasing=T)[1:3]

fit=lm(went_on_backorder~. -forecast_6_month -sales_6_month,data=p_train)
sort(vif(fit),decreasing=T)[1:3]

fit=lm(went_on_backorder~. -forecast_6_month -sales_6_month -sales_9_month,
       data=p_train)
sort(vif(fit),decreasing=T)[1:3]

fit=lm(went_on_backorder~. -forecast_6_month 
       -sales_6_month -sales_9_month -forecast_9_month,data=p_train)
sort(vif(fit),decreasing=T)[1:3]

fit=lm(went_on_backorder~. -forecast_6_month 
       -sales_6_month -sales_9_month -forecast_9_month -sales_1_month,data=p_train)
sort(vif(fit),decreasing=T)[1:3]

formula(fit)

p_train=p_train %>% 
  select(-forecast_6_month, 
         -sales_6_month, -sales_9_month, -forecast_9_month, -sales_1_month)

library(randomForest)

fit=randomForest(as.factor(went_on_backorder)~.,data=p_train,classwt=c(0.99,0.01),do.trace=T)
fit

response=predict(fit,newdata = p_test,type= "response")





