setwd("D:\\Business Analytics\\Project 1\\")
getwd()

library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
re_train = read.csv("D:\\Business Analytics\\Project 1\\housing_train.csv",stringsAsFactors = F)
re_test = read.csv("D:\\Business Analytics\\Project 1\\housing_test.csv",stringsAsFactors = F)

write.csv(re_train,"re_train.csv",row.names = F)
library(dplyr)
glimpse(re_train)


re_train$Price = as.numeric(re_train$Price)
re_train$data = "train"
re_test$data = "test"

re_test$Price = NA

re_all = rbind(re_train,re_test)
for(col in names(re_all)){
  if(sum(is.na(re_all[,col]))>0 & !(col %in% c("data","Price"))){
    re_all[is.na(re_all[,col]),col]=mean(re_all[re_all$data=='train',col],na.rm=T)
  }
}
write.csv(re_all,"re_all.csv",row.names = F)
re_all$Address = NULL
####
sum(is.na(re_all$YearBuilt))
length(table((re_all$Postcode)))
length(unique(re_all$Postcode))

library(dplyr)
means = re_all %>%
  group_by(table(re_all$Type)) %>%
  summarise(Valuemean = mean(Value, na.rm = TRUE))

max(table(re_train$Price,re_train$SellerG))
glimpse(re_all)

write.csv(re_all,"proper_submission_file_name.csv",row.names = F)
table(re_all$SellerG,re_all$Price)
#####

glimpse(re_all)
cat_cols = names(re_all)[sapply(re_all,is.character)]
unique(cat_cols)
max()
cat_cols=cat_cols[!(cat_cols %in% c('data'))]
cat_cols

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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

for (i in cat_cols){
  re_all = CreateDummies(re_all,i,100)
}

re_all = re_all[!((is.na(re_all$Price)) & re_all$data=='train'), ]

glimpse(re_all)



for(col in names(re_all)){
  if(sum(is.na(re_all[,col]))>0 & !(col %in% c("data","Price"))){
    re_all[is.na(re_all[,col]),col]=mean(re_all[re_all$data=='train',col],na.rm=T)
  }
}

re_train = re_all %>% 
        filter(data == "train") %>% 
        select(-data)

re_test = re_all %>% 
  filter(data == "test") %>% 
  select(-data,-Price)
max(re_train$Price)

table(re_all$)
set.seed(2)
s=sample(1:nrow(re_train),0.7*nrow(re_train))
re_train1=re_train[s,]
re_train2=re_train[-s,]


for_vif = lm(Price~.,data=re_train1)
library(car)

sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(Price~.-CouncilArea_,data=re_train1)
rm(for_vif)
fit = lm(Price~.-CouncilArea_,data=re_train1)
sort(vif(fit),decreasing = T)[1:3]
summary(fit)

formula(fit)
fit = lm(Price ~ (Rooms + Distance + Postcode + Bedroom2 + Bathroom + 
                    Car + Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
                    Suburb_Hampton +  Suburb_Balwyn + 
                    Suburb_MalvernEast + Suburb_Camberwell + 
                    Suburb_PortMelbourne +  Suburb_Hawthorn + Suburb_BalwynNorth + 
                    Suburb_Kew + Suburb_Brighton + 
                     Suburb_Essendon +  
                    Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                     Suburb_Reservoir + Type_u + Type_h + 
                     Method_PI + Method_S + SellerG_Kay +  
                    SellerG_Miles +  SellerG_RT +  SellerG_Biggin + 
                    SellerG_Marshall +  
                    SellerG_hockingstuart + SellerG_Jellis +
                    CouncilArea_Whitehorse +  
                    CouncilArea_HobsonsBay +  
                    CouncilArea_Banyule + CouncilArea_PortPhillip + CouncilArea_Yarra + 
                   CouncilArea_Moreland + 
                    CouncilArea_Boroondara + CouncilArea_) - CouncilArea_,data = re_train1)
summary(fit)
fit=step(fit)

summary(fit)


val.pred=predict(fit,newdata=re_train2)

errors=re_train2$Price-val.pred

errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data

fit.final=lm(Price~.-CouncilArea_,data=re_train)

sort(vif(fit.final),decreasing = T)[1:3]
fit.final=step(fit.final)

summary(fit.final)

formula(fit.final)

fit.final =lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
                 Suburb_Hampton + Suburb_Balwyn + 
                Suburb_MalvernEast + Suburb_Camberwell + Suburb_PortMelbourne + 
                 Suburb_BrightonEast + Suburb_Hawthorn + 
                Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
                Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                Suburb_Reservoir + Type_u + Type_h +  
                Method_S + SellerG_Kay +  SellerG_Miles +  
                SellerG_RT + SellerG_Biggin +  SellerG_Marshall + 
                SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
                CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
                CouncilArea_PortPhillip + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_Moreland + 
                CouncilArea_Boroondara,data=re_train)
summary(fit.final)

test.predictions=predict(fit.final,newdata=re_test)

write.table(test.predictions,'Amar_Shinde_P1_part2.csv',row.names = F,sep = ",",col.names = c("Price"))

var(re_train$Price)

############# logistic regression

log_fit = glm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
                Suburb_Hampton + Suburb_Balwyn + 
                Suburb_MalvernEast + Suburb_Camberwell + Suburb_PortMelbourne + 
                Suburb_BrightonEast + Suburb_Hawthorn + 
                Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
                Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                Suburb_Reservoir + Type_u + Type_h +  
                Method_S + SellerG_Kay +  SellerG_Miles +  
                SellerG_RT + SellerG_Biggin +  SellerG_Marshall + 
                SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
                CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
                CouncilArea_PortPhillip + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_Moreland + 
                CouncilArea_Boroondara,data=re_train,family = "binomial")

log_fit = step(log_fit)
glimpse(re_train)

### Descicion Tree

ld.tree=tree(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                 Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
                 Suburb_Hampton + Suburb_Balwyn + 
                 Suburb_MalvernEast + Suburb_Camberwell + Suburb_PortMelbourne + 
                 Suburb_BrightonEast + Suburb_Hawthorn + 
                 Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
                 Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                 Suburb_Reservoir + Type_u + Type_h +  
                 Method_S + SellerG_Kay +  SellerG_Miles +  
                 SellerG_RT + SellerG_Biggin +  SellerG_Marshall + 
                 SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
                 CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
                 CouncilArea_PortPhillip + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                 CouncilArea_Stonnington + CouncilArea_Moreland + 
                 CouncilArea_Boroondara,data=re_train1)

ld.tree
plot(ld.tree)
text(ld.tree)

val.IR=predict(ld.tree,newdata = re_train2)

rmse_val=((val.IR)-(re_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val



####

