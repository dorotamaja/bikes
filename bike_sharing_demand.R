library(dplyr)
library(lubridate)
library(randomForest)
library(xgboost)

set.seed(2243)

rowery<-read.csv("train.csv")

rowery %>% 
  mutate(hour = hour(datetime),
         month =month(datetime),
         year = year(datetime),
         wday = wday(datetime))-> rowery


rowery%>%mutate(roznica=atemp-temp)->rowery
rowery%>%filter(roznica> -10)->rowery
rowery%>%select(-roznica)->rowery

rowery%>%mutate(wday=ifelse((wday==7),0,wday))-> rowery

rowery%>%select(-datetime,-count,-registered)->rowery_c
rowery%>% select(-datetime,-count,-casual)->rowery_r

rowery_c%>%mutate(szczyt_c=ifelse((hour %in% c(11:18)),0,1))->rowery_c
rowery_r%>%mutate(szczyt_r=ifelse((hour %in% c(8,17,18) &  workingday==1),0,1))->rowery_r

xc = rowery_c %>% select(-casual) %>% as.matrix()
yc = rowery_c$casual

xr = rowery_r %>% select(-registered) %>% as.matrix()
yr = rowery_r$registered


dtrain_c = xgb.DMatrix(xc, label = log(yc+1))



model_c = xgb.train(data = dtrain_c,nrounds=1000,
                    max_depth=4,
                    alpha=5,
                    eta=0.05,
                    subsample=0.8,
                    colsample_bytree=0.6)


dtrain_r = xgb.DMatrix(xr, label = log(yr+1))
model_r = xgb.train(data = dtrain_r,nrounds=300,
                    alpha=3,
                    max_depth=4,
                    eta=0.4,
                    subsample=0.9,
                    colsample_bytree=0.7)


rowery.test<-read.csv("test.csv")

rowery.test %>% 
  mutate(hour = hour(datetime),
         month =month(datetime),
         year = year(datetime),
         wday = wday(datetime))-> rowery.test 



rowery.test%>%mutate(wday=ifelse((wday==7),0,wday))-> rowery.test
rowery.test%>%mutate(szczyt_c=ifelse((hour %in% c(11:18)),0,1))->rowery.test
rowery.test%>%mutate(szczyt_r=ifelse((hour %in% c(8,17,18) & workingday==1),0,1))->rowery.test
rowery.test %>% select(-datetime)->rowery.test

rozw_c<-exp(predict(model_c,as.matrix(rowery.test)))-1

rozw_r<-exp(predict(model_r,as.matrix(rowery.test)))-1

#########################RANDOM FOREST PART##############################################3
rowery<-read.csv("train.csv")

rowery %>% 
  mutate(hour = hour(datetime),
         month =month(datetime),
         year = year(datetime),
         wday = as.factor(wday(datetime)))->rowery

rowery%>%mutate(roznica=atemp-temp)->rowery
rowery%>%filter(roznica> -10)->rowery
rowery%>%select(-roznica)->rowery


rowery%>%select(-datetime,-count,-registered)->rowery_c
rowery%>% select(-datetime,-count,-casual)->rowery_r


rowery_r%>% mutate(registered=log(registered+1))->rowery_r
rowery_c%>% mutate(casual=log(casual+1))->rowery_c



my_forest_r <- randomForest(registered~.,
                            data = rowery_r, importance = TRUE, ntree = 300)



my_forest_c <- randomForest(casual~.,
                            data = rowery_c, importance = TRUE, ntree = 1000)



rowery.test<-read.csv("test.csv")
rowery.test %>% 
  mutate(hour = hour(datetime),
         month =month(datetime),
         year = year(datetime),
         wday = as.factor(wday(datetime)))-> rowery.test 
id<-rowery.test$datetime

rowery.test %>% select(-datetime)->rowery.test


rozw_cf<-exp(predict(my_forest_c,rowery.test))-1

#Combining two models

rozw_suma<-rozw_rf/10+rozw_cf/10+rozw_c*(9/10)+rozw_r*(9/10)
for (i in 1:length(rozw_suma))
{if (rozw_suma[i]<0)
{rozw_suma[i]=0}
}


rozw<-data.frame(id,rozw_suma)

names(rozw)<-c("datetime","count")


write.csv(rozw,file="rozw1.csv",row.names = FALSE)

