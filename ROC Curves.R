#create a graph of AUC values from 0.5 to 1

#Create variable that we are trying to predict
log_binary<-sample(x=c(-1,1),size=1000,replace=TRUE)

#Create variable used for a perfect fit
perfect<-log_binary

#Create variable for a 50/50 shot of guessing
a<-log_binary[1:500]
b<- -log_binary[501:1000]
coin_flip<-c(a,b)

#Variable for AUC of 0.7
a<-log_binary[1:700]
b<- -log_binary[701:1000]
var70<- c(a,b)

#Put all of the data together
df_temp<-data.frame(log_binary,perfect,coin_flip,var70)
df<-df_temp[sample(nrow(df_temp)),]

#randomly sort the df

#Model for the different AUCs
perfect_fit<-lm(log_binary~perfect, df)
coin_flip_fit<-lm(log_binary~coin_flip,df)
var70_fit<-lm(log_binary~var70,df)


#Graph the ROC Curves
library(ROCR)

#Perfect Fit
predictions<-predict(perfect_fit,df)
pr <- prediction(predictions, df$log_binary)
prf_perfect <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf_perfect)
auc_perfect <- performance(pr, measure = "auc")
auc_perfect <- auc_perfect@y.values[[1]]

#Coin Flip Fit
predictions<-predict(coin_flip_fit,df)
pr <- prediction(predictions, df$log_binary)
prf_coin_flip <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf_coin_flip)
auc_coin_flip <- performance(pr, measure = "auc")
auc_coin_flip <- auc_coin_flip@y.values[[1]]

#AUC of 70 Fit
predictions<-predict(var70_fit,df)
pr <- prediction(predictions, df$log_binary)
prf_var70 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf_var70)
auc_var70 <- performance(pr, measure = "auc")
auc_var70 <- auc_var70@y.values[[1]]


