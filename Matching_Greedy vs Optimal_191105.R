
###############################

#Optimal Matching

###############################

library(MatchIt)
library(optmatch)

set.seed(632)
x<-rnorm(100)
y<-x*2+rnorm(100)/50
z<-x*y+rnorm(100)/100
id<-as.character(1:100)
test_indicator<-sample(c(1,0),size=100,prob=c(0.45,0.55),replace=TRUE)

df<-data.frame(id,x,y,z,test_indicator)

match_opt<-matchit(test_indicator~x+y+z,data=df,method="optimal")

print(summary(match_opt))
plot(match_opt)
#plot(match_opt, type="jitter")


matched_data_opt<-data.frame(test=rownames(match_opt$match.matrix),match_opt$match.matrix)
names(matched_data_opt)<-c("test","control")

df.match_opt <- match.data(match_opt)



###############################

#Greedy Matching/Nearest Neighbor

###############################

match_nn<-matchit(test_indicator~x+y+z,data=df,method="nearest")

print(summary(match_nn))
plot(match_nn)
#plot(match_nn, type="jitter")


matched_data_nn<-data.frame(test=rownames(match_nn$match.matrix),match_nn$match.matrix)
names(matched_data_nn)<-c("test","control")

df.match_nn <- match.data(match_nn)



###############################

#Compare SMD

###############################

print("OPTIMAL MATCHING")
print(CreateTableOne(vars=c("x","y","z"), strata ="test_indicator", 
                     data=df.match_opt, test = FALSE),smd=TRUE)

print("NEAREST NEIGHBOR")
print(CreateTableOne(vars=c("x","y","z"), strata ="test_indicator", 
                     data=df.match_nn, test = FALSE),smd=TRUE)










