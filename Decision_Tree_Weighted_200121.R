#####################################################

#User Inputs

#####################################################

#Yarn queue name
yarn_queue<-'starwars'

#Working Directory Location (Where the data file is and where to save pdf to)
workingdirectory<-"/appdata/workspace/cateam/shared/fa982526/R/Projects/2020/200121_Decision_Tree_Update"

#MAX ROWS TO PULL TO THE EDGE NODE
max_rows<-750000

#####################################################
setwd(workingdirectory)

## I don't need all of these, but I typically just load all of them
library(sparklyr)
library(dplyr)
library(DBI)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(readr)
library(tictoc)
library(ggplot2)
library(gridExtra)
library(grid)
library(knitr)
library(rmarkdown)
library(dbplot)
library(rpart)
library(rpart.plot)


# Connect to Yarn/Spark/Hive
sc <-
    spark_connect(master='yarn-client', config=list(
        spark.submit.deployMode='client',
        spark.yarn.queue=yarn_queue,
        spark.executor.memory='8G',
        spark.executor.cores=4,
        spark.sql.crossJoin.enabled='true',
        spark.driver.maxResultSize='4G',
        'sparklyr.shell.driver-memory'='4G',
        'sparklyr.log.console',
        spark.rpc.message.maxSize=999
    ))

dbGetQuery(sc, "USE cadm")


import_19<- tbl(sc,sql("SELECT
                          case when clfmeincyn = 'Y' then 'Yes' else 'No' end as eitc,
                          case when (a.rac='Y' or a.rac_easy_sav='Y') then 'Yes' else 'No' end as rt,
                          case when cltotagi <5000 then 'A Under $5K'
                                when cltotagi >=5000 and cltotagi <10000 then 'B $5K-$10K'
                                when cltotagi >=10000 and cltotagi <25000 then 'C $10K-$25K'
                                when cltotagi >=25000 and cltotagi <50000 then 'D $25K-$50K'
                                when cltotagi >=50000 and cltotagi <75000 then 'E $50K-$75K'
                                when cltotagi >=75000  then 'F $75K Plus' end as agi,
                          case when clage <= 24 then 'A 24 and Under'
                                when clage >24 and clage <=34 then 'B 25 to 34'
                                when clage >34 and clage <=44 then 'C 35 to 44'
                                when clage >44 and clage <=54 then 'D 45 to 54'
                                when clage >54 and clage <=64 then 'E 55 to 64'
                                when clage >64 then 'F 65 Plus' end as age,
                          case when SUBSTR(TENURE_RETAIL_TAXPREP,1,2) in ('FT','RL') then 'N' else 'P' end as new_prior,
                          case when cldepchyn = 'Y' then 'Yes' else 'No' end as dependent_child,
                          case when clfmcschyn = 'Y' then 'Yes' else 'No' end as sch_c,
                          case when clfilstat not in (1, 2, 4) then 1 else clfilstat end as clfilstat,
                          netprepfee - cltotdisc as rev_19
                          
                          FROM RETURN_SUBSET19_V AS A
                          
                          WHERE (  
                          a.EDB_RETURN='Y' AND
                          a.nontps_rtn = 'N' AND
                          a.SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                          a.RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-04-30') 

                          )"))


grouped_19<-import_19%>%
    group_by(eitc,rt,agi,age,new_prior,dependent_child,sch_c,clfilstat)%>%
    summarize(ts19_return = n(),
              ts19_rev = sum(rev_19)
              )


import_20<- tbl(sc,sql("SELECT
                          case when clfmeincyn = 'Y' then 'Yes' else 'No' end as eitc,
                          case when (a.rac='Y' or a.rac_easy_sav='Y') then 'Yes' else 'No' end as rt,
                          case when cltotagi <5000 then 'A Under $5K'
                                when cltotagi >=5000 and cltotagi <10000 then 'B $5K-$10K'
                                when cltotagi >=10000 and cltotagi <25000 then 'C $10K-$25K'
                                when cltotagi >=25000 and cltotagi <50000 then 'D $25K-$50K'
                                when cltotagi >=50000 and cltotagi <75000 then 'E $50K-$75K'
                                when cltotagi >=75000  then 'F $75K Plus' end as agi,
                          case when clage <= 24 then 'A 24 and Under'
                                when clage >24 and clage <=34 then 'B 25 to 34'
                                when clage >34 and clage <=44 then 'C 35 to 44'
                                when clage >44 and clage <=54 then 'D 45 to 54'
                                when clage >54 and clage <=64 then 'E 55 to 64'
                                when clage >64 then 'F 65 Plus' end as age,
                          case when SUBSTR(TENURE_RETAIL_TAXPREP,1,2) in ('FT','RL') then 'N' else 'P' end as new_prior,
                          case when cldepchyn = 'Y' then 'Yes' else 'No' end as dependent_child,
                          case when clfmcschyn = 'Y' then 'Yes' else 'No' end as sch_c,
                          case when clfilstat not in (1, 2, 4) then 1 else clfilstat end as clfilstat,
                          netprepfee - cltotdisc as rev_20
                          
                          FROM RETURN_SUBSET20_V AS A
                          
                          WHERE (  
                          a.EDB_RETURN='Y' AND
                          a.nontps_rtn = 'N' AND
                          a.SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                          a.RETURNDATE BETWEEN Date('2020-01-01') AND Date('2020-04-30') 

                          )"))


grouped_20<-import_20%>%
    group_by(eitc,rt,agi,age,new_prior,dependent_child,sch_c,clfilstat)%>%
    summarize(ts20_return = n(),
              ts20_rev = sum(rev_20)
    )

#THE LEFT JOIN WILL JOIN ON ALL COMMON COLUMN NAMES (since no columns were specified)
merged_1920<-grouped_19%>%full_join(grouped_20)

#REPLACE NULLS IN RETURNS & REV WITH ZEROS
final_spark_df<-merged_1920%>%mutate(ts19_return=ifelse(is.na(ts19_return)==TRUE,0,ts19_return),
                                     ts19_rev=ifelse(is.na(ts19_rev)==TRUE,0,ts19_rev),
                                     ts20_return=ifelse(is.na(ts20_return)==TRUE,0,ts20_return),
                                     ts20_rev=ifelse(is.na(ts20_rev)==TRUE,0,ts20_rev))

#CALCULATE THE PERCENT CHANGE IN RETURNS
final_spark_df<-final_spark_df%>%mutate(return_growth=ts20_return/ts19_return -1)

#####################################################

# BRING DATA LOCALLY

#####################################################
#BRING DATA TO THE EDGE NODE - DUE TO EDGE NODE LIMITS, ONLY PULL THE WHOLE DATA DOWN
#IF THE ROWS ARE LESS THAN OUR MAX, ELSE RANODMLY SAMPLE DOWN TO OUR MAX
n_obs <- sdf_nrow(final_spark_df)

if(n_obs > max_rows) {
    frac_pct <- (num_to_sample/n_obs)
    collected_data <- final_spark_df %>% 
        sdf_sample(fraction = frac_pct, replace = FALSE, seed = 1041) %>% 
        as.data.frame()
} else {
    collected_data <- final_spark_df %>% 
        as.data.frame()
}


#####################################################

# DECISION TREE CODE

#####################################################

#Prior Client Growth:
temp_data<-collected_data[collected_data$new_prior=="P",]
#Randomly arrange rows
n <- nrow(temp_data)
set.seed(123)
roworder <- sample(1:n, n)
shuffled_data <- temp_data[roworder, ]  

# GROW TREE - MANUALLY ENTER THE VARIABLES YOU WANT TO MODEL ON
fit_prior <- rpart(return_growth~eitc+rt+agi+age+dependent_child+sch_c+clfilstat, 
                   weights=ts19_return,
                   method="anova", 
                   #method='class', #This is for binary outcomes
                   data=temp_data)

#New Client Growth:
temp_data<-collected_data[collected_data$new_prior=="N",]
#Randomly arrange rows
n <- nrow(temp_data)
set.seed(123)
roworder <- sample(1:n, n)
shuffled_data <- temp_data[roworder, ]  

# GROW TREE - MANUALLY ENTER THE VARIABLES YOU WANT TO MODEL ON
fit_new <- rpart(return_growth~eitc+rt+agi+age+dependent_child+sch_c+clfilstat, 
                 weights=ts19_return,
                 method="anova", 
                 #method='class', #This is for binary outcomes
                 data=temp_data)

pdf(paste0("Decision_Tree_",Sys.Date(),".pdf"))
rpart.plot(fit_prior,type=2,box.palette="RdYlGn",main=paste0("Prior Client Growth - Jan 1 through ",
                                                             months(Sys.Date()-1,abbreviate=TRUE)," ",
                                                             format(Sys.Date()-1,"%d"),
                                                             "; Day-to-Day Comparison"),
           tweak=1,prefix="Mean Growth: "
)
rpart.plot(fit_new,type=2,box.palette="RdYlGn",main=paste0("New Client Growth - Jan 1 through ",
                                                           months(Sys.Date()-1,abbreviate=TRUE)," ",
                                                           format(Sys.Date()-1,"%d"),
                                                           "; Day-to-Day Comparison"),
           tweak=1,prefix="Mean Growth: ")
dev.off()


#####################################################

# ACCURACY

#####################################################
#R-Squared FOR NON-BINARY PREDICTIONS
rsq <- function (x, y) cor(x, y, use="complete.obs") ^ 2
pred_new<-predict(fit_new, temp_data)
actual_new<-temp_data$return_growth
rsq_new<-rsq(pred_new,actual_new)
print(rsq_new) 
plot(actual_new, pred_new)
    
#ACCURACY FOR BINARY PREDICTIONS
#Replace 'temp_data$binary_indiators_name' with the data and variable you are trying to predict
library(caret)
pred_new <- predict(object=fit_new,temp_data,type="class")
t_new <- table(temp_data$binary_indiators_name,pred_new)
confusionMatrix(t_new)



