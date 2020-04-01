# THIS FILE SHOWS SOME COMMONLY USED SAS QUERRIES AND HOW TO RECREATE THEM IN R


library(sparklyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(DBI)
library(ggplot2)
library(caret)
library(reshape2)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(tictoc)


theme_set(theme_gray()+ theme(panel.background=element_rect(fill="lightgray"))+
              theme(plot.title = element_text(hjust = 0.5)))

Sys.setenv("SPARK_HOME"="/opt/cloudera/parcels/SPARK2/lib/spark2")


#may need to use this
spark_disconnect_all()

#keep track of time
ptm <- proc.time()

#changed the connection string. No major changes.

#set up spark connection
sc <-
    spark_connect(master = "yarn-client", config = list(
        spark.submit.deployMode = "client",
        spark.yarn.queue="starwars",
        spark.executor.memory = "16G",
        spark.executor.cores = 4,
        "sparklyr.shell.driver-memory"="8G",
        
        "spark.kryoserializer.buffer.max"="2000m"
    ))


dbGetQuery(sc, "USE cadm")


#################################################################

#BASIC SQL SERVER DATA PULL


#This replaced the sql connection code. Notice that the r code
#is basically identical to the SAS code wiht a few exceptions:
#   Date Formats need to use Date()
#   You have to use subtring() instead of substr()
#Also, because r is case sensitive, all of my column names are
#lower case (to make it easier to remember their names later on)

#################################################################

import_19=tbl(sc,sql("SELECT a.guid, 

            case when source in ('FRANCHISE') then 'Fran'
            when source in ('COMPANY') and hrb not in ('BLK ADV', 'PREMIUM') then 'SAS'
            when source in ('COMPANY') and hrb in ('BLK ADV', 'PREMIUM') then 'BA'
            else 'NA' end as source, 
            
            CASE WHEN substring(A.tenure_retail_taxprep,1,2) in('FT','RL') THEN
				'NEW' WHEN substring(A.tenure_retail_taxprep,1,2) in('RC') THEN 
				'PRIOR' ELSE 'NA' END AS tenure,
			tenure_retail_taxprep,
            
            cltotagi, clage,

    		case when RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-02-28') then 'FIRST HALF'
    		WHEN RETURNDATE BETWEEN Date('2019-03-01') AND Date('2019-04-30') THEN 'SECOND HALF'
    		ELSE 'NA' END AS half
        from return_subset19_v as a
                                
         where                          EDB_RETURN='Y' AND
                                        nontps_rtn = 'N' and
                                        SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                                        RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-04-30') 


                             "))



#################################################################

#GET SOME DESCRIPTIVE STATS FOR THE DATA

#################################################################
#GET DIMENSIONS
sdf_dim(import_19) #OUTPUT: 10666549        7



#GET COLUMN NAMES: either of the following will work, but they result in
#different output formats
View(tbl_vars(import_19)) #NOTE: you can search for values this way; this outputs colum names into a separate table
tbl_vars(import_19)



#VIEW A SAMPLE OF THE DATA: the blue number is how many rows to output
View(collect(head(import_19,150)))



#VIEW THE FIRST150 ROWS OF JUST THE GUIDS: the blue number is how many rows to output
View(collect(head(import_19%>%dplyr::select(guid),150)))






#################################################################

#PROC FREQ EQUIVALENT

#################################################################
#I am getting a proc freq on the variable tenure_retail_taxprep
#I didn't save it off to a variable, but you are able to do so
#I give you two options: the first is easier,but can't calculate 
#percentages; the second is a bit more complicated

#OPTION 1
import_19%>%group_by(tenure_retail_taxprep)%>%
    summarize(frequency=n()
              )

#OPTION 2
numb_rows=sdf_nrow(import_19)
import_19%>%group_by(tenure_retail_taxprep)%>%
    summarize(frequency=n(),
              percentages=n() / numb_rows
    )



#NOTE: YOU CAN FILTER THIS CODE TOO
#EXAMPLE: get frequency of tenure_retail_taxprep for all clients 
#under the age of 26
import_19%>%
    filter(clage<26)%>%
    group_by(tenure_retail_taxprep)%>%
    summarize(frequency=n()
    )

#NOTE THAT WHEN I ADDED THE 'filter()' FUNCTION THAT I ALSO ADDED
#THE '%>%' OPERATOR, ALSO KNOWN AS THE PIPELINE OPERATOR, WHICH
#LETS YOU LINK ALL OF THE FUNCTIONS TOGETHER

#NOTE: that order doesn't matter for the group_by() and filter() functions, 
#but the summarize() function has to go third(). 
#I also added an arrange function, which sorts the rows by descending frequency

import_19%>%
    group_by(tenure_retail_taxprep)%>%
    filter(clage<26)%>%
    summarize(frequency=n())%>%
    arrange(desc(frequency))
    






#################################################################

#PROC UNIVARIATE EQUIVALENT

#################################################################
#I am going to run a proc univariate equivalent on the cltotagi
#variable. Once again, I didn't save it off to a variable, but 
#you are able to do so

#You can pass it a vector of probabilities you are interested in
sdf_quantile(import_19,'cltotagi',probabilities=c(0,.1,.25,.5,.75,.9,1))

#Or you can generate a sequence for quicker speed.
#I chose a sequence from zero to one, increasing by 0.01
#(So outputting quantiles by percentage point)
sdf_quantile(import_19,'cltotagi',probabilities=seq(0,1,.01))






#################################################################

#PROC MEANS EQUIVALENT

#################################################################
#there is a lot of flexibility here, but the following will
#calcuate some common stats on cltotagi


#Note type out the column name every time, which is a little frustraing at first,
#but allows for more dynamic calculations later. See how below I can get stats on multiple 
#variables at once
import_19%>%
    summarize(count=n(),
              mean=mean(cltotagi),
              std_dev=sd(cltotagi),
              minimum=min(cltotagi),
              maximum=max(cltotagi),
              
              #SOME ADDITION CALCS THAT YOU MIGHT BE INTERESTED IN
              median=percentile(cltotagi,0.5),
              q1=percentile(cltotagi,0.25),
              q3=percentile(cltotagi,0.75),
              number_of_clients_below_31_years_old=sum(case_when(clage<31~1,TRUE~0)),
                  #the case when statement translates to sas as: 
                  #case when clage < 31 then 1 else 0 end
              percent_of_clients_below_31_years_old=sum(case_when(clage<31~1,TRUE~0))/n()
              
              )
#Note that i put a comma after every new data calculation (because each of those lines
#are going to be their own columns)

#The default option is to ignore NAs, but to silence the warings, you can
#add 'na.rm=TRUE'
import_19%>%
    summarize(min_cltotagi=min(cltotagi,na.rm=TRUE),
              max_cltotagi=max(cltotagi,na.rm=TRUE)
              )

#You can also summarize all columns using the summarize_all() function 
#(note I had to drop the character variables like guid and half),
#but notice that n() and sd() don't work in this function
import_19%>%select(-guid,-half,-tenure_retail_taxprep,-source,-tenure)%>%
    summarize_all(list(avg=mean,minimum=min,maximum=max))


#################################################################

#PROC SORT EQUIVALENT

#################################################################
#the example below sorts the data by clage and then cltotagi and stores
#it to a new variable. Note: that you have to save the data
#off- it won't automatically update like in SAS
sorted_data=import_19%>%arrange(clage,cltotagi)


#And the following works as a proc sort with 'nodupkey' ie 
#removes all duplicates except the first row
data_dedup<-import_19 %>% 
    group_by(guid) %>% 
    filter(row_number(guid) == 1) %>% 
    ungroup()





#################################################################

#PROC SURVEYSELECT EQUIVALENT

#################################################################
#The following will produce a simple random sampling of your data
random_sample=sdf_sample(x=import_19,seed=2134,replacement=FALSE,fraction=30/10600000)
    #x is your data
    #seed is an option integer seed
    #replacement is a TRUE/FALSE value, do you want to sample with replacement?
    #fraction is the fraction to sample. I choose to get roughly 30 rows (out of our 10.6M)





#################################################################

#PROC REG EQUIVALENT

#################################################################
#The following will produce a linear regression. In this example,
#I will want to model cltotagi based on clage and tenure_retail_taxprep.
#For serious modeling, I would use the h2o and not sdf, but that
#requires a different connection string (above)

linear_model=import_19%>%ml_linear_regression(cltotagi~clage+tenure_retail_taxprep)
linear_model #typing this into the console will give you the coefficients
summary(linear_model) #gives you the coefficients, deviance residuals, r-squared, and root mean squared error




#################################################################

#OTHER USEFUL EQUIVALENTS - JOINS

#################################################################

#Joins work basically the same as in sas. The following creates a 
#second data frame and joins it to the original
import_18=tbl(sc,sql("SELECT y19_guid, cltotagi as cltotagi_18
        from return_subset18_v as a
        where       EDB_RETURN='Y' AND
                    nontps_rtn = 'N' and
                    SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                    RETURNDATE BETWEEN Date('2018-01-01') AND Date('2018-04-30') 
                             "))   
    
left_joined_data=import_19 %>% left_join(import_18,by=c("guid"="y19_guid"))    
        #Instead of joining on a.guid=b.ts19_guid, you use the 'by' statement
        #the first variable you list is 'a' and the second is the 'b'

inner_joined_data=import_19%>%inner_join(import_18,by=c("guid"="y19_guid"),suffix=c("","_ts18"))
        #note that I added an optional 'suffix'. This allows you to change all of the column names
        #in one or more tables. I chose to add nothing to the end of the first table's column names
        #and I added a '_ts18' to the end of the second table's column names


#you can also create a chain of joins (with and without the suffix):
multiple_joins=import_19 %>% 
                    left_join(import_18,by=c("guid"="y19_guid"),suffix=c("","_A"))%>%
                    left_join(import_18,by=c("guid"="y19_guid"),suffix=c("","_B"))
    
    
    

#################################################################

#OTHER USEFUL EQUIVALENTS - CASE WHENS

#################################################################
#CASE WHENS are similar in r and SAS, but with a few key differences
#Note: that these changes only apply when you are working outside of
#the import statement (where we created import_19). 

#This example creates buckets for agi:
import_19_agi_buckets=import_19%>%mutate(agi_buckets=case_when(cltotagi<25000 ~ '$25k or Less',   
                                                                    #creates first bucket for clients with an agi of
                                                                    #less than $25k and calls it "$25k or Less"
                                                                    #in SAS, you would write this as: 
                                                                    #case when cltotagi < 25000 then '$25k or Less'
                                                               cltotagi<50000 ~ '$25k to $50k',
                                                               cltotagi<75000 ~ '$50k to $75k',
                                                               cltotagi>75000 ~ '$75k or More',
                                                               TRUE~ 'OTHER'
                                                                    #The above statement 'TRUE~'OTHER'' is like the else bucket
                                                                    #and says that for any other cases, their bucket should be
                                                                    #labeled 'OTHER. 
                                                                    #in SAS, you would write this as:
                                                                    #else 'OTHER' end
                                                               )
                                         )
#the mutate() function allows you to add additional columns to your table
                                                        
#You can get a quick view of the data by typing either of the following
head(import_19_agi_buckets,10)
#or
View(collect(head(import_19_agi_buckets,10)))









