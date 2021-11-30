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
        spark.executor.memory = "64G",
        spark.executor.cores = 8,
        "sparklyr.shell.driver-memory"="16G",
        
        #spark.executor.memory = "16G",
        #spark.executor.cores = 4,
        #"sparklyr.shell.driver-memory"="8G",
        
        "spark.kryoserializer.buffer.max"="2000m"
    ))


dbGetQuery(sc, "USE cadm")


#################################################################

#TS19

#################################################################

import_19<- tbl(sc,sql("SELECT a.guid, 

            case when source in ('FRANCHISE') then 'Fran'
            when source in ('COMPANY') and hrb not in ('BLK ADV', 'PREMIUM') then 'SAS'
            when source in ('COMPANY') and hrb in ('BLK ADV', 'PREMIUM') then 'BA'
            else 'NA' end as source, 
            
            CASE WHEN substring(A.tenure_retail_taxprep,1,2) in('FT','RL') THEN
				'NEW' WHEN substring(A.tenure_retail_taxprep,1,2) in('RC') THEN 
				'PRIOR' ELSE 'NA' END AS tenure,
            tenure_retail_taxprep

            /*OTHER COMMONLY USED VARIABLES*/
            
            
            CASE WHEN CLAGE <=24 THEN 'AGE<=24'
				WHEN CLAGE>24 AND CLAGE<=34 THEN 'AGE>24,<=34'
				WHEN CLAGE >34 AND CLAGE<=44 THEN 'AGE>34,<=44'
				WHEN CLAGE >44 AND CLAGE<=64 THEN 'AGE>44,<=64'
				WHEN CLAGE >64 THEN 'AGE>64'
				ELSE 'NA' END AS clage,
				
		case when (a.cldephome+a.cldepnhome) = 0 then 0 else 1 end as kids,
		
		case when RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-02-28') then 'FIRST HALF'
		WHEN RETURNDATE BETWEEN Date('2019-03-01') AND Date('2019-04-30') THEN 'SECOND HALF'
		ELSE 'NA' END AS half,
				
		CASE WHEN D.EIC_AMT > 0 THEN 1 ELSE 0 END AS eitc,
		

				
		case when a.clfmbschyn='Y' then 1 else 0 end as schb,
		case when a.clfmcschyn='Y' then 1 else 0 end as schc,
		
		case when cltotagi >=0 and cltotagi <5000 then 'A: <5k'
		    when cltotagi >=5000 and cltotagi <10000  then 'B: 5k-10k'
		    when cltotagi >=10000 and cltotagi <25000  then 'C: 10k-25k'
		    when cltotagi >=25000 and cltotagi <50000  then 'D: 25k-50k'
		    when cltotagi >=50000 and cltotagi <75000  then 'E: 50k-75k'
		    when cltotagi >=75000   then 'F: >=75k'
		    when cltotagi <0 then 'G: <0k'
		    else 'H: NA' end as agi

        from return_subset19_v as a
                                
         where                          EDB_RETURN='Y' AND
                                        nontps_rtn = 'N' and
                                        SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                                        RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-04-30') 


                             "))