library(sparklyr)
library(plyr)
library(dplyr)
library(DBI)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)
library(dbplot)
library(reshape2)


queue<-"starwars"

sc <-
    spark_connect(master='yarn-client', config=list(
        spark.submit.deployMode='client',
        spark.yarn.queue=queue,
        spark.executor.memory='64G',
        spark.executor.cores=8,
        spark.sql.crossJoin.enabled='true',
        spark.driver.maxResultSize='4G',
        'sparklyr.shell.driver-memory'='8G',
        
        'sparklyr.log.console',
        spark.rpc.message.maxSize=999
    ))

import_19<-tbl(sc,sql("SELECT a.guid, 

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
        from cadm.return_subset19_v as a
                                
         where                          EDB_RETURN='Y' AND
                                        nontps_rtn = 'N' and
                                        SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                                        RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-04-30') 


                             "))
