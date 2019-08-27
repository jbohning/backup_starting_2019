#########################################################

#DUPLICATES

#########################################################

data<- tbl(sc,sql("SELECT
                          A.GUID,TAXLOCTN,CLBASEFORM,CLFILSTAT
                          FROM RETURN_SUBSET18_V AS A
                          
                          WHERE (  
                          a.EDB_RETURN='Y' AND
                          a.nontps_rtn = 'N' AND
                          a.SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                          a.RETURNDATE BETWEEN Date('2018-01-01') AND Date('2018-04-10') 
                          
                          )"))

#Identify duplicate GUIDS
duplicates<-data %>%
    group_by(GUID) %>%
    filter(n()>1)
sdf_dim(duplicates) #I'd expect an output of 0x1 if there are no duplicates

#dim(duplicates) #use this code if you have a data.frame instead of sparklyr tables. If you
#aren't sure if you have a data.frame or a sparklyr table type:
class(data)

#Identify distinct GUIDS
unique_guids<-data%>%distinct(GUID) 


#########################################################

#FILTER TO FIRST ROW OF DUPLICATES

#########################################################

data_dedup<-data %>% 
    group_by(GUID) %>% 
    filter(row_number(GUID) == 1) %>% 
    ungroup()



#########################################################

#CHECK FOR NAs

#########################################################

#identify missing guids
missing_data_19<-data %>%
    #group_by(GUID) %>%
    filter(is.na(GUID)==TRUE)
sdf_dim(missing_data_19)

#or use the following to check for NAs across all columns
num_rows <- sdf_nrow(data)
NA_data <- data %>% 
    mutate_all(is.na) %>% 
    mutate_all(as.numeric) %>%
    summarize_all(sum) %>% 
    collect() %>% 
    gather(var_name, num_na) %>% 
    filter(num_na > 0) %>% 
    mutate(pct_missing = num_na/num_rows)
NA_data

#########################################################

#RETENTION

#########################################################
retention<- tbl(sc,sql("SELECT

                              count(*) as total_clients,
                                SUM(CASE WHEN Y19_RETURN = 'Y' AND Y19_RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-04-30') THEN 1 ELSE 0 end)  as retail_retained,
                                SUM(CASE WHEN Y19_RETURN = 'Y' AND Y19_RETURNDATE BETWEEN Date('2019-01-01') AND Date('2019-04-30') THEN 1 ELSE 0 end)/count(*) as retail_retention,

                                SUM(CASE WHEN Y19_GUID IS NOT NULL THEN 1
                                    WHEN Y19_ONLINE_GUID IS NOT NULL THEN 1
                                    WHEN Y19_SOFTWARE_GUID IS NOT NULL THEN 1
                                    ELSE 0 END) as enterprise_retained,

                                SUM(CASE WHEN Y19_GUID IS NOT NULL THEN 1
                                    WHEN Y19_ONLINE_GUID IS NOT NULL THEN 1
                                    WHEN Y19_SOFTWARE_GUID IS NOT NULL THEN 1
                                    ELSE 0 END) / count(*) as enterprise_retention

            
                              
                              FROM RETURN_SUBSET18_V AS A

                              
                              
                              WHERE (  
                              a.EDB_RETURN='Y' AND
                              a.nontps_rtn = 'N' AND
                              a.SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                              a.RETURNDATE BETWEEN Date('2018-01-01') AND Date('2018-04-30') 
                              
                              )"))

View(collect(head(retention,150)))




#########################################################

#RA DATA FOR TS18 OR TS19 (AND LATER?)
#yes, it references what looks like a TS19 table, but those 
#tables contain both TS18 and TS19 data

#########################################################
ra_18<- tbl(sc,sql("SELECT guid,  
        case when e.app_registered_ts is not null then 1 else 0 end as ra_applied,
		case when e.app_registered_ts is not null and e.app_decision_txt='APPROVED' then 1 else 0 end as ra_approved,
		case when e.app_approved_amt>0 then e.app_approved_amt else 0 end as RA_loan_amount
		

from cadm.return_subset18_v as a
left join (select return_guid,
           app_registered_ts, app_registered_ts,
           app_decision_txt, app_approved_amt, adv_bal_due_amt
           from cadm.refund_advance_account_19 as c
                inner join cadm.refund_advance_party_19 as d on c.account_no=d.account_no and c.loan_app_type_txt=d.type_cd
                where year(app_submitted_ts)=2018 ) as e on a.guid=e.return_guid
           
 WHERE (  
                              a.EDB_RETURN='Y' AND
                              a.nontps_rtn = 'N' AND
                              a.SOURCE IN ('COMPANY', 'FRANCHISE') AND 
                              a.RETURNDATE BETWEEN Date('2018-01-01') AND Date('2018-04-30') 
                              
                              )"))        

sdf_dim(ra_18%>%filter(ra_applied==1))           
View(collect(head(ra_18%>%filter(ra_applied==1),150)))      


#################################################################

#CREATE YoY GROWTH DATA FOR TS16-TS19

#################################################################
import_19<-spark_read_csv(sc,"import_19",
                          "/user/fa982526/csv_tables/MATCHING_BEST_PRACTICES/TS19_Data_Cleaned_csv")
import_18<-spark_read_csv(sc,"import_18",
                          "/user/fa982526/csv_tables/MATCHING_BEST_PRACTICES/TS18_Data_Cleaned_csv")
import_17<-spark_read_csv(sc,"import_17",
                          "/user/fa982526/csv_tables/MATCHING_BEST_PRACTICES/TS17_Data_Cleaned_csv")
import_16<-spark_read_csv(sc,"import_16",
                          "/user/fa982526/csv_tables/MATCHING_BEST_PRACTICES/TS16_Data_Cleaned_csv")


new_office_ids.unique<-rbind(import_19%>%dplyr::select(new_office_id),
                             import_18%>%select(new_office_id),
                             import_17%>%select(new_office_id),
                             import_16%>%select(new_office_id)
)
new_office_ids.unique<-new_office_ids.unique %>% distinct(new_office_id) %>% arrange(new_office_id) %>%na.omit()
sdf_register(new_office_ids.unique)

#Combine all of the data together
data.merged <- new_office_ids.unique %>% 
    left_join(import_19,by=c("new_office_id"="new_office_id")) %>%
    left_join(import_18,by=c("new_office_id"="new_office_id")) %>%
    left_join(import_17,by=c("new_office_id"="new_office_id")) %>%
    left_join(import_16,by=c("new_office_id"="new_office_id")) 

sdf_register(data.merged)

#ADD CALCULATIONS (means and percentages)
#converting sparklyr data to local dataframe
data_a <- as.data.frame(data.merged)

#ASSEMBLE EQUATION AS A FUNCTION:
# (VAR_CY- VAR_PY) / VAR_PY
variable_list<-tbl_vars(data_a)

#REMOVE ALL YEAR INFO
#variable_list<-variable_list[grepl("_1[[:digit:]]$",variable_list)]
variable_list<-unique(gsub(pattern="_1[[:digit:]]$",replacement="",variable_list))

#DROP PRCT VARIABLES
variable_list<-variable_list[!grepl("^prct_",variable_list)]

variable_list<-variable_list[!variable_list %in% c("new_office_id","hierarchy_change",
                                                   "zip_9_cd"
)]

library(rlang)
equations_temp<-paste0("(",variable_list,"_cy - ",variable_list,"_py)/", variable_list,"_py")
var_temp<-paste0("yoy_growth_",variable_list)

equations_function <- function(df,new_var, expression) {
    df %>% 
        mutate(!! new_var := !! parse_expr(expression))
}


#LOOP THROUGH THE COLUMNS TO CREATE THE YOY CHANGE IN VALUES
data_b<-data_a
for (i in 1:3){
    cy=as.character(16+i)
    py=as.character(15+i)
    eq_temp<-gsub(pattern="_cy",replacement=paste0("_",cy),equations_temp)
    eq_temp<-gsub(pattern="_py",replacement=paste0("_",py),eq_temp)
    var_temp_b<-paste0(var_temp,"_",py,cy)
    for(j in 1:length(eq_temp)){
        data_b<-equations_function(data_b,var_temp_b[j],eq_temp[j])
    }
}
