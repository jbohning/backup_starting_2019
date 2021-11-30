theme_set(theme_gray()+ theme(panel.background=element_rect(fill="lightgray"))+
              theme(plot.title = element_text(hjust = 0.5)))

####################################
#CHECK FOR NAs
####################################

query<-paste0("select ",paste("sum(case when ",tbl_vars(retail_data)," is NULL then 1 else 0 end) as ",
                              tbl_vars(retail_data),collapse = ","),
              " from retail_data")
retail_NAs<-sdf_sql(sc,query)
sdf_register(retail_NAs)
View(t(collect(retail_NAs)))


####################################
#COMBINE CSVs
####################################
temp <- list.files(path="/project_data/assets/data_asset/Gap_data_office_mar24_run.csv/",pattern="*.csv")
gap<-data.frame(NULL)
for (i in 1:length(temp)){
    gap<-rbind(gap,
               read.csv(paste0("/project_data/assets/data_asset/Gap_data_office_mar24_run.csv/",temp[i]),
                        stringsAsFactors = FALSE))
}


####################################
#WRITE TO NETEZZA
####################################
nps_write <- function(df, name, batchsize = '100000'){
    library(projectLib)
    project <- projectLib::Project$new()
    credentials = project$get_connection(name="CAPLAYGROUND")
    url = paste("jdbc:netezza://", credentials[][["host"]], ":", credentials[][["port"]], "/", credentials[][["database"]], sep="")
    df %>%
        spark_write_jdbc(name=name,
                         options=list('url'=url,
                                      'user'=credentials$username,
                                      'password'=credentials$password,
                                      'port'=credentials$port,
                                      'batchsize'=batchsize))
}


####################################
#DROP FROM NETEZZA
####################################
nps_drop_table <- function(name){
    library(projectLib)
    library(RJDBC)
    project <- projectLib::Project$new()
    credentials = project$get_connection(name="CAPLAYGROUND")
    drv <- JDBC(driverClass="org.netezza.Driver", classPath="/opt/ibm/connectors/others-db-drivers/nzjdbc3.jar")
    CAPLAYGROUND_connection <- dbConnect(drv,
                                         paste("jdbc:netezza://", credentials[][["host"]], ":", credentials[][["port"]], "/", credentials[][["database"]], sep=""),
                                         credentials[][["username"]],
                                         credentials[][["password"]])
    query <- paste("drop table",name)
    dbSendUpdate(CAPLAYGROUND_connection, query)
    dbDisconnect(CAPLAYGROUND_connection)
}


####################################
#CREATE NEW FOLDER IF IT DOESN'T EXIST YET
####################################
if(!dir.exists(paste(getwd(), "Independent_Vars", sep = "/"))) {dir.create(paste(getwd(), "Independent_Vars",sep = "/"))}


####################################
#RENAME THE COLUMN NAMES IN SPARK TO LOWER CASE 
####################################
newnames<-tolower(tbl_vars(data_table_1))
data_table_2<-data_table_1%>%dplyr::select(setNames(colnames(data_table_1),newnames))
temp<-sdf_register(data_table_2,"data_table_2")



