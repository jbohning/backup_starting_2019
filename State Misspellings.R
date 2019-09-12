setwd("G:/marketing/Marketing Analytics/Bohning_Jessica/Projects/2019/190912_Webscrape_State_Misspellings")

#install.packages("rvest")
library("rvest")
url <- "https://offices.net/misspelled-state-names.htm"
states <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()

states_b<-data.frame(states)

names(states_b)<-c("state","misspelling")

states_b$misspelling<-gsub("Commonly misspelled as ",replacement="",states_b$misspelling)

states_b<-states_b[(which(nchar(states_b$state)!=1)),]

write.csv(states_b,"state_misspellings.csv")