#####################################

#FUNCTION TO CREATE WORD CLOUDS
#AUTHOR: JESSICA BOHNING

#####################################


word_cloud_function<-function(data,pdf_save_name_and_location,word_cloud_frequency_name_and_location){
    #data requires a character vector
    #pdf_save_name_and_location requires you to name the world cloud pdf and where to save it too. If NULL, then 
        #no file will be saved.
        #example: pdf_save_name_and_location="/appdata/workspace/cateam/shared/fa982526/word_cloud.pdf"
    #word_cloud_frequency_name_and_location requires you to name the csv file and where to save it to if you want
        #to save the individual words and their frequency of use. If NULL then no file is saved
        #example: word_cloud_frequency_name_and_location="/appdata/workspace/cateam/shared/fa982526/word_cloud_freq.csv"
    
    #example:
    # word_cloud_function(data=test,
    #                     pdf_save_name_and_location=paste0(getwd(),"/testwc.pdf"),
    #                     word_cloud_frequency_name_and_location=paste0(getwd(),"/testfreq.csv"))
    
    #LOAD LIBRARIES
    library(tm)
    library(SnowballC) 
    library(wordcloud) 
    library(RColorBrewer) 
    library(stringr)
    library(plyr)
    library(ngram)
    library(hunspell)
    library(purrr)
    library(plyr)
    library(dplyr)
    library(rlang)
    
    print("Cleaning data")
    # returns string w/o leading or trailing whitespace
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    word_cloud_data<-trim(data)
    
    #remove special characters
    word_cloud_data<-gsub("[^[:alnum:][:space:]]","",word_cloud_data)
    
    word_cloud_data<-iconv(word_cloud_data,"WINDOWS-1252","UTF-8")
    
    #transform to lower case
    word_cloud_data<-tolower(word_cloud_data)
    
    #Eliminate places with more than one space
    word_cloud_data<-gsub("\\s+", " ", str_trim(word_cloud_data))
    
    print("load data as a corpus")
    ## Load the data as a corpus
    docs<-Corpus(VectorSource(word_cloud_data))
    
    ## you can use inspect(docs) to see the content of the document
    
    ##Text Transformation
    toSpace<-content_transformer(function (x,pattern) gsub(pattern,"",x))
    docs<-tm_map(docs,toSpace,"/")
    docs<- tm_map(docs,toSpace,"@")
    docs<- tm_map(docs,toSpace,"\\|")
    docs<- tm_map(docs,toSpace,"??")
    docs<- tm_map(docs,toSpace,"??")
    
    ##Convert the text to lower case
    docs<-tm_map(docs,content_transformer(tolower))
    
    ## Remove numbers
    docs<-tm_map(docs, removeNumbers)
    
    ##Remove english common stopwords
    docs<-tm_map(docs,removeWords,stopwords("en"))
    
    ## Remove your own stop words
    ## docs<-tm_map(docs,removeWords,c("InsertWord1Here", "InsertWord2Here"))
    
    
    ## Remove punctuation
    docs<-tm_map(docs,removePunctuation)
    docs<-tm_map(docs,stripWhitespace)
    
    print("Build term-document matrix")
    ## Build a term-document matrix (a document matrix is a table containing the 
    ## frequency of the words)
    dtm<-TermDocumentMatrix(docs)
    m<-as.matrix(dtm)
    v<-sort(rowSums(m),decreasing=TRUE)
    d<-data.frame(word=names(v),freq=v)
    head(d,10)
    
    print("stemming_a")
    #new stemming: get it to work
    d$word<-as.character(d$word)
    a<-stemDocument(d$word)
    dictionary<-d$word
    b<-data.frame(original=d$word,
                  frequency=d$freq,
                  stemmed=a,
                  completed=stemCompletion(a,d$word,type="first"))
    #write.csv(b,paste0("Word Freqs/","allwords_stemmed",Sys.Date(),".csv"))
    
    
    #MANUALLY ADJUST SOME OF THE STEMS
    print("stemming_b")
    b$original<-as.character(b$original)
    b$completed<-as.character(b$completed)
    b<-mutate(b,completed2=case_when(is.na(b$completed)==TRUE ~ b$original,
                                     b$original=="business" ~ b$original,
                                     b$original=="management" ~ b$original,
                                     b$original=="passionate" ~ b$original,
                                     b$original=="executive" ~ b$original,
                                     b$original=="executives" ~ "executive",
                                     b$original=="office" ~ b$original,
                                     b$original=="offices" ~ "office",
                                     b$original=="service" ~ b$original,
                                     b$original=="services" ~ "service",
                                     b$original=="improve" ~ b$original,
                                     b$original=="improvement" ~ b$original,
                                     b$original=="improvements" ~ "improvement",
                                     b$original=="opportunity" ~ b$original,
                                     b$original=="opportunities" ~ "opportunity",
                                     b$original=="appointment" ~ b$original,
                                     b$original=="appointments" ~ "appointment",
                                     b$original=="customer" ~ "customer",
                                     b$original=="execute" ~ "execute",   
                                     b$original=="execution" ~ "execute", 
                                     b$original=="executed" ~ "execute", 
                                     b$original=="executing" ~ "execute", 
                                     
                                     b$original=="automate" ~ "automate",
                                     b$original=="automation" ~ "automate",
                                     b$original=="automated" ~ "automate",
                                     b$original=="automating" ~ "automate",
                                     b$original=="automatic" ~ "automatic",
                                     b$original=="automatically" ~ "automatic",
                                     
                                     b$original=="really" ~ "really",
                                     b$original=="especially" ~ "especially",
                                     b$original=="technically" ~ "technically",
                                     b$original=="technical" ~ "technical",
                                     b$original=="try" ~ "try",
                                     b$original=="trying" ~ "try",
                                     b$original=="tried" ~ "try",
                                     b$original=="tries" ~ "try",
                                     b$original=="trip" ~ "trip",
                                     b$original=="trips" ~ "trip",
                                     b$original=="ability" ~ "ability",
                                     b$original=="abilities" ~ "ability",
                                     b$original=="elevate" ~ "elevate",
                                     b$original=="elevating" ~ "elevate",
                                     b$original=="elevated" ~ "elevate",
                                     
                                     b$original=="available" ~ "available",
                                     b$original=="availability" ~ "available",
                                     b$original=="unavailable" ~ "unavailable",
                                     b$original=="availabilities" ~ "available",
                                     b$original=="guidance" ~ "guidance",
                                     b$original=="guide" ~ "guide",
                                     b$original=="guided" ~ "guide",
                                     b$original=="guiding" ~ "guide",
                                     b$original=="guides" ~ "guide",
                                     b$original=="external" ~ "external",
                                     b$original=="externally" ~ "external",
                                     b$original=="externs" ~ "external",
                                     b$original=="externalization" ~ "external",
                                     b$original=="experience" ~ "experience",
                                     b$original=="experiences" ~ "experience",
                                     b$original=="experiencing" ~ "experience",
                                     b$original=="experienced" ~ "experience",
                                     b$original=="experiment" ~ "experiment",
                                     b$original=="experimenting" ~ "experiment",
                                     b$original=="experimented" ~ "experiment",
                                     b$original=="experiments" ~ "experiment",
                                     b$original=="many" ~ "many",
                                     
                                     
                                     TRUE~ b$completed))
    #Verification step
    #b[grep(pattern="continu",b$stemmed),]
    
    d2<-b%>%group_by(completed2)%>%dplyr::summarize(freq=sum(frequency))
    
    names(d2)<-c("word","freq")
    d2<-arrange(d2,desc(freq))
    
    #Generate the word cloud and save it if pdf_save_name_and_location is not NULL
    print("generating word cloud")
    if(is.na(pdf_save_name_and_location)==TRUE){
        set.seed(76)
        wordcloud(words=d2$word, freq=d2$freq, min.freq = 1, max.words = 200,
                  random.order = FALSE, rot.per=0.35, scale=c(3,.5),
                  colors=brewer.pal(8,"Dark2"))
    }else{
        pdf(pdf_save_name_and_location)
        set.seed(76)
        wordcloud(words=d2$word, freq=d2$freq, min.freq = 1, max.words = 200,
                  random.order = FALSE, rot.per=0.35, scale=c(3,.5),
                  colors=brewer.pal(8,"Dark2"))
        dev.off()
        
    }
    #save freq off if word_cloud_frequency_name_and_location is not NULL
    if(is.na(word_cloud_frequency_name_and_location)==FALSE){
        write.csv(d2,word_cloud_frequency_name_and_location)
    }
    print("finished")
}