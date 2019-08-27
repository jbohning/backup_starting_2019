#Spell Checking Example
#https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html

library(hunspell)

#Check indiviual words
words <- c("beer", "wiskey", "wine")
correct <- hunspell_check(words)
print(correct)

# Find suggestions for incorrect words
hunspell_suggest(words[!correct])


#In practice we often want to spell check an entire document at once
#by searching for incorrect words. This is done using the 
#hunspell function:

bad <- hunspell("spell checkers are not neccessairy for langauge ninjas")
print(bad[[1]])

hunspell_suggest(bad[[1]])

download.file("https://arxiv.org/e-print/1406.4806v1", "1406.4806v1.tar.gz",  mode = "wb")
untar("1406.4806v1.tar.gz", "content.tex")
text <- readLines("content.tex", warn = FALSE)
bad_words <- hunspell(text, format = "latex")
sort(unique(unlist(bad_words)))


#The hunspell_stem looks up words from the dictionary which match the 
#root of the given word. Note that the function returns a list because
#some words can have multiple matches.

# Stemming
words <- c("love", "loving", "lovingly", "loved", "lover", "lovely")
hunspell_stem(words)


#The hunspell_analyze function is similar, but it returns both the stem
#and the affix syntax of the word:
hunspell_analyze(words)



#To support spell checking on documents, Hunspell includes parsers for
#various document formats, including text, html, xml, man or latex. 
#The Hunspell package also exposes these tokenizers directly so they 
#can be used for other application than spell checking.

text <- readLines("content.tex", warn = FALSE)
allwords <- hunspell_parse(text, format = "latex")

# Third line (title) only
print(allwords[[3]])


#Summarizing Text
#In text analysis we often want to summarize text via it’s stems. 
#For example we can count words for display in a wordcloud:
allwords <- hunspell_parse(janeaustenr::prideprejudice)
stems <- unlist(hunspell_stem(unlist(allwords)))
words <- sort(table(stems), decreasing = TRUE)
print(head(words, 30))

#filter out stop words
df <- as.data.frame(words)
df$stems <- as.character(df$stems)
stops <- df$stems %in% stopwords::stopwords(source="stopwords-iso")
wcdata <- head(df[!stops,], 150)
print(wcdata, max = 40)

library(wordcloud2)
names(wcdata) <- c("word", "freq")
wcdata$freq <- (wcdata$freq)^(2/3)
wordcloud2(wcdata)


