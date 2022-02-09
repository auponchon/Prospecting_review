library(tm)
library(wordcloud)
library(tidyverse)
library(here)


files<-list.files(here("Data","Abstracts"), pattern=".txt", full.names = T)

#read text files
text<-lapply(files,readLines)

#convert text to a corpus where text is c()
docs <- Corpus(VectorSource(text))
inspect(docs)

#Clean text
docs.clean <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(removeWords, stopwords(lang)) # Remove stopwords for the language 

    
    
#generate a matrix 
dtm <- TermDocumentMatrix(docs.clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#create the word cloud
wordcloud(words = d$word, freq = d$freq, min.freq =20,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(dtm, terms = "prospecting", corlimit = 0.5)
