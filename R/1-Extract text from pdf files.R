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
#    tm_map(stripWhitespace) %>% 
#    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("en")) 

    
    #generate a matrix 
dtm <- TermDocumentMatrix(docs.clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)


d <- data.frame(word = names(v),freq=v)
head(d, 10)

d<-d[-c(8,16,25,36,37),]


#create the word cloud
tiff(here::here("Figures","WordCloud.tiff"),compression="lzw", height=3000,width=4500,res=500)
wordcloud(words = d$word, freq = d$freq, min.freq =30,random.color=F,
          max.words=20, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(6, "Dark2"))
dev.off()



findAssocs(dtm, terms = "dispersal", corlimit = 0.6)
