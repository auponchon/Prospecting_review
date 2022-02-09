#files<-list.files("Data/Biblio prospecting", pattern=".pdf", full.names = T)

# #relevant_l <- lapply(files, function(file) {
# names<-file %>% 
#        str_remove("prospecting/") %>% 
#        word(2)
# 
#  years<-  file %>% 
#           parse_number()
#  
#  text<-file %>% 
#      pdftools::pdf_text() %>% 
#      str_replace_all("[\\s]+", " ") 
#  
#  if(unique(str_detect(text, "DOI"))==FALSE){
#      DOI.in <- "false"
#  }
#  
#  else{ DOI.in<-"true"
#      place<-which(str_detect(text, "DOI")==T)
#      DOI<-str_extract_all(text[place], boundary("word"))
#          
#          str_extract(text[place],"DOI\w")
#        
#  
#  
#  
#  }
#  
#  
#  
#  relevant_l <- lapply(files, function(file) {
#      
#      # print status message
#      message("processing: ", basename(file))
#      
#      lines <- unlist(stringr::str_split(pdftools::pdf_text(file), "\n"))
#      start <- stringr::str_which(lines, "ITEM 1A.\\s+RISK FACTORS")
#      end <- stringr::str_which(lines, "ITEM 1B.\\s+UNRESOLVED STAFF COMMENTS")
#      
#      # cover a few different outcomes depending on what was found
#      if (length(start) == 1 & length(end) == 1) {
#          relevant <- lines[start:end]
#      } else if (length(start) == 0 | length(end) == 0) {
#          relevant <- "Pattern not found"
#      } else {
#          relevant <- "Problems found"
#      }
#      
#      return(relevant)
#  })
#  
#  
#  
#  docs<-Corpus(VectorSource(text))
#  
#  xx<-text %>% 
#      str_extract("DOI")
#  
