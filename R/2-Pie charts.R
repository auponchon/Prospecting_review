library(ggplot2)
library(tidyverse)
library(readxl)
library(here)
library(viridis)
library(plyr)


#import data table from xlsx file
dat<-read_xlsx(here("Data","Prospecting bibliography.xlsx"),n_max=154,col_names = T)
names(dat)<- str_replace_all(names(dat), " ", "_") 

#select dataset with prospecting
dat$Order<-revalue(as.factor(dat$Order), c("annelid" = "Annelids",
                                              "fish" = "Fish",
                                              "insect" = "Insects",
                                              "mammal" = "Mammals",
                                              "other bird" = "Other birds",
                                              "passerine" = "Passerines",
                                              "seabird" = "Seabirds",
                                              "raptor" = "Other birds",
                                              "shorebird" = "Other birds",
                                              "waterfowl" = "Other birds"))
dat$Order<-droplevels(dat$Order)

prosp.taxa<-dat %>% 
       dplyr::filter(Class== "prospecting") %>% 
       group_by(Order) %>% 
       count() %>% 
       arrange(n)


rosp.taxa$Label <- paste(prosp.taxa$Order, 
                          paste(round(((prosp.taxa$n/sum(prosp.taxa$n))*100),1),"%"), sep=" - ")

#set colors for taxa with birds within same color
colo1<-turbo(6,begin=0.8, end=1)
colo2<-turbo(4, begin=0.1,end=0.8)




taxa<-ggplot(prosp.taxa, aes(x = 1, y = n, fill = Order)) + 
    geom_bar(stat = "identity") +
    coord_polar(theta = 'y') + theme_void() +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5))

print(taxa)
