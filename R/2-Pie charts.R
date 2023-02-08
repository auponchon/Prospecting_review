library(ggplot2)
library(plyr)
library(tidyverse)
library(readxl)
library(here)
library(viridis)
library(ggrepel)
library(gridExtra)


#import data table from xlsx file
dat<-read_xlsx(here::here("Data","Prospecting bibliography.xlsx"),sheet=1,col_names = T)
names(dat)<- str_replace_all(names(dat), " ", "_") 

#rename taxa
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


#select dataset with prospecting and count taxa
prosp.taxa<-dat %>% 
       dplyr::filter(Class== "prospecting") %>% 
       group_by(Order) %>% 
       count() %>% 
       arrange(desc(n))

prosp.taxa$Order<-factor(prosp.taxa$Order, levels=rev(c("Passerines","Other birds", "Mammals","Seabirds",
                                                  "Insects","Fish","Annelids")))

Labs<-paste(prosp.taxa$Order, 
      paste(round(((prosp.taxa$n/sum(prosp.taxa$n))*100),0),"%"), sep=" - ")
#Labs<-Labs[c(1,3,4,2,5:7)]

Breaks<-cumsum(prosp.taxa$n) - (prosp.taxa$n/ 2)

#set colors for taxa with birds within same color
colo<-viridis(nrow(prosp.taxa),begin=0.1)

#create a pie with prosportions of taxa
taxa<-ggplot(prosp.taxa, aes(x = 1, y = n, fill = Order)) + 
    geom_bar(stat="identity",color="black") +
    coord_polar(theta='y',start=0) +
     geom_label_repel  (aes(x=1.4, y = Breaks, label = Labs, fill=Order),
                      size = 3, nudge_x = .3, 
                      segment.size = .7, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Taxa"))+
    scale_fill_manual(values=colo) +
    labs(tag="a)") +
    theme_void() 
   

print(taxa)



##########################################################################################
# plotting pie chart with tracking methods
dat<-read_xlsx(here::here("Data","Prospecting bibliography.xlsx"),col_names = T,sheet=1) %>% 
               dplyr::filter(Class== "prospecting") 
dat$Method<-revalue(as.factor(dat$Method), c("ringing" = "Ringing/\nDirect obs",
                                           "direct observations" = "Ringing/\nDirect obs",
                                           "GPS-UHF" = "GPS/PTT",
                                           "GPS" = "GPS/PTT",
                                           "PTT" = "GPS/PTT",
                                           "GPS-GSM" = "GPS/PTT",
                                           "GPS-PTT+VHF" = "GPS/PTT",
                                           "GPS-PTT" = "GPS/PTT",
                                           "GPS+PTT" = "GPS/PTT",
                                           "Automated VHF" = "VHF",
                                           "video-recording" = "Video-recording")) 

#Counting the number of studies by tracking method
prosp.track<-dat %>% 
    
    group_by(Method) %>% 
    count() %>% 
    arrange(desc(n))

prosp.track$Method<-factor(prosp.track$Method, levels= c("Ringing/\nDirect obs",
                                                         "VHF","GPS/PTT", "RFID","Video-recording"))


Labs.track<-rev(paste(prosp.track$Method, 
            paste(round(((prosp.track$n/sum(prosp.track$n))*100),0),"%"), sep="\n"))

Breaks.track<-cumsum(rev(prosp.track$n)) - (rev(prosp.track$n)/ 2)

#set colors for taxa with birds within same color
colo.trk<-magma(nrow(prosp.track),begin=0.25)

#create a pie with prosportions of taxa
meth<-ggplot(prosp.track, aes(x = 1, y = n, fill = Method)) + 
    geom_bar(stat="identity",color="black") +
    coord_polar(theta='y',start=0) +
    geom_label_repel(aes(x=1.4, y = rev(Breaks.track), label = rev(Labs.track)),
                       size = 3, nudge_x = .3,
                       segment.size = .7, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Method")) +
    scale_fill_manual(values=colo.trk) +
    labs(tag="b)") +
    theme_void() 

print(meth)

tiff(here::here("outputs","Piecharts_studies.tiff"),height=2000, width=6000,res=500,compression="lzw")
grid.arrange(taxa,meth,ncol=2)
dev.off()

##########################################################################################
# plotting pie chart with tracking methods
dat<-read_xlsx(here::here("Data","Prospecting bibliography.xlsx"),sheet=1,col_names = T)

#Counting the number of studies by year
prosp.year<-dat %>% 
    dplyr::filter(Class== "prospecting") %>% 
    complete(Year=seq(min(Year),max(Year),1)) %>% 
  mutate(Include=ifelse(is.na(Nb),0,1)) %>% 
    group_by(Year) %>% 
   summarize(n=sum(Include))
 

evol<-ggplot(prosp.year,aes(x=Year, y=n)) +
    #geom_bar(stat="identity",color="black",fill="white") +
    geom_line() +
  geom_point(shape=18,size=3) +
    scale_x_continuous(breaks=seq(1990,2022,5),limits=c(1989,2023),expand=c(0,0)) +
    scale_y_continuous(breaks=seq(0,12,2),limits=c(0,12),expand=c(0,0)) +
    labs(y="Number of studies",tag="c) ") +
    theme_classic()
print(evol)


##########################################################################################
# plotting pie chart with countries
# dat<-read_xlsx(here::here("Data","Prospecting bibliography.xlsx"),sheet=1,col_names = T)
# 
# #Counting the number of studies by year
# prosp.pays<-dat %>% 
#     dplyr::filter(Class== "prospecting") %>% 
#     group_by(Region) %>% 
#     count() %>% 
#     arrange(desc(n))
# 
# pays<-ggplot(prosp.pays, aes(x = 1, y = n, fill = Region)) + 
#     geom_bar(stat="identity",color="black") +
#     coord_polar(theta='y',start=0) +
#     # geom_label_repel(aes(x=1.4, y = rev(Breaks.track), label = rev(Labs.track)),
#     #                  size = 3, nudge_x = .3,
#     #                  segment.size = .7, show.legend = FALSE) +
#     guides(fill = guide_legend(title = "Country")) +
#    # scale_fill_manual(values=colo.trk) +
#     labs(tag="b)") +
#     theme_void() 
# 
# print(pays)
# 
