library(ggplot2)
library(plyr)
library(tidyverse)
library(readxl)
library(here)
library(viridis)
library(ggrepel)
library(gridExtra)


#import data table from xlsx file
dat<-read_xlsx(here::here("data","References_used_for_review.xlsx"),sheet=1,col_names = T)
names(dat)<- str_replace_all(names(dat), " ", "_") 

#rename taxa
dat$Taxa<-revalue(as.factor(dat$Taxa), c("annelid" = "Annelids",
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
       group_by(Taxa) %>% 
       count() %>% 
       arrange(desc(n))

prosp.taxa$Taxa<-factor(prosp.taxa$Taxa, levels=rev(c("Passerines","Mammals","Other birds", "Seabirds",
                                                  "Insects","Fish","Annelids")))

Labs<-paste(prosp.taxa$Taxa, 
      paste(round(((prosp.taxa$n/sum(prosp.taxa$n))*100),0),"%"), sep=" - ")
#Labs<-Labs[c(1,3,4,2,5:7)]

Breaks<-cumsum(prosp.taxa$n) - (prosp.taxa$n/ 2)

#set colors for taxa with birds within same color
#colo<-viridis(nrow(prosp.taxa),begin=0.1, alpha=0.7)
colo<-c("#ef4043","#f26b21","#ff9900","#f9c74f","#90be6d","#43aa8b","#577590")

#create a pie with prosportions of taxa
taxa<-ggplot(prosp.taxa, aes(x = 1, y = n, fill = Taxa)) + 
    geom_bar(stat="identity",color="black") +
    coord_polar(theta='y',start=0) +
     geom_label_repel  (aes(x=1.4, y = Breaks, label = Labs, fill=Taxa),
                      size = 3, nudge_x = .3, 
                      segment.size = .7, show.legend = FALSE) +
    guides(fill = "none")+
    scale_fill_manual(values=colo) +
     labs(tag="a)") +
    theme_void() +
   theme( plot.margin = unit(c(0, 0, 0, 0), "null"),
         panel.spacing = unit(c(0, 0, 0, 0), "null"))
    
   

print(taxa)



##########################################################################################
# plotting pie chart with tracking methods
dat<-read_xlsx(here::here("data","References_used_for_review.xlsx"),col_names = T,sheet=1) 
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
colo.trk<-magma(nrow(prosp.track),begin=0.25, alpha=0.7)

#create a pie with prosportions of taxa
meth<-ggplot(prosp.track, aes(x = 1, y = n, fill = Method)) + 
    geom_bar(stat="identity",color="black") +
    coord_polar(theta='y',start=0) +
    geom_label_repel(aes(x=1.4, y = rev(Breaks.track), label = rev(Labs.track)),
                       size = 3, nudge_x = .3,
                       segment.size = .7, show.legend = FALSE) +
    guides(fill = "none") +
    scale_fill_manual(values=colo.trk) +
    labs(tag="b)") +
    theme_void() +
  theme( plot.margin = unit(c(0, 0, 0, 0), "null"),
         panel.spacing = unit(c(0, 0, 0, 0), "null"))

print(meth)

prosp.year1<-dat %>% 
  dplyr::filter(Year >=1999) %>% 
  complete(Year=seq(min(Year),max(Year),1)) %>% 
  mutate(Include=ifelse(is.na(Nb),0,1),
         Method=as.factor(Method))  %>% 
  group_by(Year, Method) %>% 
  summarize(N=sum(Include))

prosp.year1$Method<-factor(prosp.year1$Method, levels= c("Ringing/\nDirect obs",
                                                         "VHF","GPS/PTT", "RFID","Video-recording"))

  
# year<-ggplot(prosp.year1, aes(x=Year, y=n))+
#  # geom_bar(stat='identity', aes(fill=Method), position = position_dodge())
#   #geom_line(aes(color=Method))
# print(year)

##########################################################################################
# plotting pie chart with tracking methods
dat<-read_xlsx(here::here("data","References_used_for_review.xlsx"),sheet=1,col_names = T)

#Counting the number of studies by year
prosp.year<-dat %>% 
    dplyr::filter(Year >=1999) %>% 
    complete(Year=seq(min(Year),max(Year),1)) %>% 
  mutate(Include=ifelse(is.na(Nb),0,1)) %>% 
    group_by(Year) %>% 
   summarize(n=sum(Include))
colo.trk1<-magma(nrow(prosp.track),begin=0.25)


evol<-ggplot(prosp.year,aes(x=Year, y=n)) +
    #geom_bar(stat="identity",color="black",fill="white") +
    geom_line() +
  # geom_line(data=prosp.year1, aes(x=Year, y=N, color=Method)) + 
  # geom_point(data=prosp.year1, aes(x=Year, y=N, color=Method),
  #            shape=17,size=1.2) +
  geom_point(shape=18,size=3) +
    scale_x_continuous(breaks=seq(2000,2022,5),limits=c(1998,2023),expand=c(0,0)) +
    scale_y_continuous(breaks=seq(0,12,2),limits=c(0,12),expand=c(0.1,0.1)) +
    labs(y="Number of studies",tag="c) ") +
 # scale_colour_manual(values=colo.trk1) +
    theme_classic()+
  theme( plot.margin = unit(c(0, 0, 0, 0), "null"),
         panel.spacing = unit(c(0, 0, 0, 0), "null"))
         #legend.position = "bottom")
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

#############################################################################################
#ploting bars according to factors addressed
#

themes<-read_xlsx(here::here("data","References_used_for_review.xlsx"),sheet=2,col_names = T,
                  n_max=124)
names(themes)<- str_replace_all(names(themes), " ", "_") 

#rename taxa
themes$Taxa<-revalue(as.factor(themes$Taxa), c("annelid" = "Annelids",
                                           "fish" = "Fish",
                                           "insect" = "Insects",
                                           "mammal" = "Mammals",
                                           "other bird" = "Other birds",
                                           "passerine" = "Passerines",
                                           "seabird" = "Seabirds",
                                           "raptor" = "Other birds",
                                           "shorebird" = "Other birds",
                                           "waterfowl" = "Other birds"))


themes$Sociality<-revalue(as.factor(themes$Sociality), c("territorial" = "Territorial",
                                               "colonial" = "Colonial",
                                               "cooperative" = "Cooperative/social",
                                               "non-territorial" = "Non-territorial",
                                               "parasitic" = "Parasitic",
                                               "semi-colonial" = "Semi-colonial",
                                               "social" = "Cooperative/social"))

#Give proportion of socialities covered in species
xx<-themes %>% 
  group_by(Sociality) %>% 
  count() %>% 
  mutate(n=n/124*100)
xx

#remove columns with less than 5%
themes<-themes[,c(1:18)]


themes_long<-themes %>% 
    pivot_longer(cols=Sex:Other,
                 names_to="Theme",
                 values_to = "Value") %>% 
   dplyr::mutate(Theme=as.factor(Theme)) %>% 
  dplyr::filter(Value==1)

themes_long$Theme<-factor(themes_long$Theme, levels=c("Sex","Breeding_status/Age","Social_cues","Timing",
                                                      "Habitat_quality/availability", "Exploration","Tactics","Familiarity",
                                                      "Opportunistic_record","Cost","Other"))

themes_long$Taxa<-factor(themes_long$Taxa, levels=rev(c("Passerines","Mammals","Other birds", "Seabirds",
                                                      "Insects","Fish","Annelids")))

themes_long$Sociality<-factor(themes_long$Sociality, levels=rev(c("Territorial","Colonial","Cooperative/social",
                                                              "Parasitic","Semi-colonial","Non-territorial")))


themes_gg<-ggplot(themes_long, aes(fill=Taxa, y=Value, x=Theme)) + 
           geom_bar(position="stack", stat="identity") + 
  scale_x_discrete(labels=c("Sex","Age and/or\nbreeding status","Social cues","Timing",
                                 "Habitat quality\navailability", "General\nexploration","Tactics",
                            "Habitat\nfamiliarity",                 
                                 "Opportunistic\nobservations","Cost","Other"))+
  scale_y_continuous(limits=c(0,52), breaks=seq(0,50,10), expand=c(0.01,0.01)) +
  labs(x="",y="Number of studies",tag="d)") + 
  scale_fill_manual(values=colo) +
  # scale_y_continuous(limits=c(0,60),
  #                    expand=c(0.001,0.001),
  #                    breaks=seq(0,60,15))+
  theme_classic()+
  theme(legend.position=c(.9,.75))
print(themes_gg)

tiff(here::here("outputs","Figure_1_Piecharts_studies.tiff"),height=6200, width=5500,
     res=600,compression="lzw")
grid.arrange(arrangeGrob(taxa,meth,ncol=2),
             evol,themes_gg,nrow=3,heights=c(1,0.7,1))
dev.off()
