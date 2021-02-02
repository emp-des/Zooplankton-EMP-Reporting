#Figure production
#Created by Arthur Barros 
#EMP Zooplankton Enviornmental Scientist
#arthur.barros@wildlife.ca.gov
#last updated 2/1/2021
rm( list = ls()) #clear env
#read in packages
library(ggplot2)
library(plotrix)
library(cowplot)
library(tidyverse)

#Set Color Palette for common names--------------------------------------------------------------
cop_pal<-c(
  "Acartia spp."="#8DD3C7",
  "Acartiella sinensis"="#FFFFB3",
  "Eurytemora spp."="#BEBADA",
  "Limnoithona sinensis"="#BEBED9",
  "Limnoithona spp."="#FDB462",
  "Limnoithona tetraspina"="#FDB462",
  "Oithona davisae"="#B3DE69",
  "Pseudodiaptomus forbesi"="#FB8072",
  "Sinocalanus doerrii"="#80B1D3",
  "Bosmina longirostris"="#8DD3C7",
  "Daphnia spp."="#FFFFB3",
  "Diaphanosoma spp."="#BEBADA",
  "Ceriodaphnia spp."="#D9D9D9",
  "Keratella spp."="#8DD3C7",
  "other Rotifers"="#BEBADA",
  "Polyarthra spp."="#D9D9D9",
  "Synchaeta bicornis"="#BC80BD",
  "Synchaeta spp."="#FDB462",
  "Trichocerca spp."="#B3DE69",
  "Acanthomysis aspera"="#8DD3C7",
  "Acanthomysis hwanhaiensis"="#FFFFB3",
  "Alienacanthomysis macropsis"="#BEBADA",
  "Deltamysis holmquistae"="#D9D9D9",
  "Hyperacanthomysis longirostris"="#BC80BD",
  "Neomysis kadiakensis"="#FDB462",
  "Neomysis mercedis"="#B3DE69",
  "Unidentified mysid"="#FCCDE5",
  "Other"="#FCCDE5"
)

#Set Color Palette for zoop orders--------------------------------------------------------------
order_pal<-c(
  "Calanoida"="#FB8072",
  "Cyclopoida"="#FDB462",
  "Cladocera"="#8DD3C7",
  "Rotifer"="#BC80BD",
  "Mysida"="#B3DE69"
)

#Read in calculated indices
Zoop_annual_indices<-readRDS("Data/Zoop_annual_indices.rds")
Zoop_seasonal_indices<-readRDS("Data/Zoop_seasonal_indices.rds")
Zoop_regional_indices<-readRDS("Data/Zoop_regional_indices.rds")
Year_spatial_indices<-readRDS("Data/Year_spatial_indices.rds")

#Sampling heat map -------------------------------------------------------------

#Read in clean catch matrices
CB_clean<-readRDS("Data/CB_clean.rds")
Mysid_clean<-readRDS("Data/Mysid_clean.rds")
Pump_clean<-readRDS("Data/Pump_clean.rds")

#create tables with each sampling event and gear type
CB_tows<-unique(select(CB_clean,Year,Survey,StationNZ))
CB_tows$gear<-"CB" #ihclude column with gear type
Pump_tows<-unique(select(Pump_clean,Year,Survey,StationNZ))
Pump_tows$gear<-"Pump"
Mysid_tows<-unique(select(Mysid_clean,Year,Survey,StationNZ))
Mysid_tows$gear<-"Mysid"

All_tows<-CB_tows%>%
  rbind(Pump_tows)%>%
  rbind(Mysid_tows)

#calculate number of tows at each station by year, month, and gear type
All_tows<-All_tows%>%
  group_by(Year,Survey,gear)%>%
  summarize(tows=length(StationNZ))

#make lookup list of gear types
gear_types<-c("CB","Pump","Mysid")

#make heat-map plot forloop to create three different plots (one for each gear type), showing the number of tows for each month/survey of each year
plot_list<-list()
for(i in 1:3){
  target_gear<-gear_types[i]
  d=All_tows%>%
    filter(gear==target_gear)
  
  p<-ggplot(d,aes(Year,Survey))+
    geom_tile(aes(fill=tows))+
    theme_classic()+
    theme(axis.text.x=element_text(angle=90,size=10),axis.text.y = element_text(size=10),legend.position = "none")+
    scale_fill_gradient(low="white",high="red")+
    geom_text(aes(label=round(tows,1)),size=4)+
    guides(colour=F)+
    ggtitle(paste(target_gear,"Sampling Coverage",sep=" "))+
    scale_y_continuous(expand = c(0, 0),breaks = round(seq(min(1),
                                                           max(12),
                                                           by = 1),1))+
    scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(All_tows$Year),
                                                           max(All_tows$Year),
                                                           by = 2),1))
  plot_list[[i]]<-p
}
gear_plots<-plot_grid(plotlist=plot_list,labels = "AUTO", align = "v",ncol=1)
gear_plots
saveRDS(gear_plots,"Data/sampling_coverage.rds")
save_plot("Figures/sampling_coverage.png", gear_plots,ncol=1,base_height = 14,base_width = 10)

# Zooplankton Figures --------------------------------------------------------

#####################################################################3
#Annual zooplankton indices##########################################3
#####################################################################3

#only want to display these common orders, many species are not shown
target_order=c("Calanoida","Cyclopoida","Cladocera","Rotifer","Mysida")

#change L tetraspina to L spp. to group them on the plots (otherwise it looks like there is a huge change in community in ~1995, but really it was just a change in taxonomic ID)
Lspp<-c("Limnoithona tetraspina","Limnoithona sinensis")
Zoop_annual_indices$CommonName<-ifelse(Zoop_annual_indices$CommonName%in%Lspp,"Limnoithona spp.",Zoop_annual_indices$CommonName)

plot_list<-list()
#make annual abundance plots for each order in forloops
for(i in 1:length(target_order)){
  order=target_order[i]
  d=Zoop_annual_indices%>%
    filter(Order==order)
  #calculate maximum sum of species to set y-axis
  s_max<-d%>%
    group_by(CommonName)%>%
    summarise(max=sum(max(Indices)))
  s_max<-sum(s_max$max)
  p<-ggplot(d,aes(Year,Indices,fill=CommonName))+
    geom_bar(stat="identity")+
    theme_classic()+
    scale_fill_manual(values=cop_pal)+
    #geom_errorbar(aes(ymin=maxCPUE,ymax = maxCPUE+se), width=0.3)+
    ggtitle(paste("Annual Mean Density ",order,sep=""))+
    theme(axis.text.x=element_text(angle=90,vjust=0.5,size=10),axis.text.y = element_text(size=10),legend.title=element_blank(),legend.text = element_text(size=12,face="italic"))+
    scale_x_continuous(breaks = round(seq(min(d$Year), max(d$Year),
                                          by = 2),1))+
    scale_y_continuous(expand = c(0, 0),breaks = round(seq(min(0),
                                                           max(s_max),
                                                           by = if(order=="Calanoida"){
                                                             500
                                                           } else if (order=="Cyclopoida"){
                                                             5000
                                                           } else if (order=="Cladocera"){
                                                             1000
                                                           } else if (order=="Rotifer"){
                                                             20000
                                                           }  else if (order=="Mysida"){
                                                             10
                                                           })))+
    ylab(expression ("CPUE"~(count/m^3)))+
    expand_limits(y = 0)
  print(p)
  plot_list[[i]]=p
}
annual_plots<-plot_grid(plotlist=plot_list,labels = "AUTO", align = "v",ncol=1)
annual_plots
saveRDS(annual_plots,"Data/annual_indices.rds")
save_plot("Figures/annual_indices.png", annual_plots,ncol=1,base_height = 14,base_width = 10)

#####################################################################3
#Annual regional indices##########################################3
#####################################################################3
target_order=c("Calanoida","Cyclopoida","Cladocera","Rotifer","Mysida")

plot_list=list()
#make annual abundance line plots for each order and each region in forloops
for(i in 1:length(target_order)){
  o=target_order[i]
  d=Zoop_regional_indices%>%
    filter(Order==o)
  p<-ggplot(d,aes(Year,(Indices),color=Order))+
    geom_line(stat="identity",size=1.5)+
    theme_classic()+
    scale_color_manual(values=order_pal)+
    ggtitle(paste("Annual Mean Density",o,sep=" "))+
    theme(axis.text.x=element_text(angle=90,vjust=0.5,size=12,face="bold"),axis.text.y = element_text(size=12,face="bold"),legend.title=element_blank(),legend.text = element_text(size=12),legend.position = "none",strip.text.x = element_text(size = 14))+
    scale_x_continuous(breaks = round(seq(min(d$Year), max(d$Year),
                                          by = 5),1))+
    ylab(expression ("CPUE"~(count/m^3)))+
    xlab("")+
    facet_wrap(~region_of_estuary,nrow=1)+
    expand_limits(y = 0)
  plot_list[[i]]=p
}
regional_plots<-plot_grid(plotlist=plot_list, labels="AUTO",align = "v",ncol=1)
regional_plots
saveRDS(regional_plots,"Data/regional_indices.rds")
save_plot("Figures/regional_indices.png", regional_plots,ncol=1,base_height = 14,base_width = 14)

############################################################
#Seasonal Indices##########################################3
############################################################

Zoop_seasonal_indices$seasons<-factor(Zoop_seasonal_indices$seasons,levels = c("Winter","Spring","Summer","Fall"))
plot_list=list()
#make annual abundance line plots for each order in each season in forloops
for(i in 1:length(target_order)){
  order=target_order[i]
  d<-Zoop_seasonal_indices%>%
    filter(Order==order)
  p<-ggplot(d,aes(water_year,Indices))+
    geom_line(aes(color=seasons),size=1)+
    geom_point(aes(color=seasons))+
    theme_classic()+
    scale_fill_manual(values=cop_pal)+
    theme(axis.text.x=element_text(angle=90,vjust=0.5,size=10),legend.title=element_blank(),legend.text=element_text(size=12))+
    scale_x_continuous(breaks = round(seq(min(d$water_year), max(d$water_year),
                                          by = 2),1))+
    ylab(expression ("CPUE"~(count/m^3)))+
    xlab("Year")+
    ggtitle(paste(order,"Seasonal Abundances"))+
    expand_limits(y = 0)+
    scale_y_continuous(expand = c(0,0))
  print(p)
  plot_list[[i]]=p
}
seasonal_plots<-plot_grid(plotlist=plot_list, labels="AUTO",align = "v",ncol=1)
seasonal_plots
saveRDS(seasonal_plots,"Data/seasonal_indices.rds")
save_plot("Figures/seasonal_indices.png", seasonal_plots,ncol=1,base_height = 14,base_width = 10)


#########################################################################3
#Report Year seasonal and Regional indices for Zoops#############################3
#########################################################################3


#set target year
target_year=2019
#set target regions
target_regions<-c("San Pablo Bay","Suisun Bay","Suisun Marsh","West Delta","Central Delta","East Delta")

plot_list<-list()
#make a bar plot for breaking up seasonal distriubtion and region for the target year
for(i in 1:length(target_order)){
  o=target_order[i]
  d<-Year_spatial_indices%>%
    filter(water_year==target_year,Order==o)
  #calculate maximum sum of species to set y-axis limits
  sc_max<-d%>%
    group_by(CommonName,seasons,water_year,region_of_estuary)%>%
    summarise(max=sum(max(Indices)))
  s_max<-sc_max%>%
    group_by(water_year,seasons,region_of_estuary)%>%
    summarise(max=sum(max))
  max=max(s_max$max)
  l=c(0,max)
  p<-ggplot(d,aes(region_of_estuary,Indices,fill=CommonName))+
    geom_bar(stat="identity")+
    scale_fill_manual(values=cop_pal)+
    facet_wrap(~seasons,ncol=4)+
    theme_classic()+
    theme(axis.text.x=if(i==length(target_order)){element_text(angle=90,vjust=0.5,size=11)}else{element_blank()},legend.title=element_blank(),axis.title.x = if(i!=length(target_order)){element_blank()},legend.text = element_text(size=12,face="italic"))+
    #geom_errorbar(aes(ymin=max,ymax=max+se,width=.3))+
    ylab(expression ("CPUE"~(count/m^3)))+
    xlab("Region")+
    expand_limits(y = 0)+
    ggtitle(paste(target_year,o,sep=" "))+
    scale_y_continuous(expand = c(0, 0),limits=l)
  print(p)
  plot_list[[i]]=p
}

year_plots<-plot_grid(plotlist=plot_list,labels = "AUTO", align = "v",ncol=1,rel_heights = c(1,1,1,1,1.5))
year_plots
saveRDS(year_plots,paste("Data/regions_",target_year,".rds"))
save_plot(paste("Figures/regions_",target_year,".png",sep=""), year_plots,ncol=1,base_height = 14,base_width = 10)
