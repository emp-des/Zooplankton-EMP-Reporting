#Calculating yearly and seasonal catch indices for zooplankton matrices
#Created by Arthur Barros 
#EMP Zooplankton Enviornmental Scientist
#arthur.barros@wildlife.ca.gov
#last updated 2/1/2021
rm( list = ls()) #clear env

#Read in clean catch matrices
CB_clean<-readRDS("Data/CB_clean.rds")
Mysid_clean<-readRDS("Data/Mysid_clean.rds")
Pump_clean<-readRDS("Data/Pump_clean.rds")

######################################
#Set target parameters for figure data
######################################
#target adults
target_adults<-c("ACARTELA","ACARTIA","EURYTEM","PDIAPFOR","SINOCAL","OITHDAV","LIMNOSPP","LIMNOSINE","LIMNOTET","OTHCYC","KERATELA","OTHROT","POLYARTH","SYNCH","SYNCHBIC","TRICHO","BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO","H_longirostris","N_kadiakensis","N_mercedis")

#change Pump order from Ploima to Rotifer for ease
Pump_clean$Order<-ifelse(Pump_clean$Order=="Ploima","Rotifer",Pump_clean$Order)

#target specific taxa orders for specific gear types based on gear efficiency
CB_orders<-c("Calanoida","Cladocera")
Pump_orders<-c("Cyclopoida","Rotifer")
Mysid_orders<-c("Mysida")

#filter by target parameters
CB_targets<-CB_clean%>%
  filter(Order%in%CB_orders,ZooCode%in%target_adults,Current=="yes")
Pump_targets<-Pump_clean%>%
  filter(Order%in%Pump_orders,ZooCode%in%target_adults,Current=="yes")
Mysid_targets<-Mysid_clean%>%
  filter(Order%in%Mysid_orders,ZooCode%in%target_adults,Current=="yes")

#Join all catch matrices into one
Zoop_all<-CB_targets%>%
  rbind(Pump_targets)%>%
  rbind(Mysid_targets)

###################################
#Calculate Annual Abundance indices
###################################

#annual abundance is calculated as mean of March - Nov, because winter was not consistently sampled until 1995
#drop winter samples
winter<-c(12,1,2)
Zoop_annual<-Zoop_all%>%
  filter(!Survey%in%winter)

#calculate annual indices, which is just the mean of each years CPUE for each taxa
Zoop_annual_indices<-Zoop_annual%>%
  group_by(Year,CommonName,Order)%>%
  summarise(Indices=mean(CPUE))

#save Zoop_annual_indices for figure creation
saveRDS(Zoop_annual_indices,"Data/Zoop_annual_indices.rds")

#####################################
#Calculate Seasonal Abundance Indices
#####################################
#Function to calculate water years so that December of the year (i-1) is included in the winter season of year (i)
wtr_yr <- function(dates, start_month=12) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon == start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

#calculate water years for each matrix
Zoop_seasonal<-Zoop_all
Zoop_seasonal$water_year<-wtr_yr(Zoop_seasonal$SampleDate)

#make season names lookup table
winter<-c(12,1,2)
spring<-c(3,4,5)
summer<-c(6,7,8)
fall<-c(9,10,11)
seasons<-rep(c("Winter","Spring","Summer","Fall"), each = 3)
Survey<-c(winter,spring,summer,fall)
season_lookup<-data.frame(seasons,Survey)

#join season names to Zoop table
Zoop_seasonal<-Zoop_seasonal%>%inner_join(season_lookup)


#for winter surveys filter for year 1995 on, as we didn't consistently sample winter before than
Zoop_winter<-Zoop_seasonal%>%
  filter(seasons=="Winter",Year>=1995,water_year<2020) #also filter out last water year so that we don't have one month of data at end of figures
Zoop_notwinter<-Zoop_seasonal%>%
  filter(seasons!="Winter")
#joining the two above matrices provides a full matrix with all years, with only the winter surveys after 1995, when sampling was consistent
Zoop_seasonal<-Zoop_winter%>%
  rbind(Zoop_notwinter)

#Set season factor levels
Zoop_seasonal$seasons<-factor(Zoop_seasonal$seasons,levels = c("Winter","Spring","Summer","Fall"))

#set target year for filtering water years, so you don't show december[latest_year] as [latest+1] because of water year calcs
latest_year=2019
#calculate seasonal indices, which is just the mean of each years seasonal CPUE for each taxa
Zoop_seasonal_indices<-Zoop_seasonal%>%
  filter(water_year<=latest_year)%>%
  group_by(water_year,Order,seasons)%>%
  summarise(Indices=mean(CPUE))

#save Zoop_seasonal_indices for figure creation
saveRDS(Zoop_seasonal_indices,"Data/Zoop_seasonal_indices.rds")

##################################
#Calculate Annual Regional Indices
##################################
#build region lookup based on STR2018 (April's methods)
StationNZ<-c("NZD41","NZ41A","NZD06","NZ028","NZ048","NZ054","NZ032","NZS42","NZ060","NZ064","NZ074","NZD16","NZD19","NZ086","NZD28","NZ092","NZM10")
region_of_estuary<-c(rep("San Pablo Bay",2),rep("Suisun Bay",4),rep("Suisun Marsh",2),rep("West Delta",3),rep("Central Delta",4),rep("East Delta",2))
region_lookup<-data.frame(StationNZ,region_of_estuary)

#set factor order for region_lookup
region_lookup$region_of_estuary<-factor(region_lookup$region_of_estuary,levels=c("San Pablo Bay","Suisun Bay","Suisun Marsh","West Delta","Central Delta","East Delta"))

#remove winter months because winter wasn't sampled until 1995
Zoop_regional<-Zoop_all%>%
  filter(!Survey%in%winter)%>%
  inner_join(region_lookup)

#calculate indices
Zoop_regional_indices<-Zoop_regional%>%
  group_by(Year,Order,region_of_estuary)%>%
  summarise(Indices=mean(CPUE))

#save Zoop_regional_indices for figure creation
saveRDS(Zoop_regional_indices,"Data/Zoop_regional_indices.rds")

####################################################
#Calculate Report Year Seasonal and Regional Indices
####################################################
Year_seasonal_regional<-Zoop_seasonal%>%
  inner_join(region_lookup)
#set target year
target_year=2019
#set target regions
target_regions<-c("San Pablo Bay","Suisun Bay","Suisun Marsh","West Delta","Central Delta","East Delta")

#calculate indices
Year_seasonal_regional_indices<-Year_seasonal_regional%>%
  filter(water_year==target_year)%>%
  group_by(water_year,CommonName,Order,seasons,region_of_estuary)%>%
  summarise(Indices=mean(CPUE))

#save Year_seasonal_regional_indices for figure creation
saveRDS(Year_seasonal_regional_indices,"Data/Year_spatial_indices.rds")
