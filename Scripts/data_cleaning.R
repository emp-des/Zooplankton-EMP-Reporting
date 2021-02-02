#EMP Zooplankton data download and cleaning script
#Created by Arthur Barros 
#EMP Zooplankton Enviornmental Scientist
#arthur.barros@wildlife.ca.gov
#last updated 2/1/2021
rm( list = ls()) #clear env

#Load packages:
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(lubridate)
library(measurements)

############################
##Download the data from EDI
############################
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.522.4

#CB_matrix
CB_url<-"https://portal.edirepository.org/nis/dataviewer?packageid=edi.522.4&entityid=c0916b64396edab85b07038e32ff0342"
#desination file
CB_dest<-"Data/CB_matrix.csv"
download.file(CB_url,CB_dest)
CB_matrix<-read.csv("Data/CB_matrix.csv",fileEncoding="UTF-8-BOM") #file incoding to stop import from adding junk characters to first column name

#Pump_matrix
Pump_url<-"https://portal.edirepository.org/nis/dataviewer?packageid=edi.522.4&entityid=0f7ffacf41372643865af053c0b07663"
#desination file
Pump_dest<-"Data/Pump_matrix.csv"
download.file(Pump_url,Pump_dest)
Pump_matrix<-read.csv("Data/Pump_matrix.csv",fileEncoding="UTF-8-BOM") #file incoding to stop import from adding junk characters to first column name

#Mysid_matrix
Mysid_url<-"https://portal.edirepository.org/nis/dataviewer?packageid=edi.522.4&entityid=0080191932b0987243936eff1bb54ee8"
#desination file
Mysid_dest<-"Data/Mysid_matrix.csv"
download.file(Mysid_url,Mysid_dest)
Mysid_matrix<-read.csv("Data/Mysid_matrix.csv",fileEncoding="UTF-8-BOM") #file incoding to stop import from adding junk characters to first column name

#Zoocode lookup table
Zoocode_url<-"https://portal.edirepository.org/nis/dataviewer?packageid=edi.522.4&entityid=a3be37d31cf146c51bc583632dbfcf06"
#desination file
Zoocode_dest<-"Data/Zoocode_lookup.csv"
download.file(Zoocode_url,Zoocode_dest)
Zoocode_lookup<-read.csv("Data/Zoocode_lookup.csv",fileEncoding="UTF-8-BOM") #file incoding to stop import from adding junk characters to first column name

#Station lookup table
Station_url<-"https://portal.edirepository.org/nis/dataviewer?packageid=edi.522.4&entityid=71dd301f30a2bc2e40f5da573dde9f97"
#desination file
Station_dest<-"Data/Station_lookup.csv"
download.file(Station_url,Station_dest)
Station_lookup<-read.csv("Data/Station_lookup.csv")



################################
##Cleaning/Prep Data for Figures
################################

#Format dates
CB_matrix$SampleDate<-as.Date(CB_matrix$SampleDate,"%m/%d/%Y")
Mysid_matrix$SampleDate<-as.Date(Mysid_matrix$SampleDate,"%m/%d/%Y")
Pump_matrix$SampleDate<-as.Date(Pump_matrix$SampleDate,"%m/%d/%Y")

#remove "adult" from Zooplankton common names
Zoocode_lookup$CommonName<-str_remove(Zoocode_lookup$CommonName," adult")

#Join station info to catch matrices
Station_lookup<-select(Station_lookup,-Core,-lat_degrees,-lat_minutes,-lat_seconds,-lon_degrees,-lon_minutes,-lon_seconds,-year_start,-year_end) #remove superfluous columns
CB_matrix<-CB_matrix%>%
  inner_join(Station_lookup)
Mysid_matrix<-Mysid_matrix%>%
  inner_join(Station_lookup)
Pump_matrix<-Pump_matrix%>%
  inner_join(Station_lookup)

#Use pivot_longer to gather catch matrices CPUE data from wide to long format
CB_matrix_long<-pivot_longer(CB_matrix,cols=ACARTELA:CRABZOEA,names_to = "ZooCode",values_to = "CPUE")
Mysid_matrix_long<-pivot_longer(Mysid_matrix,cols=A_aspera:Unidentified_mysid, names_to="ZooCode", values_to="CPUE")
Pump_matrix_long<-pivot_longer(Pump_matrix,cols=LIMNOSINE:BARNNAUP, names_to="ZooCode", values_to="CPUE")

#join in taxonomic information
CB_matrix_long<-CB_matrix_long%>%
  inner_join(Zoocode_lookup)
Mysid_matrix_long<-Mysid_matrix_long%>%
  inner_join(Zoocode_lookup)
Pump_matrix_long<-Pump_matrix_long%>%
  inner_join(Zoocode_lookup)

#drop data before 1974 because we stopped doing winter sampling from 1974-1994
CB_matrix_long<-CB_matrix_long%>%
  filter(Year>=1974)
Mysid_matrix_long<-Mysid_matrix_long%>%
  filter(Year>=1974)
Pump_matrix_long<-Pump_matrix_long%>%
  filter(Year>=1974)

#select necessary columns
CB_clean<-select(CB_matrix_long,SurveyCode,Core,Current,Year,Survey,SampleDate,StationNZ,
                Secchi,Chl_a,Temperature,ECSurfacePreTow,ECBottomPreTow,
                lat,lon,km,ZooCode,CPUE,NativeOrIntro,Phylum,Class,Order,Family,CommonName)
Mysid_clean<-select(Mysid_matrix_long,SurveyCode,Core,Current,Year,Survey,SampleDate,StationNZ,
                   Secchi,Chl_a,Temperature,ECSurfacePreTow,ECBottomPreTow,
                   lat,lon,km,ZooCode,CPUE,NativeOrIntro,Phylum,Class,Order,Family,CommonName)
Pump_clean<-select(Pump_matrix_long,SurveyCode,Core,Current,Year,Survey,SampleDate,StationNZ,
                  Secchi,Chl_a,Temperature,ECSurfacePreTow,ECBottomPreTow,
                  lat,lon,km,ZooCode,CPUE,NativeOrIntro,Phylum,Class,Order,Family,CommonName)

#Save RDS of each clean catch matrix
saveRDS(CB_clean,"Data/CB_clean.rds")
saveRDS(Mysid_clean,"Data/Mysid_clean.rds")
saveRDS(Pump_clean,"Data/Pump_clean.rds")
