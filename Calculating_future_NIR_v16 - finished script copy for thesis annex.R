### Script for model used in the following thesis project: ###
#"Impacts of Climate Change on Water Stress and Crop Yields of Non-Cereal Crops 
#in Thuringia, Germany"
#Student: Edward Digman
#Hiroshima Student ID: IM213298



#NOTE: Initial Codebase was provided by colleagues in the DroughtMAS Project
# This code is provided for reference.
# Researcher's own code begins on line 406

#Load Libraries
library(readxl)
library(plyr)
library(dplyr)
library(plm)           # working with panel data
library(zoo)
library(reshape2)
library(tidyr)
library(data.table)
library(openxlsx)
library(rlang)
library(tidyr)
library(readr)
library(splitstackshape)
library(purrr)
library(tibble)
library(tibbletime)

#install FAO package:
#install.packages("FAO56")
library("FAO56")

#Calculating mean Net Irrigation Requirements (NIRs) for maintaining historical 
#crop yields
#setting working directory:
setwd("C:/Users/edigm/Documents/Joint MSc/Thesis Work/CMIP5_climate_data_monthly")

#####creating data frame of codes to match with JASMIN's files
library("readxl")
codes=read_excel(
  "C:/Users/edigm/Documents/Joint MSc/Thesis Work/CMIP5_climate_data_monthly/
  NUTS3_codes/County level Yield of 10 field Crops_2016.xlsx",
  sheet = "41241-01-03-4", range="A8:O545")
codes=subset(codes, codes$`District names in NEW Shape File`!=0)
names(codes)[1]="NUTS3_codes_RS"
names(codes)[13]="NUTS3_codes"
names(codes)[14]="NUTS3_names"
names(codes)[15]="subdistrict"
codes=codes[ c(1,13,14,15) ]
mismatch=subset(codes, codes$NUTS3_codes_RS != codes$NUTS3_codes) #to identify non-identical columns


#### Deriving NIR from FAO 33 Irrigation and Drainage paper "Yield response to water" #####################
############ Assuming ET(a) = ET(m) lead to: #############################################################
############### NIR = (k(m)*E(m))-p(m) ###################################################################
############ k(m) = Empirically-derived crop coefficient #################################################
############ E(m) = Reference crop evapotranspiration ####################################################
############ p(m) = Percipitation (mm) ###################################################################

###
library(httr)
library(tidyverse)
library(sf)
library(tmap)
library(gifski)
library(stplanr)
library(magrittr)

####Read NUTS3 shapefile to extract latitude (of the centroid)

##read in shape file 
####### 
NUTS3_shp=st_read(dsn="C:/Users/edigm/Documents/Joint MSc/Thesis Work/
                  CMIP5_climate_data_monthly/NUTS3_shapefile/VG250_KRS.shp")

NUTS3_shp = sf::st_transform(NUTS3_shp, "+proj=longlat +datum=WGS84")

###Calculate centroid of NUTS3 unit to determine latitude
NUTS3_shp$centroids <- st_centroid(NUTS3_shp$geometry)

NUTS3_shp %<>%
  mutate(long = unlist(map(NUTS3_shp$centroids,1)),
         lat = unlist(map(NUTS3_shp$centroids,2)))

NUTS3_shp=subset(NUTS3_shp, NUTS3_shp$GF==4)

names(NUTS3_shp)[4]="NUTS3_codes"
NUTS3_shp=merge(NUTS3_shp, codes, by = "NUTS3_codes")


###Load EDGE CLIMATE PROJECTIONS max and min T and P data on NUTS3-level
############## Create lists #########################################

## Climate Models
list_GCM <- c( "HadGEM2-ES","GFDL-ESM2M","IPSL-CM5A-LR", "MIROC-ESM-CHEM", "NorESM1-M")
## Representative Concentration Pathways
list_RCP <- c("rcp2p6", "rcp6p0", "rcp8p5")
## Meteo Variables
list_meteo = c("P", "Tmax", "Tmin")

#####################################################################
print("Start function")
function_GCM <- function(GCM){
  print("The model considered is:")
  print(list_GCM[[GCM]])
  
  print("Start function")
  function_RCP <- function(RCP){
    print("The RCP considered is:")
    print(list_RCP[[RCP]])
    
    print("Start function")
    function_meteo <- function(meteo){
      print("The variable considered is:")
      print(list_meteo[[meteo]])
      
      ###Load data

      climate_monthly_input <- read_csv(paste(
        "C:/Users/edigm/Documents/Joint MSc/Thesis Work/CMIP5_climate_data_monthly/
        climate_monthly_input_NorESM1-M_" 
        , list_RCP[RCP], "_", list_meteo[[meteo]], ".csv", sep = ""))
      
      
      return(climate_monthly_input)

    }
    climate_monthly_input <- lapply(seq_along(list_meteo), FUN = function_meteo)
    return(climate_monthly_input)
    
    climate_monthly_input_P=subset(climate_monthly_input, climate_monthly_input$variable=="P")
    climate_monthly_input_Tmax=subset(climate_monthly_input, climate_monthly_input$variable=="Tmax")
    climate_monthly_input_Tmin=subset(climate_monthly_input, climate_monthly_input$variable=="Tmin")
    
    climate_monthly_input=cbind(climate_monthly_input_P, 
                                    climate_monthly_input_Tmax,
                                    climate_monthly_input_Tmin)
    
    
    return(climate_monthly_input)

  }
  climate_monthly_input <- lapply(seq_along(list_RCP), FUN = function_RCP)
  return(climate_monthly_input)
}
climate_monthly_input<- lapply(seq_along(list_GCM), FUN = function_GCM)


#### Average among climate models
###RCP2.6
climate_monthly_input_rcp2p6_1 <- as.data.frame(climate_monthly_input[[1]][1])
climate_monthly_input_rcp2p6_2 <- as.data.frame(climate_monthly_input[[2]][1])
climate_monthly_input_rcp2p6_3 <- as.data.frame(climate_monthly_input[[3]][1])
climate_monthly_input_rcp2p6_4 <- as.data.frame(climate_monthly_input[[4]][1])
climate_monthly_input_rcp2p6_5 <- as.data.frame(climate_monthly_input[[5]][1])

climate_monthly_input_rcp2p6 = rbind(climate_monthly_input_rcp2p6_1, 
                                     climate_monthly_input_rcp2p6_2,
                                     climate_monthly_input_rcp2p6_3,
                                     climate_monthly_input_rcp2p6_4,
                                     climate_monthly_input_rcp2p6_5)

climate_monthly_input_rcp2p6=climate_monthly_input_rcp2p6[-c(6,7,8,11,12,13)]
names(climate_monthly_input_rcp2p6)[5]="P"
names(climate_monthly_input_rcp2p6)[7]="Tmax"
names(climate_monthly_input_rcp2p6)[9]="Tmin"
climate_monthly_input_rcp2p6=climate_monthly_input_rcp2p6[-c(4,6,8)]

### Form averages
climate_monthly_input_rcp2p6 = climate_monthly_input_rcp2p6 %>% 
  #pivot_longer(-c(comId, month, year), names_to = "variable", values_to = "value") %>% 
  group_by(comId, month, year) %>% 
  dplyr::summarise(across(everything(), list(mean)))

colnames(climate_monthly_input_rcp2p6)<-gsub("_1","",colnames(climate_monthly_input_rcp2p6))

climate_monthly_input_rcp2p6$rcp="rcp2p6"

###RCP6.0
climate_monthly_input_rcp6p0_1 <- as.data.frame(climate_monthly_input[[1]][2])
climate_monthly_input_rcp6p0_2 <- as.data.frame(climate_monthly_input[[2]][2])
climate_monthly_input_rcp6p0_3 <- as.data.frame(climate_monthly_input[[3]][2])
climate_monthly_input_rcp6p0_4 <- as.data.frame(climate_monthly_input[[4]][2])
climate_monthly_input_rcp6p0_5 <- as.data.frame(climate_monthly_input[[5]][2])

climate_monthly_input_rcp6p0 = rbind(climate_monthly_input_rcp6p0_1, 
                                     climate_monthly_input_rcp6p0_2,
                                     climate_monthly_input_rcp6p0_3,
                                     climate_monthly_input_rcp6p0_4,
                                     climate_monthly_input_rcp6p0_5)

climate_monthly_input_rcp6p0=climate_monthly_input_rcp6p0[-c(6,7,8,11,12,13)]
names(climate_monthly_input_rcp6p0)[5]="P"
names(climate_monthly_input_rcp6p0)[7]="Tmax"
names(climate_monthly_input_rcp6p0)[9]="Tmin"
climate_monthly_input_rcp6p0=climate_monthly_input_rcp6p0[-c(4,6,8)]

### Form averages
climate_monthly_input_rcp6p0 = climate_monthly_input_rcp6p0 %>% 
  #pivot_longer(-c(comId, month, year), names_to = "variable", values_to = "value") %>% 
  group_by(comId, month, year) %>% 
  dplyr::summarise(across(everything(), list(mean)))

colnames(climate_monthly_input_rcp6p0)<-gsub("_1","",colnames(climate_monthly_input_rcp6p0))

climate_monthly_input_rcp6p0$rcp="rcp6p0"

###RCP8.5
climate_monthly_input_rcp8p5_1 <- as.data.frame(climate_monthly_input[[1]][3])
climate_monthly_input_rcp8p5_2 <- as.data.frame(climate_monthly_input[[2]][3])
climate_monthly_input_rcp8p5_3 <- as.data.frame(climate_monthly_input[[3]][3])
climate_monthly_input_rcp8p5_4 <- as.data.frame(climate_monthly_input[[4]][3])
climate_monthly_input_rcp8p5_5 <- as.data.frame(climate_monthly_input[[5]][3])

climate_monthly_input_rcp8p5 = rbind(climate_monthly_input_rcp8p5_1, 
                                     climate_monthly_input_rcp8p5_2,
                                     climate_monthly_input_rcp8p5_3,
                                     climate_monthly_input_rcp8p5_4,
                                     climate_monthly_input_rcp8p5_5)

climate_monthly_input_rcp8p5=climate_monthly_input_rcp8p5[-c(6,7,8,11,12,13)]
names(climate_monthly_input_rcp8p5)[5]="P"
names(climate_monthly_input_rcp8p5)[7]="Tmax"
names(climate_monthly_input_rcp8p5)[9]="Tmin"
climate_monthly_input_rcp8p5=climate_monthly_input_rcp8p5[-c(4,6,8)]

### Form averages
climate_monthly_input_rcp8p5 = climate_monthly_input_rcp8p5 %>% 
  #pivot_longer(-c(comId, month, year), names_to = "variable", values_to = "value") %>% 
  group_by(comId, month, year) %>% 
  dplyr::summarise(across(everything(), list(mean)))

colnames(climate_monthly_input_rcp8p5)<-gsub("_1","",colnames(climate_monthly_input_rcp8p5))

climate_monthly_input_rcp8p5$rcp="rcp8p5"

### Rbind all RCPs in one dataframe
climate_monthly_input_all=rbind(climate_monthly_input_rcp2p6,
                                climate_monthly_input_rcp6p0, 
                                climate_monthly_input_rcp8p5)



##change comID of Göttingen 
climate_monthly_input_all$comId[climate_monthly_input_all$comId == 3152] <- 3159


### ATTENTION: Somehow, the Kreis Garmisch-Partenkirchen is missing in the climate data. 
#For now: replace with data from Bad Tölz-Wolfratshausen
climate_monthly_input_all = rbind(climate_monthly_input_all,
      climate_monthly_input_all %>% 
        filter(comId == 9173) %>% 
        mutate(comId = 9180))

## Also missing: 9763, 9776 and 9780
## REPLACE FOR NOW
climate_monthly_input_all = rbind(climate_monthly_input_all,
                                  climate_monthly_input_all %>% 
                                    filter(comId == 9173) %>% 
                                    mutate(comId = 9763))

climate_monthly_input_all = rbind(climate_monthly_input_all,
                                  climate_monthly_input_all %>% 
                                    filter(comId == 9173) %>% 
                                    mutate(comId = 9776))

climate_monthly_input_all = rbind(climate_monthly_input_all,
                                  climate_monthly_input_all %>% 
                                    filter(comId == 9173) %>% 
                                    mutate(comId = 9780))



####Change comID for Mecklenburg-Vorpommern because of Kreisreform
codesMV=subset(codes, codes$NUTS3_codes>=13000 & codes$NUTS3_codes<14000)

##13071 = 13052 + 13055 + 13056 + 13002
##13072 = 13051 + 13053
##13073 = 13061 + 13057 + 13005
##13074 = 13058 + 13006
##13075 = 13062 + 13059 + 13001
##13076 = 13054 + 13060 

climate_monthly_input_all_MV=subset(climate_monthly_input_all, 
                                    climate_monthly_input_all$comId>=13000 & 
                                      climate_monthly_input_all$comId<14000)

##13071 = 13052 + 13055 + 13056 + 13002
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13052] <- 13071
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13055] <- 13071
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13056] <- 13071
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13002] <- 13071

##13072 = 13051 + 13053
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13051] <- 13072
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13053] <- 13072

##13073 = 13061 + 13057 + 13005
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13061] <- 13073
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13057] <- 13073
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13005] <- 13073

##13074 = 13058 + 13006
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13058] <- 13074
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13006] <- 13074

##13075 = 13062 + 13059 + 13001
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13062] <- 13075
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13059] <- 13075
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13001] <- 13075

##13076 = 13054 + 13060 
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13054] <- 13076
climate_monthly_input_all_MV$comId[climate_monthly_input_all_MV$comId == 13060] <- 13076

##Group
climate_monthly_input_all_MV = climate_monthly_input_all_MV %>% 
  group_by(comId, year, month, rcp) %>% 
  dplyr::summarise(across(everything(), list(mean)))

colnames(climate_monthly_input_all_MV)<-gsub("_1","",colnames(climate_monthly_input_all_MV))


##Replace new MV comIDs in climate_monthly_input_all
climate_monthly_input_all=subset(climate_monthly_input_all, climate_monthly_input_all$comId<=13000 
                                 | climate_monthly_input_all$comId>14000)
climate_monthly_input_all=rbind(climate_monthly_input_all, climate_monthly_input_all_MV)

################################################################################################
#### BASE PERIOD: Subset for years 2000 - 2020

##Add NUTS3 codes to merge
names(codes)[1]="comId"
codes$comId=as.numeric(codes$comId)
climate_monthly_input_all=merge(climate_monthly_input_all, codes, by = "comId")

#Merge with latitude data
NUTS3_lat=NUTS3_shp[c(1,28)]

NUTS3_lat %<>%
  st_drop_geometry()

climate_monthly_input_all=merge(climate_monthly_input_all, NUTS3_lat, by = "NUTS3_codes")

###Determine ref crop evapotranspiration with Hargreaves method 

###Calculate extra-terrestial radiation as a function of the latitude and the month

##add data frame with dates 
date = expand.grid(month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                   year = (2000))
date= date %>% 
  mutate(date_format = paste0(year,"/", month, "/15"))
names(date)[3]="date"

##d_r = Inverse relative earth-sun distance
D_r = 
  date %>% rowwise() %>% mutate(EarSunDis(date))
names(D_r)[4]="d_r"

## delta = Solar declination (rad)
Delta = 
  date %>% rowwise() %>% mutate(FAO56::SolDec(date))
names(Delta)[4]="delta"

EarthSunData=merge(D_r, Delta, by = c("month", "year", "date"))
EarthSunData=EarthSunData[-c(2,3)]

##Merge with climate_monthly_input_all
climate_monthly_input_all=merge(climate_monthly_input_all, EarthSunData, by = "month")

##omega_s = Sunset hour angle (rad)
##phi = the latitude in radians 
climate_monthly_input_all$phi=climate_monthly_input_all$lat*pi/180

climate_monthly_input_all_ETo = 
  climate_monthly_input_all %>% rowwise() %>% mutate(omega_s=c(SunHA(phi, delta)))

##calculate extraterrestial radiation
climate_monthly_input_all_ETo = 
  climate_monthly_input_all_ETo %>% rowwise() %>% mutate(ExRad(d_r, omega_s, phi, delta, G_sc = 0.082))

names(climate_monthly_input_all_ETo)[16]="R_a"


##Applying Hargreaves
# Hargreaves
climate_monthly_input_all_ETo = 
  climate_monthly_input_all_ETo %>% rowwise() %>% mutate(ETo_Hrg(T_min = Tmin, T_max = Tmax, R_a))

names(climate_monthly_input_all_ETo)[17]="ETo_Hrg"

######### Attention: Double checking has proved that ETo values derived by Hargreaves method are overestimated. 
######### Therefore, we scale it to an estimate derived from Penman-Monteith method. 
######### Example is from: https://www.fao.org/3/x0490e/x0490e08.htm
######### EXAMPLE 18 for Brussels: ETo = 3.9 with PM, ETo = 9.5 with Hargreaves (using the same input data)
######### Assumption: ETo (Hargreaves) * 0.41 = ETo (PM)

#NOTE - this is Jasmin's original script to generate the ETo_PM value - has been replaced with my script below.
#climate_monthly_input_all_ETo$ETo_PM=climate_monthly_input_all_ETo$ETo_Hrg*0.41



##########################################################################
## All script below this point is my own work within the thesis project ##
##########################################################################

# here the script for calculating ETo directly with Penman-Monteith FAO instead begins

#make a backup copy of climate_monthly_input_all_ETo
CMIA_ETo_backup <- climate_monthly_input_all_ETo

#import the relevant csv file from ArcGIS Pro, containing elevation per nuts3 district:
Elevation <- read.csv ("C:\\Users\\edigm\\Documents\\Joint MSc\\Thesis Work\\CMIP5_climate_data_monthly\\
                       DE_NUTS3_elev_3_edited_fixedver.csv", header = TRUE)
#names(Elevation)[3] <- "comId"
climate_monthly_input_all_ETo=merge(climate_monthly_input_all_ETo, Elevation , by = "comId")


SB_c = 4.903*10^-9 #Stefan-Boltzmann constant

#Pr = air pressure
climate_monthly_input_all_ETo$Pr <- 101.3 * ((293 - 0.0065 * climate_monthly_input_all_ETo$MEAN_ELEV) / 293) ^ 5.26

#Y = psychrometric constant (kPa/C)


  climate_monthly_input_all_ETo$Y <- 0.665*10^-3*(climate_monthly_input_all_ETo$Pr)

#Mean Saturation Vapour Pressure (e_s)
#calc. min temp SVP:
climate_monthly_input_all_ETo$e_s_min <- 0.6108 * exp( (17.27*climate_monthly_input_all_ETo$Tmin) / 
                                                         (climate_monthly_input_all_ETo$Tmin+237.3) )
#calc. max temp SVP:
climate_monthly_input_all_ETo$e_s_max <- 0.6108 * exp( (17.27*climate_monthly_input_all_ETo$Tmax) / 
                                                         (climate_monthly_input_all_ETo$Tmax+237.3) )
#calc. mean temp SVP (e_s):
climate_monthly_input_all_ETo$e_s <- (climate_monthly_input_all_ETo$e_s_min + 
                                        climate_monthly_input_all_ETo$e_s_max)/2  



#actual vapour pressure (e_a)
climate_monthly_input_all_ETo$e_a <- 0.6108*exp( (17.27* climate_monthly_input_all_ETo$Tmin ) / 
                                                   (climate_monthly_input_all_ETo$Tmin + 237.3))

#estimate actual solar radiation (r_s)
#calculated using temperature differences and a coefficient
#coefficient is 0.16 for inland regions, goes up to 0.19 closer you get to a coastal region
#represents influence of large bodies of water (so also for huge lakes)
#see: Allen et al (1998), p61
climate_monthly_input_all_ETo$r_s <- 0.16 * sqrt(climate_monthly_input_all_ETo$Tmax - 
                                                   climate_monthly_input_all_ETo$Tmin) * 
                                                    climate_monthly_input_all_ETo$R_a
  
climate_monthly_input_all_ETo$r_so <- (0.75 + 2 * 10^-5 * climate_monthly_input_all_ETo$MEAN_ELEV) * 
  climate_monthly_input_all_ETo$R_a #this equation is verified, correct

climate_monthly_input_all_ETo$r_nl = (( (SB_c * (climate_monthly_input_all_ETo$Tmax + 273.15)^4)+
                                          (SB_c * (climate_monthly_input_all_ETo$Tmin+273.15)^4))/2) * 
  (0.34 - 0.14 * sqrt(climate_monthly_input_all_ETo$e_a)) * 
  (1.35 * (climate_monthly_input_all_ETo$r_s / climate_monthly_input_all_ETo$r_so) - 0.35)

climate_monthly_input_all_ETo$r_ns = (1-0.23)*climate_monthly_input_all_ETo$r_s 
#0.23 is the coefficient for the reference grass surface.

climate_monthly_input_all_ETo$r_n <- climate_monthly_input_all_ETo$r_ns - climate_monthly_input_all_ETo$r_nl

climate_monthly_input_all_ETo$Tmean <- (climate_monthly_input_all_ETo$Tmax + climate_monthly_input_all_ETo$Tmin) /2

#This calculates the final ETO value:
#NOTE - this is substituting the ETo_PM value that the original script generated with a DIFFERENT VALUE
climate_monthly_input_all_ETo$ETo_PM <- ( 0.408 * (climate_monthly_input_all_ETo$r_n - 0) * 
                                            climate_monthly_input_all_ETo$delta / 
      (climate_monthly_input_all_ETo$delta + climate_monthly_input_all_ETo$Y * (1+0.34 * 2)) )
  + ( 900 / (climate_monthly_input_all_ETo$Tmean + 273) * 2 * 
        (climate_monthly_input_all_ETo$e_s - climate_monthly_input_all_ETo$e_a) * 
        climate_monthly_input_all_ETo$Y / 
        (climate_monthly_input_all_ETo$delta + climate_monthly_input_all_ETo$Y*(1+0.34*2)) )

#replaces any negative ETo_PM values with 0.
climate_monthly_input_all_ETo <- climate_monthly_input_all_ETo %>%
  rowwise() %>%
  mutate (ETo_PM = ifelse(ETo_PM < 0, 0, ETo_PM) )

#add the RH_min value that you need to calculate yields with:
climate_monthly_input_all_ETo$RH_min <- 100 * (climate_monthly_input_all_ETo$e_a / 
                                                 climate_monthly_input_all_ETo$e_s_max )

#This leaves us with just the replaced value of ETo_PM, an ET value calculated 
#from the Penman Monteith FAO method directly from the available weather data





##next steps: bringing in the WP and FC, splitting the table into multiple sub tables##

#import the csv file with the wilting point (WP) and field capacity (FC) per nuts3 district:
WP_FC <- read.csv ("C:\\Users\\edigm\\Documents\\Joint MSc\\Thesis Work\\THesis datasets\\NUTS3_WP_FC_v2.csv", 
                   header = TRUE)
climate_monthly_input_all_ETo=merge(climate_monthly_input_all_ETo, WP_FC , by = "comId")


#remove all non-Thuringian states from dataset
THR_set <- subset (climate_monthly_input_all_ETo, grepl("^DEG", NUTS3))

#clean up work environment by removing all the other climate datasets:
remove(climate_monthly_input_all_MV,
       climate_monthly_input_rcp2p6,
       climate_monthly_input_rcp2p6_1,
       climate_monthly_input_rcp2p6_2,
       climate_monthly_input_rcp2p6_3,
       climate_monthly_input_rcp2p6_4,
       climate_monthly_input_rcp2p6_5,
       climate_monthly_input_rcp6p0,
       climate_monthly_input_rcp6p0_1,
       climate_monthly_input_rcp6p0_2,
       climate_monthly_input_rcp6p0_3,
       climate_monthly_input_rcp6p0_4,
       climate_monthly_input_rcp6p0_5,
       climate_monthly_input_rcp8p5,
       climate_monthly_input_rcp8p5_1,
       climate_monthly_input_rcp8p5_2,
       climate_monthly_input_rcp8p5_3,
       climate_monthly_input_rcp8p5_4,
       climate_monthly_input_rcp8p5_5,
       climate_monthly_input_all)


#then we split it up by DISTRICT and by RCP SCENARIO. It's better to do everything after the split 
#rather than mix it, even if it means repeating some calculations.

#splitting by RCP scenario:
scenario_list <- split ( THR_set, THR_set$rcp)
THR_2.6 <- scenario_list$rcp2p6
THR_6.0 <- scenario_list$rcp6p0
THR_8.5 <- scenario_list$rcp8p5

#splitting by district:
THR_2.6_district <- split (THR_2.6, THR_2.6$NUTS3_codes)
THR_6.0_district <- split (THR_6.0, THR_6.0$NUTS3_codes)
THR_8.5_district <- split (THR_8.5, THR_8.5$NUTS3_codes)

#TESTING - now we re-merge it all as a big list:
THR_all <- c (THR_2.6_district, THR_6.0_district, THR_8.5_district)

##NEXT STAGE - assemble the crop dataset:


#first step - write the crop parameters file:

cropdat <- data.frame (
cropcode = 1:14, #numerical ref. code for crops
cropname = c("drybeans", "onions","cabbage","asparagus","alfalfa","apples",
             "cherries","spring_oats","maize","rapeseed","potatoes","sugarbeets",
             "winterwheat","winterbarley"), #name of crop
overwinter = c(0,0,0,1,0,0,0,0,0,0,0,0,1,1), #flag for crops planted in winter and harvested spring
mintemp = c (9,10,10,11,5,7,7,0,8,0,7,5,5,1), #min temp below which crop becomes dormant & growth pauses (celsius)
Kcb_ini = c(0.15,0.15,0.15,0.15,0.3,0.35,0.35,0.15,0.15,0.15,0.15,0.15,0.15,0.15), #crop coefficient early growth stage
Kcb_mid = c(1.1,1.05,0.95,0.9,1.15,0.9,0.9,1.1,1.15,0.95,1.1,1.15,1.1,1.1), #crop coefficient mid growth stage
Kcb_end = c(0.25,0.7,0.85,0.2,1.1,0.65,0.65,0.15,0.15,0.25,0.65,0.5,0.15,0.15), #crop coefficient late growth stage
ga = c(151,31,61,121,61,31,31,91,91,91,91,91,241,241), #start month (days)
gb = c(180,50,106,181,106,31,31,121,133,111,121,113,313,307), #growth stage 2 (days)
gc = c(199,78,174,301,151,166,166,158,185,145,158,158,403,389), #growth stage 3 (days)
gd = c(260,165,219,331,195,233,273,203,248,186,207,210,510,488), #growth stage 4 (days)
ge = c(270,210,240,480,240,300,300,240,300,210,240,240,600,570), #end month (days)
zr_ini = c(0.15,0.15,0.15,1.8,0.15,2,2,0.15,0.15,0.15,0.15,0.15,0.15,0.15), #starting root depth (mm)
zr_fin = c(0.9,0.6,0.8,1.8,2,2,2,1.5,1.7,1.5,0.6,1.2,1.8,1.5), #finishing root depth (mm)
ky_a = c(0.2,0.45,0.2,1,1.1,1,1,1,0.4,0.3,0.625,1,0.2,1), #ky value (yield adjustment during water stress) for stage A
ky_b = c(1.1,0.625,0.325,1,1.1,1,1,1,1.5,0.55,0.6625,1,0.65,1), #ky value stage b
ky_c =c(0.75,0.8,0.45,1,1.1,1,1,1,0.5,0.6,0.7,1,0.55,1), #ky value stage c
ky_d = c(0.2,0.3,0.6,1,1.1,1,1,1,0.2,0.6,0.2,1,0.55,1), #ky value stage d
ky_e = c(0.2,0.3,0.6,1,1.1,1,1,1,0.2,0.6,0.2,1,0.55,1), #ky value stage e
h = c(0.4,0.5,0.4,0.8,0.7,4,4,1,2,0.6,0.6,0.5,1,1), # max height (m)
p = c(0.45,0.35,0.45,0.45,0.55,0.5,0.5,0.55,0.55,0.6,0.35,0.55,0.55,0.55), #water absorbtion in root zone
hi = c(40,75,67,48,100,65,65,50,50,25,77,70,47,40), #harvest index
wp = c(15,15,15,15.7,15,15,15,12.4,33,15,19,17,15,15), #normalised water productivity (Mg per mm per ha)
wp_yf = c(13.5,15,15,15.7,15,15,15,12.4,33,15,19,17,15,15) # WP during yield formation (stage c)
)

#turn this alert setting on "summarise()" off:
options(dplyr.summarise.inform = FALSE)

###big loop starts here##

for (c in 1: nrow(cropdat) ) {

#this loop writes a special crop file "cr_default" for each crop
#then matches it with the environment files.
  
#every loop the easiest thing to do is get rid of the old cr_default
  remove (cr_default)
#also remove the old crlist_default
  remove (crlist_default)
  
#we add a list, "crlist_default". This is the list all the outputs are stored in.
#it starts out empty, and so it is generated using list()
  crlist_default <- list()
  
#then rewrite it from the start with the right number of rows:
  
  cr_default <- data.frame (
    day = 1:720, #"days" that we will then average into months
    cropcode = NA, #code of the crop
    cropname = NA, #name of the crop
    overwinter = NA, #whether the crop is planted in autumn + harvested following year
    Kcb = NA, #crop coefficient
    zr = NA, #root depth (mm)
    CH = NA, #crop height (m)
    fc = NA, #fraction of soil covered by canopy
    rp = NA, #fraction of soil water the crop can absorb
    hi = NA, #harvest index
    wp = NA, #water productivity factor
    ky = NA #factor adjusting yield in response to water stress
  )

#asparagus works a bit differently, so (unfortunately) it gets its own loop for now.
#In the future, it'll involve adjusting the cropdat file to allow it to work with asparagus more smoothly.
if (cropdat$cropcode[c] == 4) {

print ("I'm doing the asparagus crop parameters!")  
#We are doing Kcb differently for asparagus:
# stage a-b = ini-mid, b-c = mid, c-d = mid-end, d-e = end.
  cr_default$Kcb[cropdat$ga[c]:cropdat$gb[c]] = seq (from = cropdat$Kcb_ini[c], 
                                                     to = cropdat$Kcb_mid[c], length = (1+cropdat$gb[c]-cropdat$ga[c]))
  cr_default$Kcb[cropdat$gb[c]:cropdat$gc[c]] =  cropdat$Kcb_mid[c]
  cr_default$Kcb[cropdat$gc[c]:cropdat$gd[c]] = seq (from = cropdat$Kcb_mid[c], 
                                                     to = cropdat$Kcb_end[c], length = (1+cropdat$gd[c]-cropdat$gc[c]))
  cr_default$Kcb[cropdat$gd[c]:cropdat$ge[c]] = cropdat$Kcb_end[c]

#zr value is the same continuously - it is a perennial being modeled as fully grown from the outset:
 cr_default$zr[cropdat$ga[c]:cropdat$ge[c]] <-cropdat$zr_fin[c]  

#canopy coverage also works differently: 
  cr_default$fc[cropdat$ga[c]:cropdat$gb[c]] = seq (from = 0, to = 0.8, length = (1+cropdat$gb[c] - cropdat$ga[c]) )
  cr_default$fc[cropdat$gb[c]:cropdat$gc[c]] = seq (from = 0.8, to = 1, length = (1+cropdat$gc[c] - cropdat$gb[c]) )
  cr_default$fc[cropdat$gc[c]:cropdat$gd[c]] = seq (from = 1, to = 0.8, length = (1+cropdat$gd[c] - cropdat$gc[c]) )
  cr_default$fc[cropdat$gd[c]:cropdat$ge[c]] = 0
  
    
#Asparagus foliage is cut back over the winter, so gd-ge the value should be 0 for height:
  cr_default$CH[cropdat$ga[c]:cropdat$gc[c]] <- ( cr_default$Kcb[cropdat$ga[c]:cropdat$gc[c]] / 
                                                    cropdat$Kcb_mid[c] ) * cropdat$h[c]
  cr_default$CH[cropdat$gc[c]:cropdat$gd[c]] <-cropdat$h[c]
  cr_default$CH[cropdat$gd[c]:cropdat$ge[c]] <- 0

#add the ky values in each growth stage (varying Ky values are drawn from FAO website datasets) - same for asparagus as normal
  cr_default$ky[cropdat$ga[c]:cropdat$gb[c]] = seq (from = cropdat$ky_a[c], to = cropdat$ky_b[c], 
                                                    length = (1+cropdat$gb[c] - cropdat$ga[c]) )
  cr_default$ky[cropdat$gb[c]:cropdat$gc[c]] = seq (from = cropdat$ky_b[c], to = cropdat$ky_c[c], 
                                                    length = (1+cropdat$gc[c] - cropdat$gb[c]) )
  cr_default$ky[cropdat$gc[c]:cropdat$gd[c]] = seq (from = cropdat$ky_c[c], to = cropdat$ky_d[c], 
                                                    length = (1+cropdat$gd[c] - cropdat$gc[c]) )
  cr_default$ky[cropdat$gd[c]:cropdat$ge[c]] = seq (from = cropdat$ky_d[c], to = cropdat$ky_e[c], 
                                                    length = (1+cropdat$ge[c] - cropdat$gd[c]) )

#add the p values
  cr_default$rp[cropdat$ga[c]:cropdat$ge[c]] = cropdat$p[c]
  
#add the wp values
  cr_default$wp [cropdat$ga[c]:cropdat$gb[c]] = cropdat$wp[c]
  cr_default$wp [cropdat$gb[c]:cropdat$gc[c]] = seq (from = cropdat$wp[c], to = cropdat$wp_yf[c], 
                                                     length = (1+cropdat$gc[c] - cropdat$gb[c]) )
  cr_default$wp [cropdat$gc[c]:cropdat$gd[c]] = seq (from = cropdat$wp_yf[c], to = cropdat$wp[c], 
                                                     length = (1+cropdat$gd[c] - cropdat$gc[c]) )
  cr_default$wp [cropdat$gd[c]:cropdat$ge[c]] = cropdat$wp[c]    

#add the hi values
  cr_default$hi [cropdat$ga[c]:cropdat$ge[c]] = cropdat$hi[c]
  
#use the day value adjustment for an overwinter crop  
  cr_default <- subset(cr_default, day %in% 241:600)
  cr_default$day <- ifelse(cr_default$day > 360, cr_default$day - 360, cr_default$day)
  setorder( cr_default, day)
  

        
} else {
  
print ("I'm doing the normal crop parameters!")  
  
#add the Kcb values in each growth stage:
cr_default$Kcb[cropdat$ga[c]:cropdat$gb[c]] = cropdat$Kcb_ini[c]
cr_default$Kcb[cropdat$gb[c]:cropdat$gc[c]] =  seq (from = cropdat$Kcb_ini[c], to = cropdat$Kcb_mid[c], 
                                                    length = (1+cropdat$gc[c]-cropdat$gb[c]))
cr_default$Kcb[cropdat$gc[c]:cropdat$gd[c]] = cropdat$Kcb_mid[c]
cr_default$Kcb[cropdat$gd[c]:cropdat$ge[c]] = seq (from = cropdat$Kcb_mid[c], to = cropdat$Kcb_end[c], 
                                                   length = (1+cropdat$ge[c]-cropdat$gd[c]))

#add the zr values: (Allen et al 1998, p279, for equation below)
cr_default$zr[cropdat$ga[c]:cropdat$gc[c]] <- cropdat$zr_ini[c] + (cropdat$zr_fin[c] - cropdat$zr_ini[c]) * 
  ( ( cr_default$Kcb[cropdat$ga[c]:cropdat$gc[c]] - cropdat$Kcb_ini[c] ) / (cropdat$Kcb_mid[c] - cropdat$Kcb_ini[c]) )
cr_default$zr[cropdat$gc[c]:cropdat$ge[c]] <-cropdat$zr_fin[c]

#add the fc values (from table 21 on allen et al 1998, p149)
cr_default$fc[cropdat$ga[c]:cropdat$gb[c]] = seq (from = 0, to = 0.1, length = (1+cropdat$gb[c] - cropdat$ga[c]) )
cr_default$fc[cropdat$gb[c]:cropdat$gc[c]] = seq (from = 0.1, to = 0.8, length = (1+cropdat$gc[c] - cropdat$gb[c]) )
cr_default$fc[cropdat$gc[c]:cropdat$gd[c]] = seq (from = 0.8, to = 1, length = (1+cropdat$gd[c] - cropdat$gc[c]) )
cr_default$fc[cropdat$gd[c]:cropdat$ge[c]] = seq (from = 0.8, to = 0.2, length = (1+cropdat$ge[c] - cropdat$gd[c]) )

#add the height values (allen et al 1998, box 8.1, p277, footnote 3)
cr_default$CH[cropdat$ga[c]:cropdat$gc[c]] <- ( cr_default$Kcb[cropdat$ga[c]:cropdat$gc[c]] / cropdat$Kcb_mid[c] ) * cropdat$h[c]
cr_default$CH[cropdat$gc[c]:cropdat$ge[c]] <-cropdat$h[c]

#add the ky values in each growth stage (varying Ky values are drawn from FAO website datasets)
cr_default$ky[cropdat$ga[c]:cropdat$gb[c]] = seq (from = cropdat$ky_a[c], to = cropdat$ky_b[c], 
                                                  length = (1+cropdat$gb[c] - cropdat$ga[c]) )
cr_default$ky[cropdat$gb[c]:cropdat$gc[c]] = seq (from = cropdat$ky_b[c], to = cropdat$ky_c[c], 
                                                  length = (1+cropdat$gc[c] - cropdat$gb[c]) )
cr_default$ky[cropdat$gc[c]:cropdat$gd[c]] = seq (from = cropdat$ky_c[c], to = cropdat$ky_d[c], 
                                                  length = (1+cropdat$gd[c] - cropdat$gc[c]) )
cr_default$ky[cropdat$gd[c]:cropdat$ge[c]] = seq (from = cropdat$ky_d[c], to = cropdat$ky_e[c], 
                                                  length = (1+cropdat$ge[c] - cropdat$gd[c]) )

#add the p values
cr_default$rp[cropdat$ga[c]:cropdat$ge[c]] = cropdat$p[c]

#add the wp values
cr_default$wp [cropdat$ga[c]:cropdat$gb[c]] = cropdat$wp[c]
cr_default$wp [cropdat$gb[c]:cropdat$gc[c]] = seq (from = cropdat$wp[c], to = cropdat$wp_yf[c], 
                                                   length = (1+cropdat$gc[c] - cropdat$gb[c]) )
cr_default$wp [cropdat$gc[c]:cropdat$gd[c]] = seq (from = cropdat$wp_yf[c], to = cropdat$wp[c], 
                                                   length = (1+cropdat$gd[c] - cropdat$gc[c]) )
cr_default$wp [cropdat$gd[c]:cropdat$ge[c]] = cropdat$wp[c]

#add the hi values
cr_default$hi [cropdat$ga[c]:cropdat$ge[c]] = cropdat$hi[c]

##this part depends on whether its an overwinter crop or not.
#idea behind this part - subset a different range of the dataframe and then 
#rename the days in it as appropriate?


  if (cropdat$overwinter[c] == 0) {
    cr_default <- subset(cr_default, day %in% 1:360)
  } else {
    cr_default <- subset(cr_default, day %in% 241:600)
    cr_default$day <- ifelse(cr_default$day > 360, cr_default$day - 360, cr_default$day)
    setorder( cr_default, day)
  }



}


  
##once it's all finished##
#refine it into a monthly dataframe:
#this makes a dataframe with the average of each block of 30 of the original dataframe as a new record.
cr_month <- cr_default %>%
  group_by(group = (row_number() - 1) %/% 30) %>%
  summarize_all(mean) %>%
  ungroup()
names(cr_month)[1] <- "month"
cr_month$month [1:12] <- sprintf("%02d", seq(1, 12))

#add the crop code and crop name:
cr_month$cropcode = cropdat$cropcode[c]
cr_month$cropname = cropdat$cropname[c]
print (cropdat$cropname[c])


#the RCP + district sub-loop starts here:
#what does this loop do?
#each entry in the list represents a district.
#this loop takes the current dataframe, makes the frame "test01" out of it
#then it does all the calculations to "test01".
#then it produces an output dataframe, named after the district + RCP scenario
#this output is thus unique.
#then the loop iterates again until the entire list is completed.

#for (i in seq_along(THR_2.6_district)) {
for (i in seq_along(THR_all)) {

#this is where the current dataframe [i] is converted into "test01"
test01 <- THR_all[[i]]

#remove all the unnecessary columns:
test01 <- subset (test01, select = c(month, comId, year, P, Tmax, Tmin, Tmean, rcp, 
                                     subdistrict, ETo_PM, RH_min, WP_avg, FC_avg) )

#merge with the crop data:
test01 <- merge (test01, cr_month, by = "month")

#arrange the dataset chronologically:
test01 <- test01 %>% arrange (year, month)

#convert all NA values into 0:
test01 [is.na (test01)] <- 0

#add additional columns for the stages in the crop calculations:
test01 <- test01 %>%
  mutate (
    TEW = NA, #total evaporable water in the soil (depth in mm)
    REW = NA, #readily evaporable water in the soil (depth in mm)
    Ir =  NA, #irrigation in mm
    fw = NA, #factor representing wetness of the soil after rain or irrigation
    Kc_max = NA, #maximum possible combined evapotranspiration coefficient of a surface 
    few = NA, #fraction of soil surface exposed (i.e. not under the crop canopy)
    De_t = NA, #rolling value representing root zone water depletion level over the day (mm)
    Kr = NA, #soil evaporation reduction coefficient
    Ke = NA, #soil evaporation coefficient (to be multiplied by ETo)
    Ev = NA, #evaporation of the soil surface (mm/day)
    DP = NA, #deep percolation of evaporating layer of soil (mm)
    De = NA, #depth of evaporation (mm)
    c_ETc = NA, #actual crop evapotranspiration (mm/day)
    c_p_adj = NA, #adjusted value of p (fraction of water roots can absorb)
    c_TAW = NA, #total available water in root zone (mm)
    c_RAW = NA, #readily available water in the root zone (mm)
    S_DP = NA, #deep percolation of water in the soil
    c_Dr_t = NA, #Dr value during the day, if rainfall/irrigation occur early in the day
    Ks = NA, #water stress adjustment coefficient
    ETc_adj = NA, #ETc value adjusted for water stress.
    c_Dr = NA, #crop root zone depletion
    Kc = NA, #final crop coefficient
    Y_x = NA, #unadjusted yield
    Y_a = NA #actual yield
  )


## first: calculate values that don't require a loop ##
#TEW value (mm)
#note - equation is #73, p144, allen et al 1998. Multiplied by 10 to get it in the correct unit.
test01 <- test01 %>%
  rowwise() %>%
  mutate(
    TEW = 10 * ( ( FC_avg - (0.5 * WP_avg) ) * 0.125 )
  )

#REW value (mm)
#REW value is drawn from a table instead of calculated, a "typical value" for REW is between 5-12 mm.
#Given high values of TEW (~30) in the dataset, higher TEW generally corresponds to higher REW, 
#so a high value (10) is used here.
test01 <- test01 %>% 
  mutate ( REW = 10) 



#calculate Kc_max:
#note - in the "2 - 2" bracket, the first 2 = windspeed. We are defaulting to windspeed = 2 here.
#experimental added here - if average temp is below a certain level, growth will not occur.
#each crop has its own min. temp. requirement, present in the literature.
#this value (cropdat$mintemp) is thus added to cropdat
test01 <- test01 %>%
  rowwise() %>%
  mutate(
    Kc_max = ifelse(Kcb == 0 , 0, 
                    max((1.2 + (0.04 * (2 - 2) - 0.004 * (RH_min - 45)) * (CH / 3) ^ 0.3), (Kcb + 0.05))
    )
  )

#set fw value as 1 (we are not using irrigation in this model, so it can always be 1)
test01 <- test01 %>% 
  mutate ( fw = 1) 

#set Ir value as 0 (Ir = irrigation, not used in the model, can be 0)
test01 <- test01 %>%
  mutate (Ir = 0)

#calculate few (fraction of soil surface exposed, not covered by canopy)
test01 <- test01 %>%
  rowwise() %>%
  mutate(
    few = min (1- fc, fw)
  )

## second: everything that requires a loop ##

#NOTE - below is taken from another script - gotta edit it.
#we need TEW and REW

#we start with the "day 1" calculations of everything:
test01 <- within (test01,{
  
  #initialise the De_t, starting with yesterday's De:
  #note - the "0" is yesterday's De value for the first day - we start at 0.
  De_t[1] <- max ( 0 - (Ir[1]/fw[1]) - P[1], 0)
  
  #equation for Kr on day 1
  Kr[1] <-  ifelse(  De_t[1] < REW[1], 1, (TEW[1] - De_t[1]) / (TEW[1] - REW[1]) )
  
  #equation for Ke on day 1
  Ke[1] <- min ( Kr[1] * (Kc_max[1] - Kcb[1]), few[1] * Kc_max[1])  
  
  #equation for E on day 1
  Ev[1] <- ( Ke[1] * ETo_PM[1] )
  
  #equation for DP on day 1
  DP[1] <- max ( P[1] + (Ir[1]/fw[1]) - 0, 0 )
  
  #equation for De on day 1
  
  De[1] <- max(0 - P[1] - (Ir[1] / fw[1]) + (Ev[1] / few[1]) + DP[1], 0)
  
  
})

#next step - loop through the dataset to calculate the values for every other day:
# Loop through the dataset to calculate De values
#note - the length of each climate dataset is 1320
for (i in 2:1320) {
  #how the above works:
  #i = which row the loop is calculating
  #2 : 1320 means it starts on row 2 (where i = 2) and it ends on row (i = 1320).

  
  #NOTE - everything within these {} brackets is the loop
  #the loop calculates all the values for a given day.
  
  #equation for few
  test01$few[i] <- min (1- test01$fc[i], test01$fw[i])
  
  #initialise the De_t, starting with yesterday's De:
  test01$De_t[i] <- max ( test01$De[i-1] - (test01$Ir[i]/test01$fw[i]) - test01$P[i], 0)
  
  #equation for Kr
  test01$Kr[i] <-  ifelse(
    test01$De_t[i] < test01$REW[i], 1, 
    (test01$TEW[i] - test01$De_t[i]) / 
      (test01$TEW[i] - test01$REW[i]) 
  )
  
  
  #equation for Ke  
  test01$Ke[i] <- min ( 
    test01$Kr[i]  * (test01$Kc_max[i] - test01$Kcb[i]), 
    test01$few[i] * test01$Kc_max[i]
  )
  
  
  #equation for E    
  test01$Ev[i] <- (test01$Ke[i] * test01$ETo_PM[i])
  
  #equation for DP
  test01$DP[i] <- max ( test01$P[i] + (test01$Ir[i]/test01$fw[i]) - test01$De[i-1], 0 )
  
  #equation for De  
  test01$De[i] <- max(test01$De[i-1] - test01$P[i] - (test01$Ir[i] / test01$fw[i]) + 
                        (test01$Ev[i] / test01$few[i]) + test01$DP[i], 0)     
  
}

#calculate c_ETc from the ref ETo and the Kc
#NOTE - we need the 'regular' ETc before we can get the 'c_ETc_adj' value 
#representing ETc with environ. condits. taken into account.

test01 <- test01 %>%
  rowwise() %>%
  mutate(
    c_ETc = ETo_PM * ( Kcb + Ke)
  )

#add the static values & values that do not need to be calculated in a loop first:
test01 <- test01 %>%
  rowwise() %>%
  mutate (
    c_p_adj = rp + 0.04 * (5-c_ETc), #adjusted value of p (depletion fraction for roots) 
                                     #based on ETc value
    c_TAW = 10 * (FC_avg - WP_avg) * zr, #total available water in root zone (mm)
    c_RAW = c_TAW * c_p_adj, #readily available water in root zone (mm)
    
  )


#add values that involve the loop:
#day 1:
test01 <- within (test01, {
    c_Dr_t[1] <- max ( 0 -Ir[1] -P[1], 0)
    Ks[1] <- ifelse (Kcb[1] == 1, 1, 
      ifelse (c_Dr_t [1] <= c_RAW[1], 1, 
                     (c_TAW[1] - c_Dr_t[1]) / ( c_RAW[1] * c_TAW[1] )
                    )
            )
    S_DP[1] <- max (P[1] + Ir[1] - c_ETc - 0, 0)
    Kc[1] <- Ks[1] * Kcb[1] + Ke[1]
    c_Dr[1] <- min (max (0 - P[1] - Ir[1] + c_ETc[1] + S_DP[1], 0), c_TAW[1])
    ETc_adj[1] <- ( (Ks[1]*Kcb[1]) + Ke[1]) * ETo_PM[1]
    
    #yield calculations:
    
    #we can do a straight calculation, which is:
    #Y_x should be "max yield" - so ETo without any water constraints, 
    #and water productivity and HI without water constraints.
    #unadjusted yield = crop transpiration * water production * harvest index
    Y_x[1] <- ifelse ( (Tmean[1] < cropdat$mintemp[c]), 0, (ETo_PM[1] * Kcb[1] *  
                                                              wp[1] * (hi[1]/100) ) )
    
    
    #adjusted yield (incorporates adjusted ETc)
    Y_a [1] <- ifelse ( (ETo_PM[1] == 0 | Tmean[1] < cropdat$mintemp[c]), 0, 
                        ( ifelse ((Kcb[1] == 0), 0,
                                  Y_x[1] * ( 1 - ky[1] * (1 - (ETc_adj[1] / c_ETc[1]) ) )
                                 )
                        )
                      )
      })

#then the same for the loop:
for (i in 2:1320) {
  test01 <- within (test01, {
    c_Dr_t[i] <- max ( c_Dr[i-1] -Ir[i] -P[i], 0)
    Ks[i] <- ifelse (Kcb[i] == 0, 1, 
      (ifelse (c_Dr_t [i] <= c_RAW[i], 1, 
                     (c_TAW[i] - c_Dr_t[i]) / ( c_RAW[i] * c_TAW[i] )
                    ) ) 
      )
    S_DP[i] <- max (P[i] + Ir[i] - c_ETc - c_Dr[i-1], 0)
    Kc[i] <- Ks[i] * Kcb[i] + Ke[i]
    c_Dr[i] <- min (max (c_Dr[i-1] - P[i] - Ir[i] + c_ETc[i] + S_DP[i], 0), c_TAW[i])
    ETc_adj[i] <- ( (Ks[i]*Kcb[i]) + Ke[i]) * ETo_PM[i]
    
    #yield calculations:
    #max potential yield
    Y_x[i] <- ifelse ( (Tmean[i] < cropdat$mintemp[c]), 0, (ETo_PM[i] * Kcb[i] *  
                                                              wp[i] * (hi[i]/100) ) )
    
    #adjusted yield (incorporates adjusted ETc, ky adjustment factor)
    #this has two separate ifelse() to stop if ETo_PM, or if Kcb, are 0.
    Y_a [i] <- ifelse ( (ETo_PM[i] == 0 | Tmean[i] < cropdat$mintemp[c]), 0, 
      ( ifelse ((Kcb[i] == 0), 0,
                       Y_x[i] * ( 1 - ky[i] * (1 - (ETc_adj[i] / c_ETc[i]) ) )
                      )
      )
      )
      })
}


#simplify the results to use them:
#multiply precipitation, ETo, and yield by 30 - to reverse the simplification of the model.

#average all values per year.
#then possibly - array them all per decade, get the decade average.

#analytical calculations:
#irrigation demand (i.e. dif. between ETc and Precip)
#change in yield over time (if any)
#change in irrigation demand over time (if any)


#condense the values into annual values:

if ( cropdat$overwinter[c] == 0  ) {
  print ("I'm doing the summer calculation!")
test01 <- test01 %>%
  group_by(rcp, cropcode, cropname, comId, subdistrict, year) %>%
  summarise(P = sum(P, na.rm = TRUE),
            ETo_PM = sum (ETo_PM, na.rm = TRUE),
            c_ETc = sum (c_ETc, na.rm = TRUE),
            ETc_adj = sum (ETc_adj, na.rm = TRUE),
            Y_x = sum (Y_x, na.rm = TRUE),
            Y_a = sum (Y_a, na.rm = TRUE)
  )%>%
  ungroup()
} else {
  print ("I'm doing the overwinter calculation!")
  #this is how we do it if its an overwinter crop:
  #add an extra column, crop_year - this is the year the crop will be harvested
  test01$crop_year = NA
  #convert month to numeric, as we need to calculate with it
  test01$month <- as.numeric (test01$month)
  
  
  test01 <- test01 %>%
    
    # Create a new variable 'crop_year' to represent the crop year
    # generate the sum of this value in each year row.
    mutate(crop_year = ifelse(month %in% 1:((cropdat$ga[c]-1)/30), year, year + 1)) %>%
    group_by (crop_year) %>%
    mutate (Y_x_sum = sum (Y_x),
            Y_a_sum = sum (Y_a) ) %>%
    ungroup () %>%
    #if the year =/= crop year, we turn the month's yield sum values into NA
    mutate(Y_x_sum = ifelse(year == crop_year, Y_x_sum, NA),
           Y_a_sum = ifelse(year == crop_year, Y_a_sum, NA))
  
  #now run a similar script to the prev. script:
  test01 <- test01 %>%
    group_by(rcp, cropcode, cropname, comId, subdistrict, year) %>%
    summarise(P = sum(P, na.rm = TRUE),
              ETo_PM = sum (ETo_PM, na.rm = TRUE),
              c_ETc = sum (c_ETc, na.rm = TRUE),
              ETc_adj = sum (ETc_adj, na.rm = TRUE),
  #key difference year - as we have already calculated the annual sum yields,
  #this just calculates the means, ignoring the NAs
  #this directly gives us the sum we already calculated.
              Y_x = mean (Y_x_sum, na.rm = TRUE),
              Y_a = mean (Y_a_sum, na.rm = TRUE)
    )%>%
    ungroup()
  
  
    }


##### next step: ######
#we take the annual values and we average them into a single line:

#we take the annual record, "test01", and split off individual year groups:

#this also works for picking out a subset of years.
test01_91_00 <- subset (test01, test01$year >= 1991 & test01$year <= 2000)
test01_11_20 <- subset (test01, test01$year >= 2011 & test01$year <= 2020)
test01_89_98 <- subset (test01, test01$year >= 2089 & test01$year <= 2098)

#per subset, we calculate the mean of each value.
#1991 - 2000:
test01_91_00 <- test01_91_00 %>%
  group_by(rcp, cropcode, cropname, comId, subdistrict) %>%
  summarise(P_91_00 = mean (P, na.rm = TRUE),
            ETo_91_00 = mean (ETo_PM, na.rm = TRUE),
            ETc_91_00 = mean (c_ETc, na.rm = TRUE),
            ETc_adj_91_00 = mean (ETc_adj, na.rm = TRUE),
            Y_x_91_00 = mean (Y_x, na.rm = TRUE),
            Y_a_91_00 = mean (Y_a, na.rm = TRUE)
  )%>%
  ungroup()


#2011 - 2020:
test01_11_20 <- test01_11_20 %>%
  group_by(rcp, cropcode, cropname, comId, subdistrict) %>%
  summarise(P_11_20 = mean (P, na.rm = TRUE),
            ETo_11_20 = mean (ETo_PM, na.rm = TRUE),
            ETc_11_20 = mean (c_ETc, na.rm = TRUE),
            ETc_adj_11_20 = mean (ETc_adj, na.rm = TRUE),
            Y_x_11_20 = mean (Y_x, na.rm = TRUE),
            Y_a_11_20 = mean (Y_a, na.rm = TRUE)
  )%>%
  ungroup()

#2089-2098
test01_89_98 <- test01_89_98 %>%
  group_by(rcp, cropcode, cropname, comId, subdistrict) %>%
  summarise(P_89_98 = mean (P, na.rm = TRUE),
            ETo_89_98 = mean (ETo_PM, na.rm = TRUE),
            ETc_89_98 = mean (c_ETc, na.rm = TRUE),
            ETc_adj_89_98 = mean (ETc_adj, na.rm = TRUE),
            Y_x_89_98 = mean (Y_x, na.rm = TRUE),
            Y_a_89_98 = mean (Y_a, na.rm = TRUE)
  )%>%
  ungroup()

#calculate the irrigation deficiency per year:
test01_91_00 <-  test01_91_00%>%
  mutate ( Ir_def_91_00 =  max ( ETc_91_00 - P_91_00, 0) )

test01_11_20 <-  test01_11_20%>%
  mutate ( Ir_def_11_20 =  max ( ETc_11_20 - P_11_20, 0) )

test01_89_98 <-  test01_89_98%>%
  mutate ( Ir_def_89_98 =  max ( ETc_89_98 - P_89_98, 0) )

#combine all three summaries back into one row:
test01_11_20 <- test01_11_20[,6:12]
test01_89_98 <- test01_89_98[,6:12]
test01 <- cbind (test01_91_00, test01_11_20, test01_89_98)

#calculate percentage differences between start and end periods:
test01 <- test01 %>%
  mutate (
    P_change = ifelse (P_89_98==P_91_00, 0, ((( P_89_98 - P_91_00) / P_91_00 ) * 100) ),
    ETc_adj_change = ifelse (ETc_adj_89_98==ETc_adj_91_00, 0, 
                             ((( ETc_adj_89_98 - ETc_adj_91_00) / ETc_adj_91_00 ) * 100) ),
    Yield_change = ifelse (Y_a_89_98==Y_a_91_00, 0, 
                           ((( Y_a_89_98 - Y_a_91_00) / Y_a_91_00 ) * 100) ),
    Ir_def_change = ifelse (Ir_def_89_98==Ir_def_91_00, 0, 
                            ((( Ir_def_89_98 - Ir_def_91_00) / Ir_def_91_00 ) * 100) ),
  )

#delete other frames:
remove( test01_91_00, test01_11_20, test01_89_98)

#now "test01" is a single line.
#each instance of "test01" represents:
#changes in crop values between the baseline (1991-2000) and middle (2011-20) and end (2089-98) periods
#values are per crop, per district, and per RCP

#now each instance of "test01" needs to be added to a list:

crlist_default[[length(crlist_default) + 1]] <- test01

#print a completion message:
print ( (paste0("calculated averages for ", test01$rcp[1], ", district ", 
                test01$subdistrict[1], ", crop ", test01$cropname[1]) )  )

#test01 can now be removed:
remove (test01)

#sub-loop applying crop calcs to the climate data ends here.

}

#we turn crlist_default from a list into a dataframe:
crlist_default <-  do.call ( rbind.data.frame, crlist_default) 

#here we rename "crlist_default" based on the name of the crop in the loop
assign ( (paste0 ("crlist_", cropdat$cropcode[c], "_", cropdat$cropname[c] ) ), 
         crlist_default )

#print a completion message: 
print ( (paste0("list for ", cropdat$cropname[c], " completed.") )  )

#calculate the federal-level average per crop:
THR_crlist_default <- crlist_default %>%
  group_by(rcp, cropcode, cropname) %>%
  summarise(P_91_00 = mean(P_91_00, na.rm = TRUE),
            ETo_91_00 = mean (ETo_91_00, na.rm = TRUE),
            ETc_91_00 = mean (ETc_91_00, na.rm = TRUE),
            ETc_adj_91_00 = mean (ETc_adj_91_00, na.rm = TRUE),
            Y_x_91_00 = mean (Y_x_91_00, na.rm = TRUE),
            Y_a_91_00 = mean (Y_a_91_00, na.rm = TRUE),
            Ir_def_91_00 = mean (Ir_def_91_00, na.rm = TRUE),
            P_11_20 = mean(P_11_20, na.rm = TRUE),
            ETo_11_20 = mean (ETo_11_20, na.rm = TRUE),
            ETc_11_20 = mean (ETc_11_20, na.rm = TRUE),
            ETc_adj_11_20 = mean (ETc_adj_11_20, na.rm = TRUE),
            Y_x_11_20 = mean (Y_x_11_20, na.rm = TRUE),
            Y_a_11_20 = mean (Y_a_11_20, na.rm = TRUE),
            Ir_def_11_20 = mean (Ir_def_11_20, na.rm = TRUE),
            P_89_98 = mean(P_89_98, na.rm = TRUE),
            ETo_89_98 = mean (ETo_89_98, na.rm = TRUE),
            ETc_89_98 = mean (ETc_89_98, na.rm = TRUE),
            ETc_adj_89_98 = mean (ETc_adj_89_98, na.rm = TRUE),
            Y_x_89_98 = mean (Y_x_89_98, na.rm = TRUE),
            Y_a_89_98 = mean (Y_a_89_98, na.rm = TRUE),
            Ir_def_89_98 = mean (Ir_def_89_98, na.rm = TRUE),
            P_change = mean (P_change, na.rm = TRUE),
            ETc_adj_change = mean (ETc_adj_change, na.rm = TRUE),
            Yield_change = mean (Yield_change, na.rm = TRUE),
            Ir_def_change = mean (Ir_def_change, na.rm = TRUE)
  )%>%
  ungroup()

#give the generic state-level average dataframe a specific name:
assign ( (paste0("THR_crlist_", cropdat$cropname[c]) ) , THR_crlist_default)

#print a finishing message:
print ( (paste0("calculated state-level averages for ", cropdat$cropname[c]) )  )

#THR_crlist_default can then be removed:
remove (THR_crlist_default)

#big loop running the calculations for each crop ends here.

}

#remove default crlist dataframe
remove (crlist_default)

#we can produce an output list combining the CRlist of every crop:
crlist_allcrops <- lapply ( (ls ( pattern = "^crlist_")) , get )
crlist_allcrops <-  do.call ( rbind.data.frame, crlist_allcrops)


#summarise the crlist_allcrops dataframe:

THR_crlist_big_v02 <- crlist_allcrops %>%
  group_by(rcp, cropcode, cropname) %>%
  summarise(P_91_00 = mean(P_91_00, na.rm = TRUE),
            ETo_91_00 = mean (ETo_91_00, na.rm = TRUE),
            ETc_adj_91_00 = mean (ETc_adj_91_00, na.rm = TRUE),
            Y_a_91_00 = mean (Y_a_91_00, na.rm = TRUE),
            Ir_def_91_00 = mean (Ir_def_91_00, na.rm = TRUE),
            P_89_98 = mean(P_89_98, na.rm = TRUE),
            ETo_89_98 = mean (ETo_89_98, na.rm = TRUE),
            ETc_adj_89_98 = mean (ETc_adj_89_98, na.rm = TRUE),
            Y_a_89_98 = mean (Y_a_89_98, na.rm = TRUE),
            Ir_def_89_98 = mean (Ir_def_89_98, na.rm = TRUE),
            P_change = mean (P_change, na.rm = TRUE),
            ETc_adj_change = mean (ETc_adj_change, na.rm = TRUE),
            Yield_change = mean (Yield_change, na.rm = TRUE),
            Ir_def_change = mean (Ir_def_change, na.rm = TRUE),
            range_P_start = sd (P_91_00, na.rm = TRUE),
            range_ETo_start = sd (ETo_91_00, na.rm = TRUE),
            range_ETc_start = sd (ETc_adj_91_00, na.rm = TRUE),
            range_Y_start = sd (Y_a_91_00, na.rm = TRUE),
            range_Irdef_start = sd (Ir_def_91_00, na.rm = TRUE),
            range_P_end = sd (P_89_98, na.rm = TRUE),
            range_ETo_end = sd (ETo_89_98, na.rm = TRUE),
            range_ETc_end = sd (ETc_adj_89_98, na.rm = TRUE),
            range_Y_end = sd (Y_a_89_98, na.rm = TRUE),
            range_Irdef_end = sd (Ir_def_89_98, na.rm = TRUE)
  )%>%
  ungroup()



#now we can also combine all of the THR-level lists into one:

THR_crlist_allcrops <- lapply ( (ls ( pattern = "^THR_crlist_")) , get )
THR_crlist_allcrops <-  do.call ( rbind.data.frame, THR_crlist_allcrops)



THR_crlist_allcrops_RCP <- THR_crlist_allcrops%>% 
  group_split (rcp)

THR_crlist_allcrops_RCP2p6 <- THR_crlist_allcrops_RCP[[1]]
THR_crlist_allcrops_RCP6p0 <- THR_crlist_allcrops_RCP[[2]]
THR_crlist_allcrops_RCP8p5 <- THR_crlist_allcrops_RCP[[3]]

THR_crlist_allcrops_fin <- subset( THR_crlist_allcrops, select =c(rcp, cropcode, 
                           cropname, P_91_00, ETc_adj_91_00, Y_a_91_00, Ir_def_91_00, 
                           P_89_98, ETc_adj_89_98, Y_a_89_98, Ir_def_89_98, P_change, 
                           ETc_adj_change, Yield_change, Ir_def_change))

#this is all the outputs:
#averaged values across the state (THR_avg_allcrops...)
#state-level values for each crop and each RCP scenario (list_01 - list_14)
#each table contains:
#ETo - evapotranspiration rate (mm per ha / yearly) of a reference crop
#ETc & ETc_adj - evapotranspiration crop (mm/ha yearly) of the actual crop, 
#in ideal (no water stress) and actual conditions
#Y_x & Y_a - ideal (no water stress) and actual yields, in Mg/ha
#P - precipitation (mm/year/ha)






