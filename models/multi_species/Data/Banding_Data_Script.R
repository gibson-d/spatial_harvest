# Banding Data
# The raw banding and recovery data were downloaded from the BBL's internet data portal (data files queried: NABBP_2022_grp_04.csv, NABBP_2022_grp_05.csv,NABBP_2022_grp_06.csv, NABBP_2022_grp_07.csv,NABBP_2022_grp_08.csv, and NABBP_2022_grp_09.csv) 
# They are not provided here due to the size of hosting the files.
# Banding Data Cleaning R Script:

rm(list=ls())
library(nimble)
require(sp)
library(RColorBrewer)
library(maps)
library(mapproj)
library(raster)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

#-------------------------------------------------------------------------------
# Data manipulation of banding data 
#------------------------------------------------------------------------------
# Read in dataset
# 1320 	MALL
# 1350 	GADW
# 1370 	AMWI
# 1390 	AGWT
# 1400 	BWTE
# 1430 	NOPI

sp_id <- c(1,2,2,2,2,2,3)

species_list <- c(1320,1350,1370,1390,1400,1430)

expand.grid(sex = c('F','M'),age = c('HY','AHY'),sp = species_list )
 # read in raw data
raw.band1<-read.csv("NABBP_2022_grp_04.csv")  #reading in CSV
raw.band2<-read.csv("NABBP_2022_grp_05.csv")  #reading in CSV
raw.band3<-read.csv("NABBP_2022_grp_06.csv")  #reading in CSV
raw.band4<-read.csv("NABBP_2022_grp_07.csv")  #reading in CSV
raw.band5<-read.csv("NABBP_2022_grp_08.csv")  #reading in CSV
raw.band6<-read.csv("NABBP_2022_grp_09.csv")  #reading in CSV

raw.band <- rbind.data.frame(raw.band1,raw.band2,raw.band3,raw.band4,raw.band5,raw.band6)

dabblers <- subset(raw.band, SPECIES_ID%in%species_list )
releases <- subset(dabblers, EVENT_TYPE == 'B' & BIRD_STATUS == 3)

extra_excl <- c('01','02','09','1','10','15','2','25','29','34','40','69','71','80','81','85','87','88','89','9')
canada <- c('CA-AB','CA-MB','CA-SK')
usa    <- c('US-MT','US-ND','US-SD','US-MN')

# n = 2574780      
releases_can <- subset(releases, ISO_SUBDIVISION%in%canada & EVENT_MONTH >= 6 & EVENT_MONTH <= 9 & AGE_CODE != 0 & SEX_CODE >= 4 & SEX_CODE <= 5 & !(EXTRA_INFO %in% extra_excl))
# n = 1300097      
releases_usa <- subset(releases, ISO_SUBDIVISION%in%usa & EVENT_MONTH >= 6 & EVENT_MONTH <= 9 & AGE_CODE != 0 & SEX_CODE >= 4 & SEX_CODE <= 5 & !(EXTRA_INFO %in% extra_excl))

RC <- c('03','04')
# n = 125201     
rC_can  <- subset(releases, ISO_SUBDIVISION%in%canada & EVENT_MONTH >= 6 & EVENT_MONTH <= 9 & AGE_CODE != 0 & SEX_CODE >= 4 & SEX_CODE <= 5 & EXTRA_INFO %in% RC)
# n = 24805 
rC_usa  <- subset(releases, ISO_SUBDIVISION%in%usa & EVENT_MONTH >= 6 & EVENT_MONTH <= 9 & AGE_CODE != 0 & SEX_CODE >= 4 & SEX_CODE <= 5 & EXTRA_INFO %in% RC)

# n = 288824
recoveries_can <- subset(dabblers, ORIGINAL_BAND %in% releases_can$ORIGINAL_BAND & HOW_OBTAINED == 1)
# n = 156738
recoveries_usa <- subset(dabblers, ORIGINAL_BAND %in% releases_usa$ORIGINAL_BAND & HOW_OBTAINED == 1)
# n = 27417
recoveries_rc_can <- subset(dabblers, ORIGINAL_BAND %in% rC_can$ORIGINAL_BAND & HOW_OBTAINED == 1)
# n = 5047
recoveries_rc_usa <- subset(dabblers, ORIGINAL_BAND %in% rC_usa$ORIGINAL_BAND & HOW_OBTAINED == 1)

#######################################################################################################################
releases_can$Age_Factor <- ifelse(releases_can$AGE_CODE == 2 | releases_can$AGE_CODE == 4, 1,2)
releases_usa$Age_Factor <- ifelse(releases_usa$AGE_CODE == 2 | releases_usa$AGE_CODE == 4, 1,2)
rC_can$Age_Factor <- ifelse(rC_can$AGE_CODE == 2 | rC_can$AGE_CODE == 4, 1,2)
rC_usa$Age_Factor <- ifelse(rC_usa$AGE_CODE == 2 | rC_usa$AGE_CODE == 4, 1,2)


#######################################################################################################################
recov_can <- recoveries_can[,c('ORIGINAL_BAND','EVENT_DATE','EVENT_MONTH', 'EVENT_YEAR', 'ISO_COUNTRY', 'ISO_SUBDIVISION', 'LAT_DD' ,'LON_DD','MIN_AGE_AT_ENC')]
colnames(recov_can) <- c('ORIGINAL_BAND','HARV_DATE','HARV_MONTH', 'HARV_YEAR', 'HARV_COUNTRY', 'HARV_SUBDIVISION', 'HARV_LAT_DD' ,'HARV_LON_DD','MIN_HARV_AGE')

REL_REC_CAN <- left_join(releases_can,recov_can, multiple = 'all')

REL_REC_CAN$HARV_MONTH_EDIT <- ifelse(REL_REC_CAN$HARV_MONTH <= 12, REL_REC_CAN$HARV_MONTH,
                                                           ifelse(REL_REC_CAN$HARV_MONTH == 93, 11,
                                                                  ifelse(REL_REC_CAN$HARV_MONTH == 94, 11, 99)))

REL_REC_CAN <- subset(REL_REC_CAN, HARV_MONTH_EDIT <= 3 | HARV_MONTH_EDIT >= 9 & HARV_MONTH_EDIT <= 12)
REL_REC_CAN$HARV_YEAR_EDIT <- ifelse(REL_REC_CAN$HARV_MONTH_EDIT <= 4,REL_REC_CAN$HARV_YEAR-1, REL_REC_CAN$HARV_YEAR)

REL_REC_CAN_Direct <- subset(REL_REC_CAN, REL_REC_CAN$HARV_YEAR_EDIT - REL_REC_CAN$EVENT_YEAR == 0)
REL_REC_CAN_Direct <- subset(REL_REC_CAN_Direct, HARV_COUNTRY == 'CA' | HARV_COUNTRY == 'US' | HARV_COUNTRY == 'MX')

REL_REC_CAN_Direct$HARV_SUBDIVISION <- ifelse(REL_REC_CAN_Direct$HARV_COUNTRY == 'MX', 'MX', REL_REC_CAN_Direct$HARV_SUBDIVISION )

retain <- c( 'CA-AB', 'CA-BC', 'CA-MB','CA-ON','CA-SK','MX','US-AL', 'US-AR', 'US-AZ', 'US-CA', 'US-CO', 'US-FL', 'US-IA', 'US-ID', 'US-IL', 'US-IN', 'US-KS', 'US-KY', 'US-LA',
             'US-MI', 'US-MN', 'US-MO', 'US-MS', 'US-MT', 'US-ND', 'US-NE', 'US-NM', 'US-NV', 'US-OH', 'US-OK', 'US-OR', 'US-SD', 'US-TN', 'US-TX', 'US-UT', 'US-WA', 'US-WI', 'US-WY')

REL_REC_CAN_RETAIN <- subset(REL_REC_CAN_Direct, HARV_SUBDIVISION %in% retain)

#######################################################################################################################
recov_usa <- recoveries_usa[,c('ORIGINAL_BAND','EVENT_DATE','EVENT_MONTH', 'EVENT_YEAR', 'ISO_COUNTRY', 'ISO_SUBDIVISION', 'LAT_DD' ,'LON_DD','MIN_AGE_AT_ENC')]
colnames(recov_usa) <- c('ORIGINAL_BAND','HARV_DATE','HARV_MONTH', 'HARV_YEAR', 'HARV_COUNTRY', 'HARV_SUBDIVISION', 'HARV_LAT_DD' ,'HARV_LON_DD','MIN_HARV_AGE')

REL_REC_USA <- left_join(releases_usa,recov_usa, multiple = 'all')

REL_REC_USA$HARV_MONTH_EDIT <- ifelse(REL_REC_USA$HARV_MONTH <= 12, REL_REC_USA$HARV_MONTH,
                                      ifelse(REL_REC_USA$HARV_MONTH == 93, 11,
                                             ifelse(REL_REC_USA$HARV_MONTH == 94, 11, 99)))

REL_REC_USA <- subset(REL_REC_USA, HARV_MONTH_EDIT <= 3 | HARV_MONTH_EDIT >= 9 & HARV_MONTH_EDIT <= 12)
REL_REC_USA$HARV_YEAR_EDIT <- ifelse(REL_REC_USA$HARV_MONTH_EDIT <= 4,REL_REC_USA$HARV_YEAR-1, REL_REC_USA$HARV_YEAR)

REL_REC_USA_Direct <- subset(REL_REC_USA, REL_REC_USA$HARV_YEAR_EDIT - REL_REC_USA$EVENT_YEAR == 0)
REL_REC_USA_Direct <- subset(REL_REC_USA_Direct, HARV_COUNTRY == 'CA' | HARV_COUNTRY == 'US' | HARV_COUNTRY == 'MX')

REL_REC_USA_Direct$HARV_SUBDIVISION <- ifelse(REL_REC_USA_Direct$HARV_COUNTRY == 'MX', 'MX', REL_REC_USA_Direct$HARV_SUBDIVISION )

retain <- c( 'CA-AB', 'CA-BC', 'CA-MB','CA-ON','CA-SK','MX','US-AL', 'US-AR', 'US-AZ', 'US-CA', 'US-CO', 'US-FL', 'US-IA', 'US-ID', 'US-IL', 'US-IN', 'US-KS', 'US-KY', 'US-LA',
             'US-MI', 'US-MN', 'US-MO', 'US-MS', 'US-MT', 'US-ND', 'US-NE', 'US-NM', 'US-NV', 'US-OH', 'US-OK', 'US-OR', 'US-SD', 'US-TN', 'US-TX', 'US-UT', 'US-WA', 'US-WI', 'US-WY')

REL_REC_USA_RETAIN <- subset(REL_REC_USA_Direct, HARV_SUBDIVISION %in% retain)
#############################################################################################################

recov_rc_can <- recoveries_rc_can[,c('ORIGINAL_BAND','EVENT_DATE','EVENT_MONTH', 'EVENT_YEAR', 'ISO_COUNTRY', 'ISO_SUBDIVISION', 'LAT_DD' ,'LON_DD','MIN_AGE_AT_ENC')]
colnames(recov_rc_can) <- c('ORIGINAL_BAND','HARV_DATE','HARV_MONTH', 'HARV_YEAR', 'HARV_COUNTRY', 'HARV_SUBDIVISION', 'HARV_LAT_DD' ,'HARV_LON_DD','MIN_HARV_AGE')

REL_REC_CAN_rC <- left_join(rC_can,recov_rc_can, multiple = 'all')

REL_REC_CAN_rC$HARV_MONTH_EDIT <- ifelse(REL_REC_CAN_rC$HARV_MONTH <= 12, REL_REC_CAN_rC$HARV_MONTH,
                                      ifelse(REL_REC_CAN_rC$HARV_MONTH == 93, 11,
                                             ifelse(REL_REC_CAN_rC$HARV_MONTH == 94, 11, 99)))

REL_REC_CAN_rC <- subset(REL_REC_CAN_rC, HARV_MONTH_EDIT <= 3 | HARV_MONTH_EDIT >= 9 & HARV_MONTH_EDIT <= 12)
REL_REC_CAN_rC$HARV_YEAR_EDIT <- ifelse(REL_REC_CAN_rC$HARV_MONTH_EDIT <= 4,REL_REC_CAN_rC$HARV_YEAR-1, REL_REC_CAN_rC$HARV_YEAR)

REL_REC_CAN_rC_Direct <- subset(REL_REC_CAN_rC, REL_REC_CAN_rC$HARV_YEAR_EDIT - REL_REC_CAN_rC$EVENT_YEAR == 0)
REL_REC_CAN_rC_Direct <- subset(REL_REC_CAN_rC_Direct, HARV_COUNTRY == 'CA' | HARV_COUNTRY == 'US' | HARV_COUNTRY == 'MX')

REL_REC_CAN_rC_Direct$HARV_SUBDIVISION <- ifelse(REL_REC_CAN_rC_Direct$HARV_COUNTRY == 'MX', 'MX', REL_REC_CAN_rC_Direct$HARV_SUBDIVISION )

retain <- c( 'CA-AB', 'CA-BC', 'CA-MB','CA-ON','CA-SK','MX','US-AL', 'US-AR', 'US-AZ', 'US-CA', 'US-CO', 'US-FL', 'US-IA', 'US-ID', 'US-IL', 'US-IN', 'US-KS', 'US-KY', 'US-LA',
             'US-MI', 'US-MN', 'US-MO', 'US-MS', 'US-MT', 'US-ND', 'US-NE', 'US-NM', 'US-NV', 'US-OH', 'US-OK', 'US-OR', 'US-SD', 'US-TN', 'US-TX', 'US-UT', 'US-WA', 'US-WI', 'US-WY')

REL_REC_CAN_rC_RETAIN <- subset(REL_REC_CAN_rC_Direct, HARV_SUBDIVISION %in% retain)

#############################################################################################################

recov_rc_usa <- recoveries_rc_usa[,c('ORIGINAL_BAND','EVENT_DATE','EVENT_MONTH', 'EVENT_YEAR', 'ISO_COUNTRY', 'ISO_SUBDIVISION', 'LAT_DD' ,'LON_DD','MIN_AGE_AT_ENC')]
colnames(recov_rc_usa) <- c('ORIGINAL_BAND','HARV_DATE','HARV_MONTH', 'HARV_YEAR', 'HARV_COUNTRY', 'HARV_SUBDIVISION', 'HARV_LAT_DD' ,'HARV_LON_DD','MIN_HARV_AGE')

REL_REC_USA_rC <- left_join(rC_usa,recov_rc_usa, multiple = 'all')

REL_REC_USA_rC$HARV_MONTH_EDIT <- ifelse(REL_REC_USA_rC$HARV_MONTH <= 12, REL_REC_USA_rC$HARV_MONTH,
                                         ifelse(REL_REC_USA_rC$HARV_MONTH == 93, 11,
                                                ifelse(REL_REC_USA_rC$HARV_MONTH == 94, 11, 99)))

REL_REC_USA_rC <- subset(REL_REC_USA_rC, HARV_MONTH_EDIT <= 3 | HARV_MONTH_EDIT >= 9 & HARV_MONTH_EDIT <= 12)
REL_REC_USA_rC$HARV_YEAR_EDIT <- ifelse(REL_REC_USA_rC$HARV_MONTH_EDIT <= 4,REL_REC_USA_rC$HARV_YEAR-1, REL_REC_USA_rC$HARV_YEAR)

REL_REC_USA_rC_Direct <- subset(REL_REC_USA_rC, REL_REC_USA_rC$HARV_YEAR_EDIT - REL_REC_USA_rC$EVENT_YEAR == 0)
REL_REC_USA_rC_Direct <- subset(REL_REC_USA_rC_Direct, HARV_COUNTRY == 'CA' | HARV_COUNTRY == 'US' | HARV_COUNTRY == 'MX')

REL_REC_USA_rC_Direct$HARV_SUBDIVISION <- ifelse(REL_REC_USA_rC_Direct$HARV_COUNTRY == 'MX', 'MX', REL_REC_USA_rC_Direct$HARV_SUBDIVISION )

retain <- c( 'CA-AB', 'CA-BC', 'CA-MB','CA-ON','CA-SK','MX','US-AL', 'US-AR', 'US-AZ', 'US-CA', 'US-CO', 'US-FL', 'US-IA', 'US-ID', 'US-IL', 'US-IN', 'US-KS', 'US-KY', 'US-LA',
             'US-MI', 'US-MN', 'US-MO', 'US-MS', 'US-MT', 'US-ND', 'US-NE', 'US-NM', 'US-NV', 'US-OH', 'US-OK', 'US-OR', 'US-SD', 'US-TN', 'US-TX', 'US-UT', 'US-WA', 'US-WI', 'US-WY')

REL_REC_USA_rC_RETAIN <- subset(REL_REC_USA_rC_Direct, HARV_SUBDIVISION %in% retain)
#######################################################################################################################

rel_can <- releases_can %>% group_by(SPECIES_ID, Age_Factor,  SEX_CODE,EVENT_YEAR,ISO_SUBDIVISION) %>% tally()
dir_can <- REL_REC_CAN_RETAIN %>% group_by(SPECIES_ID, Age_Factor,  SEX_CODE,HARV_YEAR_EDIT,ISO_SUBDIVISION, HARV_SUBDIVISION) %>% tally() %>% pivot_wider(names_from = HARV_SUBDIVISION,values_from = n     )

names(dir_can)[names(dir_can) == 'HARV_YEAR_EDIT'] <- 'EVENT_YEAR'

rel_dir_can <- left_join(rel_can,dir_can)

rel_usa <- releases_usa %>% group_by(SPECIES_ID, Age_Factor,  SEX_CODE,EVENT_YEAR,ISO_SUBDIVISION) %>% tally()
dir_usa <- REL_REC_USA_RETAIN %>% group_by(SPECIES_ID, Age_Factor,  SEX_CODE,HARV_YEAR_EDIT,ISO_SUBDIVISION, HARV_SUBDIVISION) %>% tally() %>% pivot_wider(names_from = HARV_SUBDIVISION,values_from = n     )

names(dir_usa)[names(dir_usa) == 'HARV_YEAR_EDIT'] <- 'EVENT_YEAR'

rel_dir_usa <- left_join(rel_usa,dir_usa)


rel_can_rc <- rC_can %>% group_by(SPECIES_ID,EXTRA_INFO, Age_Factor,  SEX_CODE,EVENT_YEAR,ISO_SUBDIVISION) %>% tally()
dir_can_rc <- REL_REC_CAN_rC_RETAIN %>% group_by(SPECIES_ID,EXTRA_INFO , Age_Factor,  SEX_CODE,HARV_YEAR_EDIT,ISO_SUBDIVISION, HARV_SUBDIVISION) %>% tally() %>% pivot_wider(names_from = HARV_SUBDIVISION,values_from = n     )

names(dir_can_rc)[names(dir_can_rc) == 'HARV_YEAR_EDIT'] <- 'EVENT_YEAR'

rel_dir_can_rc <- left_join(rel_can_rc,dir_can_rc)

rel_usa_rc <- rC_usa %>% group_by(SPECIES_ID,EXTRA_INFO, Age_Factor,  SEX_CODE,EVENT_YEAR,ISO_SUBDIVISION) %>% tally()
dir_usa_rc <- REL_REC_USA_rC_RETAIN %>% group_by(SPECIES_ID,EXTRA_INFO , Age_Factor,  SEX_CODE,HARV_YEAR_EDIT,ISO_SUBDIVISION, HARV_SUBDIVISION) %>% tally() %>% pivot_wider(names_from = HARV_SUBDIVISION,values_from = n     )

names(dir_usa_rc)[names(dir_usa_rc) == 'HARV_YEAR_EDIT'] <- 'EVENT_YEAR'

rel_dir_usa_rc <- left_join(rel_usa_rc,dir_usa_rc)

rel_total <- dplyr::bind_rows(rel_dir_can,rel_dir_usa,rel_dir_can_rc,rel_dir_usa_rc)

rel_total_rc <- subset(rel_total, is.na(EXTRA_INFO) == FALSE)
rel_total <- subset(rel_total, is.na(EXTRA_INFO) == TRUE)

rel_total$EXTRA_INFO <- NULL
rel_total[is.na(rel_total)] <- 0
rel_total$not_rec <- rel_total$n - rowSums(rel_total[,-c(1:6)])

rel_total_rc[is.na(rel_total_rc)] <- 0
rel_total_rc$not_rec <- rel_total_rc$n - rowSums(rel_total_rc[,-c(1:6,45)])

#####################################################################################
# Final Decision to retain species
# 1320 	MALL
# 1350 	GADW
# 1370 	AMWI
# 1390 	AGWT
# 1400 	BWTE
# 1430 	NOPI

#specKeep <- c(1320,1350,1370,1390,1400,1430) # 6 species model
specKeep <- c(1320,1400,1430)                 # 3 species model
nsp <- length(specKeep)

rel_total <- subset(rel_total, SPECIES_ID %in% specKeep)

# Multispecies m-array
marr <- rel_total[,-c(1:6)]
rel  <- rel_total$n

# Multispecies constants
species  <- as.numeric(as.factor(rel_total$SPECIES_ID))
age      <- as.numeric(as.factor(rel_total$Age_Factor))
sex      <- as.numeric(as.factor(rel_total$SEX_CODE))
year     <- as.numeric(as.factor(rel_total$EVENT_YEAR))
releases   <- as.numeric(as.factor(rel_total$ISO_SUBDIVISION))

type <- paste(species,age ,sex)
groups <- expand.grid( sex = 1:2, age = 1:2,species = 1:nsp)

# control-reward band m-array

marr_rc <- rel_total_rc[,-c(1:6,45)]
rel_rc <- rel_total_rc$n

#Control-Reward constants
species_rc    <- rep(1, length = nrow(rel_total_rc))
age_rc        <- as.numeric(as.factor(rel_total_rc$Age_Factor))
sex_rc        <- as.numeric(as.factor(rel_total_rc$SEX_CODE)) 
year_rc       <- rel_total_rc$EVENT_YEAR - 1959
releases_rc   <-  as.numeric(as.factor(rel_total_rc$ISO_SUBDIVISION))
rc            <- as.numeric(as.factor(rel_total_rc$EXTRA_INFO)) # 1 reward, 2 control

type_rc <- paste(age_rc ,sex_rc)
groups_rc <- expand.grid( sex = 1:2, age = 1:2)

