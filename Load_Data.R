#POXC metaanalysis based on 2.2024 POXC file
#Authors: Cécile Chéron-Bessou, Damien Beillouin, Alexis Thoumazeau, Lydie Chapuis-Lardy, Tiphaine Chevallier, Julien Demenois, Paul N Nelson

############################
########### 1. Load DATA
############################


#libraries in Utes but some paste here for Damien

#libraries
library(tidyverse)   #wrangling with data
library(tidyr)       #clean data part of tidyverse?
library(tibble)
library(lubridate)   #for dates
library(ggplot2)     #visualise data
library(cowplot)
library(ggpubr)
library(hrbrthemes)
library(viridis)     #to have nice colours and robust to colourblindness
library(extrafont)
library(ragg)
library(janitor)
library(forcats)
library(dplyr)       #to select columns
library(stringr) #to modify character strings in columns
library(ggtext)
library(writexl) #to export dataframe to Excel
library(openxlsx)

library(dmetar)   #for metanalysis by Mathias Harrer
library(metafor)
library(metadat)
library(esc)

#Cecile
setwd(dir="D:/Mes Donnees/A_MOBILITY/JCU_2022-2024/SAAFE/DATA/POXC/METAANALYSIS")

# Damien
setwd("~/Documents/POXC")

#1. Data----
##1.1 POXC points for the mapping----

POXC_map <-file.choose()
POXC_map <-read.csv(POXC_map, h=T, stringsAsFactors=FALSE, fileEncoding="latin1")
POXC_map <- separate(POXC_map, Location, c("Country", "Loc_details"), "/")


##1.2 POXC tibble----
#3957 obs of 42 variables
#Data

POXC_meta <-file.choose()
POXC_meta <-read.csv(POXC_meta,h=T, stringsAsFactors = FALSE)
POXC_meta <- POXC_meta[,-c(2,36:42)]

POXC_metaC <- POXC_meta %>%
#filter(!POXC_ID %in% c('626_2','626_3','626_4', '626_12'))

glimpse(POXC_meta)
summary(POXC_meta,maxsum=50)

stringsAsFactors = FALSE  #to avoid pb with NA introduced by coercion
POXC_meta  <- POXC_meta  %>%
  mutate(Treat_Rep = as.numeric(Treat_Rep)) %>%
  mutate(Control_Rep = as.numeric(Control_Rep)) %>%
  mutate(Length_Years = as.numeric(Length_Years)) %>%
  mutate(POXC_Mean_T  = as.numeric(POXC_Mean_T)) %>%
  mutate(POXC_SE_T  = as.numeric(POXC_SE_T)) %>%
  mutate(POXC_SD_T  = as.numeric(POXC_SD_T)) %>%
  mutate(POXC_Mean_C = as.numeric(POXC_Mean_C)) %>%
  mutate(POXC_SE_C = as.numeric(POXC_SE_C)) %>%
  mutate(POXC_SD_C = as.numeric(POXC_SD_C)) %>%
    mutate(Length_Years = as.factor(Length_Years)) %>%
  mutate(Cat_Intervention  = as.factor(Cat_Intervention )) %>%
  mutate(Irrigation = as.factor(Irrigation)) %>%
  mutate(Pseudo_Rep = as.factor(Pseudo_Rep)) %>%
  mutate(Diachronic = as.factor(Diachronic)) %>%
      mutate(Depth = if_else(Depth == "Oct-20", "10-20", Depth)) %>%
      mutate(Depth = if_else(Depth == "5-Oct", "5-10", Depth)) %>%
      mutate(Depth = if_else(Depth == "May-15", "5-15", Depth)) %>%
      mutate(Depth = if_else(Depth == "Oct-15", "10-15", Depth)) %>%
      mutate(Depth = if_else(Depth == "Oct-25", "10-25", Depth)) %>%
      mutate(Depth = if_else(Depth == "Oct-25", "10-25", Depth)) %>%
  mutate(Depth = as.factor(Depth))

#POXC <- POXC[rowSums(is.na(POXC)) !=ncol(POXC),]

POXC_meta <- POXC_meta %>%
  mutate(POXC_SD_T1 = case_when(is.na(POXC_SD_T)~POXC_SE_T*sqrt(Treat_Rep),
                        TRUE ~POXC_SD_T),
         .after="POXC_SD_T")

POXC_meta <- POXC_meta %>%
  mutate(POXC_SD_C1 = case_when(is.na(POXC_SD_C)~POXC_SE_C*sqrt(Control_Rep),
                                TRUE ~POXC_SD_C),
         .after="POXC_SD_C")

##1.2 POXC1 = without missing SD and strange Mean values---- OLD version, there should not be too many erroneous values now + lnRR would handle NAs
#POXC1 <- POXC %>%
 # filter(!Replicate=="NA") %>%
 # filter(!POXC_Mean_T %in% c(0, NA)) %>%
 # filter(!POXC_Mean_C %in% c(0, NA)) %>%
 # filter(!POXC_Mean_T>30) %>%
 # filter(!POXC_Mean_C>30) %>%
 # filter(!POXC_SD_T1=="NA") %>%
 # filter(!POXC_SD_C1=="NA")


#2. Effect-sizes with log transformed ratio of means----

## ln(RR) + yi (effec size) and vi (sampling variance) addition to POXC_meta

lnRR <- escalc(measure = "ROM", n1i = POXC_meta$Treat_Rep, n2i = POXC_meta$Control_Rep, m1i = POXC_meta$POXC_Mean_T,
               m2i = POXC_meta$POXC_Mean_C, sd1i = POXC_meta$POXC_SD_T1, sd2i = POXC_meta$POXC_SD_C1)

POXC_meta <- cbind(POXC_meta, data.frame(lnRR))



##2.1 Sub-samples by intervention and soil depth-----


POXC_meta_20 <- POXC_meta %>%         #the most encountered depth is 0-15cm, we agreed with Paul that it could be grouped together with 0-10 7 0-20cm considering +/-5cm error margin
   filter(Depth %in% c("0-10", "0-15", "0-20"))

POXC_NOtoTill <- POXC_meta_20 %>%
  filter(Cat_Intervention == "arable land, no tillage to arable land, tillage") #as an example

forest(POXC_NOtoTill$yi, POXC_NOtoTill$vi)

#3. Mixed effect model----
##3.1

random_m <- rma(yi = yi, vi = vi, method = "REML", data = POXC_meta)
summary(random_m)
forest(random_m)
