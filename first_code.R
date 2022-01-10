#Microdata analysis PNAD - using package microdadosBrasil ----------------------
#Decided to use this package after problem with the 2001 widths - not showing properly weights (see file analysis_microdata)

rm(list=ls(all=T))
#install.packages("devtools")
#install.packages("stringi") 
#devtools::install_github("lucasmation/microdadosBrasil")
library(microdadosBrasil)
library(survey)
library(tidyverse)
library(ggplot2)
library(stringr)
library(purrr)
library(dplyr)

#creates vector year
##years = c()
##for(i in 2001:2015) { 
##  years = c(years,i)
##}
##print(years) # check the vector

#modifies function so it continues the iteration even with an error
##download_sourceData2 <- possibly(download_sourceData, otherwise = NA)
#uses lapply with the new function to download the data
##lapply(years, download_sourceData2, dataset = "PNAD")
#this method for downloading the data has several errors and makes the directory chaotic,
#a better possibility would be organizing it and then importing setting the file argument

#PNAD2001 ----------------------------------------------------------------------

#download_sourceData("PNAD", 2001, unzip = T) ##usado para fazer o download dos arquivos, errático - 
#coloquei manualmente seguindo a estrutura de arquivos criada

#reading file PES, var of interest - controle/serie + migration + some socioeconomic indicators for PES
PES2001_subset <- read_PNAD("pessoas", 2001, file = "./Data/PES2001.txt", vars_subset = c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                             "V0504", "V0505", "V5005", "V5505", "V5061", "V5062", "V5063", "V5064",
                                                             "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                             "V5123", "V5124", "V5125", "V0513", "V9001", "V4729", "V4732")) 

#reading file DOM with variables for merging later for svydesign
DOM2001_subset <- read_PNAD("domicilios", file = "./Data/DOM2001.txt", 2001, vars_subset = c("V0102", "V0103", "STRAT", "PSU"))

pnadmig_2001 <- merge(PES2001_subset, DOM2001_subset,
                      by = c("V0102", "V0103")) #merging by control and series


pnadmig_2001 <- pnadmig_2001 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2001) #add year variable, V0101


#useless code before 
#DOM2001_mig <- DOM2001_subset %>%
#                mutate(UF = str_extract(V0102, "^\\d{2}")) %>%
#                mutate(year = 2001)
#creating final migration df for analysis               
#PNADmig_2001 <- merge(PES2001_mig, DOM2001_mig,
#                      by = c("year", "V0102", "V0103")) #merging by year, control and series
#identical(PES2001_mig[['UF.x']],PES2001_mig[['UF.y']]) #since it created two UFs columns (UF.x and UF.y), checking if they are identical
#identical, so removing extra column
#PNADmig_2001 <- PNADmig_2001[,-"UF.x"] #removing extra UF column
#PNADmig_2001 <- rename(PNADmig_2001, UF = UF.y) #renaming column UF.y


#PNAD2002 ----------------------------------------------------------------------

##download_sourceData("PNAD", 2002, unzip = T)

#compared to 2001, removed V5505, V5505 and V0513
PES2002_subset <- read_PNAD("pessoas", 2002, file = "./Data/PES2002.txt", vars_subset = c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                             "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                             "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                             "V5123", "V5124", "V5125", "V9001", "V4729", "V4732")) 

#STRAT became V4617 and UPA became V4618
DOM2002_subset <- read_PNAD("domicilios", file = "./Data/DOM2002.txt", 2002, vars_subset = c("V0102", "V0103", "V4617", "V4618"))


pnadmig_2002 <- merge(PES2002_subset, DOM2002_subset,
                      by = c("V0102", "V0103")) #merging by control and series


pnadmig_2002 <- pnadmig_2002 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2002) #add year variable, V0101

#PNAD2003 ----------------------------------------------------------------------

#download_sourceData("PNAD", 2003, unzip = T)
#compared to 2002, no changes
PES2003_subset <- read_PNAD("pessoas", 2003, file = "./Data/PES2003.txt", vars_subset = c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                             "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                             "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                             "V5123", "V5124", "V5125", "V9001", "V4729", "V4732")) #file 'pessoas'

#compared to 2002, no changes                            
DOM2003_subset <- read_PNAD("domicilios", 2003, file = "./Data/DOM2003.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) #file 'domicilios'

pnadmig_2003 <- merge(PES2003_subset, DOM2003_subset,
                      by = c("V0102", "V0103")) #merging by control and series


pnadmig_2003 <- pnadmig_2003 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2003) #add year variable, V0101

#PNAD2004 ----------------------------------------------------------------------

##download_sourceData("PNAD", 2004, unzip = F)
#compared to 2003, no changes
PES2004_subset <- read_PNAD("pessoas", 2004, file = "./Data/PES2004.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))
#compared to 2003, no changes
DOM2004_subset <- read_PNAD("domicilios", 2004, file = "./Data/DOM2004.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2004 <- merge(PES2004_subset, DOM2004_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2004 <- pnadmig_2004 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2004) #add year variable, V0101

#PNAD2005 ----------------------------------------------------------------------

##download_sourceData("PNAD", 2005, unzip = T)

#compared to 2004, no changes
PES2005_subset <- read_PNAD("pessoas", 2005, file = "./Data/PES2005.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2005_subset <- read_PNAD("domicilios", 2005, file = "./Data/DOM2005.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2005 <- merge(PES2005_subset, DOM2005_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2005 <- pnadmig_2005 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2005) #add year variable, V0101

#PNAD2006 ----------------------------------------------------------------------

##download_sourceData("PNAD", 2006, unzip = T)

#compared to 2005, no changes
PES2006_subset <- read_PNAD("pessoas", 2006, file = "./Data/PES2006.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2005, no changes
DOM2006_subset <- read_PNAD("domicilios", 2006, file = "./Data/DOM2006.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2006 <- merge(PES2006_subset, DOM2006_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2006 <- pnadmig_2006 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2006) #add year variable, V0101

#PNAD2007 ----------------------------------------------------------------------
###CHECK VARIABLES FROM HERE

##download_sourceData("PNAD", 2007, unzip = T)

#compared to 2005, no changes
PES2007_subset <- read_PNAD("pessoas", 2007, file = "./Data/PES2007.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2007_subset <- read_PNAD("domicilios", 2007, file = "./Data/DOM2007.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2007 <- merge(PES2007_subset, DOM2007_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2007 <- pnadmig_2007 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2007) #add year variable, V0101

#PNAD2008 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2008, unzip = T)

#compared to 2005, no changes
PES2008_subset <- read_PNAD("pessoas", 2008, file = "./Data/PES2008.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2008_subset <- read_PNAD("domicilios", 2008, file = "./Data/DOM2008.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2008 <- merge(PES2008_subset, DOM2008_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2008 <- pnadmig_2008 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2008) #add year variable, V0101

#PNAD2009 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2009, unzip = T)

#compared to 2005, no changes
PES2009_subset <- read_PNAD("pessoas", 2009, file = "./Data/PES2009.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2009_subset <- read_PNAD("domicilios", 2009, file = "./Data/DOM2009.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2009 <- merge(PES2009_subset, DOM2009_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2009 <- pnadmig_2009 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2009) #add year variable, V0101

#PNAD2010 ----------------------------------------------------------------------
##N/A, census year

#PNAD2011 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2011, unzip = T)

#compared to 2005, no changes
PES2011_subset <- read_PNAD("pessoas", 2011, file = "./Data/PES2011.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2011_subset <- read_PNAD("domicilios", 2011, file = "./Data/DOM2011.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2011 <- merge(PES2011_subset, DOM2011_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2011 <- pnadmig_2011 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2011) #add year variable, V0101

#PNAD2012 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2012, unzip = T)

#compared to 2005, no changes
PES2012_subset <- read_PNAD("pessoas", 2012, file = "./Data/PES2012.txt",  vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2012_subset <- read_PNAD("domicilios", 2012, file = "./Data/DOM2012.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2012 <- merge(PES2012_subset, DOM2012_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2012 <- pnadmig_2012 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2012) #add year variable, V0101

#PNAD2013 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2013, unzip = T)

#compared to 2005, no changes
PES2013_subset <- read_PNAD("pessoas", file = "./Data/PES2013.txt", 2013, vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2013_subset <- read_PNAD("domicilios", 2013, file = "./Data/DOM2013.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2013 <- merge(PES2013_subset, DOM2013_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2013 <- pnadmig_2013 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2013) #add year variable, V0101

#PNAD2014 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2014, unzip = T)

#compared to 2005, no changes
PES2014_subset <- read_PNAD("pessoas", 2014, file = "./Data/PES2014.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2014_subset <- read_PNAD("domicilios", 2014, file = "./Data/DOM2014.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2014 <- merge(PES2014_subset, DOM2014_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2014 <- pnadmig_2014 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2014) #add year variable, V0101

#PNAD2015 ----------------------------------------------------------------------
##download_sourceData("PNAD", 2015, unzip = T)

#compared to 2005, no changes
PES2015_subset <- read_PNAD("pessoas", 2015, file = "./Data/PES2015.txt", vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
                                                              "V0504", "V0505", "V5061", "V5062", "V5063", "V5064",
                                                              "V5065", "V0507", "V5080", "V5090", "V0510", "V0511", "V5121", "V5122",
                                                              "V5123", "V5124", "V5125", "V9001", "V4729", "V4732"))

#compared to 2004, no changes
DOM2015_subset <- read_PNAD("domicilios", 2015, file = "./Data/DOM2015.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

pnadmig_2015 <- merge(PES2015_subset, DOM2015_subset,
                      by = c("V0102", "V0103")) #merging by control and series

pnadmig_2015 <- pnadmig_2015 %>%
  mutate(UF = str_extract(V0102, "^\\d{2}")) %>% #creates UF from the control variable
  mutate(year = 2015) #add year variable, V0101

#Merging all datasets----------------------------------------------------------
#Seems to be possible due to the stratification by state - each one behaving as a separate SRS (see page 32 of Lumley - Complex Surveys)

pnadmig_2001_2015 <- rbind(pnadmig_2001, pnadmig_2002, pnadmig_2003, pnadmig_2004, pnadmig_2005,
                           pnadmig_2006, pnadmig_2007, pnadmig_2008, pnadmig_2009,
                           pnadmig_2011, pnadmig_2012, pnadmig_2013, pnadmig_2014, pnadmig_2015, fill = TRUE)

#Creating objects for different analyses----------------------------------------

##Variables will be analysed according to (1) total population; (2) respondents in the Southeast, and
##(3) people coming from the Northeast - to estimate emmigration from the region

###POPULATION-------------------------------------------------------------------

#Survey design

#2001

pnadmig_2001d <- svydesign(
  ids = ~ PSU,
  strata = ~ STRAT,
  weight = ~ V4729,
  data = pnadmig_2001,
  nest = TRUE
)

#2002

pnadmig_2002d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2002,
  nest = TRUE
)

svytotal(~V0502, pnadmig_2002d)

#2003

pnadmig_2003d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2003,
  nest = TRUE
)

#2004

pnadmig_2004d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2004,
  nest = TRUE
)

#2005

pnadmig_2005d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2005,
  nest = TRUE
)

#2006

pnadmig_2006d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2006,
  nest = TRUE
)

#2007

pnadmig_2007d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2007,
  nest = TRUE
)

#2008

pnadmig_2008d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2008,
  nest = TRUE
)

#2009

pnadmig_2009d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2009,
  nest = TRUE
)

#2011

pnadmig_2011d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2011,
  nest = TRUE
)

#2012

pnadmig_2012d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2012,
  nest = TRUE
)

#2013

pnadmig_2013d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2013,
  nest = TRUE
)

#2014

pnadmig_2014d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2014,
  nest = TRUE
)

#2015

pnadmig_2015d <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2015,
  nest = TRUE
)




#RESPONDENTS FROM THE SOUTHEAST-------------------------------------------------
##Filtering data for respondents in the Southeast--------------------------------

pnadmig_2001_2015_SE <- pnadmig_2001_2015 %>%
                        filter(UF %in% c("31", "32", "33", "35"))

#Creation of survey designs-----------------------------------------------------

#2001

pnadmig_2001_SE <- pnadmig_2001_2015_SE %>%
                   filter(year == 2001)

pnadmig_2001d_SE <- svydesign(
  ids = ~ PSU,
  strata = ~ STRAT,
  weight = ~ V4729,
  data = pnadmig_2001_SE,
  nest = TRUE
)

#2002

pnadmig_2002_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2002)

pnadmig_2002d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2002_SE,
  nest = TRUE
)

V0502_2002__SE <- svyby(
  ~V0502, 
  by = ~year, 
  design = pnadmig_2002d_SE, 
  svytotal)

#2003

pnadmig_2003_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2003)

pnadmig_2003d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2003_SE,
  nest = TRUE
)

#2004

pnadmig_2004_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2004)

pnadmig_2004d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2004_SE,
  nest = TRUE
)

#2005

pnadmig_2005_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2005)

pnadmig_2005d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2005_SE,
  nest = TRUE
)

#2006

pnadmig_2006_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2006)

pnadmig_2006d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2006_SE,
  nest = TRUE
)

#2007

pnadmig_2007_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2007)

pnadmig_2007d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2007_SE,
  nest = TRUE
)

#2008

pnadmig_2008_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2008)

pnadmig_2008d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2008_SE,
  nest = TRUE
)

#2009

pnadmig_2009_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2009)

pnadmig_2009d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2009_SE,
  nest = TRUE
)

#2011

pnadmig_2011_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2011)

pnadmig_2011d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2011_SE,
  nest = TRUE
)

#2012

pnadmig_2012_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2012)

pnadmig_2012d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2012_SE,
  nest = TRUE
)

#2013

pnadmig_2013_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2013)

pnadmig_2013d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2013_SE,
  nest = TRUE
)

#2014

pnadmig_2014_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2014)

pnadmig_2014d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2014_SE,
  nest = TRUE
)

#2015

pnadmig_2015_SE <- pnadmig_2001_2015_SE %>%
  filter(year == 2015)

pnadmig_2015d_SE <- svydesign(
  ids = ~ V4618,
  strata = ~ V4617,
  weight = ~ V4729,
  data = pnadmig_2015_SE,
  nest = TRUE
)

#Analysis-----------------------------------------------------------------------

##V0502 - Nasceu nesta UF

###V0502_POP - Population

#Overview of the full population

V0502_2001_SE <- svytotal(~V0502,
                          design = pnadmig_2001d_SE,
                          na.rm = TRUE)

##V0502_SE - Nasceu nesta UF

###2001
#finding the totals for each category
V0502_2001_SE <- svytotal(~V0502,
                       design = pnadmig_2001d_SE,
                       na.rm = TRUE)

V0502_2001_SE <- as.data.frame(V0502_2001_SE) #dataframe
rownames(V0502_2001_SE) <- c("NA", "Sim", "Nao") #changes row names
V0502_2001_SE$nasceu_nesta_UF <- rownames(V0502_2001_SE) #assign row names to a new variable - for chart later
V0502_2001_SE <- V0502_2001_SE %>% 
              mutate(proportion = total/sum(total), year = "2001") #adding year and calculating proportion

###2002

options(survey.lonely.psu="adjust") #conservative solution to the stratum with only 1 PSU - the data for the single-PSU stratum are centered at the sample grand mean rather than the stratum mean.
V0502_2002_SE <- svytotal(~V0502,
                          design = pnadmig_2002d_SE,
                          na.rm = TRUE)

V0502_2002_SE <- as.data.frame(V0502_2002_SE) #dataframe
rownames(V0502_2002_SE) <- c("NA", "Sim", "Nao")
V0502_2002_SE$nasceu_nesta_UF <- rownames(V0502_2002_SE)
V0502_2002_SE <- V0502_2002_SE %>% 
  mutate(proportion = total/sum(total), year = "2002") #adding year and calculating proportion


###2003

V0502_2003_SE <- svytotal(~V0502,
                          design = pnadmig_2003d_SE,
                          na.rm = TRUE)

V0502_2003_SE <- as.data.frame(V0502_2003_SE) #dataframe
rownames(V0502_2003_SE) <- c("NA", "Sim", "Nao")
V0502_2003_SE$nasceu_nesta_UF <- rownames(V0502_2003_SE)
V0502_2003_SE <- V0502_2003_SE %>% 
  mutate(proportion = total/sum(total), year = "2003") #adding year and calculating proportion

###2004

V0502_2004_SE <- svytotal(~V0502,
                          design = pnadmig_2004d_SE,
                          na.rm = TRUE)

V0502_2004_SE <- as.data.frame(V0502_2004_SE) #dataframe
rownames(V0502_2004_SE) <- c("NA", "Sim", "Nao")
V0502_2004_SE$nasceu_nesta_UF <- rownames(V0502_2004_SE)
V0502_2004_SE <- V0502_2004_SE %>% 
  mutate(proportion = total/sum(total), year = "2004") #adding year and calculating proportion

###2005

V0502_2005_SE <- svytotal(~V0502,
                          design = pnadmig_2005d_SE,
                          na.rm = TRUE)

V0502_2005_SE <- as.data.frame(V0502_2005_SE) #dataframe
rownames(V0502_2005_SE) <- c("NA", "Sim", "Nao")
V0502_2005_SE$nasceu_nesta_UF <- rownames(V0502_2005_SE)
V0502_2005_SE <- V0502_2005_SE %>% 
  mutate(proportion = total/sum(total), year = "2005") #adding year and calculating proportion

###2006

V0502_2006_SE <- svytotal(~V0502,
                          design = pnadmig_2006d_SE,
                          na.rm = TRUE)

V0502_2006_SE <- as.data.frame(V0502_2006_SE) #dataframe
rownames(V0502_2006_SE) <- c("NA", "Sim", "Nao")
V0502_2006_SE$nasceu_nesta_UF <- rownames(V0502_2006_SE)
V0502_2006_SE <- V0502_2006_SE %>% 
  mutate(proportion = total/sum(total), year = "2006") #adding year and calculating proportion

###2007

V0502_2007_SE <- svytotal(~V0502,
                          design = pnadmig_2007d_SE,
                          na.rm = TRUE)

V0502_2007_SE <- as.data.frame(V0502_2007_SE) #dataframe
rownames(V0502_2007_SE) <- c("NA", "Sim", "Nao")
V0502_2007_SE$nasceu_nesta_UF <- rownames(V0502_2007_SE)
V0502_2007_SE <- V0502_2007_SE %>% 
  mutate(proportion = total/sum(total), year = "2007") #adding year and calculating proportion

###2008

V0502_2008_SE <- svytotal(~V0502,
                          design = pnadmig_2008d_SE,
                          na.rm = TRUE)

V0502_2008_SE <- as.data.frame(V0502_2008_SE) #dataframe
rownames(V0502_2008_SE) <- c("NA", "Sim", "Nao")
V0502_2008_SE$nasceu_nesta_UF <- rownames(V0502_2008_SE)
V0502_2008_SE <- V0502_2008_SE %>% 
  mutate(proportion = total/sum(total), year = "2008") #adding year and calculating proportion

###2009

V0502_2009_SE <- svytotal(~V0502,
                          design = pnadmig_2009d_SE,
                          na.rm = TRUE)

V0502_2009_SE <- as.data.frame(V0502_2009_SE) #dataframe
rownames(V0502_2009_SE) <- c("NA", "Sim", "Nao")
V0502_2009_SE$nasceu_nesta_UF <- rownames(V0502_2009_SE)
V0502_2009_SE <- V0502_2009_SE %>% 
  mutate(proportion = total/sum(total), year = "2009") #adding year and calculating proportion

###2011

V0502_2011_SE <- svytotal(~V0502,
                          design = pnadmig_2011d_SE,
                          na.rm = TRUE)

V0502_2011_SE <- as.data.frame(V0502_2011_SE) #dataframe
rownames(V0502_2011_SE) <- c("NA", "Sim", "Nao")
V0502_2011_SE$nasceu_nesta_UF <- rownames(V0502_2011_SE)
V0502_2011_SE <- V0502_2011_SE %>% 
  mutate(proportion = total/sum(total), year = "2011") #adding year and calculating proportion

###2012

V0502_2012_SE <- svytotal(~V0502,
                          design = pnadmig_2012d_SE,
                          na.rm = TRUE)

V0502_2012_SE <- as.data.frame(V0502_2012_SE) #dataframe
rownames(V0502_2012_SE) <- c("NA", "Sim", "Nao")
V0502_2012_SE$nasceu_nesta_UF <- rownames(V0502_2012_SE)
V0502_2012_SE <- V0502_2012_SE %>% 
  mutate(proportion = total/sum(total), year = "2012") #adding year and calculating proportion


###2013

V0502_2013_SE <- svytotal(~V0502,
                          design = pnadmig_2013d_SE,
                          na.rm = TRUE)

V0502_2013_SE <- as.data.frame(V0502_2013_SE) #dataframe
rownames(V0502_2013_SE) <- c("NA", "Sim", "Nao")
V0502_2013_SE$nasceu_nesta_UF <- rownames(V0502_2013_SE)
V0502_2013_SE <- V0502_2013_SE %>% 
  mutate(proportion = total/sum(total), year = "2013") #adding year and calculating proportion

###2014

V0502_2014_SE <- svytotal(~V0502,
                          design = pnadmig_2014d_SE,
                          na.rm = TRUE)

V0502_2014_SE <- as.data.frame(V0502_2014_SE) #dataframe
rownames(V0502_2014_SE) <- c("NA", "Sim", "Nao")
V0502_2014_SE$nasceu_nesta_UF <- rownames(V0502_2014_SE)
V0502_2014_SE <- V0502_2014_SE %>% 
  mutate(proportion = total/sum(total), year = "2014") #adding year and calculating proportion

###2015

V0502_2015_SE <- svytotal(~V0502,
                          design = pnadmig_2015d_SE,
                          na.rm = TRUE)

V0502_2015_SE <- as.data.frame(V0502_2015_SE) #dataframe
rownames(V0502_2015_SE) <- c("NA", "Sim", "Nao")
V0502_2015_SE$nasceu_nesta_UF <- rownames(V0502_2015_SE)
V0502_2015_SE <- V0502_2015_SE %>% 
  mutate(proportion = total/sum(total), year = "2015") #adding year and calculating proportion

###Merge V0502_SE data

V0502_2001_2015_SE <- rbind(V0502_2001_SE, V0502_2002_SE, V0502_2003_SE, V0502_2004_SE, V0502_2005_SE,
                            V0502_2006_SE, V0502_2007_SE, V0502_2008_SE, V0502_2009_SE, V0502_2011_SE,
                            V0502_2012_SE, V0502_2013_SE, V0502_2014_SE, V0502_2015_SE)

rownames(V0502_2001_2015_SE) <- NULL
V0502_2001_2015_SE$nasceu_nesta_UF <- as.factor(V0502_2001_2015_SE$nasceu_nesta_UF)


##V0502_2001_2015_SE <- V0502_2001_2015_SE %>%
##  group_by(nasceu_nesta_UF) %>%               #group_by not working here to calculate change
##  mutate(prop_change = (proportion/lead(proportion) - 1) * 100)

V0502_2001_2015_SE %>%
  filter(nasceu_nesta_UF == "Nao") %>%
  ggplot(mapping = aes(x = year, y = proportion)) +
  geom_line(group = 1) +
  ylim(0, 0.5)

V0502_2001_2015_SE %>%
  filter(nasceu_nesta_UF == "Nao") %>%
  ggplot(mapping = aes(x = year, y = total)) +
  geom_line(group = 1)
  
V0502_2001_2015_SE %>%
  ggplot(aes(x = proportion, y = year, fill = nasceu_nesta_UF)) +
  geom_bar(position = "stack", stat = "identity")




