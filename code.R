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
  mutate(year = 20068) #add year variable, V0101

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
DOM2006_subset <- read_PNAD("domicilios", 2007, file = "./Data/DOM2007.txt", vars_subset = c("V0102", "V0103", "V4617", "V4618")) 

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
PES2013_subset <- read_PNAD("pessoas", file = "./2013/Dados_20170807/Dados/PES2013.txt", 2013, vars_subset =  c("V0102", "V0103", "V0302", "V8005", "V0501", "V0502", "V5030",
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


#Analysis ----------------------------------------------------------------------
##2001

pnadmig_2001d <- svydesign(
                 ids = ~ PSU,
                 strata = ~ STRAT,
                 weight = ~ V4729,
                 data = pnadmig_2001,
                 nest = TRUE
)

svyby(pnadmig_2001$V0502, pnadmig_2001d)
pnadmig
pnadmig_2001$V0502 <- as.factor(pnadmig_2001$V0502)
str(pnadmig_2001)
levels(pnadmig_2001$V0502)

tab_w <- svytable(~V0502, design = pnadmig_2001d) %>%
  as.data.frame() %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))

tab_wmig <-  svyby(formula = ~V0502, by = ~UF,
                   FUN=svymean, design = pnadmig_2001d, na.rm=TRUE)
tab_wmig


tab_totals <- svytotal(~V0502,
                       design = pnadmig_2001d,
                       na.rm = TRUE)

tab_totals <- svytotal(~V0501,
                       design = pnadmig_2001d,
                       na.rm = TRUE)

rownames(tab_totals) <- c("")

##Changing 999 to NAs
##test <-PNADmig_2004 %>%
##mutate(V8005 = na_if(V8005, "999"))
