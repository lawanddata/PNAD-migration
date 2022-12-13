library(tidyverse)
rm(list = ls())
source("import_code.R", local = knitr::knit_global())


###Change 2001-2015


V0502_year_pop <- svyby(
                  ~V0502, 
                  by = ~year, 
                  design = pnadmig_2001_2015d, 
                  svytotal)
V0502_year_pop <- as.data.frame(V0502_year_pop)
V0502_year_pop <- V0502_year_pop %>% 
                  rename(NDisponivel = V0502, Sim = V05022, Nao = V05024, se.NDisponivel = se.V0502,                           se.Sim = se.V05022, se.Nao = se.V05024) %>%
                  group_by(year) %>%
                  mutate(prop.Nao = Nao/sum(Sim, Nao, NDisponivel)) %>%
                  ungroup()
print(V0502_year_pop) #check how to print better table


V0502_year_pop %>%
  ggplot(aes(x = year, y = prop.Nao)) +
  geom_line(group = 1) +
  ylim(0.1, 0.2)

##V0502_SE

pnadmig_2001_2015d_SE <- subset(pnadmig_2001_2015d, UF %in% c(31, 32, 33, 35)) #subsetting design for respondents in SE states(MG, SP, RJ, ES)
V0502_year_SE <- svyby(
                  ~V0502, 
                  by = ~year, 
                  design = pnadmig_2001_2015d_SE, 
                  svytotal) #totals per year
V0502_year_SE <- as.data.frame(V0502_year_SE) #converting to dataframe
V0502_year_SE <- V0502_year_SE %>%
                  rename(NDisponivel = V0502, Sim = V05022, Nao = V05024, se.NDisponivel = se.V0502,                           se.Sim = se.V05022, se.Nao = se.V05024) %>% #renaming the variables
                  group_by(year) %>%
                  mutate(prop.Nao = Nao/sum(Sim, Nao, NDisponivel)) %>% #adding new column with proportions of migrants
                  ungroup()
print(V0502_year_SE) #check how to print better table

V0502_year_SE %>%
  ggplot(aes(x = factor(year), y = prop.Nao)) +
  geom_line(group = 1) +
  ylim(0.1, 0.2)


#V0502_NE/V05030_NE - Was born in the Northeast

#Calculating respondents in the Southeast who were born in the Northeast

pnadmig_2001_2015d_SENE <- subset(pnadmig_2001_2015d_SE, V5030 %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) #subsetting design for respondents in SE states(MG, SP, RJ, ES) who answered they were born in the NE
pnadmig_2001_2015SENE <- svyby(
                         ~V5030,
                         by = ~year,
                         design = pnadmig_2001_2015d_SENE,
                         svytotal)
pnadmig_2001_2015SENE


#Calculating Respondents in the whole country who were born outside the state of #response and in the Northeast (estimation of emigration from NE)


pnadmig_2001_2015d_NE <- subset(pnadmig_2001_2015d, V5030 %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) #subsetting design for people who answered they were born in the NE


V5030_year_pop_NE <- svyby(
                ~V5030, 
                by = ~year, 
                design = pnadmig_2001_2015d_NE, 
                svytotal) #totals of UF born per year (only considering those born in states of the NE)
V5030_year_pop_NE


V5030_total_NE <- V5030_year_pop_NE %>%
                  as.data.frame() %>%
                  group_by(year) %>%
                  summarize(totalV5030_NE = sum(V503021, V503022, V503023, V503024, V503025, V503026,V503027, V503028, V503029)) %>%
                  ungroup()
V5030_total_NE

V5030_total_NE %>%
  ggplot(aes(x = factor(year), y = totalV5030_NE)) +
  geom_line(group = 1)

pnadmig_2001d_NE <- subset(pnadmig_2001_2015d_NE, year == 2001)
V5030_2001_NE <- svytotal(
              ~V5030,
              by = ~year,
              design = pnadmig_2001d_NE)
pnadmig_2002d_NE <- subset(pnadmig_2001_2015d_NE, year == 2002)
V5030_2002_NE <- svytotal(
              ~V5030,
              by = ~year,
              design = pnadmig_2002d_NE)


#Subsetting for respondent not in the Northeast + Born in the Northeast (estimation of emigration from the NE with destination anywhere)

#calculating emigrants from NE 2010/2015 - para comparar com o artigo https://www.scielo.br/j/rbeur/a/SCVMhkFZ7Hx7d7zwnp56h5q/?format=pdf&lang=pt

#in 2015 did not live in the NE
#Responderam estado do NE em V5080

`%!in%` <- Negate(`%in%`)
pnadmig_2001_2015_NotNE <- subset(pnadmig_2001_2015d, UF %!in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) #not respondents in NE
pnadmig_2015_NotNE <- subset(pnadmig_2001_2015_NotNE, year == 2015) #year 2015
emigrants_NE_2015 <- svytotal(
  ~V5080,
  pnadmig_2015_NotNE)
emigrants_NE_2015 <- as.data.frame(emigrants_NE_2015) %>%
  subset(rownames(emigrants_NE_2015) %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) %>%
  summarize(total = sum(total))
emigrants_NE_2015$ID <- rownames(emigrants_NE_2015)
emigrants_NE_2015_sub <- emigrants_NE_2015 %>%
  filter(ID %in% c("V508021", "V508022", "V508023", "V508024", "V508025", "V508026", "V508027", "V508028", "V508029")) %>%
  summarize(total = sum(total))
emigrants_NE_2015_sub

#Contigency table - state lived 5 years before + current UF


pnadmig_2015d <- subset(pnadmig_2001_2015d, year == 2015)
#table_5080_UF_2015_prop <- prop.table(svytable(~V5080 + UF,
#         design = pnadmig_2015d),margin = 1)
#table_5080_UF_2015 <- as.data.frame(table_5030_UF_2015)
View(table_5080_UF_2015)

#Calculating emmigration from NE 2010-2015 (to all regions)


emigration_NE_2015 <- table_5080_UF_2015_reg %>%
  filter(reg_em_2010 == "Nordeste") %>% #filtering respondents who were in NE 5 years before
  filter(reg_em_2015 != "Nordeste") %>% #filtering respondents who are in any other region (expcept NE) when answering the survey in 2015
  summarize(emigration_NE = sum(Freq)) %>% #calculating emigration flow from the NE
  mutate(period_start = 2010, period_end = 2015) #between 2010 and 2015

#Calculating immigration to NE 2010-2015 (to all states)


imigration_NE_2015 <- table_5080_UF_2015_reg %>%
  filter(reg_em_2010 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2015 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2010 != "") %>% #removing the UF population row
  filter(UF_em_2010 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2010, period_end = 2015)

#Calculating net migration in the NE


net_migration_NE_2015 <- imigration_NE_2015$imigration_NE-emigration_NE_2015$emigration_NE
#to calculate net migration rate - *1,000/mid-year pop - literature divides by the first period (2010 in this case), but if using moving windows see how to do it

h <- 35713
z <- 496299


##Immigration to NE (from all states)


###Calculating emmigration from NE 2009-2014 (to all regions)


#2015
immigration_NE_2015 <- table_5080_UF_2015 %>%
  filter(reg_em_2010 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2015 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2010 != "") %>% #removing the UF population row
  filter(UF_em_2010 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2010, period_end = 2015)
#2014
immigration_NE_2014 <- table_5080_UF_2014 %>%
  filter(reg_em_2009 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2014 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2009 != "") %>% #removing the UF population row
  filter(UF_em_2009 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2009, period_end = 2014)
#2013
immigration_NE_2013 <- table_5080_UF_2013 %>%
  filter(reg_em_2008 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2013 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2008 != "") %>% #removing the UF population row
  filter(UF_em_2008 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2008, period_end = 2013)
#2012
immigration_NE_2012 <- table_5080_UF_2012 %>%
  filter(reg_em_2007 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2012 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2007 != "") %>% #removing the UF population row
  filter(UF_em_2007 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2007, period_end = 2012)
#2011
immigration_NE_2011 <- table_5080_UF_2011 %>%
  filter(reg_em_2006 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2011 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2006 != "") %>% #removing the UF population row
  filter(UF_em_2006 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2006, period_end = 2011)
#2009
immigration_NE_2009 <- table_5080_UF_2009 %>%
  filter(reg_em_2004 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2009 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2004 != "") %>% #removing the UF population row
  filter(UF_em_2004 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2004, period_end = 2009)
#2008
immigration_NE_2008 <- table_5080_UF_2008 %>%
  filter(reg_em_2003 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2008 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2003 != "") %>% #removing the UF population row
  filter(UF_em_2003 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2003, period_end = 2008)
#2007
immigration_NE_2007 <- table_5080_UF_2007 %>%
  filter(reg_em_2002 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2007 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2002 != "") %>% #removing the UF population row
  filter(UF_em_2002 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2002, period_end = 2007)
#2006
immigration_NE_2006 <- table_5080_UF_2006 %>%
  filter(reg_em_2001 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2006 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2001 != "") %>% #removing the UF population row
  filter(UF_em_2001 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2001, period_end = 2006)
#2005
immigration_NE_200 <- table_5080_UF_2006 %>%
  filter(reg_em_2001 != "Nordeste") %>% #respondents who were not living in the NE in 2010
  filter(reg_em_2006 == "Nordeste") %>% #respondents in who are in the NE in 2015
  filter(UF_em_2001 != "") %>% #removing the UF population row
  filter(UF_em_2001 != "Pais estrangeiro") %>% #removing immigrants from foreign countries
  summarize(imigration_NE = sum(Freq)) %>%
  mutate(period_start = 2001, period_end = 2006)