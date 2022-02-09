
library(foreign)
library(dplyr)
library(psych)
library(broom)
library(jtools)
library(margins)
library(ggplot2)
library("readxl")
library(sm)
library(pastecs)
library("ggpubr")
library("gplots")
library(rstatix)
library(stargazer)
library(devtools)
library(sjmisc)
library(sjPlot)
library(table1)
library(boot)
library(expss)
library(summarytools)
library(tidyr)

# To replicate, first download necessary datasets from the github repository. 
# Set working directory as necessary to read in the data.

# Read-in CCES module data (2018 and 2020 already pooled and stripped of identifying information)
cces <- read.csv("cces.csv")

# State Contextual Data #

context <- read.csv("CCES_contextual_states.csv")
cces1 <- merge(cces, context, by=c("inputstate"), all=TRUE)
rm(context)

# MERGE IN CONGRESSIONAL DISTRICT DATA ##

cd <- read.csv("cdid116_v2.csv", header = TRUE)
cd <- subset(cd, select=c(state2, cdid116, biden2020, trump2020))

cd$cdid116 <- as.numeric(cd$cdid116)
cces1$cdid116 <- as.numeric(cces1$cdid116)

cces1 <- merge(cces1, cd, by=c("state2", "cdid116"), all=TRUE)

# remove original CCES for housekeeping
rm(cces)
# "cces1 is new baseline for data

# merge presidential vote data

load("~/countypres_2000-2016.RData")
#detach plyr to allow group_by functions to work
require(plyr) # package
detach(package:plyr)

presvote <- x
rm(x)
presvote <- subset(presvote, year==2016)
presvote <- subset(presvote, party!="NA")

presvote2party <- presvote %>% 
  group_by(FIPS, year) %>% 
  summarise(Frequency = sum(candidatevotes, na.rm=TRUE))

colnames(presvote2party) <- c("FIPS","year","presvotes_2party")

presvote <- merge(presvote, presvote2party, by = c("FIPS", "year"))
rm(presvote2party)

presvote$voteshare2party <- (presvote$candidatevotes/presvote$presvotes_2party)*100

presvote$d_voteshare2party = ifelse(presvote$party == "democrat", presvote$voteshare2party, NA)
presvote$r_voteshare2party = ifelse(presvote$party == "republican", presvote$voteshare2party, NA)
presvote_d <- subset(presvote, d_voteshare2party!="NA")
presvote_r <- subset(presvote, r_voteshare2party!="NA")

presvote_d <- presvote_d[c(-15)]
presvote_r <- presvote_r[c(-14)]

## Just 2016 vote share by county - Democrats ##
avg_dvote <- presvote_d %>% 
  group_by(FIPS) %>% 
  summarise(Frequency = sum(d_voteshare2party))

colnames(avg_dvote) <- c("FIPS","avg_d_voteshare2party")

avg_presvote_d <- merge(presvote_d, avg_dvote, by = c("FIPS"))

avg_presvote_d <- subset(avg_presvote_d, select=c(FIPS, state, state_po, county, avg_d_voteshare2party))

rm(avg_dvote)
rm(presvote_d)

## just 2016 - Republicans ##
avg_rvote <- presvote_r %>% 
  group_by(FIPS) %>% 
  summarise(Frequency = sum(r_voteshare2party))

colnames(avg_rvote) <- c("FIPS","avg_r_voteshare2party")

avg_presvote_r <- merge(presvote_r, avg_rvote, by = c("FIPS"))

avg_presvote_r <- subset(avg_presvote_r, select=c(FIPS, state, state_po, county, avg_r_voteshare2party))

rm(avg_rvote)
rm(presvote_r)

##

avg_dvote <- subset(avg_presvote_d, select=c(FIPS, state, state_po, county, avg_d_voteshare2party))
avg_rvote <- subset(avg_presvote_r, select=c(FIPS, avg_r_voteshare2party))

presvote <- presvote[c(-14, -15)]
presvote <- merge(avg_dvote, avg_rvote, by = c("FIPS"))

rm(avg_dvote)
rm(avg_rvote)
rm(avg_presvote_d)
rm(avg_presvote_r)

# add leading zero to FIPS in presvote data
library(stringr)
presvote$FIPS <- str_pad(presvote$FIPS, 5, pad = "0")
presvote$FIPS <- as.integer((presvote$FIPS))
typeof(presvote$FIPS)

cces1$FIPS <- cces1$countyfips

## New Dataset - 'cces2' (before merging, keep cces1 as is)
## But first - ensure that everything gets merged, even if has some missing data

library(tidyr)

cces2 <- merge(cces1, presvote, by="FIPS", all = FALSE)
cces2 <- cces2 %>% drop_na(state2)

rm(presvote)

# Create County Partisanship Indicator #

cces2$Pres_Vote <- ifelse(cces2$avg_r_voteshare2party >=65, "Solid Republican",
                          ifelse(cces2$avg_r_voteshare2party >=50 & cces2$avg_r_voteshare2party < 65, "Lean Republican",
                                 ifelse(cces2$avg_r_voteshare2party <50 & cces2$avg_r_voteshare2party >=35, "Lean Democrat",
                                        ifelse(cces2$avg_r_voteshare2party <35, "Solid Democrat", NA))))

###################################
## Exclude True Independents ##
cces2.indy <- subset(cces2, pid=="Independent")
cces2.2 <- subset(cces2, pid!="Independent")

## Drop Independent Factor Level
cces2.2$pid <- droplevels(cces2.2$pid) 

table(cces2.2$pid)


## Subsets specifically for Margins and Plotting (custom labelling)

# School Board Margins Plots #

sbMargins <- cces2.2

sbMarginsNP <- c("RCO322_np", "avg_d_voteshare2party", "pid",
                 "perceive_extreme_very", "polint", "educ.int",
                 "birthyr", "Pres_Vote", "biden2020", "trump2020")

sbMarginsNP2 <- sbMargins[sbMarginsNP]

#Ensure factors for glm

sbMarginsNP2$pid.f <- as.factor(sbMarginsNP2$pid)
sbMarginsNP2$pid.f <- relevel(sbMarginsNP2$pid.f, ref = "Republican")
#
sbMarginsNP2$polint.f <- as.factor(sbMarginsNP2$polint)
#
sbMarginsNP2$perceive_extreme_very.f <- as.factor(sbMarginsNP2$perceive_extreme_very)
#
sbMarginsNP2$educ.int.f <- as.factor(sbMarginsNP2$educ.int)
#
sbMarginsNP2$Pres_Vote.f <- as.factor(sbMarginsNP2$Pres_Vote)
sbMarginsNP2$Pres_Vote.f <- factor(sbMarginsNP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                        "Lean Democrat", 
                                                                        "Lean Republican",
                                                                        "Solid Republican"))

sbMarginsNP2$Pres_Vote.f <- relevel(sbMarginsNP2$Pres_Vote.f, ref = "Solid Democrat")

# LEO Margins Plots #

leoMargins <- cces2.2

leoMarginsNP <- c("RCO321_np", "avg_d_voteshare2party", "pid",
                  "perceive_extreme_very", "polint", "educ.int",
                  "birthyr", "Pres_Vote", "biden2020", "trump2020")

leoMarginsNP2 <- leoMargins[leoMarginsNP]

#Ensure factors for glm

leoMarginsNP2$pid.f <- as.factor(leoMarginsNP2$pid)
leoMarginsNP2$pid.f <- relevel(leoMarginsNP2$pid.f, ref = "Republican")
#
leoMarginsNP2$polint.f <- as.factor(leoMarginsNP2$polint)
#
leoMarginsNP2$perceive_extreme_very.f <- as.factor(leoMarginsNP2$perceive_extreme_very)
#
leoMarginsNP2$educ.int.f <- as.factor(leoMarginsNP2$educ.int)
#
leoMarginsNP2$Pres_Vote.f <- as.factor(leoMarginsNP2$Pres_Vote)
leoMarginsNP2$Pres_Vote.f <- factor(leoMarginsNP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                          "Lean Democrat", 
                                                                          "Lean Republican",
                                                                          "Solid Republican"))

leoMarginsNP2$Pres_Vote.f <- relevel(leoMarginsNP2$Pres_Vote.f, ref = "Solid Democrat")

#######

# Sheriff Margins Plots #

sherMargins <- cces2.2

sherMarginsNP <- c("RCO323_np", "avg_d_voteshare2party", "pid",
                   "perceive_extreme_very", "polint", "educ.int",
                   "birthyr", "Pres_Vote", "biden2020", "trump2020")

sherMarginsNP2 <- sherMargins[sherMarginsNP]

#Ensure factors for glm

sherMarginsNP2$pid.f <- as.factor(sherMarginsNP2$pid)
sherMarginsNP2$pid.f <- relevel(sherMarginsNP2$pid.f, ref = "Republican")
#
sherMarginsNP2$polint.f <- as.factor(sherMarginsNP2$polint)
#
sherMarginsNP2$perceive_extreme_very.f <- as.factor(sherMarginsNP2$perceive_extreme_very)
#
sherMarginsNP2$educ.int.f <- as.factor(sherMarginsNP2$educ.int)
#
sherMarginsNP2$Pres_Vote.f <- as.factor(sherMarginsNP2$Pres_Vote)
sherMarginsNP2$Pres_Vote.f <- factor(sherMarginsNP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                            "Lean Democrat", 
                                                                            "Lean Republican",
                                                                            "Solid Republican"))

sherMarginsNP2$Pres_Vote.f <- relevel(sherMarginsNP2$Pres_Vote.f, ref = "Solid Democrat")


#######

# DA Margins Plots #

daMargins <- cces2.2

daMarginsNP <- c("RCO324_np", "avg_d_voteshare2party", "pid",
                 "perceive_extreme_very", "polint", "educ.int",
                 "birthyr", "Pres_Vote", "biden2020", "trump2020")

daMarginsNP2 <- daMargins[daMarginsNP]

#Ensure factors for glm

daMarginsNP2$pid.f <- as.factor(daMarginsNP2$pid)
daMarginsNP2$pid.f <- relevel(daMarginsNP2$pid.f, ref = "Republican")
#
daMarginsNP2$polint.f <- as.factor(daMarginsNP2$polint)
#
daMarginsNP2$perceive_extreme_very.f <- as.factor(daMarginsNP2$perceive_extreme_very)
#
daMarginsNP2$educ.int.f <- as.factor(daMarginsNP2$educ.int)
#
daMarginsNP2$Pres_Vote.f <- as.factor(daMarginsNP2$Pres_Vote)
daMarginsNP2$Pres_Vote.f <- factor(daMarginsNP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                        "Lean Democrat", 
                                                                        "Lean Republican",
                                                                        "Solid Republican"))

daMarginsNP2$Pres_Vote.f <- relevel(daMarginsNP2$Pres_Vote.f, ref = "Solid Democrat")

#######

# House Margins Plots #

houseMargins <- cces2.2

houseMarginsNP <- c("RCO327_np", "avg_d_voteshare2party", "pid",
                    "perceive_extreme_very", "polint", "educ.int",
                    "birthyr", "Pres_Vote", "biden2020", "trump2020")

houseMarginsNP2 <- daMargins[houseMarginsNP]

#Ensure factors for glm

houseMarginsNP2$pid.f <- as.factor(houseMarginsNP2$pid)
houseMarginsNP2$pid.f <- relevel(houseMarginsNP2$pid.f, ref = "Republican")
#
houseMarginsNP2$polint.f <- as.factor(houseMarginsNP2$polint)
#
houseMarginsNP2$perceive_extreme_very.f <- as.factor(houseMarginsNP2$perceive_extreme_very)
#
houseMarginsNP2$educ.int.f <- as.factor(houseMarginsNP2$educ.int)
#
houseMarginsNP2$Pres_Vote.f <- as.factor(houseMarginsNP2$Pres_Vote)
houseMarginsNP2$Pres_Vote.f <- factor(houseMarginsNP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                              "Lean Democrat", 
                                                                              "Lean Republican",
                                                                              "Solid Republican"))

houseMarginsNP2$Pres_Vote.f <- relevel(houseMarginsNP2$Pres_Vote.f, ref = "Solid Democrat")

#####################################################################

## Subsets specifically for Regression Outputs ##
library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(glmmTMB)
library(Hmisc)
library(glmmTMB)

###################################################

## School Board ##

sbGLM <- cces2.2

sbGLM$pid.f <- as.factor(sbGLM$pid)
sbGLM$polint.f <- as.factor(sbGLM$polint)
sbGLM$perceive_extreme_very.f <- as.factor(sbGLM$perceive_extreme_very)
sbGLM$educ.int.f <- as.factor(sbGLM$educ.int)
sbGLM$Pres_Vote.f <- as.factor(sbGLM$Pres_Vote)

## Change Labels ##

sbGLM$polint.f <- 
  factor(sbGLM$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

sbGLM$perceive_extreme_very.f <- 
  factor(sbGLM$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

sbGLM$educ.int.f <- 
  factor(sbGLM$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

sbGLM$Pres_Vote.f <- 
  factor(sbGLM$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(sbGLM$pid.f) <- "Party"
label(sbGLM$RCO322_np) <- "Preference for Nonpartisan School Board"
label(sbGLM$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(sbGLM$educ.int.f) <- "Education"
label(sbGLM$perceive_extreme_very.f) <- "Outparty Extremity"
label(sbGLM$polint.f) <- "News Interest"


###################################################

## LEO ##

leoGLM <- cces2.2

leoGLM$pid.f <- as.factor(leoGLM$pid)
leoGLM$polint.f <- as.factor(leoGLM$polint)
leoGLM$perceive_extreme_very.f <- as.factor(leoGLM$perceive_extreme_very)
leoGLM$educ.int.f <- as.factor(leoGLM$educ.int)
leoGLM$Pres_Vote.f <- as.factor(leoGLM$Pres_Vote)

## Change Labels ##

leoGLM$polint.f <- 
  factor(leoGLM$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

leoGLM$perceive_extreme_very.f <- 
  factor(leoGLM$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

leoGLM$educ.int.f <- 
  factor(leoGLM$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

leoGLM$Pres_Vote.f <- 
  factor(leoGLM$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(leoGLM$pid.f) <- "Party"
label(leoGLM$RCO321_np) <- "Preference for Nonpartisan Local Election Official"
label(leoGLM$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(leoGLM$educ.int.f) <- "Education"
label(leoGLM$perceive_extreme_very.f) <- "Outparty Extremity"
label(leoGLM$polint.f) <- "News Interest"



###################################################

## Sheriff ##

sherGLM <- cces2.2

sherGLM$pid.f <- as.factor(sherGLM$pid)
sherGLM$polint.f <- as.factor(sherGLM$polint)
sherGLM$perceive_extreme_very.f <- as.factor(sherGLM$perceive_extreme_very)
sherGLM$educ.int.f <- as.factor(sherGLM$educ.int)
sherGLM$Pres_Vote.f <- as.factor(sherGLM$Pres_Vote)

## Change Labels ##

sherGLM$polint.f <- 
  factor(sherGLM$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

sherGLM$perceive_extreme_very.f <- 
  factor(sherGLM$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

sherGLM$educ.int.f <- 
  factor(sherGLM$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

sherGLM$Pres_Vote.f <- 
  factor(sherGLM$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(sherGLM$pid.f) <- "Party"
label(sherGLM$RCO323_np) <- "Preference for Nonpartisan Sheriff"
label(sherGLM$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(sherGLM$educ.int.f) <- "Education"
label(sherGLM$perceive_extreme_very.f) <- "Outparty Extremity"
label(sherGLM$polint.f) <- "News Interest"


###################################################

## DA ##

daGLM <- cces2.2

daGLM$pid.f <- as.factor(daGLM$pid)
daGLM$polint.f <- as.factor(daGLM$polint)
daGLM$perceive_extreme_very.f <- as.factor(daGLM$perceive_extreme_very)
daGLM$educ.int.f <- as.factor(daGLM$educ.int)
daGLM$Pres_Vote.f <- as.factor(daGLM$Pres_Vote)

## Change Labels ##

daGLM$polint.f <- 
  factor(daGLM$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

daGLM$perceive_extreme_very.f <- 
  factor(daGLM$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

daGLM$educ.int.f <- 
  factor(daGLM$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

daGLM$Pres_Vote.f <- 
  factor(daGLM$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(daGLM$pid.f) <- "Party"
label(daGLM$RCO324_np) <- "Preference for Nonpartisan District Attorney"
label(daGLM$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(daGLM$educ.int.f) <- "Education"
label(daGLM$perceive_extreme_very.f) <- "Outparty Extremity"
label(daGLM$polint.f) <- "News Interest"


###################################################

## U.S. House ##

houseGLM <- cces2.2

houseGLM$pid.f <- as.factor(houseGLM$pid)
houseGLM$polint.f <- as.factor(houseGLM$polint)
houseGLM$perceive_extreme_very.f <- as.factor(houseGLM$perceive_extreme_very)
houseGLM$educ.int.f <- as.factor(houseGLM$educ.int)
houseGLM$Pres_Vote.f <- as.factor(houseGLM$Pres_Vote)

## Change Labels ##

houseGLM$polint.f <- 
  factor(houseGLM$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

houseGLM$perceive_extreme_very.f <- 
  factor(houseGLM$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

houseGLM$educ.int.f <- 
  factor(houseGLM$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

houseGLM$Pres_Vote.f <- 
  factor(houseGLM$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(houseGLM$pid.f) <- "Party"
label(houseGLM$RCO327_np) <- "Preference for Nonpartisan U.S. House"
label(houseGLM$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(houseGLM$educ.int.f) <- "Education"
label(houseGLM$perceive_extreme_very.f) <- "Outparty Extremity"
label(houseGLM$polint.f) <- "News Interest"

###################################

## PARTISAN AS THE DV ##

## Subsets specifically for Margins and Plotting (custom labelling)

# School Board Margins Plots #

sbMargins <- cces2.2

sbMarginsP <- c("RCO322_p", "avg_d_voteshare2party", "pid",
                "perceive_extreme_very", "polint", "educ.int",
                "birthyr", "Pres_Vote", "biden2020", "trump2020")

sbMarginsP2 <- sbMargins[sbMarginsP]

#Ensure factors for glm

sbMarginsP2$pid.f <- as.factor(sbMarginsP2$pid)
sbMarginsP2$pid.f <- relevel(sbMarginsP2$pid.f, ref = "Republican")
#
sbMarginsP2$polint.f <- as.factor(sbMarginsP2$polint)
#
sbMarginsP2$perceive_extreme_very.f <- as.factor(sbMarginsP2$perceive_extreme_very)
#
sbMarginsP2$educ.int.f <- as.factor(sbMarginsP2$educ.int)
#
sbMarginsP2$Pres_Vote.f <- as.factor(sbMarginsP2$Pres_Vote)
sbMarginsP2$Pres_Vote.f <- factor(sbMarginsP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                      "Lean Democrat", 
                                                                      "Lean Republican",
                                                                      "Solid Republican"))

sbMarginsP2$Pres_Vote.f <- relevel(sbMarginsP2$Pres_Vote.f, ref = "Solid Democrat")

# LEO Margins Plots #

leoMargins <- cces2.2

leoMarginsP <- c("RCO321_p", "avg_d_voteshare2party", "pid",
                 "perceive_extreme_very", "polint", "educ.int",
                 "birthyr", "Pres_Vote", "biden2020", "trump2020")

leoMarginsP2 <- leoMargins[leoMarginsP]

#Ensure factors for glm

leoMarginsP2$pid.f <- as.factor(leoMarginsP2$pid)
leoMarginsP2$pid.f <- relevel(leoMarginsP2$pid.f, ref = "Republican")
#
leoMarginsP2$polint.f <- as.factor(leoMarginsP2$polint)
#
leoMarginsP2$perceive_extreme_very.f <- as.factor(leoMarginsP2$perceive_extreme_very)
#
leoMarginsP2$educ.int.f <- as.factor(leoMarginsP2$educ.int)
#
leoMarginsP2$Pres_Vote.f <- as.factor(leoMarginsP2$Pres_Vote)
leoMarginsP2$Pres_Vote.f <- factor(leoMarginsP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                        "Lean Democrat", 
                                                                        "Lean Republican",
                                                                        "Solid Republican"))

leoMarginsP2$Pres_Vote.f <- relevel(leoMarginsP2$Pres_Vote.f, ref = "Solid Democrat")

#######

# Sheriff Margins Plots #

sherMargins <- cces2.2

sherMarginsP <- c("RCO323_p", "avg_d_voteshare2party", "pid",
                  "perceive_extreme_very", "polint", "educ.int",
                  "birthyr", "Pres_Vote", "biden2020", "trump2020")

sherMarginsP2 <- sherMargins[sherMarginsP]

#Ensure factors for glm

sherMarginsP2$pid.f <- as.factor(sherMarginsP2$pid)
sherMarginsP2$pid.f <- relevel(sherMarginsP2$pid.f, ref = "Republican")
#
sherMarginsP2$polint.f <- as.factor(sherMarginsP2$polint)
#
sherMarginsP2$perceive_extreme_very.f <- as.factor(sherMarginsP2$perceive_extreme_very)
#
sherMarginsP2$educ.int.f <- as.factor(sherMarginsP2$educ.int)
#
sherMarginsP2$Pres_Vote.f <- as.factor(sherMarginsP2$Pres_Vote)
sherMarginsP2$Pres_Vote.f <- factor(sherMarginsP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                          "Lean Democrat", 
                                                                          "Lean Republican",
                                                                          "Solid Republican"))

sherMarginsP2$Pres_Vote.f <- relevel(sherMarginsP2$Pres_Vote.f, ref = "Solid Democrat")


#######

# DA Margins Plots #

daMargins <- cces2.2

daMarginsP <- c("RCO324_p", "avg_d_voteshare2party", "pid",
                "perceive_extreme_very", "polint", "educ.int",
                "birthyr", "Pres_Vote", "biden2020", "trump2020")

daMarginsP2 <- daMargins[daMarginsP]

#Ensure factors for glm

daMarginsP2$pid.f <- as.factor(daMarginsP2$pid)
daMarginsP2$pid.f <- relevel(daMarginsP2$pid.f, ref = "Republican")
#
daMarginsP2$polint.f <- as.factor(daMarginsP2$polint)
#
daMarginsP2$perceive_extreme_very.f <- as.factor(daMarginsP2$perceive_extreme_very)
#
daMarginsP2$educ.int.f <- as.factor(daMarginsP2$educ.int)
#
daMarginsP2$Pres_Vote.f <- as.factor(daMarginsP2$Pres_Vote)
daMarginsP2$Pres_Vote.f <- factor(daMarginsP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                      "Lean Democrat", 
                                                                      "Lean Republican",
                                                                      "Solid Republican"))

daMarginsP2$Pres_Vote.f <- relevel(daMarginsP2$Pres_Vote.f, ref = "Solid Democrat")

#######

# House Margins Plots #

houseMargins <- cces2.2

houseMarginsP <- c("RCO327_p", "avg_d_voteshare2party", "pid",
                   "perceive_extreme_very", "polint", "educ.int",
                   "birthyr", "Pres_Vote", "biden2020", "trump2020")

houseMarginsP2 <- daMargins[houseMarginsP]

#Ensure factors for glm

houseMarginsP2$pid.f <- as.factor(houseMarginsP2$pid)
houseMarginsP2$pid.f <- relevel(houseMarginsP2$pid.f, ref = "Republican")
#
houseMarginsP2$polint.f <- as.factor(houseMarginsP2$polint)
#
houseMarginsP2$perceive_extreme_very.f <- as.factor(houseMarginsP2$perceive_extreme_very)
#
houseMarginsP2$educ.int.f <- as.factor(houseMarginsP2$educ.int)
#
houseMarginsP2$Pres_Vote.f <- as.factor(houseMarginsP2$Pres_Vote)
houseMarginsP2$Pres_Vote.f <- factor(houseMarginsP2$Pres_Vote.f, levels = c("Solid Democrat", 
                                                                            "Lean Democrat", 
                                                                            "Lean Republican",
                                                                            "Solid Republican"))

houseMarginsP2$Pres_Vote.f <- relevel(houseMarginsP2$Pres_Vote.f, ref = "Solid Democrat")

#####################################################################

## Subsets specifically for Regression Outputs ##
library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(glmmTMB)
library(Hmisc)
library(glmmTMB)

###################################################

## School Board ##

sbGLMp <- cces2.2

sbGLMp$pid.f <- as.factor(sbGLMp$pid)
sbGLMp$polint.f <- as.factor(sbGLMp$polint)
sbGLMp$perceive_extreme_very.f <- as.factor(sbGLMp$perceive_extreme_very)
sbGLMp$educ.int.f <- as.factor(sbGLMp$educ.int)
sbGLMp$Pres_Vote.f <- as.factor(sbGLMp$Pres_Vote)

## Change Labels ##

sbGLMp$polint.f <- 
  factor(sbGLMp$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

sbGLMp$perceive_extreme_very.f <- 
  factor(sbGLMp$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

sbGLMp$educ.int.f <- 
  factor(sbGLMp$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

sbGLMp$Pres_Vote.f <- 
  factor(sbGLMp$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(sbGLMp$pid.f) <- "Party"
label(sbGLMp$RCO322_p) <- "Preference for Partisan School Board"
label(sbGLMp$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(sbGLMp$educ.int.f) <- "Education"
label(sbGLMp$perceive_extreme_very.f) <- "Outparty Extremity"
label(sbGLMp$polint.f) <- "News Interest"


###################################################

## LEO ##

leoGLMp <- cces2.2

leoGLMp$pid.f <- as.factor(leoGLMp$pid)
leoGLMp$polint.f <- as.factor(leoGLMp$polint)
leoGLMp$perceive_extreme_very.f <- as.factor(leoGLMp$perceive_extreme_very)
leoGLMp$educ.int.f <- as.factor(leoGLMp$educ.int)
leoGLMp$Pres_Vote.f <- as.factor(leoGLMp$Pres_Vote)

## Change Labels ##

leoGLMp$polint.f <- 
  factor(leoGLMp$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

leoGLMp$perceive_extreme_very.f <- 
  factor(leoGLMp$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

leoGLMp$educ.int.f <- 
  factor(leoGLMp$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

leoGLMp$Pres_Vote.f <- 
  factor(leoGLMp$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(leoGLMp$pid.f) <- "Party"
label(leoGLMp$RCO321_p) <- "Preference for Partisan Local Election Official"
label(leoGLMp$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(leoGLMp$educ.int.f) <- "Education"
label(leoGLMp$perceive_extreme_very.f) <- "Outparty Extremity"
label(leoGLMp$polint.f) <- "News Interest"



###################################################

## Sheriff ##

sherGLMp <- cces2.2

sherGLMp$pid.f <- as.factor(sherGLMp$pid)
sherGLMp$polint.f <- as.factor(sherGLMp$polint)
sherGLMp$perceive_extreme_very.f <- as.factor(sherGLMp$perceive_extreme_very)
sherGLMp$educ.int.f <- as.factor(sherGLMp$educ.int)
sherGLMp$Pres_Vote.f <- as.factor(sherGLMp$Pres_Vote)

## Change Labels ##

sherGLMp$polint.f <- 
  factor(sherGLMp$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

sherGLMp$perceive_extreme_very.f <- 
  factor(sherGLMp$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

sherGLMp$educ.int.f <- 
  factor(sherGLMp$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

sherGLMp$Pres_Vote.f <- 
  factor(sherGLMp$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(sherGLMp$pid.f) <- "Party"
label(sherGLMp$RCO323_p) <- "Preference for Partisan Sheriff"
label(sherGLMp$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(sherGLMp$educ.int.f) <- "Education"
label(sherGLMp$perceive_extreme_very.f) <- "Outparty Extremity"
label(sherGLMp$polint.f) <- "News Interest"


###################################################

## DA ##

daGLMp <- cces2.2

daGLMp$pid.f <- as.factor(daGLMp$pid)
daGLMp$polint.f <- as.factor(daGLMp$polint)
daGLMp$perceive_extreme_very.f <- as.factor(daGLMp$perceive_extreme_very)
daGLMp$educ.int.f <- as.factor(daGLMp$educ.int)
daGLMp$Pres_Vote.f <- as.factor(daGLMp$Pres_Vote)

## Change Labels ##

daGLMp$polint.f <- 
  factor(daGLMp$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

daGLMp$perceive_extreme_very.f <- 
  factor(daGLMp$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

daGLMp$educ.int.f <- 
  factor(daGLMp$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

daGLMp$Pres_Vote.f <- 
  factor(daGLMp$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(daGLMp$pid.f) <- "Party"
label(daGLMp$RCO324_p) <- "Preference for Partisan District Attorney"
label(daGLMp$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(daGLMp$educ.int.f) <- "Education"
label(daGLMp$perceive_extreme_very.f) <- "Outparty Extremity"
label(daGLMp$polint.f) <- "News Interest"


###################################################

## U.S. House ##

houseGLMp <- cces2.2

houseGLMp$pid.f <- as.factor(houseGLMp$pid)
houseGLMp$polint.f <- as.factor(houseGLMp$polint)
houseGLMp$perceive_extreme_very.f <- as.factor(houseGLMp$perceive_extreme_very)
houseGLMp$educ.int.f <- as.factor(houseGLMp$educ.int)
houseGLMp$Pres_Vote.f <- as.factor(houseGLMp$Pres_Vote)

## Change Labels ##

houseGLMp$polint.f <- 
  factor(houseGLMp$polint.f, levels=c("1","2","3","4"),
         labels=c("News: Hardly at all", 
                  "News: Now and then",
                  "News: Some of the time",
                  "News: Most of the time"))

houseGLMp$perceive_extreme_very.f <- 
  factor(houseGLMp$perceive_extreme_very.f, levels=c("0","1"),
         labels=c("Not extreme", 
                  "Outparty is extreme"))

houseGLMp$educ.int.f <- 
  factor(houseGLMp$educ.int, levels=c("1","2","3","4","5"),
         labels=c("H.S. or less", 
                  "Some College",
                  "2-Year Degree",
                  "4-Year Degree",
                  "Post-Graduate"))

houseGLMp$Pres_Vote.f <- 
  factor(houseGLMp$Pres_Vote.f, 
         levels = c("Solid Democrat", 
                    "Lean Democrat",
                    "Lean Republican",
                    "Solid Republican"),
         labels=c("Solid Democrat", 
                  "Lean Democrat",
                  "Lean Republican",
                  "Solid Republican"))


label(houseGLMp$pid.f) <- "Party"
label(houseGLMp$RCO327_p) <- "Preference for Partisan U.S. House"
label(houseGLMp$Pres_Vote.f) <- "County 2016 Pres. Vote"
label(houseGLMp$educ.int.f) <- "Education"
label(houseGLMp$perceive_extreme_very.f) <- "Outparty Extremity"
label(houseGLMp$polint.f) <- "News Interest"


########################################################################
########################################################################
########################################################################
########################################################################

# Replication code for Tables and Figures in main paper #

########################################################################
########################################################################
########################################################################
########################################################################

# Code below assumes that 'r_script2' has been run and resulting datasets are loaded in environment #

library("readxl")
library(sm)
library(pastecs)
library(psych)
library(dplyr)
library("ggpubr")
library("gplots")
library(rstatix)
library(ggplot2)
library(jtools)
library(stargazer)
library(devtools)
library(sjmisc)
library(sjPlot)
library(table1)
library(boot)
library(expss)
library(summarytools)
library(tidyverse)
library(expss)

##################################################################

# Table 1: Descriptive Statistics, CES Module, 2018+2020 (pooled) #

##################################################################


#re-name data for organization purposes
desc.stats <-cces2.2

desc.stats2 <- desc.stats %>% select(pid, educ.int,
                                     perceive_extreme_very,
                                     polint, Pres_Vote, 
                                     cces.year)

desc.stats2 = apply_labels(desc.stats2,
                           pid = "Party (inc. leaners)",
                           educ.int = "Education (highest degree)",
                           perceive_extreme_very = "Out-party is extreme",
                           polint = "News Interest",
                           Pres_Vote = "2016 County Pres. Vote",
                           cces.year = "CCES Year")

dfs <- (dfSummary(desc.stats2, plain.ascii = FALSE, style = "grid", 
                  graph.magnif = 0.75, missing.col = FALSE))

dfs

##################################################################
# summarySE function (for barplots) #
##################################################################
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

##################################################################

# Figure 1. Proportion of Democrats and Republicans choosing "nonpartisan election" #

##################################################################
#sb

sbPlot <- cces2.2
sbPlot$pid <- factor(sbPlot$pid)
sbPlot <- summarySE(cces2.2, measurevar="RCO322_np", groupvars=c("pid"), na.rm=TRUE)
sbPlot$office <- "school board"
sbPlot$proportion <- sbPlot$RCO322_np
sbPlot$RCO322_np <- NULL

sb_party_Plot <- ggplot(sbPlot, aes(x=pid, y=proportion, fill=pid)) 
sb_party_Plot + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#leo

leoPlot <- cces2.2
leoPlot$pid <- factor(leoPlot$pid)
leoPlot <- summarySE(cces2.2, measurevar="RCO321_np", groupvars=c("pid"), na.rm=TRUE)
leoPlot$office <- "local election official"
leoPlot$proportion <- leoPlot$RCO321_np
leoPlot$RCO321_np <- NULL

leo_party_Plot <- ggplot(leoPlot, aes(x=pid, y=proportion, fill=pid)) 
leo_party_Plot + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#sheriff

sherPlot <- cces2.2
sherPlot$pid <- factor(sherPlot$pid)
sherPlot <- summarySE(cces2.2, measurevar="RCO323_np", groupvars=c("pid"), na.rm=TRUE)
sherPlot$office <- "sheriff"
sherPlot$proportion <- sherPlot$RCO323_np
sherPlot$RCO323_np <- NULL

sher_party_Plot <- ggplot(sherPlot, aes(x=pid, y=proportion, fill=pid)) 
sher_party_Plot + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#da

daPlot <- cces2.2
daPlot$pid <- factor(daPlot$pid)
daPlot <- summarySE(cces2.2, measurevar="RCO324_np", groupvars=c("pid"), na.rm=TRUE)
daPlot$office <- "district attorney"
daPlot$proportion <- daPlot$RCO324_np
daPlot$RCO324_np <- NULL

da_party_Plot <- ggplot(daPlot, aes(x=pid, y=proportion, fill=pid)) 
da_party_Plot + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#House

housePlot <- cces2.2
housePlot$pid <- factor(housePlot$pid)
housePlot <- summarySE(cces2.2, measurevar="RCO327_np", groupvars=c("pid"), na.rm=TRUE)
housePlot$office <- "u.s. house"
housePlot$proportion <- housePlot$RCO327_np
housePlot$RCO327_np <- NULL

house_party_Plot <- ggplot(housePlot, aes(x=pid, y=proportion, fill=pid)) 
house_party_Plot + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()


### Combining dataframes ##

localofficePlot <- rbind(sbPlot, leoPlot, sherPlot, daPlot, housePlot)

localofficePlot$office <- ordered(localofficePlot$office, 
                                  levels = c("school board", "sheriff",
                                             "district attorney", 
                                             "local election official",
                                             "u.s. house"))

local_np_partyplot <- ggplot(localofficePlot, aes(x=office, y=proportion, fill=pid)) + 
  xlab("Office") +
  ylab("Proportion") +
  scale_fill_manual(values=c("blue", "red"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  ggtitle("Preference for Nonpartisan elections") +
  ylim(0,1)+
  theme_bw()

local_np_partyplot

##################################################################

# Figure 2. Proportion of Democrats and Republicans choosing "partisan election" #

##################################################################

sbPlotP <- cces2.2
sbPlotP$pid <- factor(sbPlotP$pid)
sbPlotP <- summarySE(cces2.2, measurevar="RCO322_p", groupvars=c("pid"), na.rm=TRUE)
sbPlotP$office <- "school board"
sbPlotP$proportion <- sbPlotP$RCO322_p
sbPlotP$RCO322_p <- NULL

sb_party_PlotP <- ggplot(sbPlotP, aes(x=pid, y=proportion, fill=pid)) 
sb_party_PlotP + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#leo

leoPlotP <- cces2.2
leoPlotP$pid <- factor(leoPlotP$pid)
leoPlotP <- summarySE(cces2.2, measurevar="RCO321_p", groupvars=c("pid"), na.rm=TRUE)
leoPlotP$office <- "local election official"
leoPlotP$proportion <- leoPlotP$RCO321_p
leoPlotP$RCO321_p <- NULL

leo_party_PlotP <- ggplot(leoPlotP, aes(x=pid, y=proportion, fill=pid)) 
leo_party_PlotP + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#sheriff

sherPlotP <- cces2.2
sherPlotP$pid <- factor(sherPlotP$pid)
sherPlotP <- summarySE(cces2.2, measurevar="RCO323_p", groupvars=c("pid"), na.rm=TRUE)
sherPlotP$office <- "sheriff"
sherPlotP$proportion <- sherPlotP$RCO323_p
sherPlotP$RCO323_p <- NULL

sher_party_PlotP <- ggplot(sherPlotP, aes(x=pid, y=proportion, fill=pid)) 
sher_party_PlotP + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#da

daPlotP <- cces2.2
daPlotP$pid <- factor(daPlotP$pid)
daPlotP <- summarySE(cces2.2, measurevar="RCO324_p", groupvars=c("pid"), na.rm=TRUE)
daPlotP$office <- "district attorney"
daPlotP$proportion <- daPlotP$RCO324_p
daPlotP$RCO324_p <- NULL

da_party_PlotP <- ggplot(daPlotP, aes(x=pid, y=proportion, fill=pid)) 
da_party_PlotP + 
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

#house

housePlotP <- cces2.2
housePlotP$pid <- factor(housePlotP$pid)
housePlotP <- summarySE(cces2.2, measurevar="RCO327_p", groupvars=c("pid"), na.rm=TRUE)
housePlotP$office <- "u.s. house"
housePlotP$proportion <- housePlotP$RCO327_p
housePlotP$RCO327_p <- NULL

house_party_PlotP <- ggplot(housePlotP, aes(x=pid, y=proportion, fill=pid)) 
house_party_PlotP + 
  scale_fill_manual(values=c("blue", "red"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("") +
  scale_x_discrete(breaks=c("Democrat","Republican"),
                   labels=c("", "")) +
  ylab("proportion") +
  ylim(0,0.8) +
  ggtitle("") +
  theme_bw()

### Combining dataframes ##

localofficePlotP <- rbind(sbPlotP, leoPlotP, sherPlotP, daPlotP, housePlotP)
localofficePlotP$office <- ordered(localofficePlotP$office, 
                                   levels = c("school board", "sheriff",
                                              "district attorney", 
                                              "local election official",
                                              "u.s. house"))

local_p_partyplot <- ggplot(localofficePlotP, aes(x=office, y=proportion, fill=pid)) + 
  xlab("Office") +
  ylab("Proportion") +
  scale_fill_manual(values=c("blue", "red"),
                    name="Party", # Legend label, use darker colors
                    breaks=c("Democrat", "Republican"),
                    labels=c("Democrat", "Republican")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  ggtitle("Preference for Partisan elections") +
  ylim(0,1)+
  theme_bw()

local_p_partyplot

##################################################################

# Figure 3. Inter-party differences #

##################################################################

sbContrast <- cces2.2
sbContrast <- subset(sbContrast, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

sbContrast$pid <- factor(sbContrast$pid)
sbContrastD <- subset(sbContrast, pid=="Democrat")
sbContrastR <- subset(sbContrast, pid=="Republican")

sbContrastD <- summarySE(sbContrastD, measurevar="RCO322_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
sbContrastD$office <- "school board"
sbContrastD$party <- "Democrat"
sbContrastD$proportion <- sbContrastD$RCO322_np
sbContrastD$RCO322_np <- NULL


sbContrastR <- summarySE(sbContrastR, measurevar="RCO322_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
sbContrastR$office <- "school board"
sbContrastR$party <- "Republican"
sbContrastR$proportion <- sbContrastR$RCO322_np
sbContrastR$RCO322_np <- NULL

sbContrastDR <- rbind(sbContrastD, sbContrastR)
sbContrastDR$Pres_Vote <- ordered(sbContrastDR$Pres_Vote, 
                                  levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
sbCD <- ggplot(sbContrastDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) +
  ggtitle("School Board") +
  ylim(0,1) +                        
  theme_bw() 

# leo #
leoContrast <- cces2.2
leoContrast <- subset(leoContrast, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

leoContrast$pid <- factor(leoContrast$pid)
leoContrastD <- subset(leoContrast, pid=="Democrat")
leoContrastR <- subset(leoContrast, pid=="Republican")

leoContrastD <- summarySE(leoContrastD, measurevar="RCO321_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
leoContrastD$office <- "local election official"
leoContrastD$party <- "Democrat"
leoContrastD$proportion <- leoContrastD$RCO321_np
leoContrastD$RCO321_np <- NULL


leoContrastR <- summarySE(leoContrastR, measurevar="RCO321_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
leoContrastR$office <- "local election official"
leoContrastR$party <- "Republican"
leoContrastR$proportion <- leoContrastR$RCO321_np
leoContrastR$RCO321_np <- NULL

leoContrastDR <- rbind(leoContrastD, leoContrastR)
leoContrastDR$Pres_Vote <- ordered(leoContrastDR$Pres_Vote, 
                                   levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
leoCD <- ggplot(leoContrastDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + # Use darker colors, lightness=40
  ggtitle("Local Election Official") +
  ylim(0,1) +                        # Expand y range        # Set tick every 4
  theme_bw() 


# sheriff #
sherContrast <- cces2.2
sherContrast <- subset(leoContrast, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

sherContrast$pid <- factor(sherContrast$pid)
sherContrastD <- subset(sherContrast, pid=="Democrat")
sherContrastR <- subset(sherContrast, pid=="Republican")

sherContrastD <- summarySE(sherContrastD, measurevar="RCO323_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
sherContrastD$office <- "sheriff"
sherContrastD$party <- "Democrat"
sherContrastD$proportion <- sherContrastD$RCO323_np
sherContrastD$RCO323_np <- NULL


sherContrastR <- summarySE(sherContrastR, measurevar="RCO323_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
sherContrastR$office <- "sheriff"
sherContrastR$party <- "Republican"
sherContrastR$proportion <- sherContrastR$RCO323_np
sherContrastR$RCO323_np <- NULL

sherContrastDR <- rbind(sherContrastD, sherContrastR)
sherContrastDR$Pres_Vote <- ordered(sherContrastDR$Pres_Vote, 
                                    levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
sherCD <- ggplot(sherContrastDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +  
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("Sheriff") +
  ylim(0,1) +                       
  theme_bw() 


# D.A. #
daContrast <- cces2.2
daContrast <- subset(daContrast, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

daContrast$pid <- factor(daContrast$pid)
daContrastD <- subset(daContrast, pid=="Democrat")
daContrastR <- subset(daContrast, pid=="Republican")

daContrastD <- summarySE(daContrastD, measurevar="RCO324_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
daContrastD$office <- "d.a."
daContrastD$party <- "Democrat"
daContrastD$proportion <- daContrastD$RCO324_np
daContrastD$RCO324_np <- NULL


daContrastR <- summarySE(daContrastR, measurevar="RCO324_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
daContrastR$office <- "d.a."
daContrastR$party <- "Republican"
daContrastR$proportion <- daContrastR$RCO324_np
daContrastR$RCO324_np <- NULL

daContrastDR <- rbind(daContrastD, daContrastR)
daContrastDR$Pres_Vote <- ordered(daContrastDR$Pres_Vote, 
                                  levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
daCD <- ggplot(daContrastDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) + 
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("District Attorney") +
  ylim(0,1) +                        
  theme_bw() 


# U.S. House
houseContrast <- cces2.2
houseContrast <- subset(daContrast, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

houseContrast$pid <- factor(houseContrast$pid)
houseContrastD <- subset(houseContrast, pid=="Democrat")
houseContrastR <- subset(houseContrast, pid=="Republican")

houseContrastD <- summarySE(houseContrastD, measurevar="RCO327_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
houseContrastD$office <- "u.s. house"
houseContrastD$party <- "Democrat"
houseContrastD$proportion <- houseContrastD$RCO327_np
houseContrastD$RCO327_np <- NULL

houseContrastR <- summarySE(houseContrastR, measurevar="RCO327_np", groupvars=c("Pres_Vote"), na.rm=TRUE)
houseContrastR$office <- "u.s. house"
houseContrastR$party <- "Republican"
houseContrastR$proportion <- houseContrastR$RCO327_np
houseContrastR$RCO327_np <- NULL

houseContrastDR <- rbind(houseContrastD, houseContrastR)
houseContrastDR$Pres_Vote <- ordered(houseContrastDR$Pres_Vote, 
                                     levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
houseCD <- ggplot(houseContrastDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +   
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("U.S. House") +
  ylim(0,1) +                       
  theme_bw() 



###### Multiplot for Nonpartisan #####

#extract legend from houseCD (could be any b/c all same legend)
leg <- get_legend(houseCD)

#save earlier plots but remove their legends
sbCD <- sbCD + theme(legend.position = "none")
leoCD <- leoCD + theme(legend.position = "none")
sherCD <- sherCD + theme(legend.position = "none")
daCD <- daCD + theme(legend.position = "none")
houseCD <- houseCD + theme(legend.position = "none")

#arrange plots, and now the extracted legend is a defacto plot (for positioning)
multi_npContrast <- ggarrange(sbCD, leoCD, sherCD,  daCD, houseCD, leg, 
                              ncol = 3, nrow = 2,
                              font.label=list(color="black",size=10))

annotate_figure(multi_npContrast,
                top = text_grob("", 
                                color = "black", face = "bold", size = 14),
                bottom = text_grob("2016 County Presidential Vote", 
                                   color = "black",
                                   hjust = 0.35, vjust = -0.75, size = 12),
                left = text_grob("Proportion", color = "black", rot = 90),
                right = "",
                fig.lab = "", fig.lab.face = "bold"
)

##################################################################

# Table 2. Choosing "Nonpartisan election" as preferred selection method by county-level presidential vote #

##################################################################

# Inter-party Diffs, Stratified by County, Showing only highest-level factor coefs
# Re-Running Basic models and Full, Stratified by PresVote, export tables only showing highest level

Basic.inter.sb.SolidD <- glm(RCO322_np ~ 
                               pid,
                             family="binomial",
                             data = subset(sbMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.sb.SolidD <- glm(RCO322_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(sbMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Basic.inter.sb.LeanD <- glm(RCO322_np ~ 
                              pid,
                            family="binomial",
                            data = subset(sbMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.sb.LeanD <- glm(RCO322_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(sbMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Basic.inter.sb.LeanR <- glm(RCO322_np ~ 
                              pid,
                            family="binomial",
                            data = subset(sbMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.sb.LeanR <- glm(RCO322_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(sbMarginsNP2, Pres_Vote.f=="Lean Republican"))

Basic.inter.sb.SolidR <- glm(RCO322_np ~ 
                               pid,
                             family="binomial",
                             data = subset(sbMarginsNP2, Pres_Vote.f=="Solid Republican"))

Finter.sb.SolidR <- glm(RCO322_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(sbMarginsNP2, Pres_Vote.f=="Solid Republican"))

pl <- c(
  `pidRepublican` = "Party ID",
  `polint.f4` = "News: Most of the time",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme")

tab_model(Basic.inter.sb.SolidD, Finter.sb.SolidD, Basic.inter.sb.LeanD, Finter.sb.LeanD, 
          Basic.inter.sb.LeanR, Finter.sb.LeanR, Basic.inter.sb.SolidR, Finter.sb.SolidR, 
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","","Lean Democrat","",
                        "Lean Republican","","Solid Republican",""),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)


# LEO #

Basic.inter.leo.SolidD <- glm(RCO321_np ~ 
                                pid,
                              family="binomial",
                              data = subset(leoMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.leo.SolidD <- glm(RCO321_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(leoMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Basic.inter.leo.LeanD <- glm(RCO321_np ~ 
                               pid,
                             family="binomial",
                             data = subset(leoMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.leo.LeanD <- glm(RCO321_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(leoMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Basic.inter.leo.LeanR <- glm(RCO321_np ~ 
                               pid,
                             family="binomial",
                             data = subset(leoMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.leo.LeanR <- glm(RCO321_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(leoMarginsNP2, Pres_Vote.f=="Lean Republican"))

Basic.inter.leo.SolidR <- glm(RCO321_np ~ 
                                pid,
                              family="binomial",
                              data = subset(leoMarginsNP2, Pres_Vote.f=="Solid Republican"))

Finter.leo.SolidR <- glm(RCO321_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(leoMarginsNP2, Pres_Vote.f=="Solid Republican"))

pl <- c(
  `pidRepublican` = "Party ID",
  `polint.f4` = "News: Most of the time",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme")

tab_model(Basic.inter.leo.SolidD, Finter.leo.SolidD, Basic.inter.leo.LeanD, Finter.leo.LeanD, 
          Basic.inter.leo.LeanR, Finter.leo.LeanR, Basic.inter.leo.SolidR, Finter.leo.SolidR, 
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","","Lean Democrat","",
                        "Lean Republican","","Solid Republican",""),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)


# sheriff #

Basic.inter.sher.SolidD <- glm(RCO323_np ~ 
                                 pid,
                               family="binomial",
                               data = subset(sherMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.sher.SolidD <- glm(RCO323_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(sherMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Basic.inter.sher.LeanD <- glm(RCO323_np ~ 
                                pid,
                              family="binomial",
                              data = subset(sherMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.sher.LeanD <- glm(RCO323_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(sherMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Basic.inter.sher.LeanR <- glm(RCO323_np ~ 
                                pid,
                              family="binomial",
                              data = subset(sherMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.sher.LeanR <- glm(RCO323_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(sherMarginsNP2, Pres_Vote.f=="Lean Republican"))

Basic.inter.sher.SolidR <- glm(RCO323_np ~ 
                                 pid,
                               family="binomial",
                               data = subset(sherMarginsNP2, Pres_Vote.f=="Solid Republican"))

Finter.sher.SolidR <- glm(RCO323_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(sherMarginsNP2, Pres_Vote.f=="Solid Republican"))

pl <- c(
  `pidRepublican` = "Party ID",
  `polint.f4` = "News: Most of the time",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme")

tab_model(Basic.inter.sher.SolidD, Finter.sher.SolidD, Basic.inter.sher.LeanD, Finter.sher.LeanD, 
          Basic.inter.sher.LeanR, Finter.sher.LeanR, Basic.inter.sher.SolidR, Finter.sher.SolidR, 
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","","Lean Democrat","",
                        "Lean Republican","","Solid Republican",""),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)


# d.a. #

Basic.inter.da.SolidD <- glm(RCO324_np ~ 
                               pid,
                             family="binomial",
                             data = subset(daMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.da.SolidD <- glm(RCO324_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(daMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Basic.inter.da.LeanD <- glm(RCO324_np ~ 
                              pid,
                            family="binomial",
                            data = subset(daMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.da.LeanD <- glm(RCO324_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(daMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Basic.inter.da.LeanR <- glm(RCO324_np ~ 
                              pid,
                            family="binomial",
                            data = subset(daMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.da.LeanR <- glm(RCO324_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(daMarginsNP2, Pres_Vote.f=="Lean Republican"))

Basic.inter.da.SolidR <- glm(RCO324_np ~ 
                               pid,
                             family="binomial",
                             data = subset(daMarginsNP2, Pres_Vote.f=="Solid Republican"))

Finter.da.SolidR <- glm(RCO324_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(daMarginsNP2, Pres_Vote.f=="Solid Republican"))

pl <- c(
  `pidRepublican` = "Party ID",
  `polint.f4` = "News: Most of the time",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme")

tab_model(Basic.inter.da.SolidD, Finter.da.SolidD, Basic.inter.da.LeanD, Finter.da.LeanD, 
          Basic.inter.da.LeanR, Finter.da.LeanR, Basic.inter.da.SolidR, Finter.da.SolidR, 
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","","Lean Democrat","",
                        "Lean Republican","","Solid Republican",""),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)


# House #

Basic.inter.house.SolidD <- glm(RCO327_np ~ 
                                  pid,
                                family="binomial",
                                data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.house.SolidD <- glm(RCO327_np ~ 
                             pid +
                             perceive_extreme_very.f +
                             polint.f +
                             educ.int.f +
                             birthyr,
                           family="binomial",
                           data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Basic.inter.house.LeanD <- glm(RCO327_np ~ 
                                 pid,
                               family="binomial",
                               data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.house.LeanD <- glm(RCO327_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Basic.inter.house.LeanR <- glm(RCO327_np ~ 
                                 pid,
                               family="binomial",
                               data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.house.LeanR <- glm(RCO327_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Republican"))

Basic.inter.house.SolidR <- glm(RCO327_np ~ 
                                  pid,
                                family="binomial",
                                data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Republican"))

Finter.house.SolidR <- glm(RCO327_np ~ 
                             pid +
                             perceive_extreme_very.f +
                             polint.f +
                             educ.int.f +
                             birthyr,
                           family="binomial",
                           data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Republican"))

pl <- c(
  `pidRepublican` = "Party ID",
  `polint.f4` = "News: Most of the time",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme")

tab_model(Basic.inter.house.SolidD, Finter.house.SolidD, Basic.inter.house.LeanD, Finter.house.LeanD, 
          Basic.inter.house.LeanR, Finter.house.LeanR, Basic.inter.house.SolidR, Finter.house.SolidR, 
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","","Lean Democrat","",
                        "Lean Republican","","Solid Republican",""),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)

##################################################################

### Figure 4. Inter-party differences in predicted probability of choosing nonpartisan elections.

##################################################################

Finter.sb.SolidD <- glm(RCO322_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(sbMarginsNP2, Pres_Vote.f=="Solid Democrat"))
#
Finter.sb.LeanD <- glm(RCO322_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(sbMarginsNP2, Pres_Vote.f=="Lean Democrat"))
#
Finter.sb.LeanR <- glm(RCO322_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(sbMarginsNP2, Pres_Vote.f=="Lean Republican"))
#
Finter.sb.SolidR <- glm(RCO322_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(sbMarginsNP2, Pres_Vote.f=="Solid Republican"))

# Using margins() we calculate the marginal effects for each variable:

sbInter.mSolidD <- margins(Finter.sb.SolidD)
sbInter.mLeanD <- margins(Finter.sb.LeanD)
sbInter.mLeanR <- margins(Finter.sb.LeanR)
sbInter.mSolidR <- margins(Finter.sb.SolidR)

#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

bo_gg_SolidD <- as_tibble(summary(sbInter.mSolidD))
bo_gg_LeanD <- as_tibble(summary(sbInter.mLeanD))
bo_gg_LeanR <- as_tibble(summary(sbInter.mLeanR))
bo_gg_SolidR <- as_tibble(summary(sbInter.mSolidR))

bo_gg_SolidD %>% select(factor, AME, lower, upper) 
bo_gg_LeanD %>% select(factor, AME, lower, upper) 
bo_gg_LeanR %>% select(factor, AME, lower, upper) 
bo_gg_SolidR %>% select(factor, AME, lower, upper)

## Subset further to plot only certain factors ##

bo_gg_SolidD_2 <- subset(bo_gg_SolidD, 
                         factor=="pidRepublican")
bo_gg_SolidD_2$County = "Solid Democrat"
bo_gg_SolidD_2$factor <- as.factor(bo_gg_SolidD_2$factor)
bo_gg_SolidD_2$var1 <- "Republican"

bo_gg_LeanD_2 <- subset(bo_gg_LeanD, 
                        factor=="pidRepublican")
bo_gg_LeanD_2$County = "Lean Democrat"
bo_gg_LeanD_2$factor <- as.factor(bo_gg_LeanD_2$factor)
bo_gg_LeanD_2$var1 <- "Republican"

bo_gg_LeanR_2 <- subset(bo_gg_LeanR, 
                        factor=="pidRepublican")
bo_gg_LeanR_2$County = "Lean Republican"
bo_gg_LeanR_2$factor <- as.factor(bo_gg_LeanR_2$factor)
bo_gg_LeanR_2$var1 <- "Republican"

bo_gg_SolidR_2 <- subset(bo_gg_SolidR, 
                         factor=="pidRepublican")
bo_gg_SolidR_2$County = "Solid Republican"
bo_gg_SolidR_2$factor <- as.factor(bo_gg_SolidR_2$factor)
bo_gg_SolidR_2$var1 <- "Republican"


# Combine Plots #
pd <- position_dodge(0.25)
bo_gg_SB_Combine <- rbind(bo_gg_SolidD_2, bo_gg_LeanD_2, bo_gg_LeanR_2, bo_gg_SolidR_2)
bo_gg_SB_Combine$Office <- "School Board"

ord <- c("Solid Democrat",
         "Lean Democrat",
         "Lean Republican",
         "Solid Republican")

# Correct Order for Plotting
bo_gg_SB_Combine$County <- factor(bo_gg_SB_Combine$County,levels=ord)

## NOW: Repeat above for each office, then RBIND into one big dataframe

Finter.leo.SolidD <- glm(RCO321_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(leoMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.leo.LeanD <- glm(RCO321_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(leoMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.leo.LeanR <- glm(RCO321_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(leoMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.leo.SolidR <- glm(RCO321_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(leoMarginsNP2, Pres_Vote.f=="Solid Republican"))

# Using margins() we calculate the marginal effects for each variable:

leoInter.mSolidD <- margins(Finter.leo.SolidD)
leoInter.mLeanD <- margins(Finter.leo.LeanD)
leoInter.mLeanR <- margins(Finter.leo.LeanR)
leoInter.mSolidR <- margins(Finter.leo.SolidR)

#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

LEObo_gg_SolidD <- as_tibble(summary(leoInter.mSolidD))
LEObo_gg_LeanD <- as_tibble(summary(leoInter.mLeanD))
LEObo_gg_LeanR <- as_tibble(summary(leoInter.mLeanR))
LEObo_gg_SolidR <- as_tibble(summary(leoInter.mSolidR))

LEObo_gg_SolidD %>% select(factor, AME, lower, upper) 
LEObo_gg_LeanD %>% select(factor, AME, lower, upper) 
LEObo_gg_LeanR %>% select(factor, AME, lower, upper) 
LEObo_gg_SolidR %>% select(factor, AME, lower, upper)

## Subset further to plot only certain factors ##

LEObo_gg_SolidD_2 <- subset(LEObo_gg_SolidD, 
                            factor=="pidRepublican")
LEObo_gg_SolidD_2$County = "Solid Democrat"
LEObo_gg_SolidD_2$factor <- as.factor(LEObo_gg_SolidD_2$factor)
LEObo_gg_SolidD_2$var1 <- "Republican"

LEObo_gg_LeanD_2 <- subset(LEObo_gg_LeanD, 
                           factor=="pidRepublican")
LEObo_gg_LeanD_2$County = "Lean Democrat"
LEObo_gg_LeanD_2$factor <- as.factor(LEObo_gg_LeanD_2$factor)
LEObo_gg_LeanD_2$var1 <- "Republican"

LEObo_gg_LeanR_2 <- subset(LEObo_gg_LeanR, 
                           factor=="pidRepublican")
LEObo_gg_LeanR_2$County = "Lean Republican"
LEObo_gg_LeanR_2$factor <- as.factor(LEObo_gg_LeanR_2$factor)
LEObo_gg_LeanR_2$var1 <- "Republican"

LEObo_gg_SolidR_2 <- subset(LEObo_gg_SolidR, 
                            factor=="pidRepublican")
LEObo_gg_SolidR_2$County = "Solid Republican"
LEObo_gg_SolidR_2$factor <- as.factor(LEObo_gg_SolidR_2$factor)
LEObo_gg_SolidR_2$var1 <- "Republican"

bo_gg_LEO_Combine <- rbind(LEObo_gg_SolidD_2, LEObo_gg_LeanD_2, LEObo_gg_LeanR_2, LEObo_gg_SolidR_2)
bo_gg_LEO_Combine$Office <- "Local Election Official"

############## ########### ###########

Finter.sher.SolidD <- glm(RCO323_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(sherMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.sher.LeanD <- glm(RCO323_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(sherMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.sher.LeanR <- glm(RCO323_np ~ 
                           pid +
                           perceive_extreme_very.f +
                           polint.f +
                           educ.int.f +
                           birthyr,
                         family="binomial",
                         data = subset(sherMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.sher.SolidR <- glm(RCO323_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(sherMarginsNP2, Pres_Vote.f=="Solid Republican"))

# Using margins() we calculate the marginal effects for each variable:

sherInter.mSolidD <- margins(Finter.sher.SolidD)
sherInter.mLeanD <- margins(Finter.sher.LeanD)
sherInter.mLeanR <- margins(Finter.sher.LeanR)
sherInter.mSolidR <- margins(Finter.sher.SolidR)

#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

sherbo_gg_SolidD <- as_tibble(summary(sherInter.mSolidD))
sherbo_gg_LeanD <- as_tibble(summary(sherInter.mLeanD))
sherbo_gg_LeanR <- as_tibble(summary(sherInter.mLeanR))
sherbo_gg_SolidR <- as_tibble(summary(sherInter.mSolidR))

sherbo_gg_SolidD %>% select(factor, AME, lower, upper) 
sherbo_gg_LeanD %>% select(factor, AME, lower, upper) 
sherbo_gg_LeanR %>% select(factor, AME, lower, upper) 
sherbo_gg_SolidR %>% select(factor, AME, lower, upper)

## Subset further to plot only certain factors ##

sherbo_gg_SolidD_2 <- subset(sherbo_gg_SolidD, 
                             factor=="pidRepublican")
sherbo_gg_SolidD_2$County = "Solid Democrat"
sherbo_gg_SolidD_2$factor <- as.factor(sherbo_gg_SolidD_2$factor)
sherbo_gg_SolidD_2$var1 <- "Republican"

sherbo_gg_LeanD_2 <- subset(sherbo_gg_LeanD, 
                            factor=="pidRepublican")
sherbo_gg_LeanD_2$County = "Lean Democrat"
sherbo_gg_LeanD_2$factor <- as.factor(sherbo_gg_LeanD_2$factor)
sherbo_gg_LeanD_2$var1 <- "Republican"

sherbo_gg_LeanR_2 <- subset(sherbo_gg_LeanR, 
                            factor=="pidRepublican")
sherbo_gg_LeanR_2$County = "Lean Republican"
sherbo_gg_LeanR_2$factor <- as.factor(sherbo_gg_LeanR_2$factor)
sherbo_gg_LeanR_2$var1 <- "Republican"

sherbo_gg_SolidR_2 <- subset(sherbo_gg_SolidR, 
                             factor=="pidRepublican")
sherbo_gg_SolidR_2$County = "Solid Republican"
sherbo_gg_SolidR_2$factor <- as.factor(sherbo_gg_SolidR_2$factor)
sherbo_gg_SolidR_2$var1 <- "Republican"

bo_gg_sher_Combine <- rbind(sherbo_gg_SolidD_2, sherbo_gg_LeanD_2, sherbo_gg_LeanR_2, sherbo_gg_SolidR_2)
bo_gg_sher_Combine$Office <- "Sheriff"

############## ########### ###########

Finter.da.SolidD <- glm(RCO324_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(daMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.da.LeanD <- glm(RCO324_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(daMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.da.LeanR <- glm(RCO324_np ~ 
                         pid +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(daMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.da.SolidR <- glm(RCO324_np ~ 
                          pid +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(daMarginsNP2, Pres_Vote.f=="Solid Republican"))

# Using margins() we calculate the marginal effects for each variable:

daInter.mSolidD <- margins(Finter.da.SolidD)
daInter.mLeanD <- margins(Finter.da.LeanD)
daInter.mLeanR <- margins(Finter.da.LeanR)
daInter.mSolidR <- margins(Finter.da.SolidR)

#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

dabo_gg_SolidD <- as_tibble(summary(daInter.mSolidD))
dabo_gg_LeanD <- as_tibble(summary(daInter.mLeanD))
dabo_gg_LeanR <- as_tibble(summary(daInter.mLeanR))
dabo_gg_SolidR <- as_tibble(summary(daInter.mSolidR))

dabo_gg_SolidD %>% select(factor, AME, lower, upper) 
dabo_gg_LeanD %>% select(factor, AME, lower, upper) 
dabo_gg_LeanR %>% select(factor, AME, lower, upper) 
dabo_gg_SolidR %>% select(factor, AME, lower, upper)

## Subset further to plot only certain factors ##

dabo_gg_SolidD_2 <- subset(dabo_gg_SolidD, 
                           factor=="pidRepublican")
dabo_gg_SolidD_2$County = "Solid Democrat"
dabo_gg_SolidD_2$factor <- as.factor(dabo_gg_SolidD_2$factor)
dabo_gg_SolidD_2$var1 <- "Republican"

dabo_gg_LeanD_2 <- subset(dabo_gg_LeanD, 
                          factor=="pidRepublican")
dabo_gg_LeanD_2$County = "Lean Democrat"
dabo_gg_LeanD_2$factor <- as.factor(dabo_gg_LeanD_2$factor)
dabo_gg_LeanD_2$var1 <- "Republican"

dabo_gg_LeanR_2 <- subset(dabo_gg_LeanR, 
                          factor=="pidRepublican")
dabo_gg_LeanR_2$County = "Lean Republican"
dabo_gg_LeanR_2$factor <- as.factor(dabo_gg_LeanR_2$factor)
dabo_gg_LeanR_2$var1 <- "Republican"

dabo_gg_SolidR_2 <- subset(dabo_gg_SolidR, 
                           factor=="pidRepublican")
dabo_gg_SolidR_2$County = "Solid Republican"
dabo_gg_SolidR_2$factor <- as.factor(dabo_gg_SolidR_2$factor)
dabo_gg_SolidR_2$var1 <- "Republican"

bo_gg_da_Combine <- rbind(dabo_gg_SolidD_2, dabo_gg_LeanD_2, dabo_gg_LeanR_2, dabo_gg_SolidR_2)
bo_gg_da_Combine$Office <- "District Attorney"

############## ########### ###########

Finter.house.SolidD <- glm(RCO327_np ~ 
                             pid +
                             perceive_extreme_very.f +
                             polint.f +
                             educ.int.f +
                             birthyr,
                           family="binomial",
                           data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.house.LeanD <- glm(RCO327_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.house.LeanR <- glm(RCO327_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.house.SolidR <- glm(RCO327_np ~ 
                             pid +
                             perceive_extreme_very.f +
                             polint.f +
                             educ.int.f +
                             birthyr,
                           family="binomial",
                           data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Republican"))

# Using margins() we calculate the marginal effects for each variable:

houseInter.mSolidD <- margins(Finter.house.SolidD)
houseInter.mLeanD <- margins(Finter.house.LeanD)
houseInter.mLeanR <- margins(Finter.house.LeanR)
houseInter.mSolidR <- margins(Finter.house.SolidR)

#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

housebo_gg_SolidD <- as_tibble(summary(houseInter.mSolidD))
housebo_gg_LeanD <- as_tibble(summary(houseInter.mLeanD))
housebo_gg_LeanR <- as_tibble(summary(houseInter.mLeanR))
housebo_gg_SolidR <- as_tibble(summary(houseInter.mSolidR))

housebo_gg_SolidD %>% select(factor, AME, lower, upper) 
housebo_gg_LeanD %>% select(factor, AME, lower, upper) 
housebo_gg_LeanR %>% select(factor, AME, lower, upper) 
housebo_gg_SolidR %>% select(factor, AME, lower, upper)

## Subset further to plot only certain factors ##

housebo_gg_SolidD_2 <- subset(housebo_gg_SolidD, 
                              factor=="pidRepublican")
housebo_gg_SolidD_2$County = "Solid Democrat"
housebo_gg_SolidD_2$factor <- as.factor(housebo_gg_SolidD_2$factor)
housebo_gg_SolidD_2$var1 <- "Republican"

housebo_gg_LeanD_2 <- subset(housebo_gg_LeanD, 
                             factor=="pidRepublican")
housebo_gg_LeanD_2$County = "Lean Democrat"
housebo_gg_LeanD_2$factor <- as.factor(housebo_gg_LeanD_2$factor)
housebo_gg_LeanD_2$var1 <- "Republican"

housebo_gg_LeanR_2 <- subset(housebo_gg_LeanR, 
                             factor=="pidRepublican")
housebo_gg_LeanR_2$County = "Lean Republican"
housebo_gg_LeanR_2$factor <- as.factor(housebo_gg_LeanR_2$factor)
housebo_gg_LeanR_2$var1 <- "Republican"

housebo_gg_SolidR_2 <- subset(housebo_gg_SolidR, 
                              factor=="pidRepublican")
housebo_gg_SolidR_2$County = "Solid Republican"
housebo_gg_SolidR_2$factor <- as.factor(housebo_gg_SolidR_2$factor)
housebo_gg_SolidR_2$var1 <- "Republican"

bo_gg_house_Combine <- rbind(housebo_gg_SolidD_2, housebo_gg_LeanD_2, housebo_gg_LeanR_2, housebo_gg_SolidR_2)
bo_gg_house_Combine$Office <- "U.S. House"

############## ########### ###########
### Combine Plots ###
pd <- position_dodge(0.5)
bo_gg_Inter_Combine <- rbind(bo_gg_SB_Combine, bo_gg_LEO_Combine, 
                             bo_gg_sher_Combine, bo_gg_da_Combine,
                             bo_gg_house_Combine)

ord <- c("Solid Democrat",
         "Lean Democrat",
         "Lean Republican",
         "Solid Republican")

# Correct Order for Plotting
bo_gg_Inter_Combine$County <- factor(bo_gg_Inter_Combine$County,levels=ord)

Inter_Marg_plot <- ggplot(data = bo_gg_Inter_Combine, aes(x = County,
                                                          y = AME, group=Office , ymin = lower, ymax = upper))

Inter_Marg_plot + geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange(aes(colour = Office), position=pd) +
  ylim(-0.35,0.35)+
  labs(x = "\n2016 County Presidential Vote", y = "Average Marginal Effect", col="Office") +
  ggtitle("") 

##################################################################

### Figure 5. Intra-party differences in predicted probability of choosing nonpartisan elections. ###

##################################################################

# Democrats #
# Full Model (SB)
glm.sb2.D <- glm(RCO322_np ~ 
                   perceive_extreme_very.f +
                   polint.f +
                   educ.int.f +
                   birthyr +
                   Pres_Vote.f,
                 family="binomial",
                 data = subset(sbMarginsNP2, pid=="Democrat"))

# Full Model (LEO)
glm.leo2.D <- glm(RCO321_np ~ 
                    perceive_extreme_very.f +
                    polint.f +
                    educ.int.f +
                    birthyr +
                    Pres_Vote.f,
                  family="binomial",
                  data = subset(leoMarginsNP2, pid=="Democrat"))

# Full Model (SHER)
glm.sher2.D <- glm(RCO323_np ~ 
                     perceive_extreme_very.f +
                     polint.f +
                     educ.int.f +
                     birthyr +
                     Pres_Vote.f,
                   family="binomial",
                   data = subset(sherMarginsNP2, pid=="Democrat"))

# Full Model (DA)
glm.da2.D <- glm(RCO324_np ~ 
                   perceive_extreme_very.f +
                   polint.f +
                   educ.int.f +
                   birthyr +
                   Pres_Vote.f,
                 family="binomial",
                 data = subset(daMarginsNP2, pid=="Democrat"))

# Full Model (HOUSE)
glm.house2.D <- glm(RCO327_np ~ 
                      perceive_extreme_very.f +
                      polint.f +
                      educ.int.f +
                      birthyr +
                      Pres_Vote.f,
                    family="binomial",
                    data = subset(houseMarginsNP2, pid=="Democrat"))

### Plotting ###

# Using margins() we calculate the marginal effects for each variable:

sb2.1_mD <- margins(glm.sb2.D)
leo2.1_mD <- margins(glm.leo2.D)
sher2.1_mD <- margins(glm.sher2.D)
da2.1_mD <- margins(glm.da2.D)
house2.1_mD <- margins(glm.house2.D)


#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

bo_gg_D_sb <- as_tibble(summary(sb2.1_mD))
bo_gg_D_leo <- as_tibble(summary(leo2.1_mD))
bo_gg_D_sher <- as_tibble(summary(sher2.1_mD))
bo_gg_D_da <- as_tibble(summary(da2.1_mD))
bo_gg_D_house <- as_tibble(summary(house2.1_mD))

bo_gg_D_sb %>% select(factor, AME, lower, upper) 
bo_gg_D_leo %>% select(factor, AME, lower, upper) 
bo_gg_D_sher %>% select(factor, AME, lower, upper) 
bo_gg_D_da %>% select(factor, AME, lower, upper) 
bo_gg_D_house %>% select(factor, AME, lower, upper) 

ord <- c("birthyr","perceive_extreme_very.f1",
         "educ.int.f2","educ.int.f3","educ.int.f4","educ.int.f5",
         "polint.f2","polint.f3","polint.f4",
         "Pres_Vote.fLean Democrat",
         "Pres_Vote.fLean Republican",
         "Pres_Vote.fSolid Republican")

bo_gg_D_sb$var1 <- as.character(bo_gg_D_sb$factor)
bo_gg_D_sb$var1 <- factor(bo_gg_D_sb$var1,levels=ord)

bo_gg_D_leo$var1 <- as.character(bo_gg_D_leo$factor)
bo_gg_D_leo$var1 <- factor(bo_gg_D_leo$var1,levels=ord)

bo_gg_D_sher$var1 <- as.character(bo_gg_D_sher$factor)
bo_gg_D_sher$var1 <- factor(bo_gg_D_sher$var1,levels=ord)

bo_gg_D_da$var1 <- as.character(bo_gg_D_da$factor)
bo_gg_D_da$var1 <- factor(bo_gg_D_da$var1,levels=ord)

bo_gg_D_house$var1 <- as.character(bo_gg_D_house$factor)
bo_gg_D_house$var1 <- factor(bo_gg_D_house$var1,levels=ord)

# First make a copy
bo_gg_D_sb_2 <- bo_gg_D_sb
bo_gg_D_leo_2 <- bo_gg_D_leo
bo_gg_D_sher_2 <- bo_gg_D_sher
bo_gg_D_da_2 <- bo_gg_D_da
bo_gg_D_house_2 <- bo_gg_D_house

# Copy and paste this complicated code one-by-one
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_D_sb_2$var1)[levels(bo_gg_D_sb_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_D_leo_2$var1)[levels(bo_gg_D_leo_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="educ.int.f5"] <- "Post-Graduate"       
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_D_sher_2$var1)[levels(bo_gg_D_sher_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_D_da_2$var1)[levels(bo_gg_D_da_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_D_house_2$var1)[levels(bo_gg_D_house_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

### Combine Plots ###
pd <- position_dodge(0.3)

## Subset further to plot only certain factors ##

bo_gg_D_sb_3 <- subset(bo_gg_D_sb_2, 
                       factor=="perceive_extreme_very.f1" |
                         factor=="educ.int.f5" |
                         factor=="polint.f4" |
                         factor=="Pres_Vote.fSolid Republican")
bo_gg_D_sb_3$pid = "Democrat"
bo_gg_D_sb_3$Office = "School Board"

bo_gg_D_leo_3 <- subset(bo_gg_D_leo_2, 
                        factor=="perceive_extreme_very.f1" |
                          factor=="educ.int.f5" |
                          factor=="polint.f4" |
                          factor=="Pres_Vote.fSolid Republican")
bo_gg_D_leo_3$pid = "Democrat"
bo_gg_D_leo_3$Office = "Local Election Official"


bo_gg_D_sher_3 <- subset(bo_gg_D_sher_2, 
                         factor=="perceive_extreme_very.f1" |
                           factor=="educ.int.f5" |
                           factor=="polint.f4" |
                           factor=="Pres_Vote.fSolid Republican")
bo_gg_D_sher_3$pid = "Democrat"
bo_gg_D_sher_3$Office = "Sheriff"

bo_gg_D_da_3 <- subset(bo_gg_D_da_2, 
                       factor=="perceive_extreme_very.f1" |
                         factor=="educ.int.f5" |
                         factor=="polint.f4" |
                         factor=="Pres_Vote.fSolid Republican")
bo_gg_D_da_3$pid = "Democrat"
bo_gg_D_da_3$Office = "District Attorney"

bo_gg_D_house_3 <- subset(bo_gg_D_house_2, 
                          factor=="perceive_extreme_very.f1" |
                            factor=="educ.int.f5" |
                            factor=="polint.f4" |
                            factor=="Pres_Vote.fSolid Republican")
bo_gg_D_house_3$pid = "Democrat"
bo_gg_D_house_3$Office = "U.S. House"


np_Intra_D <- rbind(bo_gg_D_sb_3, bo_gg_D_leo_3, bo_gg_D_sher_3,
                    bo_gg_D_da_3, bo_gg_D_house_3)

#####


np_Intra_D_plot <- ggplot(data = np_Intra_D, aes(x = var1,
                                                 y = AME, group=Office , ymin = lower, ymax = upper))

np_Intra_D_plot

np_Intra_D_plot <- np_Intra_D_plot + geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange(aes(colour = Office), position = pd) + 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(x = NULL, y = "Average Marginal Effect", col="Office") +
  ggtitle("Among Democrats") +
  scale_x_discrete(labels=c("County: Solid Republican" = "County:\nSolid Republican", 
                            "News: Most of the time" = "News:\nMost of the time",
                            "Post-Graduate" = "Education:\nPost-Graduate",
                            "Outparty is extreme" = "Outparty:\nis extreme"))


#### REPUBLICANS ####

# Full Model (SB)
glm.sb2.R <- glm(RCO322_np ~ 
                   perceive_extreme_very.f +
                   polint.f +
                   educ.int.f +
                   birthyr +
                   Pres_Vote.f,
                 family="binomial",
                 data = subset(sbMarginsNP2, pid=="Republican"))

# Full Model (LEO)
glm.leo2.R <- glm(RCO321_np ~ 
                    perceive_extreme_very.f +
                    polint.f +
                    educ.int.f +
                    birthyr +
                    Pres_Vote.f,
                  family="binomial",
                  data = subset(leoMarginsNP2, pid=="Republican"))

# Full Model (SHER)
glm.sher2.R <- glm(RCO323_np ~ 
                     perceive_extreme_very.f +
                     polint.f +
                     educ.int.f +
                     birthyr +
                     Pres_Vote.f,
                   family="binomial",
                   data = subset(sherMarginsNP2, pid=="Republican"))

# Full Model (DA)
glm.da2.R <- glm(RCO324_np ~ 
                   perceive_extreme_very.f +
                   polint.f +
                   educ.int.f +
                   birthyr +
                   Pres_Vote.f,
                 family="binomial",
                 data = subset(daMarginsNP2, pid=="Republican"))

# Full Model (HOUSE)
glm.house2.R <- glm(RCO327_np ~ 
                      perceive_extreme_very.f +
                      polint.f +
                      educ.int.f +
                      birthyr +
                      Pres_Vote.f,
                    family="binomial",
                    data = subset(houseMarginsNP2, pid=="Republican"))

### Plotting ###

# Using margins() we calculate the marginal effects for each variable:

sb2.1_mR <- margins(glm.sb2.R)
leo2.1_mR <- margins(glm.leo2.R)
sher2.1_mR <- margins(glm.sher2.R)
da2.1_mR <- margins(glm.da2.R)
house2.1_mR <- margins(glm.house2.R)


#we can take results from margins() and plot them ourselves. 
#To clean up the summary a little a little, we convert it to a tibble, 
#then use prefix_strip() and prefix_replace() to tidy the labels. 

bo_gg_R_sb <- as_tibble(summary(sb2.1_mR))
bo_gg_R_leo <- as_tibble(summary(leo2.1_mR))
bo_gg_R_sher <- as_tibble(summary(sher2.1_mR))
bo_gg_R_da <- as_tibble(summary(da2.1_mR))
bo_gg_R_house <- as_tibble(summary(house2.1_mR))

bo_gg_R_sb %>% select(factor, AME, lower, upper) 
bo_gg_R_leo %>% select(factor, AME, lower, upper) 
bo_gg_R_sher %>% select(factor, AME, lower, upper) 
bo_gg_R_da %>% select(factor, AME, lower, upper) 
bo_gg_R_house %>% select(factor, AME, lower, upper) 

ord <- c("birthyr","perceive_extreme_very.f1",
         "educ.int.f2","educ.int.f3","educ.int.f4","educ.int.f5",
         "polint.f2","polint.f3","polint.f4",
         "Pres_Vote.fLean Democrat",
         "Pres_Vote.fLean Republican",
         "Pres_Vote.fSolid Republican")

bo_gg_R_sb$var1 <- as.character(bo_gg_R_sb$factor)
bo_gg_R_sb$var1 <- factor(bo_gg_R_sb$var1,levels=ord)

bo_gg_R_leo$var1 <- as.character(bo_gg_R_leo$factor)
bo_gg_R_leo$var1 <- factor(bo_gg_R_leo$var1,levels=ord)

bo_gg_R_sher$var1 <- as.character(bo_gg_R_sher$factor)
bo_gg_R_sher$var1 <- factor(bo_gg_R_sher$var1,levels=ord)

bo_gg_R_da$var1 <- as.character(bo_gg_R_da$factor)
bo_gg_R_da$var1 <- factor(bo_gg_R_da$var1,levels=ord)

bo_gg_R_house$var1 <- as.character(bo_gg_R_house$factor)
bo_gg_R_house$var1 <- factor(bo_gg_R_house$var1,levels=ord)

# First make a copy
bo_gg_R_sb_2 <- bo_gg_R_sb
bo_gg_R_leo_2 <- bo_gg_R_leo
bo_gg_R_sher_2 <- bo_gg_R_sher
bo_gg_R_da_2 <- bo_gg_R_da
bo_gg_R_house_2 <- bo_gg_R_house

# Copy and paste this complicated code one-by-one
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_R_sb_2$var1)[levels(bo_gg_R_sb_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_R_leo_2$var1)[levels(bo_gg_R_leo_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_R_sher_2$var1)[levels(bo_gg_R_sher_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_R_da_2$var1)[levels(bo_gg_R_da_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     

# Copy and paste this complicated code one-by-one
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="birthyr"] <- "Birth Year"
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="perceive_extreme_very.f1"] <- "Outparty is extreme"
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="educ.int.f2"] <- "Some College"
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="educ.int.f3"] <- "2-Year Degree"
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="educ.int.f4"] <- "4-Year Degree" 
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="educ.int.f5"] <- "Post-Graduate"        
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="polint.f2"] <- "News: Now and then"     
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="polint.f3"] <- "News: Some of the time"     
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="polint.f4"] <- "News: Most of the time"     
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="Pres_Vote.fLean Democrat"] <- "County: Lean Democrat"     
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="Pres_Vote.fLean Republican"] <- "County: Lean Republican"     
levels(bo_gg_R_house_2$var1)[levels(bo_gg_R_house_2$var1)=="Pres_Vote.fSolid Republican"] <- "County: Solid Republican"     


### Combine Plots ###
pd <- position_dodge(0.3)

## Subset further to plot only certain factors ##

bo_gg_R_sb_3 <- subset(bo_gg_R_sb_2, 
                       factor=="perceive_extreme_very.f1" |
                         factor=="educ.int.f5" |
                         factor=="polint.f4" |
                         factor=="Pres_Vote.fSolid Republican")
bo_gg_R_sb_3$pid = "Republican"
bo_gg_R_sb_3$Office = "School Board"

bo_gg_R_leo_3 <- subset(bo_gg_R_leo_2, 
                        factor=="perceive_extreme_very.f1" |
                          factor=="educ.int.f5" |
                          factor=="polint.f4" |
                          factor=="Pres_Vote.fSolid Republican")
bo_gg_R_leo_3$pid = "Republican"
bo_gg_R_leo_3$Office = "Local Election Official"


bo_gg_R_sher_3 <- subset(bo_gg_R_sher_2, 
                         factor=="perceive_extreme_very.f1" |
                           factor=="educ.int.f5" |
                           factor=="polint.f4" |
                           factor=="Pres_Vote.fSolid Republican")
bo_gg_R_sher_3$pid = "Republican"
bo_gg_R_sher_3$Office = "Sheriff"

bo_gg_R_da_3 <- subset(bo_gg_R_da_2, 
                       factor=="perceive_extreme_very.f1" |
                         factor=="educ.int.f5" |
                         factor=="polint.f4" |
                         factor=="Pres_Vote.fSolid Republican")
bo_gg_R_da_3$pid = "Republican"
bo_gg_R_da_3$Office = "District Attorney"

bo_gg_R_house_3 <- subset(bo_gg_R_house_2, 
                          factor=="perceive_extreme_very.f1" |
                            factor=="educ.int.f5" |
                            factor=="polint.f4" |
                            factor=="Pres_Vote.fSolid Republican")
bo_gg_R_house_3$pid = "Republican"
bo_gg_R_house_3$Office = "U.S. House"


np_Intra_R <- rbind(bo_gg_R_sb_3, bo_gg_R_leo_3, bo_gg_R_sher_3,
                    bo_gg_R_da_3, bo_gg_R_house_3)

#####


np_Intra_R_plot <- ggplot(data = np_Intra_R, aes(x = var1,
                                                 y = AME, group=Office , ymin = lower, ymax = upper))

np_Intra_R_plot

np_Intra_R_plot <- np_Intra_R_plot + geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange(aes(colour = Office), position = pd) + 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(x = NULL, y = "Average Marginal Effect", col="Office") +
  ggtitle("") +
  scale_x_discrete(labels=c("County: Solid Republican" = "County:\nSolid Republican", 
                            "News: Most of the time" = "News:\nMost of the time",
                            "Post-Graduate" = "Education:\nPost-Graduate",
                            "Outparty is extreme" = "Outparty:\nis extreme"))

#remove labels
np_Intra_R_plot <- np_Intra_R_plot + geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange(aes(colour = Office), position = pd) + 
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(x = NULL, y = "", col="Office") +
  ggtitle("Among Republicans") +
  scale_x_discrete(labels=c("County: Solid Republican" = "", 
                            "News: Most of the time" = "",
                            "Post-Graduate" = "",
                            "Outparty is extreme" = ""))

## Combine D and R 

library(grid)
figure <- ggarrange(np_Intra_D_plot + rremove("ylab") + rremove("xlab"),
                    ggplot() + theme_void(),
                    np_Intra_R_plot + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    nrow = 1, widths = c(1, 0.001, 1),
                    common.legend = TRUE, legend = "top",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure

annotate_figure(figure, 
                bottom = text_grob("Average Marginal Effect", 
                                   color = "black",
                                   hjust = 0.25, vjust = 0, size = 12))

#############

########################################################################################################
########################################################################################################
########################################################################################################

#Replication for additional analyses, including online tables and figures#

########################################################################################################
########################################################################################################
########################################################################################################


# ONLINE APPENDIX: Descriptive Statistics, CES Module, 2018/2020 year-by-year comparison
dfs_by_year <- stby(data = desc.stats2, 
                    INDICES = desc.stats2$cces.year, 
                    FUN = descr, stats = "common", transpose = TRUE)

###### ONLINE APPENDIX - T-TESTS FOR FAMILIARITY BIAS #####

# subsetting observations by states
# with and w/o partisan school boards
states_sb_p <- subset(cces2.2, states_p_sb==1)
states_sb_np <- subset(cces2.2, states_p_sb==0)

#t-test: proportion choosing np school board living in
# PARTISAN school board states vs. proportion choosing 
# np school board living in NONPARTISAN school board states
t.test(states_sb_p$RCO322_np,states_sb_np$RCO322_np) 

# subsetting observations by states
# with and w/o partisan sheriffs
states_sher_p <- subset(cces2.2, states_p_sheriff==1)
states_sher_np <- subset(cces2.2, states_p_sheriff==0)

#t-test: proportion choosing np sheriff living in
# PARTISAN sheriff states vs. proportion choosing 
# np sheriffd living in NONPARTISAN sheriff states
t.test(states_sher_p$RCO323_np,states_sher_np$RCO323_np) 

# subsetting observations by states
# with and w/o partisan D.A.
states_da_p <- subset(cces2.2, states_p_da==1)
states_da_np <- subset(cces2.2, states_p_da==0)

#t-test: proportion choosing np d.a. living in
# PARTISAN d.a. states vs. proportion choosing 
# np d.a. living in NONPARTISAN d.a. states
t.test(states_da_p$RCO324_np,states_da_np$RCO324_np) 

##### ONLINE APPENDIX: Patterns of Response #####

#choosing PARTISAN for every office
mytable <- table(cces2.2$partydefender)
margin.table(mytable, 1)
partydefender <- subset(cces2.2, partydefender==7)
table(partydefender$pid)

#choosing NONPARTISAN for every office
mytable <- table(cces2.2$reformer)
margin.table(mytable, 1)
reformer <- subset(cces2.2, reformer==7)
table(reformer$pid)

#choosing NONPARTISAN for every local office
mytable <- table(cces2.2$reformer_local)
margin.table(mytable, 1)
reformer_local <- subset(cces2.2, reformer_local==5)
table(reformer_local$pid)


##### ONLINE APPENDIX: Number of Respondents by County Presidential Vote ####

mytable <- table(cces2.2$pid,
                 cces2.2$Pres_Vote)
mytable


##### ONLINE APPENDIX: Overall preference for local, regardless of party ####

overallSB <- cces2.2
overallSB <- summarySE(cces2.2, measurevar="RCO322_np", na.rm=TRUE)
overallSB$office <- "school board"
overallSB$preference <- "nonpartisan"
overallSB$proportion <- overallSB$RCO322_np
overallSB$RCO322_np <- NULL
overallSB$.id <- NULL

overallSBp <- summarySE(cces2.2, measurevar="RCO322_p", na.rm=TRUE)
overallSBp$office <- "school board"
overallSBp$preference <- "partisan"
overallSBp$proportion <- overallSBp$RCO322_p
overallSBp$RCO322_p <- NULL
overallSBp$.id <- NULL

overallSBa <- cces2.2
overallSBa <- summarySE(cces2.2, measurevar="RCO322_a", na.rm=TRUE)
overallSBa$office <- "school board"
overallSBa$preference <- "appointed"
overallSBa$proportion <- overallSBa$RCO322_a
overallSBa$RCO322_a <- NULL
overallSBa$.id <- NULL

overall <- rbind(overallSB, overallSBp, overallSBa)
###
###

overallleo <- cces2.2
overallleo <- summarySE(cces2.2, measurevar="RCO321_np", na.rm=TRUE)
overallleo$office <- "local election official"
overallleo$preference <- "nonpartisan"
overallleo$proportion <- overallleo$RCO321_np
overallleo$RCO321_np <- NULL
overallleo$.id <- NULL

overallleop <- summarySE(cces2.2, measurevar="RCO321_p", na.rm=TRUE)
overallleop$office <- "local election official"
overallleop$preference <- "partisan"
overallleop$proportion <- overallleop$RCO321_p
overallleop$RCO321_p <- NULL
overallleop$.id <- NULL

overallleoa <- cces2.2
overallleoa <- summarySE(cces2.2, measurevar="RCO321_a", na.rm=TRUE)
overallleoa$office <- "local election official"
overallleoa$preference <- "appointed"
overallleoa$proportion <- overallleoa$RCO321_a
overallleoa$RCO321_a <- NULL
overallleoa$.id <- NULL

overall <- rbind(overall, overallleo, overallleop, overallleoa)
###
###
overallsher <- cces2.2
overallsher <- summarySE(cces2.2, measurevar="RCO323_np", na.rm=TRUE)
overallsher$office <- "sheriff"
overallsher$preference <- "nonpartisan"
overallsher$proportion <- overallsher$RCO323_np
overallsher$RCO323_np <- NULL
overallsher$.id <- NULL

overallsherp <- summarySE(cces2.2, measurevar="RCO323_p", na.rm=TRUE)
overallsherp$office <- "sheriff"
overallsherp$preference <- "partisan"
overallsherp$proportion <- overallsherp$RCO323_p
overallsherp$RCO323_p <- NULL
overallsherp$.id <- NULL

overallshera <- cces2.2
overallshera <- summarySE(cces2.2, measurevar="RCO323_a", na.rm=TRUE)
overallshera$office <- "sheriff"
overallshera$preference <- "appointed"
overallshera$proportion <- overallshera$RCO323_a
overallshera$RCO323_a <- NULL
overallshera$.id <- NULL

overall <- rbind(overall, overallsher, overallsherp, overallshera)
###
###
overallda <- cces2.2
overallda <- summarySE(cces2.2, measurevar="RCO324_np", na.rm=TRUE)
overallda$office <- "district attorney"
overallda$preference <- "nonpartisan"
overallda$proportion <- overallda$RCO324_np
overallda$RCO324_np <- NULL
overallda$.id <- NULL

overalldap <- summarySE(cces2.2, measurevar="RCO324_p", na.rm=TRUE)
overalldap$office <- "district attorney"
overalldap$preference <- "partisan"
overalldap$proportion <- overalldap$RCO324_p
overalldap$RCO324_p <- NULL
overalldap$.id <- NULL

overalldaa <- cces2.2
overalldaa <- summarySE(cces2.2, measurevar="RCO324_a", na.rm=TRUE)
overalldaa$office <- "district attorney"
overalldaa$preference <- "appointed"
overalldaa$proportion <- overalldaa$RCO324_a
overalldaa$RCO324_a <- NULL
overalldaa$.id <- NULL

overall <- rbind(overall, overallda, overalldap, overalldaa)

###
overallhouse <- cces2.2
overallhouse <- summarySE(cces2.2, measurevar="RCO327_np", na.rm=TRUE)
overallhouse$office <- "u.s. house"
overallhouse$preference <- "nonpartisan"
overallhouse$proportion <- overallhouse$RCO327_np
overallhouse$RCO327_np <- NULL
overallhouse$.id <- NULL

overallhousep <- summarySE(cces2.2, measurevar="RCO327_p", na.rm=TRUE)
overallhousep$office <- "u.s. house"
overallhousep$preference <- "partisan"
overallhousep$proportion <- overallhousep$RCO327_p
overallhousep$RCO327_p <- NULL
overallhousep$.id <- NULL

overallhousea <- cces2.2
overallhousea <- summarySE(cces2.2, measurevar="RCO327_a", na.rm=TRUE)
overallhousea$office <- "u.s. house"
overallhousea$preference <- "appointed"
overallhousea$proportion <- overallhousea$RCO327_a
overallhousea$RCO327_a <- NULL
overallhousea$.id <- NULL

overall <- rbind(overall, overallhouse, overallhousep, overallhousea)
##
##
##
overall$office <- ordered(overall$office, 
                          levels = c("school board", "sheriff",
                                     "district attorney", 
                                     "local election official",
                                     "u.s. house"))

overallPlot <- ggplot(overall, aes(x=office, y=proportion, fill=preference)) + 
  scale_fill_manual(values=c("palegreen", "purple", "black"),
                    name="Preference", # Legend label, use darker colors
                    breaks=c("nonpartisan", "partisan", "appointed"),
                    labels=c("nonpartisan", "partisan", "appointed")) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=proportion-se, ymax=proportion+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Office") +
  ylab("Proportion of Respondents") +
  ylim(0,0.75) +
  ggtitle("") +
  theme_bw()

overallPlot

### ONLINE APPENDIX: Inter-party differences, DV=Partisan Election ###

sbContrastP <- cces2.2
sbContrastP <- subset(sbContrastP, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

sbContrastP$pid <- factor(sbContrastP$pid)
sbContrastPD <- subset(sbContrastP, pid=="Democrat")
sbContrastPR <- subset(sbContrastP, pid=="Republican")

sbContrastPD <- summarySE(sbContrastPD, measurevar="RCO322_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
sbContrastPD$office <- "school board"
sbContrastPD$party <- "Democrat"
sbContrastPD$proportion <- sbContrastPD$RCO322_p
sbContrastPD$RCO322_p <- NULL


sbContrastPR <- summarySE(sbContrastPR, measurevar="RCO322_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
sbContrastPR$office <- "school board"
sbContrastPR$party <- "Republican"
sbContrastPR$proportion <- sbContrastPR$RCO322_p
sbContrastPR$RCO322_p <- NULL

sbContrastPDR <- rbind(sbContrastPD, sbContrastPR)
sbContrastPDR$Pres_Vote <- ordered(sbContrastPDR$Pres_Vote, 
                                   levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
sbCDP <- ggplot(sbContrastPDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) +
  ggtitle("School Board") +
  ylim(0,1) +                        
  theme_bw() 


sbCDP

## partisan diffs in local election opinion 
leoContrastP <- cces2.2
leoContrastP <- subset(leoContrastP, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

leoContrastP$pid <- factor(leoContrastP$pid)
leoContrastPD <- subset(leoContrastP, pid=="Democrat")
leoContrastPR <- subset(leoContrastP, pid=="Republican")

leoContrastPD <- summarySE(leoContrastPD, measurevar="RCO321_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
leoContrastPD$office <- "local election official"
leoContrastPD$party <- "Democrat"
leoContrastPD$proportion <- leoContrastPD$RCO321_p
leoContrastPD$RCO321_p <- NULL


leoContrastPR <- summarySE(leoContrastPR, measurevar="RCO321_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
leoContrastPR$office <- "local election official"
leoContrastPR$party <- "Republican"
leoContrastPR$proportion <- leoContrastPR$RCO321_p
leoContrastPR$RCO321_p <- NULL

leoContrastPDR <- rbind(leoContrastPD, leoContrastPR)
leoContrastPDR$Pres_Vote <- ordered(leoContrastPDR$Pres_Vote, 
                                    levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
leoCDP <- ggplot(leoContrastPDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("Local Election Official") +
  ylim(0,1) +                        
  theme_bw() 

leoCD

## partisan diffs in local election opinion 
sherContrastP <- cces2.2
sherContrastP <- subset(sherContrastP, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

sherContrastP$pid <- factor(sherContrastP$pid)
sherContrastPD <- subset(sherContrastP, pid=="Democrat")
sherContrastPR <- subset(sherContrastP, pid=="Republican")

sherContrastPD <- summarySE(sherContrastPD, measurevar="RCO323_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
sherContrastPD$office <- "sheriff"
sherContrastPD$party <- "Democrat"
sherContrastPD$proportion <- sherContrastPD$RCO323_p
sherContrastPD$RCO323_p <- NULL


sherContrastPR <- summarySE(sherContrastPR, measurevar="RCO323_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
sherContrastPR$office <- "sheriff"
sherContrastPR$party <- "Republican"
sherContrastPR$proportion <- sherContrastPR$RCO323_p
sherContrastPR$RCO323_p <- NULL

sherContrastPDR <- rbind(sherContrastPD, sherContrastPR)
sherContrastPDR$Pres_Vote <- ordered(sherContrastPDR$Pres_Vote, 
                                     levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
sherCDP <- ggplot(sherContrastPDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +  
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("Sheriff") +
  ylim(0,1) +                        
  theme_bw() 

sherCD

## partisan diffs in local election opinion 
daContrastP <- cces2.2
daContrastP <- subset(daContrastP, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

daContrastP$pid <- factor(daContrastP$pid)
daContrastPD <- subset(daContrastP, pid=="Democrat")
daContrastPR <- subset(daContrastP, pid=="Republican")

daContrastPD <- summarySE(daContrastPD, measurevar="RCO324_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
daContrastPD$office <- "d.a."
daContrastPD$party <- "Democrat"
daContrastPD$proportion <- daContrastPD$RCO324_p
daContrastPD$RCO324_p <- NULL


daContrastPR <- summarySE(daContrastPR, measurevar="RCO324_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
daContrastPR$office <- "d.a."
daContrastPR$party <- "Republican"
daContrastPR$proportion <- daContrastPR$RCO324_p
daContrastPR$RCO324_p <- NULL

daContrastPDR <- rbind(daContrastPD, daContrastPR)
daContrastPDR$Pres_Vote <- ordered(daContrastDR$Pres_Vote, 
                                   levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
daCDP <- ggplot(daContrastPDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) + 
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("District Attorney") +
  ylim(0,1) +                       
  theme_bw() 

daCD

####
houseContrastP <- cces2.2
houseContrastP <- subset(houseContrastP, c(Pres_Vote == "Solid Democrat" | Pres_Vote == "Solid Republican"))

houseContrastP$pid <- factor(houseContrastP$pid)
houseContrastPD <- subset(houseContrastP, pid=="Democrat")
houseContrastPR <- subset(houseContrastP, pid=="Republican")

houseContrastPD <- summarySE(houseContrastPD, measurevar="RCO327_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
houseContrastPD$office <- "u.s. house"
houseContrastPD$party <- "Democrat"
houseContrastPD$proportion <- houseContrastPD$RCO327_p
houseContrastPD$RCO327_p <- NULL


houseContrastPR <- summarySE(houseContrastPR, measurevar="RCO327_p", groupvars=c("Pres_Vote"), na.rm=TRUE)
houseContrastPR$office <- "u.s. house"
houseContrastPR$party <- "Republican"
houseContrastPR$proportion <- houseContrastPR$RCO327_p
houseContrastPR$RCO327_p <- NULL

houseContrastPDR <- rbind(houseContrastPD, houseContrastPR)
houseContrastPDR$Pres_Vote <- ordered(houseContrastPDR$Pres_Vote, 
                                      levels = c("Solid Democrat", "Solid Republican"))

pd <- position_dodge(0.1)
houseCDP <- ggplot(houseContrastPDR, aes(x=Pres_Vote, y=proportion, colour=party, group=party, legend="right")) + 
  geom_errorbar(aes(ymin=proportion-ci, ymax=proportion+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + 
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("Democrat" = "blue", 
                              "Republican" = "red")) +   
  scale_x_discrete(breaks=c("Solid Democrat", "Solid Republican"),
                   labels=c("Solid\nDemocrat", "Solid\nRepublican")) + 
  ggtitle("U.S. House") +
  ylim(0,1) +                        
  theme_bw() 

houseCDP
####

###### Multiplot for Nonpartisan #####

#extract legend from houseCD (could be any b/c all same legend)
legP <- get_legend(houseCDP)

#save earlier plots but remove their legends
sbCDP <- sbCDP + theme(legend.position = "none")
leoCDP <- leoCDP + theme(legend.position = "none")
sherCDP <- sherCDP + theme(legend.position = "none")
daCDP <- daCDP + theme(legend.position = "none")
houseCDP <- houseCDP + theme(legend.position = "none")

#arrange plots, and now the extracted legend is a defacto plot (for positioning)
multi_npContrastP <- ggarrange(sbCDP, leoCDP, sherCDP,  daCDP, houseCDP, legP, 
                               ncol = 3, nrow = 2,
                               font.label=list(color="black",size=10))

annotate_figure(multi_npContrastP,
                top = text_grob("", 
                                color = "black", face = "bold", size = 14),
                bottom = text_grob("2016 County Presidential Vote", 
                                   color = "black",
                                   hjust = 0.35, vjust = -0.75, size = 12),
                left = text_grob("Proportion", color = "black", rot = 90),
                right = "",
                fig.lab = "", fig.lab.face = "bold"
)

#######################################################################################

### ONLINE APPENDIX: ANOVA, Chi-Square tests to see if pid*Pres_Vote is significant  ###

#######################################################################################

sbNP.interact <- glm(RCO322_np ~ 
                       pid*Pres_Vote.f +
                       perceive_extreme_very.f +
                       polint.f +
                       educ.int.f +
                       birthyr,
                     family="binomial",
                     data = subset(sbGLM))

summary(sbNP.interact)
#anova shows interaction is significant
anova(sbNP.interact, test="Chisq")

sbNP.baseline <- glm(RCO322_np ~ 
                       pid + Pres_Vote.f +
                       perceive_extreme_very.f +
                       polint.f +
                       educ.int.f +
                       birthyr,
                     family="binomial",
                     data = subset(sbGLM))

summary(sbNP.baseline)

# Compare Nonpartisan SB models, with and w/o interaction
anova(sbNP.baseline, sbNP.interact, test="Chisq")

leoNP.interact <- glm(RCO321_np ~ 
                        pid*Pres_Vote.f +
                        perceive_extreme_very.f +
                        polint.f +
                        educ.int.f +
                        birthyr,
                      family="binomial",
                      data = subset(leoGLM))

#anova shows interaction is significant
anova(leoNP.interact, test="Chisq")

leoNP.baseline <- glm(RCO321_np ~ 
                        pid + Pres_Vote.f +
                        perceive_extreme_very.f +
                        polint.f +
                        educ.int.f +
                        birthyr,
                      family="binomial",
                      data = subset(leoGLM))

# Compare Nonpartisan LEO models, with and w/o interaction
anova(leoNP.baseline, leoNP.interact, test="Chisq")

daNP.interact <- glm(RCO324_np ~ 
                       pid*Pres_Vote.f +
                       perceive_extreme_very.f +
                       polint.f +
                       educ.int.f +
                       birthyr,
                     family="binomial",
                     data = subset(daGLM))

anova(daNP.interact, test="Chisq")

daNP.baseline <- glm(RCO324_np ~ 
                       pid + Pres_Vote.f +
                       perceive_extreme_very.f +
                       polint.f +
                       educ.int.f +
                       birthyr,
                     family="binomial",
                     data = subset(daGLM))

# Compare Nonpartisan DA models, with and w/o interaction
anova(daNP.baseline, daNP.interact, test="Chisq")

sherNP.interact <- glm(RCO323_np ~ 
                         pid*Pres_Vote.f +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(sherGLM))

anova(sherNP.interact, test="Chisq")

sherNP.baseline <- glm(RCO323_np ~ 
                         pid + Pres_Vote.f +
                         perceive_extreme_very.f +
                         polint.f +
                         educ.int.f +
                         birthyr,
                       family="binomial",
                       data = subset(sherGLM))

# Compare Nonpartisan LEO models, with and w/o interaction
anova(sherNP.baseline, sherNP.interact, test="Chisq")

houseNP.interact <- glm(RCO327_np ~ 
                          pid*Pres_Vote.f +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(houseGLM))

anova(houseNP.interact, test="Chisq")

houseNP.baseline <- glm(RCO327_np ~ 
                          pid + Pres_Vote.f +
                          perceive_extreme_very.f +
                          polint.f +
                          educ.int.f +
                          birthyr,
                        family="binomial",
                        data = subset(houseGLM))

# Compare Nonpartisan LEO models, with and w/o interaction
anova(houseNP.baseline, houseNP.interact, test="Chisq")

#######################################################################################

### ONLINE APPENDIX: Models of U.S. House with pres_vote at congressional district level  ###

#######################################################################################

#create pres_Vote categories based on presidential vote at the congressional district

houseMarginsNP2$cd_presvote <- ifelse(houseMarginsNP2$trump2020 >=65, "Solid Republican",
                                      ifelse(houseMarginsNP2$trump2020 >=50 & houseMarginsNP2$trump2020 < 65, "Lean Republican",
                                             ifelse(houseMarginsNP2$trump2020 <50 & houseMarginsNP2$trump2020 >=35, "Lean Democrat",
                                                    ifelse(houseMarginsNP2$trump2020 <35, "Solid Democrat", NA))))

sbMarginsNP2$cd_presvote <- ifelse(sbMarginsNP2$trump2020 >=65, "Solid Republican",
                                   ifelse(sbMarginsNP2$trump2020 >=50 & sbMarginsNP2$trump2020 < 65, "Lean Republican",
                                          ifelse(sbMarginsNP2$trump2020 <50 & sbMarginsNP2$trump2020 >=35, "Lean Democrat",
                                                 ifelse(sbMarginsNP2$trump2020 <35, "Solid Democrat", NA))))

leoMarginsNP2$cd_presvote <- ifelse(leoMarginsNP2$trump2020 >=65, "Solid Republican",
                                    ifelse(leoMarginsNP2$trump2020 >=50 & leoMarginsNP2$trump2020 < 65, "Lean Republican",
                                           ifelse(leoMarginsNP2$trump2020 <50 & leoMarginsNP2$trump2020 >=35, "Lean Democrat",
                                                  ifelse(leoMarginsNP2$trump2020 <35, "Solid Democrat", NA))))

sherMarginsNP2$cd_presvote <- ifelse(sherMarginsNP2$trump2020 >=65, "Solid Republican",
                                     ifelse(sherMarginsNP2$trump2020 >=50 & sherMarginsNP2$trump2020 < 65, "Lean Republican",
                                            ifelse(sherMarginsNP2$trump2020 <50 & sherMarginsNP2$trump2020 >=35, "Lean Democrat",
                                                   ifelse(sherMarginsNP2$trump2020 <35, "Solid Democrat", NA))))

daMarginsNP2$cd_presvote <- ifelse(daMarginsNP2$trump2020 >=65, "Solid Republican",
                                   ifelse(daMarginsNP2$trump2020 >=50 & daMarginsNP2$trump2020 < 65, "Lean Republican",
                                          ifelse(daMarginsNP2$trump2020 <50 & daMarginsNP2$trump2020 >=35, "Lean Democrat",
                                                 ifelse(daMarginsNP2$trump2020 <35, "Solid Democrat", NA))))



Finter.house.cd.SolidD <- glm(RCO327_np ~ 
                                pid +
                                perceive_extreme_very.f +
                                polint.f +
                                educ.int.f +
                                birthyr,
                              family="binomial",
                              data = subset(houseMarginsNP2, cd_presvote=="Solid Democrat"))

Finter.house.cd.LeanD <- glm(RCO327_np ~ 
                               pid +
                               perceive_extreme_very.f +
                               polint.f +
                               educ.int.f +
                               birthyr,
                             family="binomial",
                             data = subset(houseMarginsNP2, cd_presvote=="Lean Democrat"))

Finter.house.cd.LeanR <- glm(RCO327_np ~ 
                               pid +
                               perceive_extreme_very.f +
                               polint.f +
                               educ.int.f +
                               birthyr,
                             family="binomial",
                             data = subset(houseMarginsNP2, cd_presvote=="Lean Republican"))

Finter.house.cd.SolidR <- glm(RCO327_np ~ 
                                pid +
                                perceive_extreme_very.f +
                                polint.f +
                                educ.int.f +
                                birthyr,
                              family="binomial",
                              data = subset(houseMarginsNP2, cd_presvote=="Solid Republican"))


pl <- c(
  `pidRepublican` = "Party ID",
  `polint.f4` = "News: Most of the time",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme")

tab_model(Finter.house.cd.SolidD, Finter.house.cd.LeanD, Finter.house.cd.LeanR, Finter.house.cd.SolidR,
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","Lean Democrat",
                        "Lean Republican","Solid Republican"),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)


###### Intra-party tables ####
## Intraparty - Dem full models
levels(sbMarginsNP2$polint.f)
levels(sbMarginsNP2$educ.int.f)
houseMarginsNP2$cd_presvote <- as.factor(houseMarginsNP2$cd_presvote)
levels(houseMarginsNP2$cd_presvote)
sbMarginsNP2$cd_presvote <- as.factor(sbMarginsNP2$cd_presvote)
leoMarginsNP2$cd_presvote <- as.factor(leoMarginsNP2$cd_presvote)
sherMarginsNP2$cd_presvote <- as.factor(sherMarginsNP2$cd_presvote)
daMarginsNP2$cd_presvote <- as.factor(daMarginsNP2$cd_presvote)

sbMarginsNP2 <- within(sbMarginsNP2, polint.f <- relevel(polint.f, ref = "1"))
sbMarginsNP2 <- within(sbMarginsNP2, educ.int.f <- relevel(educ.int.f, ref = "1"))
sbMarginsNP2 <- within(sbMarginsNP2, cd_presvote <- relevel(cd_presvote, ref = "Solid Democrat"))

leoMarginsNP2 <- within(leoMarginsNP2, polint.f <- relevel(polint.f, ref = "1"))
leoMarginsNP2 <- within(leoMarginsNP2, educ.int.f <- relevel(educ.int.f, ref = "1"))
leoMarginsNP2 <- within(leoMarginsNP2, cd_presvote <- relevel(cd_presvote, ref = "Solid Democrat"))

sherMarginsNP2 <- within(sherMarginsNP2, polint.f <- relevel(polint.f, ref = "1"))
sherMarginsNP2 <- within(sherMarginsNP2, educ.int.f <- relevel(educ.int.f, ref = "1"))
sherMarginsNP2 <- within(sherMarginsNP2, cd_presvote <- relevel(cd_presvote, ref = "Solid Democrat"))

daMarginsNP2 <- within(daMarginsNP2, polint.f <- relevel(polint.f, ref = "1"))
daMarginsNP2 <- within(daMarginsNP2, educ.int.f <- relevel(educ.int.f, ref = "1"))
daMarginsNP2 <- within(daMarginsNP2, cd_presvote <- relevel(cd_presvote, ref = "Solid Democrat"))

houseMarginsNP2 <- within(houseMarginsNP2, polint.f <- relevel(polint.f, ref = "1"))
houseMarginsNP2 <- within(houseMarginsNP2, educ.int.f <- relevel(educ.int.f, ref = "1"))
houseMarginsNP2 <- within(houseMarginsNP2, cd_presvote <- relevel(cd_presvote, ref = "Solid Democrat"))


# Full Model (SB)
glm.sb2.cdD <- glm(RCO322_np ~ 
                     perceive_extreme_very.f +
                     polint.f +
                     educ.int.f +
                     birthyr +
                     cd_presvote,
                   family="binomial",
                   data = subset(sbMarginsNP2, pid=="Democrat"))

# Full Model (LEO)
glm.leo2.cdD <- glm(RCO321_np ~ 
                      perceive_extreme_very.f +
                      polint.f +
                      educ.int.f +
                      birthyr +
                      cd_presvote,
                    family="binomial",
                    data = subset(leoMarginsNP2, pid=="Democrat"))

# Full Model (SHER)
glm.sher2.cdD <- glm(RCO323_np ~ 
                       perceive_extreme_very.f +
                       polint.f +
                       educ.int.f +
                       birthyr +
                       cd_presvote,
                     family="binomial",
                     data = subset(sherMarginsNP2, pid=="Democrat"))

# Full Model (DA)
glm.da2.cdD <- glm(RCO324_np ~ 
                     perceive_extreme_very.f +
                     polint.f +
                     educ.int.f +
                     birthyr +
                     cd_presvote,
                   family="binomial",
                   data = subset(daMarginsNP2, pid=="Democrat"))

# Full Model (HOUSE)
glm.house2.cdD <- glm(RCO327_np ~ 
                        perceive_extreme_very.f +
                        polint.f +
                        educ.int.f +
                        birthyr +
                        cd_presvote,
                      family="binomial",
                      data = subset(houseMarginsNP2, pid=="Democrat"))

pLintraCD <- c(
  `polint.f2` = "News: Now and then",
  `polint.f3` = "News: Some of the time",
  `polint.f4` = "News: Most of the time",
  `educ.int.f2` = "Education: Some college",
  `educ.int.f3` = "Education: 2-year degree",
  `educ.int.f4` = "Education: 4-year degree",
  `educ.int.f5` = "Education: Post-graduate",
  `perceive_extreme_very.f1` = "Out-party is very extreme",
  `cd_presvoteLean Democrat` = "House District: Lean Democrat",
  `cd_presvoteLean Republican` = "House District: Lean Republican",
  `cd_presvoteSolid Republican` = "House District: Solid Republican")


# Logit 
tab_model(glm.sb2.cdD, glm.leo2.cdD, glm.sher2.cdD, glm.da2.cdD, glm.house2.cdD,
          show.reflvl = TRUE)

tab_model(glm.sb2.cdD, glm.leo2.cdD, glm.sher2.cdD, glm.da2.cdD, glm.house2.cdD,
          transform = NULL,
          show.ci=FALSE,
          p.style = "stars",
          collapse.se=TRUE,
          show.aic=TRUE,
          show.reflvl = TRUE,
          dv.labels = c("School Board","L.E.O.",
                        "Sheriff","D.A.","U.S. House"),
          pred.labels = pLintraCD)

############################

# Full Model (SB)
glm.sb2.cdR <- glm(RCO322_np ~ 
                     perceive_extreme_very.f +
                     polint.f +
                     educ.int.f +
                     birthyr +
                     cd_presvote,
                   family="binomial",
                   data = subset(sbMarginsNP2, pid=="Republican"))

# Full Model (LEO)
glm.leo2.cdR <- glm(RCO321_np ~ 
                      perceive_extreme_very.f +
                      polint.f +
                      educ.int.f +
                      birthyr +
                      cd_presvote,
                    family="binomial",
                    data = subset(leoMarginsNP2, pid=="Republican"))

# Full Model (SHER)
glm.sher2.cdR <- glm(RCO323_np ~ 
                       perceive_extreme_very.f +
                       polint.f +
                       educ.int.f +
                       birthyr +
                       cd_presvote,
                     family="binomial",
                     data = subset(sherMarginsNP2, pid=="Republican"))

# Full Model (DA)
glm.da2.cdR <- glm(RCO324_np ~ 
                     perceive_extreme_very.f +
                     polint.f +
                     educ.int.f +
                     birthyr +
                     cd_presvote,
                   family="binomial",
                   data = subset(daMarginsNP2, pid=="Republican"))

# Full Model (HOUSE)
glm.house2.cdR <- glm(RCO327_np ~ 
                        perceive_extreme_very.f +
                        polint.f +
                        educ.int.f +
                        birthyr +
                        cd_presvote,
                      family="binomial",
                      data = subset(houseMarginsNP2, pid=="Republican"))


# Logit 
tab_model(glm.sb2.cdR, glm.leo2.cdR, glm.sher2.cdR, glm.da2.cdR, glm.house2.cdR,
          show.reflvl = TRUE)

tab_model(glm.sb2.cdR, glm.leo2.cdR, glm.sher2.cdR, glm.da2.cdR, glm.house2.cdR,
          transform = NULL,
          show.ci=FALSE,
          p.style = "stars",
          collapse.se=TRUE,
          show.aic=TRUE,
          show.reflvl = TRUE,
          dv.labels = c("School Board","L.E.O.",
                        "Sheriff","D.A.","U.S. House"),
          pred.labels = pLintraCD)


#########################################

## INTER-PARTY WITH CD AS KEY IV ##

Finter.house.CD.SolidD <- glm(RCO327_np ~ 
                                pid +
                                perceive_extreme_very.f +
                                polint.f +
                                educ.int.f +
                                birthyr,
                              family="binomial",
                              data = subset(houseMarginsNP2, cd_presvote=="Solid Democrat"))

Finter.house.CD.LeanD <- glm(RCO327_np ~ 
                               pid +
                               perceive_extreme_very.f +
                               polint.f +
                               educ.int.f +
                               birthyr,
                             family="binomial",
                             data = subset(houseMarginsNP2, cd_presvote=="Lean Democrat"))

Finter.house.CD.LeanR <- glm(RCO327_np ~ 
                               pid +
                               perceive_extreme_very.f +
                               polint.f +
                               educ.int.f +
                               birthyr,
                             family="binomial",
                             data = subset(houseMarginsNP2, cd_presvote=="Lean Republican"))

Finter.house.CD.SolidR <- glm(RCO327_np ~ 
                                pid +
                                perceive_extreme_very.f +
                                polint.f +
                                educ.int.f +
                                birthyr,
                              family="binomial",
                              data = subset(houseMarginsNP2, cd_presvote=="Solid Republican"))


tab_model(Finter.house.CD.SolidD, 
          Finter.house.cd.LeanD, 
          Finter.house.CD.LeanR, 
          Finter.house.CD.SolidR,
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("CD.Solid Democrat","CD.Lean Democrat",
                        "CD.Lean Republican","CD.Solid Republican"),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)


Finter.house.SolidD <- glm(RCO327_np ~ 
                             pid +
                             perceive_extreme_very.f +
                             polint.f +
                             educ.int.f +
                             birthyr,
                           family="binomial",
                           data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Democrat"))

Finter.house.LeanD <- glm(RCO327_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Democrat"))

Finter.house.LeanR <- glm(RCO327_np ~ 
                            pid +
                            perceive_extreme_very.f +
                            polint.f +
                            educ.int.f +
                            birthyr,
                          family="binomial",
                          data = subset(houseMarginsNP2, Pres_Vote.f=="Lean Republican"))

Finter.house.SolidR <- glm(RCO327_np ~ 
                             pid +
                             perceive_extreme_very.f +
                             polint.f +
                             educ.int.f +
                             birthyr,
                           family="binomial",
                           data = subset(houseMarginsNP2, Pres_Vote.f=="Solid Republican"))

tab_model(Finter.house.SolidD, Finter.house.LeanD, 
          Finter.house.LeanR, Finter.house.SolidR, 
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","Lean Democrat",
                        "Lean Republican","Solid Republican"),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)



tab_model(Finter.house.SolidD,Finter.house.CD.SolidD,
          Finter.house.LeanD, Finter.house.CD.LeanD,
          Finter.house.LeanR, Finter.house.CD.LeanR,
          Finter.house.SolidR, Finter.house.CD.SolidR,
          terms=c("pidRepublican",
                  "polint.f4",
                  "educ.int.f5",
                  "perceive_extreme_very.f1"),
          dv.labels = c("Solid Democrat","cd",
                        "Lean Democrat", "cd",
                        "Lean Republican","cd",
                        "Solid Republican","cd"),
          pred.labels = pl,
          collapse.se=TRUE,
          show.ci=FALSE,
          p.style="stars",
          transform=NULL,
          show.aic=TRUE)

