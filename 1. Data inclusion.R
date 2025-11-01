#Main analysis####
#1. Incorporate into the consolidated data     
#2. Generate the baseline feature description table   




##R packages####
#install.packages("haven")
library(haven)
#install.packages("readxl")
library(readxl)
#install.packages("bit")
library(bit)
#install.packages("readr")
library(readr)
#install.packages("glue")
library(glue)
#install.packages("gtsummary")
library(gtsummary)
#install.packages("survey")
library(survey)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("officer")
library(officer)
#install.packages("flextable")
library(flextable) 
#install.packages("dplyr")
library(dplyr)
#install.packages("autoReg")
library(autoReg)
#install.packages("rrtable")
library(rrtable)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyr")
library(tidyr)
#install.packages("jskm")
library(jskm)
#install.packages("survival")
library(survival)
#install.packages("survminer")
library(survminer)
#install.packages("RMediation")
library(RMediation)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("devtools")
library(devtools)
#install_github("MRCIEU/TwoSampleMR")
library(TwoSampleMR)
#install.packages("arsenal")
library(arsenal)
#install.packages("mice")
library(mice)
#install_github("jamesjiadazhan/dietaryindex")
library(dplyr)
library(haven)
library(readr)
library(dietaryindex)
library(rms) 
library(mediation)
library(regmedint)
#install.packages("glmnet")
library(Matrix)
library(glmnet)
library(foreign)
library(survival)
#install.packages("visreg")
library(visreg)
#install.packages("interactionR")
library(interactionR) 
#install.packages("epiR")
library(epiR) 
#install.packages("nhanesA")
library(nhanesA)
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)


##1.Merge the death database ####
NHANES_1999_2000 <- read_csv("NHANES_1999_2000.csv")
NHANES_2001_2002 <- read_csv("NHANES_2001_2002.csv")
NHANES_2003_2004 <- read_csv("NHANES_2003_2004.csv")
NHANES_2011_2012 <- read_csv("NHANES_2011_2012.csv")
NHANES_2013_2014 <- read_csv("NHANES_2013_2014.csv")

mortality <- dplyr::bind_rows(list(NHANES_1999_2000,NHANES_2001_2002,NHANES_2003_2004,NHANES_2011_2012,NHANES_2013_2014))
dim(mortality)   # 51057      
summary(mortality)

# Change to capitalized column names
colnames(mortality) <- toupper(colnames(mortality))

#Extract variables
MMA_mortality <- mortality[,c('SEQN', 'ELIGSTAT', 'MORTSTAT', 'UCOD_LEADING','PERMTH_EXM')]

MMA_mortality$PERMTH_Y<- MMA_mortality$PERMTH_EXM/12

MMA_mortality$MORTSTAT_CVD <- ifelse(MMA_mortality$UCOD_LEADING ==1 & MMA_mortality$MORTSTAT ==1,1,
                                     ifelse(MMA_mortality$UCOD_LEADING ==5 & MMA_mortality$MORTSTAT ==1,1,0))


##2.NHANES data####
###1.Demography####
demo_A <- read_xpt('DEMO.XPT')
demo_B <- read_xpt('DEMO_B.XPT')
demo_C <- read_xpt('DEMO_C.XPT')
demo_G <- read_xpt('DEMO_G.XPT')
demo_H <- read_xpt('DEMO_H.XPT')


demo.data.file <- dplyr::bind_rows(list(demo_A,demo_B,demo_C,demo_G,demo_H))

###Extract race, gender, age, educational attainment and family income
demo.data <- demo.data.file[,c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDEDUC2', 'INDFMPIR', 'SDMVPSU', 'SDMVSTRA','WTMEC2YR')]
demo.data$DMDEDUC2 <- ifelse(demo.data$DMDEDUC2 ==1,1,
                             ifelse(demo.data$DMDEDUC2 ==2,2,
                                    ifelse(demo.data$DMDEDUC2 ==3,3,
                                           ifelse(demo.data$DMDEDUC2 ==4,4,
                                                  ifelse(demo.data$DMDEDUC2 ==5,5,NA)))))

###2.BMI ####
bmi.A <- read_xpt("BMX.XPT")
bmi.B <- read_xpt("BMX_B.XPT")
bmi.C <- read_xpt("BMX_C.XPT")
bmi.G <- read_xpt("BMX_G.XPT")
bmi.H <- read_xpt("BMX_H.XPT")

bmi.data.file <- dplyr::bind_rows(list(bmi.A,bmi.B,bmi.C,bmi.G,bmi.H))

# BMI-BMXBMI
bmi.data <- bmi.data.file[,c('SEQN', 'BMXBMI')]


###3.smoking ####
smq.A <- read_xpt('SMQ.XPT')
smq.B <- read_xpt('SMQ_B.XPT')
smq.C <- read_xpt('SMQ_C.XPT')
smq.G <- read_xpt('SMQ_G.XPT')
smq.H <- read_xpt('SMQ_H.XPT')

smq.data.file <- dplyr::bind_rows(list(smq.A,smq.B,smq.C,smq.G,smq.H))

# Have you smoked at least 100 cigarettes? -SMQ020 Are you smoking now? -SMQ040;
smq.data <- smq.data.file[,c('SEQN', 'SMQ020', 'SMQ040')]

# Have you smoked at least 100 cigarettes? -SMQ020： 1	Yes；2	No；7	Refused	；9	Don't know；.	Missing
# Are you smoking now?-SMQ040：1	Every day；2	Some days；3	Not at all；7	Refused；9	Don't know；.	Missing
# smoking,1 Never smoker，2 Former smoker，3 Current smoker

smq.data$Smoking_status <-  ifelse(smq.data$SMQ020==2,1,
                                   ifelse(smq.data$SMQ020==1 & smq.data$SMQ040==3,2,
                                          ifelse(smq.data$SMQ020==1 & smq.data$SMQ040 %in% c(1,2),3,NA)))

smq <-  data.frame(SEQN = smq.data$SEQN,Smoking_status = smq.data$Smoking_status)


###4.drink alcohol ALQ ####
alq.A <- read_xpt("ALQ.XPT")
alq.B <- read_xpt("ALQ_B.XPT")
alq.C <- read_xpt("ALQ_C.XPT")
alq.G <- read_xpt("ALQ_G.XPT")
alq.H <- read_xpt("ALQ_H.XPT")

alq.data.file <- dplyr::bind_rows(list(alq.A,alq.B,alq.C,alq.G,alq.H))

# Drink at least 12 glasses of alcohol a year -ALQ101
# Having drunk alcohol at least 12 times in a lifetime -ALQ110;
# How often did you drink alcohol in the past 12 months? -ALQ120Q
# Units of alcohol consumption frequency in the past 12 months (week, month, year) -ALQ120U;
# How often have you consumed any type of alcoholic beverage approximately in the past 12 months? How many days should it be consumed each week, month or year? -ALQ121
alq.data <- alq.data.file[,c('SEQN', 'ALQ101','ALQ120Q', 'ALQ120U')]

#The unified unit is the month:week*4、year/12 
alq.data$ALQ.unit.month <-  ifelse(alq.data$ALQ120U == 1, 4,
                                   ifelse(alq.data$ALQ120U == 3, 1/12,
                                          ifelse(alq.data$ALQ120U == 7|alq.data$ALQ120U == 9, NA, 1)))

#The uniform value is the amount of alcohol consumed per month
alq.data$ALQ.quantity.month <-  ifelse(alq.data$ALQ120Q >= 0,
                                       alq.data$ALQ120Q * alq.data$ALQ.unit.month, NA)

#Divide the results of alcohol consumption into four grades：Non-drinker, 1-5 drinks/month, 5-10 drinks/month, 10+ drinks/month
ALQ_factor <- ifelse(alq.data$ALQ.quantity.month >=1 & alq.data$ALQ.quantity.month <5, 2,
                     ifelse(alq.data$ALQ.quantity.month >=5 & alq.data$ALQ.quantity.month <10, 3,
                            ifelse(alq.data$ALQ.quantity.month >=10, 4, 0)))

alq.data$ALQ_factor <- ALQ_factor

# Update ALQ_factor based on the value of ALQ101
# Drink at least 12 glasses of alcohol a year -ALQ101
# Having drunk alcohol at least 12 times in a lifetime -ALQ110;
# ALQ101 The answer is 1, but ALQ_factor has no specific value, so it will take the value of 1
alq.data$ALQ_factor <- ifelse((alq.data$ALQ_factor==0 | is.na(alq.data$ALQ_factor)) & alq.data$ALQ101 == 1, 2,
                              ifelse(alq.data$ALQ101==2, 1,
                                     ifelse(alq.data$ALQ_factor==2,2,
                                            ifelse(alq.data$ALQ_factor==3,3,
                                                   ifelse(alq.data$ALQ_factor==4,4,
                                                          ifelse(alq.data$ALQ101==9, NA,NA))))))

alq <-  data.frame(SEQN = alq.data$SEQN,Drinking_status = alq.data$ALQ_factor)

###5.Physical activity PAQ ####
paq_A <- read_xpt("PAQ.XPT")
paq_B <- read_xpt("PAQ_B.XPT")
paq_C <- read_xpt("PAQ_C.XPT")
paq_G <- read_xpt("PAQ_G.XPT")
paq_H <- read_xpt("PAQ_H.XPT")

#
paq.data.file <- dplyr::bind_rows(list(paq_A,paq_B,paq_C,paq_G,paq_H))

#03-06
#PAD120 - The # first task around home/yard in the past 30 days
#PAD160 - How long (minutes) each time *4.5
#PAQ050Q - # Number of walks or bike rides (30 days)
#PAQ050U - Units of Measurement (1 day /2 weeks /3 Months)
#PAD080 - How long (minutes) each day *4
#PAQ180 - Average daily physical activity level *
#PAD460 - Number of sports activities in the past 30 days
#PAQ560 - # The time you spend playing or exercising per week
#PAD590 - # Watch TV or videos from the past 30 days
#PAD600 - # The number of hours spent on the computer in the past 30 days

#07-18
# How many days of the week do you work hard? -PAQ610
# How many minutes of intense work in a day -PAD615
# How many days of the week do you work at moderate intensity? -PAQ625
# How many minutes of moderate-intensity work in a day -PAD630
# How many days of the week do you walk or cycle? -PAQ640
# How many minutes of walking or cycling in a day -PAD645
# How many days of the week are there for intense exercise, fitness or recreational activities? -PAQ655
# How many minutes of exercise, fitness or entertainment activities are there in a day? -PAD660
# How many days of the week do you engage in moderate-intensity exercise, fitness or recreational activities? -PAQ670
# How many minutes a day do you spend on exercise, fitness or recreational activities? -PAD675
paq.data <- paq.data.file[,c('SEQN', 'PAD120','PAD160','PAQ050Q','PAQ050U','PAD080',
                             'PAQ180','PAD460','PAQ560','PAD590','PAD600',
                             'PAQ610','PAD615','PAQ625','PAD630','PAQ640','PAD645','PAQ655',
                             'PAD660','PAQ670','PAD675')]

#03-06
#
paq.data$PAD120[paq.data$PAD120==77777]<-NA
paq.data$PAD120[paq.data$PAD120==99999]<-NA
paq.data$PAD160[paq.data$PAD160==77777]<-NA
paq.data$PAD160[paq.data$PAD160==99999]<-NA
paq.data$work <- paq.data$PAD120*paq.data$PAD160*4.5/4

#
paq.data$PAQ050 <-  ifelse(paq.data$PAQ050U == 1, 7,
                           ifelse(paq.data$PAQ050U == 2, 1,
                                  ifelse(paq.data$PAQ050U == 3,1/4,NA)))
paq.data$PAQ050Q[paq.data$PAQ050Q==77777]<-NA
paq.data$PAQ050Q[paq.data$PAQ050Q==99999]<-NA
paq.data$PAD080[paq.data$PAD080==77777]<-NA
paq.data$PAD080[paq.data$PAD080==99999]<-NA
paq.data$bic <- paq.data$PAQ050Q*paq.data$PAQ050*paq.data$PAD080*4

#
paq.data$PAQ180 <- ifelse(paq.data$PAQ180 == 1, 1.4,
                          ifelse(paq.data$PAQ180 == 2, 1.5,
                                 ifelse(paq.data$PAQ180 == 3,1.6,
                                        ifelse(paq.data$PAQ180 == 4,1.8,NA))))
paq.data$aver <- paq.data$PAQ180*7

#
paq.data$PAD460[paq.data$PAD460==777]<-NA
paq.data$PAD460[paq.data$PAD460==999]<-NA
paq.data$lei <- paq.data$PAD460*4/4

#
paq.data$PAQ560[paq.data$PAQ560==77777]<-NA
paq.data$PAQ560[paq.data$PAQ560==99999]<-NA
paq.data$exe <- paq.data$PAQ560*7

#
paq.data$PAD590 <- ifelse(paq.data$PAD590 == 1, 1,
                          ifelse(paq.data$PAD590 == 2,2,
                                 ifelse(paq.data$PAD590 == 3,3,
                                        ifelse(paq.data$PAD590 == 4,4,
                                               ifelse(paq.data$PAD590 == 5,5,NA)))))
paq.data$TV <- paq.data$PAD590*7

#
paq.data$PAD600 <- ifelse(paq.data$PAD600 == 1, 1,
                          ifelse(paq.data$PAD600 == 2,2,
                                 ifelse(paq.data$PAD600 == 3,3,
                                        ifelse(paq.data$PAD600 == 4,4,
                                               ifelse(paq.data$PAD600 == 5,5,NA)))))
paq.data$com <- paq.data$PAD600*1.5*7

#
paq.data$PAQ03_06 <- rowSums(paq.data[,c("work","bic","aver","lei","exe","TV","com")], na.rm = T)

#07-18
#Intense work
paq.data$PAQ610[paq.data$PAQ610==77]<-NA
paq.data$PAQ610[paq.data$PAQ610==99]<-NA
paq.data$PAD615[paq.data$PAD615==7777]<-NA
paq.data$PAD615[paq.data$PAD615==9999]<-NA
paq.data$Vig_work <- paq.data$PAQ610*paq.data$PAD615*8

#Moderate-intensity work
paq.data$PAQ625[paq.data$PAQ625==77]<-NA
paq.data$PAQ625[paq.data$PAQ625==99]<-NA
paq.data$PAD630[paq.data$PAD630==7777]<-NA
paq.data$PAD630[paq.data$PAD630==9999]<-NA
paq.data$Mod_work <- paq.data$PAQ625*paq.data$PAD630*4 

#Walk or ride a bike
paq.data$PAQ640[paq.data$PAQ640==77]<-NA
paq.data$PAQ640[paq.data$PAQ640==99]<-NA
paq.data$PAD645[paq.data$PAD645==7777]<-NA
paq.data$PAD645[paq.data$PAD645==9999]<-NA
paq.data$Walk_bic <- paq.data$PAQ640*paq.data$PAD645*4 

#Intense exercise, fitness or recreational activities
paq.data$PAQ655[paq.data$PAQ655==77]<-NA
paq.data$PAQ655[paq.data$PAQ655==99]<-NA
paq.data$PAD660[paq.data$PAD660==7777]<-NA
paq.data$PAD660[paq.data$PAD660==9999]<-NA
paq.data$Vig_lei <- paq.data$PAQ655*paq.data$PAD660*8 

#Moderate-intensity exercise, fitness or recreational activities
paq.data$PAQ670[paq.data$PAQ670==77]<-NA
paq.data$PAQ670[paq.data$PAQ670==99]<-NA
paq.data$PAD675[paq.data$PAD675==7777]<-NA
paq.data$PAD675[paq.data$PAD675==9999]<-NA
paq.data$Mod_lei <- paq.data$PAQ670*paq.data$PAD675*4 

paq.data$PAQ07_18 <- rowSums(paq.data[,c("Vig_work","Mod_work","Walk_bic","Vig_lei","Mod_lei")], na.rm = T)

paq.data <- paq.data %>%  
  mutate(Physical = ifelse(SEQN < 41475, PAQ03_06,
                           ifelse(SEQN > 41474, as.character(PAQ07_18), NA)))  

paq <- data.frame(SEQN = paq.data$SEQN,Physical = paq.data$Physical)


###6.HEI-2015 ####
FPED_AB_1 <- read_csv("FPED_AB_1.csv")
FPED_C_1 <- read_csv("FPED_C_1.csv")
FPED_G_1 <- read_sas('fped_dr1tot_1112.sas7bdat')
FPED_H_1 <- read_sas('fped_dr1tot_1314.sas7bdat')

FPED.data.1 <- dplyr::bind_rows(list(FPED_AB_1,FPED_C_1,FPED_G_1,FPED_H_1))


NUTRIENT_A_1 <- read_csv('DR1TOT.csv')
NUTRIENT_B_1 <- read_csv('DR1TOT_B.csv')
NUTRIENT_C_1 <- read_xpt('DR1TOT_C.XPT')
NUTRIENT_G_1 <- read_xpt('DR1TOT_G.XPT')
NUTRIENT_H_1 <- read_xpt('DR1TOT_H.XPT')

NUTRIENT.data.1 <- dplyr::bind_rows(list(NUTRIENT_A_1,NUTRIENT_B_1,NUTRIENT_C_1,NUTRIENT_G_1,NUTRIENT_H_1))

FPED_AB_2 <- read_csv("FPED_AB_2.csv")
FPED_C_2 <- read_csv("FPED_C_2.csv")
FPED_G_2 <- read_sas('fped_dr2tot_1112.sas7bdat')
FPED_H_2 <- read_sas('fped_dr2tot_1314.sas7bdat')

FPED.data.2 <- dplyr::bind_rows(list(FPED_AB_2,FPED_C_2,FPED_G_2,FPED_H_2))

NUTRIENT_A_2 <- read_csv('DR2TOT.csv')
NUTRIENT_B_2 <- read_csv('DR2TOT_B.csv')
NUTRIENT_C_2 <- read_xpt('DR2TOT_C.XPT')
NUTRIENT_G_2 <- read_xpt('DR2TOT_G.XPT')
NUTRIENT_H_2 <- read_xpt('DR2TOT_H.XPT')

NUTRIENT.data.2 <- dplyr::bind_rows(list(NUTRIENT_A_2,NUTRIENT_B_2,NUTRIENT_C_2,NUTRIENT_G_2,NUTRIENT_H_2))

library(dietaryindex)

HEI2015 <- HEI2015_NHANES_FPED(FPED_PATH = FPED.data.1,
                               NUTRIENT_PATH = NUTRIENT.data.1,
                               DEMO_PATH = demo.data.file, 
                               FPED_PATH2 = FPED.data.2, 
                               NUTRIENT_PATH2 = NUTRIENT.data.2)

HEI <- data.frame(SEQN = HEI2015$SEQN,HEI2015 = HEI2015$HEI2015_ALL)


###7. cell (1000 cells/uL)####
CBC.A <- read_xpt("LAB25.XPT")
CBC.B <- read_xpt("L25_B.XPT")
CBC.C <- read_xpt("L25_C.XPT")
CBC.G <- read_xpt("CBC_G.XPT")
CBC.H <- read_xpt("CBC_H.XPT")

CBC.data.file <- dplyr::bind_rows(list(CBC.A,CBC.B,CBC.C,CBC.G,CBC.H))

#LBXWBCSI - White Blood Cell Count (1000 cells /uL)
#LBDLYMNO - Lymphocyte count (1000 cells /uL)
#LBDMONO - Monocyte count (1000 cells /uL)
#LBDNENO - Segmented neutrophil count (1000 cells /uL)
#LBDEONO - Eosinophil count (1000 cells /uL)
#LBDBANO - Basophile count (1000 cells /uL)
#LBXRBCSI - Red Blood Cell Count (Million cells /uL)
#LBXHGB - Hemoglobin (g/dL)
#LBXHCT - Hematocrit (%)
#LBXMCHSI - Mean Cell Hemoglobin (pg)
#LBXRDW - Red Blood Cell Distribution Width (%)
#LBXPLTSI - Platelet Count (1000 cells /uL)

CBC.data <- CBC.data.file[,c('SEQN','LBXWBCSI','LBDLYMNO','LBDMONO','LBDNENO','LBDEONO',
                             'LBDBANO','LBXRBCSI','LBXHGB','LBXHCT','LBXMCHSI','LBXRDW','LBXPLTSI')]


###8.folate####
#Serum folate
FOLFMS_A = read_xpt('LAB06.XPT')
FOLFMS_B = read_xpt('L06_B.XPT')
FOLFMS_C = read_xpt('L06NB_C.XPT')
FOLFMS_G = read_xpt('FOLFMS_G.XPT')
FOLFMS_H = read_xpt('FOLFMS_H.XPT')

SEfolate.data.file <- dplyr::bind_rows(list(FOLFMS_A,FOLFMS_B,FOLFMS_C,FOLFMS_G,FOLFMS_H))

# LBDFOLSI - folate, Serum (nmol/L)
# LBDFOTSI - Serum Total folate (nmol/L)
SEfolate.data <- SEfolate.data.file[,c('SEQN','LBDFOLSI','LBDFOTSI')]

#Data conversion before 2006
SEfolate.data$SEfolateBR <- log(SEfolate.data$LBDFOLSI,base = 10)

SEfolate.data$SEfolateMA = 10**(0.0188*SEfolate.data$SEfolateBR^3 - 2.7109*SEfolate.data$SEfolateBR^(-1/2) + 3.8276)

SEfolate.data <- SEfolate.data %>%  
  mutate(SEfolate = ifelse(SEQN < 62160,SEfolateMA,
                           ifelse(SEQN > 62160,LBDFOTSI, NA)))

SEfolate <- data.frame(SEQN = SEfolate.data$SEQN, SEfolate = SEfolate.data$SEfolate)


##RBC folate
FOLATE_A = read_xpt('LAB06.XPT')
FOLATE_B = read_xpt('L06_B.XPT')
FOLATE_C = read_xpt('L06NB_C.XPT')
FOLATE_G = read_xpt('FOLATE_G.XPT')
FOLATE_H = read_xpt('FOLATE_H.XPT')

RBCfolate.data.file <- dplyr::bind_rows(list(FOLATE_A,FOLATE_B,FOLATE_C,FOLATE_G,FOLATE_H))

# LBDRBFSI - folate, Red blood Cells (nmol/L RBC)
# LBDRFOSI - Erythrocyte folate (nmol/L)
RBCfolate.data <- RBCfolate.data.file[,c('SEQN','LBDRBFSI','LBDRFOSI')]

# Conversion before 2006
# 1) Convert BR erythrocyte folate (RBF, nmol/L) from 1999 to 2006 to whole blood folate (WBF, nmol/L) using hematocrit (HCT, %) and BR serum folate (FOL, nmol/L). The formula is as follows:

RBCfolate.data$WBF = (RBCfolate.data$LBDRBFSI * CBC.data$LBXHCT/100) + SEfolate.data$LBDFOLSI * (1.0 - (CBC.data$LBXHCT/100))

# 2) Apply the following forward linear regression and use the WBF from Step 1 to match the 2007-2010 MA WBF to obtain the adjusted WBF (WBF-adjusted):

RBCfolate.data$WBF <- log(RBCfolate.data$WBF,base = 10)

RBCfolate.data$WBFadjusted = 10**(0.2204 + (1.017 * RBCfolate.data$WBF))

# 3) Convert the BR serum folate (foll, nmol/L) results from 1999 to 2006 to the equivalent values from 2007 to 2010 to match the MA serum folate (foll adjusted, nmol/L) results using the pre-fractional polynomial regression equation specified in the serum folate analysis description:

RBCfolate.data$FOLadjusted = 10**(0.0188 * SEfolate.data$SEfolateBR^3 - 2.7109 * SEfolate.data$SEfolateBR^(-1/2) + 3.8276)

# 4) Calculate RBFadjusted using WBFadjusted(from Step 2) and FOLadjusted(from Step 3) :

RBCfolate.data$RBFadjusted = {RBCfolate.data$WBFadjusted - (RBCfolate.data$FOLadjusted * (1.0 - (CBC.data$LBXHCT/100)))} / (CBC.data$LBXHCT/100)  

RBCfolate.data <- RBCfolate.data %>%  
  mutate(RBCfolate = ifelse(SEQN < 62160,RBFadjusted,
                            ifelse(SEQN > 62160,LBDRFOSI, NA)))

RBCfolate <- data.frame(SEQN = RBCfolate.data$SEQN, RBCfolate = RBCfolate.data$RBCfolate)


###9.Serum Vitamin B12 ####
VITB12_A = read_xpt('LAB06.XPT')
VITB12_B = read_xpt('L06_B.XPT')
VITB12_C = read_xpt('L06NB_C.XPT')
VITB12_G = read_xpt('VITB12_G.XPT')
VITB12_H = read_xpt('VITB12_H.XPT')

# Data Conversion before 2006
VITB12_ABC <- dplyr::bind_rows(list(VITB12_A,VITB12_B,VITB12_C))

# LBXB12 - Vitamin B12, Serum (pg/mL) 1999-2006
VITB12.ABC <- VITB12_ABC[,c('SEQN', 'LBXB12')]

# Data Conversion
VITB12.ABC$LBXB12 <- log(VITB12.ABC$LBXB12,base = 10)

VITB12.ABC$LBXB12 = 10**(0.97*VITB12.ABC$LBXB12 + 0.14)


# LBXB12 - Vitamin B12, Serum (pg/mL) 2006-2012
# LBDB12 - Vitamin B12 (pg/mL) 2013-2014
VITB12_H$LBXB12 <- VITB12_H$LBDB12

# Merge data
VITB12.data.file <- dplyr::bind_rows(list(VITB12.ABC,VITB12_G,VITB12_H))

# LBXB12 - Vitamin B12, Serum (pg/mL)
VITB12.data <- VITB12.data.file[,c('SEQN', 'LBXB12')]

# Conversion -> pmol/L
VITB12.data$LBXB12 <- VITB12.data$LBXB12*0.738

SEVB12 <- data.frame(SEQN = VITB12.data$SEQN, SEVB12 = VITB12.data$LBXB12)


###10.Dietary Vitamin B12 ####
#1999-2002
VB12_A <- read_xpt('DRTOT.XPT')
VB12_B <- read_xpt('DRTOT_B.XPT')

VB12.data.AB <- dplyr::bind_rows(list(VB12_A,VB12_B))

# DRXTVB12 - Vitamin B12 (mcg)
VB12.AB <- VB12.data.AB[,c('SEQN', 'DRXTVB12')]

VB12.AB <- data.frame(SEQN = VB12.AB$SEQN,DRVB12 = VB12.AB$DRXTVB12)

# After 2003
# One Day
VB12_C_1 <- read_xpt('DR1TOT_C.XPT')
VB12_G_1 <- read_xpt('DR1TOT_G.XPT')
VB12_H_1 <- read_xpt('DR1TOT_H.XPT')

VB12.data.file.1 <- dplyr::bind_rows(list(VB12_C_1,VB12_G_1,VB12_H_1))

# DR1TVB12 - Vitamin B12 (mcg)
VB12.data.1 <- VB12.data.file.1[,c('SEQN', 'DR1TVB12')]

# Two Day
VB12_C_2 <- read_xpt('DR2TOT_C.XPT')
VB12_G_2 <- read_xpt('DR2TOT_G.XPT')
VB12_H_2 <- read_xpt('DR2TOT_H.XPT')

VB12.data.file.2 <- dplyr::bind_rows(list(VB12_C_2,VB12_G_2,VB12_H_2))

# DR2TVB12 - Vitamin B12 (mcg)
VB12.data.2 <- VB12.data.file.2[,c('SEQN', 'DR2TVB12')]

VB12.data <- plyr::join_all(list(VB12.data.1,VB12.data.2),by='SEQN', type='left')

# Calculate the Mean
VB12.data$VB12 <- ifelse(is.na(VB12.data$DR2TVB12), VB12.data$DR1TVB12,
                         ifelse(is.na(VB12.data$DR1TVB12), VB12.data$DR2TVB12,
                                rowMeans(cbind(VB12.data$DR1TVB12, VB12.data$DR2TVB12), na.rm = TRUE)))

VB12.CGH <- data.frame(SEQN = VB12.data$SEQN,DRVB12 = VB12.data$VB12)

DRVB12 <- dplyr::bind_rows(list(VB12.AB,VB12.CGH))


###11.Systolic blood pressure####
bpx.A <- read_xpt("BPX.XPT")
bpx.B <- read_xpt("BPX_B.XPT")
bpx.C <- read_xpt("BPX_C.XPT")
bpx.G <- read_xpt("BPX_G.XPT")
bpx.H <- read_xpt("BPX_H.XPT")

bpx.data.file <- dplyr::bind_rows(list(bpx.A,bpx.B,bpx.C,bpx.G,bpx.H))

#Systolic blood pressure, diastolic blood pressure
bpx.data <- bpx.data.file[,c('SEQN','BPXSY1','BPXSY2','BPXSY3')]

BPXSY <- bpx.data[,c('SEQN', 'BPXSY1','BPXSY2','BPXSY3')]

# Mean
bpx.data$BPXSY <- rowMeans(BPXSY[, -1], na.rm = TRUE)


###12.Glycated hemoglobin####
ghb.A <- read_xpt("LAB10.XPT")
ghb.B <- read_xpt("L10_B.XPT")
ghb.C <- read_xpt("L10_C.XPT")
ghb.G <- read_xpt("GHB_G.XPT")
ghb.H <- read_xpt("GHB_H.XPT")

ghb.data.file <- dplyr::bind_rows(list(ghb.A,ghb.B,ghb.C,ghb.G,ghb.H))

ghb.data <- ghb.data.file[,c('SEQN', 'LBXGH')]


###13.Fasting blood glucose####
glu.A <- read_xpt("LAB10AM.XPT")
glu.B <- read_xpt("L10AM_B.XPT")
glu.C <- read_xpt("L10AM_C.XPT")
glu.G <- read_xpt("GLU_G.XPT")
glu.H <- read_xpt("GLU_H.XPT")

glu.data.file <- dplyr::bind_rows(list(glu.A,glu.B,glu.C,glu.G,glu.H))

glu.data <- glu.data.file[,c('SEQN','LBXGLU')]


###14.Fasting time ####
fastqx_A = read_xpt('PH.XPT')
fastqx_B = read_xpt('PH_B.XPT')
fastqx_C = read_xpt('PH_C.XPT')
fastqx_G = read_xpt('FASTQX_G.XPT')
fastqx_H = read_xpt('FASTQX_H.XPT')

fastqx.data.file <- dplyr::bind_rows(list(fastqx_A,fastqx_B,fastqx_C,fastqx_G,fastqx_H))

# PHAFSTHR - The total length of "fasting", in hours
fastqx.data <- fastqx.data.file[,c('SEQN', 'PHAFSTHR')]

fastqx <- data.frame(SEQN = fastqx.data$SEQN,Food_fast = fastqx.data$PHAFSTHR)


###15. Methylmalonic acid （umol/L）####
MMA.A <- read_xpt('LAB06.XPT')
MMA.B <- read_xpt('L06_B.XPT')
MMA.C <- read_xpt("L06MH_C.XPT")
MMA.G <- read_xpt("MMA_G.XPT")
MMA.H <- read_xpt("MMA_H.XPT")

MMA.data.file <- dplyr::bind_rows(list(MMA.A,MMA.B,MMA.C,MMA.G,MMA.H))

# LBXMMA - Methylmalonic acid （umol/L）1999-2004
# LBXMMASI - Methylmalonic acid （nmol/L）2011-2014
MMA.data <- MMA.data.file[,c('SEQN','LBXMMA','LBXMMASI')]

MMA.data$LBXMMA <- MMA.data$LBXMMA*1000

MMA.data <- MMA.data %>%  
  mutate(MMA = ifelse(SEQN < 62161, LBXMMA,
                      ifelse(SEQN > 62160, LBXMMASI, NA)))  

MMA <- data.frame(SEQN = MMA.data$SEQN, MMA = MMA.data$MMA)

###16.pregnant####
#RHQ140 - Do you think you are pregnant now? 1999-2000.
#RHQ141 - Do you think you are pregnant now? 2001-2002.
#RHD143 - Are you pregnant now? 2003-2018.
RHQ.A <- read_xpt("RHQ.XPT")
RHQ.B <- read_xpt("RHQ_B.XPT")
RHQ.C <- read_xpt("RHQ_C.XPT")
RHQ.D <- read_xpt("RHQ_D.XPT")
RHQ.E <- read_xpt("RHQ_E.XPT")
RHQ.F <- read_xpt("RHQ_F.XPT")
RHQ.G <- read_xpt("RHQ_G.XPT")
RHQ.H <- read_xpt("RHQ_H.XPT")

rhq.data.file <- dplyr::bind_rows(list(RHQ.A,RHQ.B,RHQ.C,RHQ.D,RHQ.E,RHQ.F,RHQ.G,RHQ.H))

rhq.data <- rhq.data.file[,c('SEQN','RHQ140','RHQ141','RHD143')]

rhq.data$RHQ140 <-ifelse(rhq.data$RHQ140==1,1,
                         ifelse(rhq.data$RHQ140==2,2,
                                ifelse(rhq.data$RHQ140==7,2,
                                       ifelse(rhq.data$RHQ140==9,2,NA))))
rhq.data$RHQ140[is.na(rhq.data$RHQ140)] <- 2

rhq.data$RHQ141 <-ifelse(rhq.data$RHQ141==1,1,
                         ifelse(rhq.data$RHQ141==2,2,
                                ifelse(rhq.data$RHQ141==7,2,
                                       ifelse(rhq.data$RHQ141==9,2,NA))))
rhq.data$RHQ141[is.na(rhq.data$RHQ141)] <- 2

rhq.data$RHD143 <-ifelse(rhq.data$RHD143==1,1,
                         ifelse(rhq.data$RHD143==2,2,
                                ifelse(rhq.data$RHD143==7,2,
                                       ifelse(rhq.data$RHD143==9,2,NA))))
rhq.data$RHD143[is.na(rhq.data$RHD143)] <- 2

rhq.data <- rhq.data %>%  
  mutate(pregnant = ifelse(SEQN <= 9965,RHQ140,
                           ifelse(SEQN >= 9966 & SEQN <= 21004,RHQ141,
                                  ifelse(SEQN >= 21005,RHD143,NA)))) 

rhq.data <- rhq.data[,-c(2:4)]


##3.Merge data ####
DATA1 <- plyr::join_all(list(demo.data,bmi.data,smq,alq,paq,HEI,bpx.data,ghb.data,glu.data,fastqx,CBC.data,SEfolate,RBCfolate,
                             SEVB12,DRVB12,MMA,rhq.data,MMA_mortality),by='SEQN', type='left')

colnames(DATA1)[colnames(DATA1) == "RIDAGEYR"] <- "Age"
colnames(DATA1)[colnames(DATA1) == "RIAGENDR"] <- "Sex"
colnames(DATA1)[colnames(DATA1) == "RIDRETH1"] <- "Race"
colnames(DATA1)[colnames(DATA1) == "DMDEDUC2"] <- "Education"
colnames(DATA1)[colnames(DATA1) == "INDFMPIR"] <- "PIR"
colnames(DATA1)[colnames(DATA1) == "LBXGLU"] <- "Fasting_Glucose"
colnames(DATA1)[colnames(DATA1) == "LBXGH"] <- "Glycohemoglobin"

DATA1 <- DATA1[,-c(15:17,22:34)]


dim(DATA1) # 51057   29


##4.Adjust the data####
DATA2 <- DATA1
dim(DATA2)   # 51057   

###1.Exclude pregnant women####
DATA2$pregnant[is.na(DATA2$pregnant)] <- 2
DATA2 <- DATA2[DATA2$pregnant > 1, ]
dim(DATA2) # 50202

###2.Exclude age####
DATA2 <- DATA2[DATA2$Age >= 20, ]
dim(DATA2) # 25926 

###3.Exclude the absence of death data ####
MORTSTAT.exclude.index <- which(is.na(DATA2$MORTSTAT))
length(MORTSTAT.exclude.index) # 44

DATA2 <- subset.data.frame(DATA2,(!is.na(MORTSTAT)))
dim(DATA2) # 25882 

###4.The missing value of folate ####
RBCfolate.exclude.index <- which(is.na(DATA2$RBCfolate))
length(RBCfolate.exclude.index) # 2976

DATA2 <- subset.data.frame(DATA2,(!is.na(RBCfolate)))
dim(DATA2) # 22906

###5.The missing value of MMA ####
MMA.exclude.index <- which(is.na(DATA2$MMA))
length(MMA.exclude.index) # 419

DATA2 <- subset.data.frame(DATA2,(!is.na(MMA)))
dim(DATA2) # 22487 


##5.data imputation ####
DATA2$Physical <- as.numeric(DATA2$Physical)
A1 <- c("Age","Sex","Race","Education","PIR","Glycohemoglobin","Fasting_Glucose","BPXSY",
        "BMXBMI","Smoking_status","Drinking_status","Physical","HEI2015","Food_fast",
        "SEVB12","DRVB12"   )

## imputation
temp1 <- DATA2[,A1]
imputed <-mice(temp1,method = "pmm",m=5,maxit = 5,seed=1000)
DATA3 <- complete(imputed)
DATA3$SEQN <- DATA2$SEQN

A2 <- c("SEQN","SDMVPSU","SDMVSTRA","WTMEC2YR","RBCfolate","MMA",
        "pregnant","ELIGSTAT","MORTSTAT","UCOD_LEADING","PERMTH_EXM",
        "PERMTH_Y","MORTSTAT_CVD")

temp2 <- DATA2[,A2]

DATA3 <- left_join(temp2,DATA3,by="SEQN") 


##6.Division of independent variables ####
DATA4 <- DATA3

###1.RBCfolate####
res.cut1 <- surv_cutpoint(DATA4, time = "PERMTH_Y", event = "MORTSTAT_CVD",
                          variables = c("RBCfolate"))
summary(res.cut1)  # 1706.94

DATA4$RBCfolate_category <- ifelse(DATA4$RBCfolate <= 1706.94,"Lower RBCfolate","Higher RBCfolate")

###2.MMA####
res.cut2 <- surv_cutpoint(DATA4, time = "PERMTH_Y", event = "MORTSTAT_CVD",
                          variables = c("MMA"))
summary(res.cut2)  # 182

DATA4$MMA_category <- ifelse(DATA4$MMA <= 182,"Lower MMA","Higher MMA")


##7.Variable extension ####
DATA5 <- DATA4

####1.Gender ####
DATA5$Sex <- ifelse(DATA5$Sex == 1, 'Male', 'Female')

####2.Race ####
DATA5$Race <- recode_factor(DATA5$Race, 
                            '1' = 'Mexican American',
                            '2' = 'Other Hispanic',
                            '3' = 'Non-Hispanic White',
                            '4' = 'Non-Hispanic Black',
                            '5' = 'Other')

####3.Education ####
DATA5$Education <-  recode_factor(DATA5$Education,
                                  '1' = 'Less than high school graduate',
                                  '2' = 'Less than high school graduate',
                                  '3' = 'High school graduate or GED',
                                  '4' = 'Some college',
                                  '5' = 'College graduate or above')

####4.Smoking ####
# Smoking at least 100 cigarettes - SMQ020:1 Yes; 2 No; 7 Refused; 9 Don't know; .	Missing
# Are you smoking now? - SMQ040:1 Every day; 2 Some days; 3 Not at all; 7 Refused; 9 Don't know; .	Missing
# Smoking,1 is Never smoker, 2 is Former smoker, 3 is Current smoker
DATA5$Smoking_status <-  recode_factor(DATA5$Smoking_status,
                                       '1' = 'Never smoker',
                                       '2' = 'Former smoker',
                                       '3' = 'Current smoker')

####5.Drinking alcohol ####
DATA5$Drinking_status <-  recode_factor(DATA5$Drinking_status,
                                        '3' = 'Moderate_drinking',
                                        '4' = 'Heavy_drinking',
                                        '1' = 'Never_drinker',
                                        '2' = 'Light_drinker')

####6.Combined classification####
DATA5$RBCfolate_MMA <- ifelse(DATA5$RBCfolate_category=="Lower RBCfolate"&DATA5$MMA_category=="Lower MMA","Lower RBCfolate & Lower MMA",
                              ifelse(DATA5$RBCfolate_category=="Lower RBCfolate"&DATA5$MMA_category=="Higher MMA","Lower RBCfolate & Higher MMA",
                                     ifelse(DATA5$RBCfolate_category=="Higher RBCfolate"&DATA5$MMA_category=="Lower MMA","Higher RBCfolate & Lower MMA",
                                            ifelse(DATA5$RBCfolate_category=="Higher RBCfolate"&DATA5$MMA_category=="Higher MMA","Higher RBCfolate & Higher MMA",NA))))


##8.Save final data####
ALL_DATA <- DATA5
save(ALL_DATA, file = "ALL_DATA.Rdata")


##9.Description table####
final <- ALL_DATA

final$RBCfolate <- log(final$RBCfolate,base = 2)
final$MMA <- log(final$MMA,base = 2)

final$RBCfolate_category <- as.factor(final$RBCfolate_category)  
final$RBCfolate_category <- relevel(final$RBCfolate_category, ref ='Lower RBCfolate')

final$MMA_category <- as.factor(final$MMA_category)  
final$MMA_category <- relevel(final$MMA_category, ref ='Lower MMA')

final$RBCfolate_MMA <- as.factor(final$RBCfolate_MMA)  
final$RBCfolate_MMA <- relevel(final$RBCfolate_MMA, ref ='Lower RBCfolate & Lower MMA')

final_design <- svydesign(data = final,
                          ids = ~SDMVPSU,
                          strata = ~SDMVSTRA,
                          nest = TRUE,
                          weights = ~ WTMEC2YR,
                          survey.lonely.psu="adjust") 
summary(final_design)

###1.Median table ####
table <- tbl_svysummary(final_design, by = RBCfolate_category, missing = 'no',
                        include = c(MORTSTAT, MORTSTAT_CVD, UCOD_LEADING, PERMTH_EXM, PERMTH_Y),
                        statistic = list(all_continuous() ~ "{mean} ± {sd}", 
                                         all_categorical() ~ "{n_unweighted} ({p}%)")) %>%  
  modify_header(all_stat_cols() ~ "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") %>% 
  add_overall() %>%
  add_p()  %>%
  add_n(statistic = "{N_nonmiss_unweighted}", #  "{n}, {N_miss_unweighted}
        col_label = "**N**",
        footnote = TRUE) %>%
  as_flex_table() %>% # Word
  flextable::save_as_docx(path = 'Median.docx')


###2.RBCfolate_category ####
table <- tbl_svysummary(
  data = final_design,
  by = RBCfolate_category,
  label = list(Age = "Age", Sex = "Sex", Race = "Race", Education = "Education",
               PIR="PIR",Glycohemoglobin="Glycohemoglobin",Fasting_Glucose="Fasting_Glucose",
               BMXBMI="BMXBMI",Smoking_status="Smoking_status",Drinking_status="Drinking_status",
               Physical="Physical",HEI2015="HEI2015",BPXSY="BPXSY",Food_fast="Food_fast",
               RBCfolate="RBCfolate",MMA="MMA",SEVB12="SEVB12",DRVB12="DRVB12"),
  statistic = list(all_continuous() ~ "{mean} ± {sd}", 
                   all_categorical() ~ "{n_unweighted} ({p}%)"),
  digits = list(all_continuous() ~ 2, all_categorical() ~ 2),
  type = NULL,
  value = NULL,
  missing = c("no"),
  missing_text = "Unknown",
  missing_stat = "{N_miss_unweighted}",
  sort = all_categorical(FALSE) ~ "alphanumeric",
  percent = c("column"),
  include = c(Age,Sex,Race,Education,PIR,Glycohemoglobin,
              Fasting_Glucose,BMXBMI,Smoking_status,
              Drinking_status,Physical,HEI2015,BPXSY,
              Food_fast,RBCfolate,MMA,SEVB12,DRVB12))%>%
  as_flex_table() %>% #  Word
  flextable::save_as_docx(path = 'RBCfolate.docx')


###3.MMA_category ####
table <- tbl_svysummary(
  data = final_design,
  by = MMA_category,
  label = list(Age = "Age", Sex = "Sex", Race = "Race", Education = "Education",
               PIR="PIR",Glycohemoglobin="Glycohemoglobin",Fasting_Glucose="Fasting_Glucose",
               BMXBMI="BMXBMI",Smoking_status="Smoking_status",Drinking_status="Drinking_status",
               Physical="Physical",HEI2015="HEI2015",BPXSY="BPXSY",Food_fast="Food_fast",
               RBCfolate="RBCfolate",MMA="MMA",SEVB12="SEVB12",DRVB12="DRVB12"),
  statistic = list(all_continuous() ~ "{mean} ± {sd}", 
                   all_categorical() ~ "{n_unweighted} ({p}%)"),
  digits = list(all_continuous() ~ 2, all_categorical() ~ 2),
  type = NULL,
  value = NULL,
  missing = c("no"),
  missing_text = "Unknown",
  missing_stat = "{N_miss_unweighted}",
  sort = all_categorical(FALSE) ~ "alphanumeric",
  percent = c("column"),
  include = c(Age,Sex,Race,Education,PIR,Glycohemoglobin,
              Fasting_Glucose,BMXBMI,Smoking_status,
              Drinking_status,Physical,HEI2015,BPXSY,
              Food_fast,RBCfolate,MMA,SEVB12,DRVB12))%>%
  as_flex_table() %>% # Word
  flextable::save_as_docx(path = 'MMA.docx')
