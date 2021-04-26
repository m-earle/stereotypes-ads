

numdata <- read.csv("raw data numeric aug 22 with gender.csv", header = TRUE, stringsAsFactors = FALSE)

numdata[numdata == ""] <- 888

View(numdata)

##just keep men and women

table(numdata$gender)

314+340+2+2

genderdat <- numdata[(numdata$gender < 3), ]
genderdata <- genderdat[complete.cases(genderdat$gender), ]


##check for completion

table(genderdata$Duration..in.seconds.)
table(genderdata$Finished)

#excluding duplicate IP addresses
library(dplyr)

IPs <- as.data.frame(table(genderdata$IPAddress))
View(IPs)

dupIPs <- filter(IPs, IPs$Freq >= 2)
View(dupIPs)

#1 duplicate IP

which(grepl("64.184.8.188", genderdata$IPAddress))

cleandata <- genderdata[-c(645), ]

cleandata[is.na(cleandata)]<- 888

group <- ifelse((cleandata$stereof1b_1 < 888) & (cleandata$stereom1b_1 == 888) & 
                  (cleandata$neutf1b_1 == 888) & (cleandata$neutm1b_1 == 888), "stereoF",
                ifelse ((cleandata$stereof1b_1 == 888) & (cleandata$stereom1b_1 < 888) & 
                          (cleandata$neutf1b_1 == 888) & (cleandata$neutm1b_1 == 888), "stereoM",
                        ifelse ((cleandata$stereof1b_1 == 888) & (cleandata$stereom1b_1 == 888) & 
                                  (cleandata$neutf1b_1 < 888 ) & (cleandata$neutm1b_1 == 888), "neutF",
                                ifelse ((cleandata$stereof1b_1 == 888) & (cleandata$stereom1b_1 == 888) & 
                                          (cleandata$neutf1b_1 == 888) & (cleandata$neutm1b_1 < 888), "neutM", NA))))

View(group)

cleandata <- data.frame(cleandata, group)

## exclude people who failed attention check 

table(cleandata$att)

## 0 people failed attention check

## compute scales

cleandata[cleandata == 888] <- NA
cleandata[cleandata == 999] <- NA

#### SDO ####

cleandata$SDO3r <- recode(cleandata$SDO3, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)

#SDO items are seen as character, need to transform into numeric

cleandata <- transform(cleandata, SDO3 = as.numeric(SDO3))

sapply(cleandata, mode)

cor(cleandata$SDO3r, cleandata$SDO3, use = "complete.obs")

cleandata$SDO4r <- recode(cleandata$SDO4, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$SDO7r <- recode(cleandata$SDO7, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$SDO8r <- recode(cleandata$SDO8, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)

cleandata <- transform(cleandata, SDO1 = as.numeric(SDO1),
                       SDO2 = as.numeric(SDO2),
                       SDO5 = as.numeric(SDO5),
                       SDO6 = as.numeric(SDO6))

sapply(cleandata, mode)

#reliability for SDO

library(psych)

SDO <- cleandata[c("SDO1", "SDO2", "SDO3r", "SDO4r", "SDO5", "SDO6", "SDO7r", "SDO8r")]
alpha(SDO)

#compute aggregate for SDO

cleandata$SDO <- rowMeans(cleandata[c
                                    ("SDO1", "SDO2", "SDO3r", "SDO4r", "SDO5", "SDO6", "SDO7r", "SDO8r")], na.rm=TRUE)
summary(cleandata$SDO)

#### RWA ####

cleandata$RWA1r <- recode(cleandata$RWA1, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$RWA2r <- recode(cleandata$RWA2, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$RWA6r <- recode(cleandata$RWA6, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$RWA7r <- recode(cleandata$RWA7, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$RWA9r <- recode(cleandata$RWA9, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)
cleandata$RWA11r <- recode(cleandata$RWA11, '1'= 7, '2'= 6, '3'= 5, '4'= 4, '5'= 3, '6'= 2, '7'= 1)

cleandata <- transform(cleandata, RWA3 = as.numeric(RWA3),
                       RWA4 = as.numeric(RWA4),
                       RWA5 = as.numeric(RWA5),
                       RWA8 = as.numeric(RWA8),
                       RWA10 = as.numeric(RWA10),
                       RWA12 = as.numeric(RWA12))

RWA <- cleandata[c("RWA1r", "RWA2r", "RWA3", "RWA4",
                   "RWA5", "RWA6r", "RWA7r", "RWA8",
                   "RWA9r", "RWA10", "RWA11r", "RWA12")]
alpha(RWA)

cleandata$RWA <- rowMeans(cleandata[c
                                    ("RWA1r", "RWA2r", "RWA3", "RWA4",
                                      "RWA5", "RWA6r", "RWA7r", "RWA8",
                                      "RWA9r", "RWA10", "RWA11r", "RWA12")], na.rm=TRUE)
summary(cleandata$RWA)

#### compute do you think ad is effective ####

cleandata[,c(24:265)] <- as.numeric(as.character(unlist(cleandata[,c(24:265)])))

str(cleandata)
sapply(cleandata, mode)

cleandata$adeff <- rowMeans(cleandata[c
                                      ("stereof1b_1", "stereof2b_1", "stereof3b_1"
                                        , "stereof4b_1", "stereo5b_1", "stereof6b_1",
                                        "stereof7b_1", "stereom1b_1", "stereom2b_1", "stereom3b_1",
                                        "stereom4b_1", "stereom5b_1", "stereo6b_1", "stereom7b_1",
                                        "neutf1b_1", "neutf2b_1", "neutf3b_1", "neutf4b_1", 
                                        "neutf5b_1", "neutf6b_1", "neutf7b_1", "neutm1b_1", "neutm2b_1", 
                                        "neutm3b_1", "neutm4b_1", "neutm5b_1", 
                                        "neutm6b_1", "neutm7b_1")], na.rm=TRUE)

colnames(cleandata)
summary(cleandata$adeff)


cleandata$adappeal <- rowMeans(cleandata[c
                                         ("stereof1b_2", "stereof2b_2", "stereof3b_2"
                                           , "stereof4b_2", "stereo5b_2", "stereof6b_2",
                                           "stereof7b_2", "stereom1b_2", "stereom2b_2", "stereom3b_2",
                                           "stereom4b_2", "stereom5b_2", "stereo6b_2", "stereom7b_2",
                                           "neutf1b_2", "neutf2b_2", "neutf3b_2", "neutf4b_2", 
                                           "neutf5b_2", "neutf6b_2", "neutf7b_2", "neutm1b_2", "neutm2b_2", 
                                           "neutm3b_2", "neutm4b_2", "neutm5b_2", 
                                           "neutm6b_2", "neutm7b_2")], na.rm=TRUE)
summary(cleandata$adappeal)

cleandata$adpurch <- rowMeans(cleandata[c
                                        ("stereof1b_3", "stereof2b_3", "stereof3b_3"
                                          , "stereof4b_3", "stereo5b_3", "stereof6b_3",
                                          "stereof7b_3", "stereom1b_3", "stereom2b_3", "stereom3b_3",
                                          "stereom4b_3", "stereom5b_3", "stereo6b_3", "stereom7b_3",
                                          "neutf1b_3", "neutf2b_3", "neutf3b_3", "neutf4b_3", 
                                          "neutf5b_3", "neutf6b_3", "neutf7b_3", "neutm1b_3", "neutm2b_3", 
                                          "neutm3b_3", "neutm4b_3", "neutm5b_3", 
                                          "neutm6b_3", "neutm7b_3")], na.rm=TRUE)
summary(cleandata$adpurch)

#### self-efficacy ####

cleandata$eff <- rowMeans(cleandata[c
                                    ("eff1", "eff2", "eff3", "eff4",
                                      "eff5", "eff6", "eff7", "eff8",
                                      "eff9", "eff10")], na.rm=TRUE)
summary(cleandata$eff)

eff <- cleandata[c("eff1", "eff2", "eff3", "eff4",
                   "eff5", "eff6", "eff7", "eff8",
                   "eff9", "eff10")]
alpha(eff)

colnames(cleandata)


#### jobcandidate selection ####

cleandata$recsarah <- rowMeans(cleandata[c
                                         ("sarahv1", "sarahv2")], na.rm=TRUE)
summary(cleandata$recsarah)

cleandata$recrebecca <- rowMeans(cleandata[c
                                           ("rebeccav1", "rebeccav2")], na.rm=TRUE)
summary(cleandata$recrebecca)

cleandata$recmichael <- rowMeans(cleandata[c
                                           ("michaelv1", "michaelv2")], na.rm=TRUE)
summary(cleandata$recmichael)

cleandata$recjoshua <- rowMeans(cleandata[c
                                          ("joshuav1", "joshuav2")], na.rm=TRUE)
summary(cleandata$recjoshua)

#aggregating selection of female candidate and male candidate

cleandata$recF <- rowMeans(cleandata[c
                                     ("recsarah", "recrebecca")], na.rm=TRUE)
cleandata$recM <- rowMeans(cleandata[c
                                     ("recjoshua", "recmichael")], na.rm=TRUE)


summary(cleandata$recF)
summary(cleandata$recM)

#looking at rank order

#recode so that higher score = greater preference

colnames(cleandata)

#sarah

cleandata$ranksarah <- rowMeans(cleandata[c
                                          ("rankv1_1", "rankv2_3")], na.rm=TRUE)
summary(cleandata$ranksarah)

cleandata$ranksarah <- recode(cleandata$ranksarah, '1'= 4, '2'= 3, '3'= 2, '4'= 1)
summary(cleandata$ranksarah)

#rebecca

cleandata$rankrebecca <- rowMeans(cleandata[c
                                            ("rankv1_2", "rankv2_4")], na.rm=TRUE)
summary(cleandata$rankrebecca)

cleandata$rankrebecca <- recode(cleandata$rankrebecca, '1'= 4, '2'= 3, '3'= 2, '4'= 1)
summary(cleandata$rankrebecca)

#michael

cleandata$rankmichael <- rowMeans(cleandata[c
                                            ("rankv1_3", "rankv2_1")], na.rm=TRUE)
summary(cleandata$rankmichael)

cleandata$rankmichael <- recode(cleandata$rankmichael, '1'= 4, '2'= 3, '3'= 2, '4'= 1)
summary(cleandata$rankmichael)

#joshua

cleandata$rankjoshua <- rowMeans(cleandata[c
                                           ("rankv1_4", "rankv2_2")], na.rm=TRUE)
summary(cleandata$rankjoshua)

cleandata$rankjoshua <- recode(cleandata$rankjoshua, '1'= 4, '2'= 3, '3'= 2, '4'= 1)
summary(cleandata$rankjoshua)

#women

cleandata$rankF <- rowMeans(cleandata[c
                                      ("ranksarah", "rankrebecca")], na.rm=TRUE)
summary(cleandata$rankF)

#men

cleandata$rankM <- rowMeans(cleandata[c
                                      ("rankmichael", "rankjoshua")], na.rm=TRUE)
summary(cleandata$rankM)

colnames(cleandata)

#### re-naming life goal variables ####

cleandata$goalchild <- cleandata$lifegoal_1
cleandata$goalmarry <- cleandata$lifegoal_2
cleandata$goalcareer <- cleandata$lifegoal_3
cleandata$goalmoney <- cleandata$lifegoal_4

summary(cleandata$goalchild)
summary(cleandata$goalmarry)
summary(cleandata$goalcareer)
summary(cleandata$goalmoney)

####  BS ####

cleandata$BS <- rowMeans(cleandata[c
                                   ("ASI1", "ASI2", "ASI4", "ASI5",
                                     "ASI10", "ASI11")], na.rm=TRUE)
summary(cleandata$BS)

BS <- cleandata[c("ASI1", "ASI2", "ASI4", "ASI5",
                  "ASI10", "ASI11")]
alpha(BS)

#### HS ####

cleandata$HS <- rowMeans(cleandata[c
                                   ("ASI3", "ASI6", "ASI7", "ASI8",
                                     "ASI9", "ASI12")], na.rm=TRUE)
summary(cleandata$HS)

HS <- cleandata[c("ASI3", "ASI6", "ASI7", "ASI8",
                  "ASI9", "ASI12")]
alpha(HS)

#### creating effect codes ####

#ef1 compares female to male
#ef2 compares stereo F to netrual F
#ef3 compares stereo M to neutral M

cleandata$ef1 <- recode(cleandata$group, 'stereoF'= 0.5, 'stereoM'= -0.5, 'neutF'= 0.5, 'neutM'= -0.5)
cleandata$ef2 <- recode(cleandata$group, 'stereoF'= 1, 'stereoM'= 0, 'neutF'= -1, 'neutM'= 0)
cleandata$ef3 <- recode(cleandata$group, 'stereoF'= 0, 'stereoM'= 1, 'neutF'= 0, 'neutM'= -1)

cor(cleandata$ef2, cleandata$ef3)
table(cleandata$group)
table(cleandata$ef1)
table(cleandata$ef2)
table(cleandata$ef3)

#### check outliers ####

out <- cleandata[c("RWA", "SDO", "HS", "BS", "adappeal", "adeff", "eff", "rankF",
                   "rankM", "recF", "recM", "goalcareer", "goalchild", "goalmarry", "goalmoney")]

colnames(out)

apply(out, 2, function(i){ #data frame, margin (i.e., columns), function
  table(scale(i, center = TRUE, scale = TRUE)) ##this creates z scores for each variable in out (dataframe) and prints
})

##1 high SDO
## 8 low adappeal
## 9 low addeff
## 5 low eff
## 2 low goalmoney

cleandata$SDOwin = cleandata$SDO ##adding in all variables to be meanwinsorized
cleandata$adeffwin = cleandata$adeff
cleandata$effwin = cleandata$eff
cleandata$moneywin = cleandata$goalmoney

winvars <- cleandata[c("SDOwin", "adeffwin", "effwin", "moneywin")]

for (i in names(winvars)){
  cleandata[[i]][cleandata[[i]] > mean(cleandata[[i]], na.rm= TRUE) + (3*(sd(cleandata[[i]], na.rm = TRUE)))] <- mean(cleandata[[i]], na.rm= TRUE) +(3*(sd(cleandata[[i]], na.rm = TRUE)))
}

describe(cleandata$SDO)
describe(cleandata$SDOwin)

#### normality ####

continuous <- cleandata[c("RWA", "SDOwin", "HS", "BS", "adappeal", "adeffwin", "effwin", "moneywin",
                          "goalchild", "goalmarry", "goalcareer")]

for (i in names(continuous)){
  print(i)
  print(describe(cleandata[[i]]))
}

## all good##

#### centering ID predictors ####

cleandata$RWA.c <- scale(cleandata$RWA, center = TRUE, scale = FALSE)
cleandata$SDOwin.c <- scale(cleandata$SDOwin, center = TRUE, scale = FALSE)
cleandata$HS.c <- scale(cleandata$HS, center = TRUE, scale = FALSE)
cleandata$BS.c <- scale(cleandata$BS, center = TRUE, scale = FALSE)

describe(cleandata$BS.c)

### demographics

table(cleandata$gender)
## 1 = male, 2 = female

describe(cleandata$age)

table(cleandata$ethnic)

479+5+1+1+9+4+1 #white
5+1+1+61+1+3 #black
1+9+1+54 #asian
11+2+19 # hispanic
1+ 4+ 1+1+2 #other

#### models ####

library(lavaan)

#ef1 compares female to male
#ef2 compares stereo F to netrual F
#ef3 compares stereo M to neutral M


#### no interactions ####


modelcondonlyMF <- 'effwin ~ ef1 + ef2 + ef3
                    goalchild ~ ef1 + ef2 +ef3
                    goalmarry ~ ef1 + ef2 + ef3
                    goalcareer ~ ef1 + ef2 + ef3
                    moneywin ~ ef1 + ef2 + ef3
                    rankF ~ ef1 + ef2 + ef3'

#estimate parameters

results.condonly <- sem(modelcondonlyMF, data = cleandata, group = 'gender', missing = "ML")
summary(results.condonly, standardized = TRUE)


### interactions with RWA ####

modelcondRWA <- 'effwin ~  ef1 + ef2 + ef3 + RWA.c + RWA.c:ef1 + RWA.c:ef2 + RWA.c:ef3
                goalchild ~ ef1 + ef2 + ef3 + RWA.c + RWA.c:ef1 + RWA.c:ef2 + RWA.c:ef3
                goalmarry ~ ef1 + ef2 + ef3 + RWA.c + RWA.c:ef1 + RWA.c:ef2 + RWA.c:ef3
                goalcareer ~ ef1 + ef2 + ef3 + RWA.c + RWA.c:ef1 + RWA.c:ef2 + RWA.c:ef3
                moneywin ~ ef1 + ef2 + ef3 + RWA.c + RWA.c:ef1 + RWA.c:ef2 + RWA.c:ef3
                rankF ~ ef1 + ef2 + ef3 + RWA.c + RWA.c:ef1 + RWA.c:ef2 + RWA.c:ef3'
results.condRWA <- sem(modelcondRWA, data = cleandata, group = 'gender', missing = "ML")
summary(results.condRWA, standardized = TRUE)

#### interactions with SDO ####

modelcondSDO <- 'effwin ~  ef1 + ef2 + ef3 + SDOwin.c + SDOwin.c:ef1 + SDOwin.c:ef2 + SDOwin.c:ef3
                goalchild ~ ef1 + ef2 + ef3 + SDOwin.c + SDOwin.c:ef1 + SDOwin.c:ef2 + SDOwin.c:ef3
                goalmarry ~ ef1 + ef2 + ef3 + SDOwin.c + SDOwin.c:ef1 + SDOwin.c:ef2 + SDOwin.c:ef3
                goalcareer ~ ef1 + ef2 + ef3 + SDOwin.c + SDOwin.c:ef1 + SDOwin.c:ef2 + SDOwin.c:ef3
                moneywin ~ ef1 + ef2 + ef3 + SDOwin.c + SDOwin.c:ef1 + SDOwin.c:ef2 + SDOwin.c:ef3
                rankF ~ ef1 + ef2 + ef3 + SDOwin.c + SDOwin.c:ef1 + SDOwin.c:ef2 + SDOwin.c:ef3'
results.condSDO <- sem(modelcondSDO, data = cleandata, group = 'gender', missing = "ML")
summary(results.condSDO, standardized = TRUE)


library(interactions)
library(semTools)

##male participants, interaction between women (vs men) ads and SDO in predicting female rank

ef1.SDO.rankf <- probe2WayMC(results.condSDO, nameX = c("ef1", "SDOwin.c", "SDOwin.c:ef1"), 
            nameY = "rankF",
            modVar = "SDOwin.c", valProbe = c(-1, 0 ,1), group = 1)
ef1.SDO.rankf
plotProbe(ef1.SDO.rankf, xlim = c(-1, 1), xlab = "Condition (Men = -1, Women = 1)", ylab = 'Rank Female Candidate')

## male participants, interaction between stereotypical men (vs neutral men) ad and SDO in predicting money goals

probe2WayMC(results.condSDO, nameX = c("ef3", "SDOwin.c", "SDOwin.c:ef3"), 
            nameY = "moneywin",
            modVar = "SDOwin.c", valProbe = c(-1, 0 ,1), group = 1)

## female participants ,interaction betwen stereotypical women (vs neutral women) ad and SDO predicting money goals

probe2WayMC(results.condSDO, nameX = c("ef2", "SDOwin.c", "SDOwin.c:ef2"), 
            nameY = "moneywin",
            modVar = "SDOwin.c", valProbe = c(-1, 0 ,1), group = 2)

#### interactions with HS ####

modelcondHS <- 'effwin ~ ef1 + ef2 + ef3 + HS.c + HS.c:ef1 + HS.c:ef2 + HS.c:ef3
                goalchild ~ ef1 + ef2 + ef3 + HS.c + HS.c:ef1 + HS.c:ef2 + HS.c:ef3
                goalmarry ~ ef1 + ef2 + ef3 + HS.c + HS.c:ef1 + HS.c:ef2 + HS.c:ef3
                goalcareer ~ ef1 + ef2 + ef3 + HS.c + HS.c:ef1 + HS.c:ef2 + HS.c:ef3
                moneywin ~ ef1 + ef2 + ef3 + HS.c + HS.c:ef1 + HS.c:ef2 + HS.c:ef3
                rankF ~ ef1 + ef2 + ef3 + HS.c + HS.c:ef1 + HS.c:ef2 + HS.c:ef3'

results.condHS <- sem(modelcondHS, data = cleandata, group = 'gender', missing = "ML")
summary(results.condHS, standardized = TRUE)


##probing interactions ###

## HS X ef1 in predicting goal child (male participants)

probe2WayMC(results.condHS, nameX = c("ef1", "HS.c", "HS.c:ef1"), 
            nameY = "goalchild",
            modVar = "HS.c", valProbe = c(-1, 0 ,1), group = 1)

## HS x ef3 in predicting money goal (male participants)

probe2WayMC(results.condHS, nameX = c("ef3", "HS.c", "HS.c:ef3"), 
            nameY = "moneywin",
            modVar = "HS.c", valProbe = c(-1, 0 ,1), group = 1)

##HS X ef1 in predicting rankF (male participants)

probe2WayMC(results.condHS, nameX = c("ef1", "HS.c", "HS.c:ef1"), 
            nameY = "rankF",
            modVar = "HS.c", valProbe = c(-1, 0 ,1), group = 1)

## HS x ef2 in predicting money goal (female participants)

probe2WayMC(results.condHS, nameX = c("ef2", "HS.c", "HS.c:ef2"), 
            nameY = "moneywin",
            modVar = "HS.c", valProbe = c(-1, 0 ,1), group = 2)

#### interactions with BS ####

modelcondBS.c <- 'effwin ~ ef1 + ef2 + ef3 + BS.c + BS.c:ef1 + BS.c:ef2 + BS.c:ef3
                goalchild ~ ef1 + ef2 + ef3 + BS.c + BS.c:ef1 + BS.c:ef2 + BS.c:ef3
                goalmarry ~ ef1 + ef2 + ef3 + BS.c + BS.c:ef1 + BS.c:ef2 + BS.c:ef3
                goalcareer ~ ef1 + ef2 + ef3 + BS.c + BS.c:ef1 + BS.c:ef2 + BS.c:ef3
                moneywin ~ ef1 + ef2 + ef3 + BS.c + BS.c:ef1 + BS.c:ef2 + BS.c:ef3
                rankF ~ ef1 + ef2 + ef3 + BS.c + BS.c:ef1 + BS.c:ef2 + BS.c:ef3'

results.condBS <- sem(modelcondBS.c, data = cleandata, group = 'gender', missing = "ML")
summary(results.condBS, standardized = TRUE)


## BS x ef2 in predicting rank F (male participants)

probe2WayMC(results.condBS, nameX = c("ef2", "BS.c", "BS.c:ef2"), 
            nameY = "rankF",
            modVar = "BS.c", valProbe = c(-1, 0 ,1), group = 1)
