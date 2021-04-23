setwd("XX")

Mergedfile <- read.csv("XX")
View(Mergedfile)

Mergedfile[Mergedfile==999] <- NA
Mergedfile[Mergedfile==""] <- NA

rangedf <- Mergedfile[4:33]

describe(rangedf)

##categorical/nominal variables
## gender, product, housework, childcare, petcare, cookserve, appear,
## yardwork, mechtech, houserepair, sport, career, leisure

##continuous
## age, obj, dismember, bodyemp, silence, passive, pose, childishadult,
## childasadult, sexual, victim, assert, violent, ideal

##adults only

mergedfile.adult <- subset(Mergedfile, agemean >= 3)

#### categorical variables ####

## interrater reliability (nominal) ####

install.packages('irr')
library('irr')

## Light's Kappa (average kappa across all rater pairs) for nominal variables

##.21-.40 fair agreement
## .41-.60 moderate agreement
## .61-.80 substantial agreement
## .81 - 1.00 almost perfect agreement

## Landis & Koch 1997

##gender

gender <- mergedfile.adult[c("gender_ME", "gender_HH", "gender_EP", "gender_NG", "gender_EC")]

kappam.fleiss(gender)

## product

product <- mergedfile.adult[c("product_ME", "product_HH", "product_EP", "product_NG", "product_EC")]
kappam.fleiss(product)

##housework (DROP- low frequencies)

housework <- mergedfile.adult[c("housework_ME", "housework_HH", "housework_EP", "housework_NG", "housework_EC")]

describe(housework)

kappam.fleiss(housework)

##childcare

childcare <- mergedfile.adult[c("childcare_ME", "childcare_HH", "childcare_EP", "childcare_NG", "childcare_EC")]

describe(childcare)

kappam.fleiss(childcare)

## petcare (DROP- low frequencies)

petcare <- mergedfile.adult[c("petcare_ME", "petcare_HH", "petcare_EP", "petcare_NG", "petcare_EC")]

describe(petcare)

##cookserve (DROP- low frequenices)

cookserve <- mergedfile.adult[c("cookserve_ME", "cookserve_HH", "cookserve_EP", "cookserve_NG", "cookserve_EC")]

describe(cookserve)

##appear

appear <- mergedfile.adult[c("appear_ME", "appear_HH", "appear_EP", "appear_NG", "appear_EC")]

describe(appear)

kappam.fleiss(appear)

##yardwork (DROP- low frequencies)

yardwork <- mergedfile.adult[c("yardwork_ME", "yardwork_HH", "yardwork_EP", "yardwork_NG", "yardwork_EC")]

describe(yardwork)

##mechtech (DROP- low frequencies)

mechtech <- mergedfile.adult[c("mechtech_ME", "mechtech_HH", "mechtech_EP", "mechtech_NG", "mechtech_EC")]

describe(mechtech)

##houserepair (DROP- low frequencies)

houserepair <- mergedfile.adult[c("houserepair_ME", "houserepair_HH", "houserepair_EP", "houserepair_NG", "houserepair_EC")]

describe(houserepair)

## sport

sport <- mergedfile.adult[c("sport_ME", "sport_HH", "sport_EP", "sport_NG", "sport_EC")]

describe(sport)

kappam.fleiss(sport)

## career

career <- mergedfile.adult[c("career_ME", "career_HH", "career_EP", "career_NG", "career_EC")]

describe(career)

kappam.fleiss(career)

##leisure

leisure <- mergedfile.adult[c("leisure_ME", "leisure_HH", "leisure_EP", "leisure_NG", "leisure_EC")]

describe(leisure)

kappam.fleiss(leisure)

#### chi square frequencies by gender ####

##discrepancies resolved through discussion use ME

table(mergedfile.adult$gender_ME)

options(scipen = 999) #get rid of scientific notation

dichotomous <- mergedfile.adult[c("childcare_ME", "appear_ME", "sport_ME", "career_ME",
                                  "leisure_ME")]

for (i in names(dichotomous)){
  print(i)
  print(table(mergedfile.adult[[i]], mergedfile.adult$gender_ME))
  print(chisq.test(table(mergedfile.adult[[i]], mergedfile.adult$gender_ME)))
  print(chisq.test(table(mergedfile.adult[[i]], mergedfile.adult$gender_ME))$expected)
}


## interrater reliability (continuous) ####

#### interclass correlation

## age

age <- mergedfile.adult[c("age_ME", "age_HH", "age_EP", "age_NG", "age_EC")]

icc(age, model= 'twoway', type='agreement', unit= 'average')

## obj (low variance)

obj <- mergedfile.adult[c("obj_ME", "obj_HH", "obj_EP", "obj_NG", "obj_EC")]

describe(obj)

##dismember

dismember <- mergedfile.adult[c("dismember_ME", "dismember_HH", "dismember_EP", "dismember_NG", "dismember_EC")]

describe(dismember)

icc(dismember, model= 'twoway', type='agreement', unit= 'average')

##bodyemp

bodyemp <- mergedfile.adult[c("bodyemp_ME", "bodyemp_HH", "bodyemp_EP", "bodyemp_NG", "bodyemp_EC")]

describe(bodyemp)

icc(bodyemp, model= 'twoway', type='agreement', unit= 'average')


##silence

silence <- mergedfile.adult[c("silence_ME", "silence_HH", "silence_EP", "silence_NG", "silence_EC")]

describe(silence)

icc(silence, model= 'twoway', type='agreement', unit= 'average')

##passive

passive <- mergedfile.adult[c("passive_ME", "passive_HH", "passive_EP", "passive_NG", "passive_EC")]

describe(passive)

icc(passive, model= 'twoway', type='agreement', unit= 'average')

##pose

pose <- mergedfile.adult[c("pose_ME", "pose_HH", "pose_EP", "pose_NG", "pose_EC")]

describe(pose)

icc(pose, model= 'twoway', type='agreement', unit= 'average')

##childishadult

childishadult <- mergedfile.adult[c("childishadult_ME", "childishadult_HH", "childishadult_EP", "childishadult_NG", "childishadult_EC")]

describe(childishadult)

icc(childishadult, model= 'twoway', type='agreement', unit= 'average')

## childasadult (DROP)

##sexual

sexual <- mergedfile.adult[c("sexual_ME", "sexual_HH", "sexual_EP", "sexual_NG", "sexual_EC")]

describe(sexual)

icc(sexual, model= 'twoway', type='agreement', unit= 'average')

##victim (DROP no/little variance)

victim <- mergedfile.adult[c("victim_ME", "victim_HH", "victim_EP", "victim_NG", "victim_EC")]

describe(victim)

##assert

assert <- mergedfile.adult[c("assert_ME", "assert_HH", "assert_EP", "assert_NG", "assert_EC")]

describe(assert)

icc(assert, model= 'twoway', type='agreement', unit= 'average')

##violent (DROP no/little variance)

violent <- mergedfile.adult[c("violent_ME", "violent_HH", "violent_EP", "violent_NG", "violent_EC")]

describe(violent)

##ideal

ideal <- mergedfile.adult[c("ideal_ME", "ideal_HH", "ideal_EP", "ideal_NG", "ideal_EC")]

describe(ideal)

icc(ideal, model= 'twoway', type='agreement', unit= 'average')

### results for continuous variables ####

### compute means ####

mergedfile.adult$agemean <- rowMeans(subset(mergedfile.adult, select = c(age_ME, age_HH,
                                                              age_EP, age_NG, age_EC)), na.rm = FALSE)
mergedfile.adult$dismembermean <- rowMeans(subset(mergedfile.adult, select = c(dismember_ME, dismember_HH,
                                                                   dismember_EP, dismember_NG, dismember_EC)), na.rm = FALSE)
mergedfile.adult$bodyempmean <- rowMeans(subset(mergedfile.adult, select = c(bodyemp_ME, bodyemp_HH,
                                                                   bodyemp_EP, bodyemp_NG, bodyemp_EC)), na.rm = FALSE)
mergedfile.adult$silencemean <- rowMeans(subset(mergedfile.adult, select = c(silence_ME, silence_HH,
                                                                   silence_EP, silence_NG, silence_EC)), na.rm = FALSE)
mergedfile.adult$passivemean <- rowMeans(subset(mergedfile.adult, select = c(passive_ME, passive_HH,
                                                                   passive_EP, passive_NG, passive_EC)), na.rm = FALSE)
mergedfile.adult$posemean <- rowMeans(subset(mergedfile.adult, select = c(pose_ME, pose_HH,
                                                                   pose_EP, pose_NG, pose_EC)), na.rm = FALSE)
mergedfile.adult$childishadultmean <- rowMeans(subset(mergedfile.adult, select = c(childishadult_ME, childishadult_HH,
                                                                   childishadult_EP, childishadult_NG, childishadult_EC)), na.rm = FALSE)
mergedfile.adult$sexualmean <- rowMeans(subset(mergedfile.adult, select = c(sexual_ME, sexual_HH,
                                                                   sexual_EP, sexual_NG, sexual_EC)), na.rm = FALSE)
mergedfile.adult$assertmean <- rowMeans(subset(mergedfile.adult, select = c(assert_ME, assert_HH,
                                                                   assert_EP, assert_NG, assert_EC)), na.rm = FALSE)
mergedfile.adult$idealmean <- rowMeans(subset(mergedfile.adult, select = c(ideal_ME, ideal_HH,
                                                                   ideal_EP, ideal_NG, ideal_EC)), na.rm = FALSE)

##check outliers

out <- mergedfile.adult[c(150:159)]

colnames(out)

apply(out, 2, function(i){ #data frame, margin (i.e., columns), function
  table(scale(i, center = TRUE, scale = TRUE)) ##this creates z scores for each variable in out (dataframe) and prints
})

## 1 high dismember
## 2 high body emp
## 10 high silence
## 5 high passive
## 5 high pose
## 4 high childishadult
## 7 high sexual
## 4 high assert

mergedfile.adult$dismembermeanwin = mergedfile.adult$dismembermean ##adding in all variables to be meanwinsorized
mergedfile.adult$bodyempmeanwin = mergedfile.adult$bodyempmean #
mergedfile.adult$silencemeanwin = mergedfile.adult$silencemean #
mergedfile.adult$passivemeanwin = mergedfile.adult$passivemean #
mergedfile.adult$posemeanwin = mergedfile.adult$posemean #
mergedfile.adult$childishadultmeanwin = mergedfile.adult$childishadultmean #
mergedfile.adult$sexualmeanwin = mergedfile.adult$sexualmean #
mergedfile.adult$assertmeanwin = mergedfile.adult$assertmean #

winvars <- mergedfile.adult[c("dismembermeanwin", "bodyempmeanwin", "silencemeanwin",
                              "passivemeanwin", "posemeanwin", "childishadultmeanwin",
                              "sexualmeanwin", "assertmeanwin")]

for (i in names(dichotomous)){
  mergedfile.adult[[i]][mergedfile.adult[[i]] > mean(mergedfile.adult[[i]], na.rm= TRUE) + (3*(sd(mergedfile.adult[[i]], na.rm = TRUE)))] <- mean(mergedfile.adult[[i]], na.rm= TRUE) +(3*(sd(mergedfile.adult[[i]], na.rm = TRUE)))
  }

#### T-tests ####

continuous <- mergedfile.adult[c("agemean", "dismembermeanwin", "bodyempmeanwin", "silencemeanwin",
                                 "passivemeanwin", "posemeanwin", "childishadultmeanwin",
                                 "sexualmeanwin", "assertmeanwin", "idealmean")]

for (i in names(continuous)){
  print(i)
  print(t.test(mergedfile.adult[[i]]~mergedfile.adult$gender_ME, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F))
}
