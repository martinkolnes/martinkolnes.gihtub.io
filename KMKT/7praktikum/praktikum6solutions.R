## Kvantitatiivsed mudelid käitumisteadustes
## Praktikum nr 6: struktuurivõrrandite mudelid I
## 24.04.2015

setwd("~/Workspace/R/KMKT/6.SEM/prax6")
# installeerime mudelite tegemiseks lisamooduli lavaan
#install.packages("lavaan")

#laadime lisamooduli lavaan
library(lavaan)

# näitab andmestiku kuut esimest rida
head(HolzingerSwineford1939)

# avab andmestiku seletuse
help(HolzingerSwineford1939)


# defineerime 3 faktoriga mudeli
mudel1 <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9"

# sobitame mudeli andmetele
fit1 <- cfa(mudel1, data=HolzingerSwineford1939)

# vaatame mudeli väljundit
summary(fit1, fit.measures=TRUE, standardized=TRUE)

# mudeli väljundit saab tellida ka osade kaupa
# sobitusastmed
fitMeasures(fit1)
# standardiseerimata parameetrid
parameterEstimates(fit1)
# standardiseeritud parameetrid
standardizedSolution(fit1)

# mudeli jäägid, antud juhul korrelatsioonijäägid
residuals(fit1, type="cor")


# lisame mudelile jääkide maatriksi põhjal kindlaks tehtud muutujate-vahelised korrelatsioonid
mudel2 <- "
#latentsed muutujad
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9

# jääkdispersioonide vahelised korrelatsioonid
x7 ~~ x1 + x2
x9 ~~ x1 + x3
x3 ~~ x5"

# sobitame mudeli andmetele
fit2 <- cfa(mudel2, data=HolzingerSwineford1939)

# vaatame mudeli väljundit
summary(fit2, fit.measures=TRUE, standardized=TRUE)



### Andmestik 2
# andmestiku seletus
help(PoliticalDemocracy)

# kirjeldame mudelis esinevad seosed
mudel3 <- "
# defineerime latentsed tunnused
maj60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8

# regressioonseosed latentsete tunnuste vahel
dem60 ~ maj60
dem65 ~ maj60 + dem60

# jääkdispersioonide vahelised korrelatsioonid
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8"

# sobitame mudeli andmetele
fit3 <- sem(mudel3, data=PoliticalDemocracy)
# uurime mudeli väljundit
summary(fit3, fit.measures=TRUE, standardized=TRUE)


##########  ÜLESANDED ##########

#### Ülesanne 1 ####
# Tabelis nimega suur.viisik on 30 Suure Viisiku isiksuseomaduste alaomadust.
# Tabelis olevate muutujate nimed saab kätte nii:
rm(list=ls())
setwd("~/Workspace/R/KMKT/6.SEM/6prax2016")
load("praktikum6.RData")
names(suur.viisik)
# Tee neid nimesid kasutades 5-faktoriline kinnitava faktoranalüüsi mudel
# ja hinda selle sobivust andmetele.
mudelBig5 <- "
# defineerime latentsed tunnused
neurootilisus =~ N1 + N2 + N3 + N4 + N5 + N6
ekstravertsus =~ E1 + E2 + E3 + E4 + E5 + E6
avatus =~ O1 + O2 + O3 + O4 + O5 + O6
sotsiaalsus =~ A1 + A2 + A3 + A4 + A5 + A6
meelekindlus =~ C1 + C2 + C3 + C4 + C5 + C6 "
fitBig5 <- cfa(mudelBig5, data=suur.viisik)
summary(fitBig5)

#### Ülesanne 2 ####
# kirjuta siia praktikumijuhendis kirjeldatud mudel
mudel5 <- "
# regressioonseosed latentsete tunnuste vahel
exercise ~ hardiness
fitness ~ exercise + stress
illness ~ fitness + stress
stress ~ fitness + hardiness
# jääkdispersioonide vahelised korrelatsioonid
hardiness ~~ exercise
"

# Kuna praegu tugineb analüüs kovariatsioonimaatriksile, mitte toorandmetele
# ja sellisel juhul kasutab mudeli sobitamise käsk natuke teisi argumente, 
# siis on sobitamine siin praegu ära tehtud
fit5 <- sem(mudel5, sample.cov=illness.cov, sample.nobs=373, fixed.x=FALSE)
summary(fit5)
