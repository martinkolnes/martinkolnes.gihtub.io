## Kvantitatiivsed mudelid käitumisteadustes
## Praktikum nr 6: struktuurivõrrandite mudelid I
## 28.04.2016

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


#Parameetrite hindamise meetodid:
fit2 <- cfa(mudel1, data=HolzingerSwineford1939, estimator = "GLS")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
fit3 <- cfa(mudel1, data=HolzingerSwineford1939, estimator = "WLS")
summary(fit3, fit.measures=TRUE, standardized=TRUE)


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

# Modifikatsiooniindeksid
HS.model <- "visual =~ x1 + x2 + x3
+ textual =~ x4 + x5 + x6
+ speed =~ x7 + x8 + x9"

fit1 <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit1, fit.measures=TRUE)

modindices(fit1)

HS.model2 <- "visual =~ x1 + x2 + x3 + x9
+ textual =~ x4 + x5 + x6
+ speed =~ x7 + x8 + x9"
fit2 <- cfa(HS.model2, data=HolzingerSwineford1939)
summary(fit2, fit.measures=TRUE)

modindices(fit2)

HS.model3 <- "visual =~ x1 + x2 + x3 + x9
+ textual =~ x4 + x5 + x6
+ speed =~ x7 + x8 + x9
+ x3 ~~ x5"
fit3 <- cfa(HS.model3, data=HolzingerSwineford1939)
summary(fit3, fit.measures=TRUE)

modindices(fit3)

HS.model4 <- "visual =~ x1 + x2 + x3 + x9
+ textual =~ x4 + x5 + x6
+ speed =~ x7 + x8 + x9
+ x3 ~~ x5
+ x4 ~~ x7"
fit4 <- cfa(HS.model4, data=HolzingerSwineford1939)
summary(fit4, fit.measures=TRUE)

anova(fit3, fit4)

##########  ÜLESANDED ##########

#### Ülesanne 1 ####
# Tabelis nimega suur.viisik on 30 Suure Viisiku isiksuseomaduste alaomadust.
# Tabelis olevate muutujate nimed saab kätte nii:
names(suur.viisik)
# Tee neid nimesid kasutades 5-faktoriline kinnitava faktoranalüüsi mudel
# ja hinda selle sobivust andmetele.



#### Ülesanne 2 ####
# kirjuta siia praktikumijuhendis kirjeldatud mudel
mudel5 <- "

"

# Kuna praegu tugineb analüüs kovariatsioonimaatriksile, mitte toorandmetele
# ja sellisel juhul kasutab mudeli sobitamise käsk natuke teisi argumente, 
# siis on sobitamine siin praegu ära tehtud
fit5 <- sem(mudel5, sample.cov=illness.cov, sample.nobs=373, fixed.x=FALSE)

