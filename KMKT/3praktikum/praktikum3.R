## Kvantitatiivsed mudelid käitumisteadustes
## Praktikum nr 3: lineaarne regressioon
#17.03.17
## KORDAMINE
# 1. Lugega andmestik *pisa* R'i 
# valige_andmestiku_nimi <- read.csv("sisseloetav andmestik")
# 
# 2. Vaadake kirjeldavaid statistikuid matemaatik (PV1MATH), lugemise (PV1READ) ja loodusteaduste (PV1SCIE) alatestide tulemuste kohta.
# 
# 3. Tehke eraldi histogrammid soo alusel loodusteaduste testi tulemuste kohta.  
# 
# 4. Missugused on korrelatsioonid nende kolme testi (PV1MATH, PV1READ, PV1SCIE) vahel? Võrrelge korrelatsioone erinevate klasside tulemuste vahel.
# library("psych")
# corr.test()#argumendiks saate panna mitu veergu


##PAARISREGRESSIOON
# paarisregressiooni mudel
pisa.mudel1  <- lm(PVSCIE ~ GENSCIE, data=pisa)

# mudeli väljund
summary(pisa.mudel1)

#Ülesanne
# Tehke paarisregressiooni mudel, mis ennustab matemaatika alatesti skoori (tunnus PVMATH) 
# lugemise skoori kaudu (PVREAD). Kas seos on oluline? Kui suure osa matemaatika testi skooride
# hajuvusest lugemise testi skoor ära seletab? Mitme punkti võrra muutub matemaatika skoor kui 
# lugemise skoor muutub ühe punkti võrra?


##MITMENE REGRESSIOON
# mitmese regressiooni mudel
pisa.mudel2 <- lm(PVSCIE ~ GENSCIE + INTSCIE + INSTSCIE, data=pisa)

# mudeli väljund
summary(pisa.mudel2)

# standardiseeritud regressioonikordajate arvutamise funktsioon
# moodulist QuantPsyc, mis tuleb enne installida ja laadida
install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(pisa.mudel2)#mitme standardhalbe v~orra muutub s~oltuv muutuja, kui prediktor muutub uhe standardhalbe v~orra (ja
#ulejaanud prediktorid jaavad samaks).

# mudeli parameetrite 95%-usalduspiirid
confint(pisa.mudel2)

# Ülesanne
# Koostage uus mudel, milles sõltuvaks tunnuseks on matemaatika testi skoor (PVMATH) 
# ja prediktoriteks samad tunnused, millega ülal ennustasime loodusteaduste testi skoori:
# teaduse oluliseks pidamine (GENSCIE), huvi teaduse vastu (INTSCIE) ja motivatsioon loodusteadusi 
# õppida (INSTSCIE). Missugused prediktorid on statistiliselt olulised? Arvutage ka 
# standardiseeritud regressioonikordajad ja mudeli parameetrite usalduspiirid.


##HIERARHILISTE MUDELITE VÕRDLEMINE

pisa.mudel1 <- lm(PVSCIE ~ GENSCIE, data = pisa)
pisa.mudel2 <- lm(PVSCIE ~ GENSCIE + INTSCIE + INSTSCIE, data=pisa)

anova(pisa.mudel1, pisa.mudel2)


## ERINDID
# mudeli standardiseeritud jäägid
mud2.standardized.residuals <- rstandard(pisa.mudel2)

# kui palju on suuri jääke
sum(abs(mud2.standardized.residuals) > 2)

# kas suuri jääke on rohkem kui 5% andmestikust
nrow(pisa) * 0.05

# kui palju on eriti suuri jääke
sum(abs(mud2.standardized.residuals) > 3)

# andmeread, mille puhul jäägid eriti suured
pisa[abs(mud2.standardized.residuals) > 3, ]

# kõige suurem Cooki kaugus
max(cooks.distance(pisa.mudel2))

# andmeread, mille puhul Cooki kaugused üle kriitilise piiri
pisa[cooks.distance(pisa.mudel2) > 1,]


# prediktorite-vahelised korrelatsioonid
# multikollineaarsuse hindamiseks
cor(pisa[,c("GENSCIE", "INTSCIE", "INSTSCIE")])

# multikollineaarsuse hindamiseks kasutatavad varieeruvusindeksid
# lisamooduli car funktsiooni vif abil
install.packages("car")
library(car)
vif(pisa.mudel2)

# hajuvusdiagramm heteroskedaktilisuse hindamiseks
plot(fitted.values(pisa.mudel2), resid(pisa.mudel2))

# jääkide histogramm
hist(mud2.standardized.residuals)

# kvantiil-kvantiil diagramm jääkide normaaljaotuse hindamiseks
qqnorm(mud2.standardized.residuals)
qqline(mud2.standardized.residuals, col="red", lwd=2)

# Durbin-Watsoni test jääkide sõltumatuse hindamiseks
# lisamoodulist car
dwt(pisa.mudel2)


##Ülesanded

# Analüüsige lähemalt eelnevalt tehtud mudelit, kus sõltuvaks tunnuseks oli matemaatika testi 
# skoor (PVMATH) ja prediktoriteks teaduse oluliseks pidamine (GENSCIE), huvi teaduse vastu 
# (INTSCIE) ja motivatsioon loodusteadusi õppida (INSTSCIE).
# 
# Arvutage mudeli standardiseeritud jäägid. Kui suur on jääkide osakaal, mille absoluutväärtus on suurem kui 2? 
# Kas neid on liiga palju?
# 
# Arvutage mudeli kohta Cooki kaugused. Kas esineb liiga suure mõjukuseastmega vaatlusi?
# 
# Kas mudelil on probleeme multikollineaarsuse, heteroskedaktilisuse, jääkide jaotuse või jääkide sõltumatusega?
