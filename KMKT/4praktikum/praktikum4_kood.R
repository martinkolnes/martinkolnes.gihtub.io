
# Kordamine
# 1. Laadige andmefailid (*neeme.csv*, *cowles.csv*, *ESS.csv*, *pisa.csv*) R'i. Määrake igale andmestikule R'is iseloomulik nimi. 
# 
# 2. Teeme PISA andmestiku alusel regressioonanalüüsi mudeli, kus sõltuvaks tunnuseks on  matemaatika testi tulemus (PVMATH) ja prediktoriteks enese-tõhusus  (SCIEEFF; *science self-efficacy*), tulevikku suunatud huvi loodusteaduste vastu (SCIEFUT; *future-oriented science motivation*), mina-pilt
# loodusteadustes (SCSCIE; *science self-concept*)

# Logistiline regressioon  

# Binaarne logistiline regressioon - ühe prediktoriga mudel 
summary(cowles)

cowles.mudel1 <- glm(volunteer ~ extraversion, data=cowles, family=binomial())

summary(cowles.mudel1)

exp(coef(cowles.mudel1))

exp(confint(cowles.mudel1))

cowles.nullmudel <- glm(volunteer ~ 1, data=cowles, family=binomial())

anova(cowles.nullmudel, cowles.mudel1, test="Chisq")

install.packages("fmsb")

library(fmsb)
NagelkerkeR2(cowles.mudel1)

cdplot(volunteer ~ extraversion, data=cowles)

cowles.mudel1 <- glm(volunteer ~ scale(extraversion), data=cowles, family=binomial())
exp(coef(cowles.mudel1))

# Binaarne logistiline regressioon - kahe prediktoriga mudel  
cowles.mudel2 <- glm(volunteer ~ scale(extraversion) + sex, data=cowles, family=binomial())
summary(cowles.mudel2)

exp(coef(cowles.mudel2))

exp(confint(cowles.mudel2))

NagelkerkeR2(cowles.mudel2)

coef(cowles.mudel2)

# Multinomiaalne logistiline regressioon  
install.packages("mlogit")
library(mlogit)

ess.mudel1 <- mlogit(partei ~ 1 | vanus, data=ESS, shape="wide")
summary(ess.mudel1)

data.frame(exp(coef(ess.mudel1)))


# Ülesanded

# 1. Andmetabelist nimega neeme on Marko Neeme magistritöö (Neeme, 2012) andmed, 
# mille uuriti 50-70-aastaste meeste suhtumist eesnäärmevähi skriiningtesti. 
# Tunnus valmidus, näitab testis osalemise valmidust (tasemed pigem jah, ja pigem ei). 
# Lisaks on tabelis ära toodud Suure Viisiku isiksuseomadused: neurootilisus (tunnus N), 
# ekstravertsus (E), avatus kogemusele (O), sotsiaalsus (A) ja meelekindlus (C). Koostage 
# binaarse logistilise regressiooni mudel, mis ennustab skriiningtestis osalemise valmidust 
# Suure Viisiku isiksuseomaduste kaudu.   

# a. Esialgu tuleks saada ülevaade andmetest. Kasutage funktsiooni *summary*, 
# argumendiks saab sisestada terve andmestiku või lihtsalt ühe veeru. 
# Milline on oslejate keskmine vanus? Missugune on valmiduse tulemuste jaotus?  

# b. Tehke ühe tunnuse tulemustest histogramm (vt. praktikum 2).  

# c.  Millised isiksusomadused omavad olulist seost valmidusega? Kas need omadused suurendavad 
# või vähendavad testis osalemise valmidust?  

# d.  Arvutage välja riskisuhted ja nende 95\%-usaldusvahemikud? Milline omadus mõjutab osalemisvalmidust kõige tugevamini?  

# e.  Arvutage mudeli sobitusastet näitav hii-ruut-statistik ja selle p-väärtus.  

# f.  Leidke mudeli pseudo-determinatsioonikordja.  


# 2. Tabelis ESS on lisaks tunnustele partei ja vanus veel tunnused *sugu* ja *aastaid_koolis* 
# (vastaja kooliskäidud aastate arv). Koostage uus multinomiaalne logistiline mudel, 
# milles on lisaks vastaja vanusele täiendavateks prediktoriteks ka sugu ja kooliskäidud aastate arv.
# Leidke ka riskisuhted ja nende usaldusvahemikud. Kas need muutujad seostuvad erakondlike eelistustega? 
# Kui jah, siis millise partei puhul ja millises suunas?
