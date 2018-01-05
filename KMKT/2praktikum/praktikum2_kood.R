## Kvantitatiivsed mudelid k?itumisteadustes
## Praktikum nr 2: andmete sisselugemine ja joonised

#### Andmete sisselugemine
# loeme sisse csv-failis olevad andmed
kysimustik <- read.csv("Kysimustik.csv", header=TRUE)

# tegelikult läheb praegu vaja funktsiooni read.csv2,
# mis loeb nö Euroopa formaadis csv-faile
kysimustik <- read.csv2("Kysimustik.csv", header=TRUE)
kysimustik <- read.csv("kysimustik.csv", header = TRUE, sep =";") #Argument "sep" võimaldab täpsustada andmeväljade eraldamise viisi toorandmetes.


## Exceli xls- ja xlsx-faile faile saab otse R-i lugeda lisamooduli xlsx abil
#selleks tuleb see kõigepealt installida
#install.packages("xlsx") # nüüd tuleb natuke oodata kuni R moodulit installib

# seejärel tuleb lisamoodul laadida
library(xlsx)

# loeme andmed sisse funktsiooni read.xlsx abil
kysimustik <- read.xlsx("Kysimustik.xls", sheetIndex=1)


## SPSS-i sav-faile saab sisse lugeda lisamoodulis foreign
# paikneva funktsiooni read.spss abil.
# Moodul tuleb R-iga kaasa ja seda pole vaja installida.
# Küll aga tuleb see laadida.
library(foreign)
pisa <- read.spss("PISA2009.sav", to.data.frame=TRUE, use.value.labels=FALSE)
# sisseloetava andmefaili saab valida ka interaktiivselt
pisa <- read.spss(file.choose(), to.data.frame=TRUE, use.value.labels=FALSE)

#### KORDAMINE

# 1. Leia uuringus osalenute keskmine vanus?  
# mean()# keskmise arvutamiseks sai kasutada funktsiooni mean()
# summary()#annab ülevaate tervest andmestikust

# 2. Kui palju tüdrukuid ja poisse oli uuringus? (andmetes: 1 = tüdruk, 2 = poiss)  
# table()

# 3. Kui palju õpilasi oli erinevates klassides?  

# 4. Milline on korrelatsioon matemaatik (PV1MATH), lugemise (PV1READ) ja loodusteaduste (PV1SCIE) alatestide tulemuste vahel?  
# cor()
# cor.test()

# 5. Võrdle meeste ja naiste keskmisi t-testiga:  
# t.test(sõltuv muutuja ~ sõktumatu muutuja)
# 5.1. lugemine   
# 5.2. matemaatika  
# 5.3.loodusteadus   

# 6. Millised olid alatestide keskmised väärtused?


#### Joonised
install.packages("ggplot2")
library(ggplot2)

# histogramm
ggplot(pisa, aes(x=PV1READ))+
        geom_histogram()

ggplot(pisa, aes(x=PV1READ))+
        geom_histogram(col = "blue", fill = "gray")+
        labs(title = "Lugemise alatesti skooride jaotus", x ="Alatesti skoor" , y = "Sagedus")

# Tulpdiagramm
ggplot(pisa, aes(GR))+
        geom_bar()+
        labs(x ="klass" , y = "õpilasi valimis")+
        theme_bw()
#Kirjeldav statistika gruppide kaupa:
tapply(pisa$PV1READ, pisa$GR, mean, na.rm=TRUE)

ggplot(pisa, aes(y=PV1MATH, x = GR))+
        geom_bar(aes(group = GR), position = "dodge", stat="identity")+
        labs(x ="klass" , y = "matemaatika alatesti keskmine skoor")

#Karpdiagramm

ggplot(pisa, aes(y = PV1MATH, x = as.factor(GR)))+
        geom_boxplot()+
        labs(x = "klass", y = "matemaatika alatesti skoor")


#Hajuvusdiagramm
ggplot(pisa, aes(x= PV1MATH, y = PV1SCIE))+
        geom_point()

ggplot(pisa, aes(x= PV1MATH, y = PV1SCIE))+
        geom_point(shape=1) +    # shape = 1 - tühjad ringid
        geom_smooth(method=lm,   # lisab regressioonisirge
                    se=FALSE)    # SE = FALSE - ei lisa usalduspiire


## Ülesanded - Joonised
# 1. Histogramm matemaatika alatesti tulemuste kohta (lisa joonisele pealkiri):

# 2. Tee karpdiagramm, mis illustreerib meeste ja naiste matemaatika alatestide skoore

# 3. Matemaatika ja lugemise alatestide tulemuste seos (lisa joonisele regressioonisirge)

# 4. Lugemise ja loodusteaduste alatestide tulemuste seos (lisa joonisele regressioonisirge)

# 5. Tulpdiagramm erinevate klasside lugemise alatesti skooride kohta

# 6. Tee eraldi histogramm naiste ja meeste matemaatika alatesti tulemuste kohta:
      
#Vihje:  
#subset(pisa, GR == 9) #ainult 9. klassi tulemuste eraldamiseks andmetest


