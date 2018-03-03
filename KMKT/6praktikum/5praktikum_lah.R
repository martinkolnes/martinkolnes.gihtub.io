## Kvantitatiivsed mudelid käitumisteadustes
## Praktikum nr 5: eksploratiivne faktoranalüüs
## 15.04.2016


# installime vajalikud lisamoodulid
install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)


bfi <- read.csv("bfi.csv")
?bfi
?bfi.dictionary
# teeme uue tabeli bfi2, millesse jätame alles tabeli bfi 10 esimest tulpa
bfi2 <- bfi[,1:10]

# Bartletti test: kas korrelatsioonimaatriksis on liiga palju nõrku korrelatsioone
cortest.bartlett(bfi2)

# Teeme toorandmetest korrelatsioonimaatriksi 
bfi2matrix <- round(cor(bfi2, use="complete"), 2)
bfi2matrix

# Avutame korrelatsioonimaatriksi determinandi. See näitab, kas andmestikul on probleeme multikollineaarsusega ehk liiga tugevate korrelatsioonidega muutujate vahel
det(bfi2matrix)

## FAKTORMUDELI KOOSTAMINE
# Teeme 2-faktorilise mudeli kasutades toorandmeid, maximum likelihood meetodi faktorite leidmiseks ja varimax meetodi pööramiseks
fa.mudel1 <- fa(bfi2, nfactors=2, rotate="varimax", fm="ml", scores=TRUE)

# Sama mudeli saaksime korrelatsioonimaatriksi abil
fa.mudel1.maatriksiga <- fa(bfi2matrix, n.obs=2800, nfactors=2, rotate="varimax", fm="ml")

# Analoogne mudel kasutades peakomponentide analüüsi
pc.mudel1 <- principal(bfi2, nfactors=2, rotate="varimax", scores=TRUE)

## FAKTORMUDELI VÄLJUND
# vaatame faktormudeli väljundit
print.psych(fa.mudel1, cut=0.3)

# Teeme pööramata faktormudeli...
fa.mudel2 <- fa(bfi2, nfactors=2, rotate="none")
# ... ja vaatame selle faktorlaadungite tabelit
fa.mudel2$loadings

# Teeme 2-faktorilise mudeli kasutades kaldnurkset pööramist
fa.mudel3 <- fa(bfi2, nfactors=2, rotate="oblimin", fm="ml")
print.psych(fa.mudel3, cut=0.3)


##FAKTORSKOORID
# Eraldame mudelist faktorskoorid ja paneme need uude tabelisse
skoorid <- data.frame(fa.mudel1$scores)
head(skoorid)

# Anname tabelis olevatele muutujatele uued nimed
colnames(skoorid) <- c("C", "A")

#ÜLESANNE 1
# Uurime t-testi abil, kas sotsiaalsuse faktori osas erineb soolisi erinevusi

#ÜLESANNE 2
# Kas sotsiaalsuse faktori ja vanuse vahel on oluline seos?


# LISA. Cronbachi alfa
alpha(bfi2[, 1:5])
# sama, kuid anname ette ka väidete suuna
alpha(bfi2[, 1:5], keys=c(-1, 1, 1, 1, 1))


## FAKTORITE ARVU MÄÄRAMINE
# kui faktorite arv pole täpselt teada?
# faktormudelist saame ainult omaväärtused kätte nii
fa.mudel1$values

# omaväärtuste graafik Cattelli kriteeriumi hindamiseks
plot(fa.mudel1$values, type="b")

# paralleelanalüüs faktorite arvu määramiseks
fa.parallel(bfi2)

## ÜLESANDED 3

# 1.  Uurige Bartletti testi ja korrelatsioonimaatriksi determinandi abil, kas andmestikul on probleeme
# liiga nõrkade või liiga tugevate muutujatevaheliste seoste rohkusega. (Kuna soovime faktoranalüüsi
# kaasata ainult omaduste hinnangud ja mitte vastaja vanust, oleks mõistlik vanus välja jätta. Seda
# saab teha andes funktsioonidele ette mitte terve tabeli omadused vaid tabeli ilma 17. tunnuseta:
# omadused[,-17])
bfom <- omadused[,-17]
cortest.bartlett(bfom)#bartlett test näitab, kas maatriksis on liiga palju nõrku korrelatsioone; p-väärtus üle 0.05
#näitab, et nõrkade korrleatsiooniseoste rohkust - probleeme selle rohkusega
#Vaatame ka korrelatsioonimaatriksit - nt, kui on näha, et on probleeme, siis tasub cor() abil nõrgad seosed üles otsida:
#cor(na.omit(bfom))
ommatrix <- round(cor(bfom, use="complete"), 2)
ommatrix

#multikolineaarsuse testimine: 
det(ommatrix)# korrelatsioonimaatriksi determinandi abil saame uurida vastupidise probleemi ehk liiga tugevate
# korrelatsioonide esinemist
# Probleemi olemasolu näitab deteminandi väärtus alla 0.00001-e.


# 2. üritage kindlaks määrata mõistlik faktorite arv. Kui me faktorite arvu ette ei tea, võiks alustuseks
# teha mõne suurema faktorite arvuga mudeli. Vaadake omaväärtusi ja tehke omaväärtuste graafik.
# Millist faktorite arvu soovitavad Kaiseri ja Cattelli kriteeriumid? Millist faktorite arvu soovitab
# paralleelanalüüs? Kui päris ühest vastust ei saa, proovige teha paar erinevat mudelit ja vaadake
# nende faktorlaadungite tabelit. Milline mudel oleks kõige lihtsamini tõlgendatav?

#omaväärtused saame faktormudelist kätte, kui lisame mudelinime lõppu dollari märgi ja values:
#mudeli teeme psych mooduli abil, kasutame funktsiooni fa:
om.m1 <- fa(omadused, nfactors=3, rotate ="varimax", fm="ml", scores =TRUE)
om.m1$values
plot(om.m1$values, tyoe ="b")
fa.parallel(omadused)

# 3. Kas esineb muutujaid, mida tasuks välja jätta (ei laadu ühelegi faktorile tugevalt, laadub rohkem
# kui ühele enam-vähem võrdselt, teistest muutujatest oluliselt madalam kommunaliteet)?
print.psych(om.m1, cut=0.3, sort=TRUE)



# 4. Proovige nii täisnurkset kui kaldnurkset pööramist. Millised on kaldnurkse pööramise puhul faktoritevahelised
# korrelatsioonid? Kas nende põhjal oleks kaldnurkselt pööratud faktorlahend antud juhul õigustatud?

#pööramata:
fa.2 <- fa(omadused, nfactors=3, rotate ="none")
fa.2$loadings

fa.3 <- fa(omadused, nfactors=3, rotate ="oblimin")#kaldnurkne pööramine
fa.3$loadings

fa.4 <- fa(omadused, nfactors=3, rotate ="varimax")#ortogonaalne pööramine 
fa.4$loadings



# 5. Kui olete parima mudeli välja valinud, katsuge faktoreid tõlgendada ja pange neile nimed.


# 6. Kui suure osa kogu andmestiku variatiivsusest need faktorid ära kirjeldavad?

# Lisaks laadungitele on tabeli paremas servas veel paar tulpa. Tulbas nimega h2 paiknevad kommunaliteedid,
# mis naitavad kui suure osa muutuja variatiivususest faktorid summaarselt ara kirjeldavad.
print.psych(fa.4, cut=0.3, sort=TRUE)


# 7. Arvutage välja faktorskoorid. Uurige regressioonimudeli abil, kas mõne faktori skoorid seostuvad
# vastaja vanusega.
skoorid <- data.frame(fa.4$scores)
head(skoorid)
str(omadused)
summary(lm(skoorid$MR1~ omadused$vanus))
summary(lm(skoorid$MR2~ omadused$vanus))
summary(lm(skoorid$MR3~ omadused$vanus))


# 8. Arvutage välja faktoritele vastavate alaskaalade Cronbachi alfad. Kas nende väärtusi võib pidada rahuldavaks?
library(dplyr)
alpha(select(bfom, vastutustundetu, hooletu, j_rjekindlusetu, p_simatu, korralik, ettevaatlik),check.keys=TRUE)# jagada kolmeks
alpha(select(bfom, eraldihoidev, seltskondlik, loid, vaikne, kartlik, salatseja),check.keys=TRUE)# jagada kolmeks
alpha(select(bfom, h_irimatu, muretu, pingevaba, enesekindel),check.keys=TRUE)# jagada kolmeks
