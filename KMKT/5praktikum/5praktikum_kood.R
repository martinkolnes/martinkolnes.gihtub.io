## Kvantitatiivsed mudelid käitumisteadustes
## Praktikum nr 5: eksploratiivne faktoranalüüs
## 13.04.2017


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
#Andmestikuga "omadused"

# 1.  Uurige Bartletti testi ja korrelatsioonimaatriksi determinandi abil, kas andmestikul on probleeme
# liiga nõrkade või liiga tugevate muutujatevaheliste seoste rohkusega. (Kuna soovime faktoranalüüsi
# kaasata ainult omaduste hinnangud ja mitte vastaja vanust, oleks mõistlik vanus välja jätta. Seda
# saab teha andes funktsioonidele ette mitte terve tabeli omadused vaid tabeli ilma 17. tunnuseta:
# omadused[,-17])


# 2. üritage kindlaks määrata mõistlik faktorite arv. Kui me faktorite arvu ette ei tea, võiks alustuseks
# teha mõne suurema faktorite arvuga mudeli. Vaadake omaväärtusi ja tehke omaväärtuste graafik.
# Millist faktorite arvu soovitavad Kaiseri ja Cattelli kriteeriumid? Millist faktorite arvu soovitab
# paralleelanalüüs? Kui päris ühest vastust ei saa, proovige teha paar erinevat mudelit ja vaadake
# nende faktorlaadungite tabelit. Milline mudel oleks kõige lihtsamini tõlgendatav?

# 3. Kas esineb muutujaid, mida tasuks välja jätta (ei laadu ühelegi faktorile tugevalt, laadub rohkem
# kui ühele enam-vähem võrdselt, teistest muutujatest oluliselt madalam kommunaliteet)?


# 4. Proovige nii täisnurkset kui kaldnurkset pööramist. Millised on kaldnurkse pööramise puhul faktoritevahelised
# korrelatsioonid? Kas nende põhjal oleks kaldnurkselt pööratud faktorlahend antud juhul õigustatud?

# 5. Kui olete parima mudeli välja valinud, katsuge faktoreid tõlgendada ja pange neile nimed.


# 6. Kui suure osa kogu andmestiku variatiivsusest need faktorid ära kirjeldavad?

# Lisaks laadungitele on tabeli paremas servas veel paar tulpa. Tulbas nimega h2 paiknevad kommunaliteedid,
# mis naitavad kui suure osa muutuja variatiivususest faktorid summaarselt ara kirjeldavad.



# 7. Arvutage välja faktorskoorid. Uurige regressioonimudeli abil, kas mõne faktori skoorid seostuvad
# vastaja vanusega.



# 8. Arvutage välja faktoritele vastavate alaskaalade Cronbachi alfad. Kas nende väärtusi võib pidada rahuldavaks?

