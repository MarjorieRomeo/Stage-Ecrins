# répertoire contenant les fichiers de sortie des capteurs
DIR.END <- "C:/Users/Marjo/OneDrive/Documents/Maitrise Biologie/été 2021/Stage/gestion/CalculNDVI/NDVI-ESE-PNE/ESE_final/"
# liste des fichiers ESE avec : DATE/HEURE/Indice NDVI
allfiles <- list.files(DIR.END, pattern=".csv", full=T)
# récupérer le nom des fichiers pour l'appliquer aux fichiers en sortie de boucle
allfiles.s <- list.files(DIR.END, pattern=".csv", full=F)
allfiles.s
# Idenfitier dans allfiles.s les différentes sites grâce aux noms des fichiers
SUR.id <- which(substr(allfiles.s,11,13)=="SUR")
54
SUR.id
CRO.id <- which(substr(allfiles.s,11,13)=="CRO")
DIS_INT.id <- which(substr(allfiles.s,19,21)=="INT")
DIS_EXT.id <- which(substr(allfiles.s,19,21)=="EXT")
#liste vide qui où les fichiers zoo créé dans la boucle ci-dessous seront enregistrés
NDVIz <-list()
NDVIz
# Aplliquer le format date et manipuler des données par rapport à leurs date
library(lubridate)
# Afficher les valeurs ndvi par rapport aux dates : package zoo facilite la manipulation de données datées
library(zoo)
# Manipuler des données NDVI
library(phenex)
# Boucle pour calculer l'indice NDVI moyen par jour et transformer le dataframe en objet zoo
for (i in 1:length(allfiles)){
    PLOT <- read.csv2(file=allfiles[i], sep=",", dec=".")
    #fusionner la colonne temps et date, mettre ça dans un objet et séparer ces 2 infos par un espace
    tmp <- paste(PLOT$Date,PLOT$Time,sep=" ")
    tmp
    # Changer le format de l'objet tmp au format date
    # ATTENTION : la partie du code format = se code en fonction du format de date de départ et non la forme que l'on souhaite lui donner
    DateTime <- as.POSIXct(tmp,format = '%d/%m/%Y %H:%M:%S',tz="UTC")
    # identifier la position dans DateTime les données comprises entre 11h et 13h
    MIDI.id <- which(hour(DateTime)>=11 & hour(DateTime)<=13)
    55
    # Verifier le nombre de données récupérer. nombre de données Suffisantes ou pas ?
    length(MIDI.id)
    # la fonction date (issu du package lubridate) sert à sélectionner les données du même jour
    # PLOT$NDVI[MIDI.id] : identifier toutes les données NDVI comprises entre 11h et 13h
    # aggregate(...,mean) : calculer la moyenne des données journalières NDVI
    tmp <- aggregate(PLOT$NDVI[MIDI.id],list(DAY=date(DateTime)[MIDI.id]),mean)
    # Verifier l'objet (fonctionne si on run ligne par ligne et non toute la boucle d'un seul coup)
    head(tmp)
    
    # Arrondir la moyenne que l'on vient de calculer à 2 chiffres après la virgule et créer une nouvelle colonne dans l'objet tmp
    tmp$NDVI <- round(tmp$x, 2)
    # transformer la colonne créée ci-dessus au format zoo
    tmpz <- zoo(tmp$NDVI)
    # la fonction time (package zoo) indique que la colonne DAY de l'objet tmp est identifié comme étant la date
    time(tmpz) <- tmp$DAY
    # enregistre l'objet zoo dans la liste créée ligne 19
    NDVIz[[i]]<- tmpz
    NDVIz[[i]]
}
# Mettre dans un objet toutes les données qui appartiennent à un site
# ligne 12 à 16 utilisées
CROZ <- do.call(merge, NDVIz[CRO.id])
head(CROZ)

SUR <- do.call(merge, NDVIz[SUR.id])
DIST_INT <- do.call(merge, NDVIz[DIS_INT.id ])
DIST_EXT <- do.call(merge, NDVIz[DIS_EXT.id ])
########################### Afficher toutes les années par site
# Total années - Crouzet (CRO)
plot(CROZ, plot.type="single", xlab="Années", ylim=c(-1,1), ylab="Indice NDVI", main = "NDVI sur le site de Crouzet, slidingperiod=5", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1)) + geom_line()
# Total années - Vallonpierre (SUR)
plot(SUR, plot.type="single", xlab="Années", ylab="Indice NDVI", main = "NDVI sur le site de Vallonpierre (PNE)", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1))
# Total années - Distroit Intérieur (DIST_INT)
plot(DIST_INT, plot.type="single", xlab="Années", ylab="Indice NDVI", main = "NDVI sur le site de Distroit Intérieur (PNE)", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1))
# Total années - Distroit Extérieur (DIST_EXT)
plot(DIST_EXT, plot.type="single", xlab="Années", ylab="Indice NDVI", main = "NDVI sur le site de Distroit Extérieur (PNE)", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1))
########################### trier les données par site
# Mettre dans un objet toutes les données qui appartiennent à un site
# ligne 12 à 16 utilisées
CROZ <- NDVIz[CRO.id]
head(CROZ)
SUR <- NDVIz[SUR.id]
DIST_INT <- NDVIz[DIS_INT.id]
DIST_EXT <- NDVIz[DIS_EXT.id]
57
########################### Année 2013 - CROUZET
# Cette ligne est utile quand il y a plusieurs années dans un objet CROZ[[1]]
YEAR_2013 <- which(year(CROZ[[1]])==2013)
YEAR_2013
# Mettre dans un objet une année d'un site
YEAR_CRO <- CROZ[[1]][YEAR_2013]
YEAR_CRO
# Débruiter les données avec la fonction rollmean
# il est possible de changer l'intensité du lissage en modifiant le "12"
# align indique la manière dont on lisse est ce que l'on prend sur 12 valeurs celle de : right / left / center
YEAR_RM = rollmean(YEAR_CRO, 12, fill = NA, align = 'right')
# Calculer la difference d'une valeur à l'autre
# lag = indique sur combien de jour la diference se calcule
# fonction diff() utilisée pour des objets zoo avec des dates
delta_2013 <- diff(YEAR_RM, lag=5)
########################### Année 2014 - Crouzet
# plusieurs objets zoo contiennent une seule année
# Indexer les valeurs associées à 2014 dans les objets Y_2014
Y1_2014 <- which(year(CROZ[[1]])==2014)
Y2_2014 <- which(year(CROZ[[2]])==2014)
Y3_2014 <- which(year(CROZ[[3]])==2014)
Y4_2014 <- which(year(CROZ[[4]])==2014)
# associé l'indexation ci-dessus et intégrer dans un objet les valeurs associés à 2014
Y1_2014 <- CROZ[[1]][Y1_2014]
Y2_2014 <- CROZ[[2]][Y2_2014]
Y3_2014 <- CROZ[[3]][Y3_2014]
Y4_2014 <- CROZ[[4]][Y4_2014]
# rbind.zoo: chaque objet ci-dessus va être intégré ligne par ligne dans un seul objet
58
# But: avoir toutes les valeurs pour une seule année dans un objet
TOTAL <- rbind.zoo(Y1_2014,Y2_2014,Y3_2014,Y4_2014)
# Arrondir les valeurs NDVI après avoir effectué un rollmean à 12
Y_2014 <- round(rollmean(TOTAL, 12, fill= NA),2)
Y_2014
# Calculer la difference entre deux valeurs, le lag indique la grandeur de cette difference
delta_2014 <- diff(Y_2014, lag=20)
# lisser les résultats de ce calcul de différence et arrondir ces valeurs
delta_roll <- round(rollmean(delta_2014, 12, fill=NA),2)
delta_roll
### identifier la date de deneigement
# quelles sont les valeurs supérieures à 0
id.zero <- which(Y_2014>0)
#afficher la position de ces valeurs
id.zero
# prendre la position de la premiere valeur de id.zero
id.zero <- Y_2014[79]
id.zero
# verifier l'information manuellement en affichant l'objet 2014
Y_2014
# identifier les Valeurs manquantes pour une année
NA_2014 <- which(is.na(Y_2014))
#nombre de valeurs manquantes
length(NA_2014)
59
### Extraire la valeur maximale entre le 1er avril et le 31 novembre : saison phénologique
MAX_2014 <- window(Y_2014, start="2014-04-01", end="2014-11-30")
ID.MAX <- which.max(MAX_2014)
#afficher la valeur et la date
PIC_2014 <- MAX_2014[ID.MAX]
PIC_2014
### Trouver la date et valeur de demarrage du verdissemeent : valeur à 50% de la valeur max NDVI
# identifier l'intervalle de temps entre le moment où le NDVI dépasse 0 à start
# et le moment où le NDVI atteint sa valeur maximale à end
DEM_2014 <- window(Y_2014, start="2014-04-22", end="2014-06-10")
# calculer la moitié de la valeur du ndvi max
PIC_2014/2
# chercher quelles sont les valeurs = à ce résultat dans la période DEM_2014
id.DEM <- which(DEM_2014==0.37)
#selectionner la valeur et la date dans DEM_2014
value.dem <- DEM_2014[id.DEM]
value.dem
### Idenfifier la dtae t la valeur où la croissance du NDVI est la plus importante
MAX.delta <- which.max(delta_roll)
PIC_delta <- delta_roll[MAX.delta]
PIC_delta
#je veux connaître quelle est la valeur du NDVI le jour du pic de delta
VAL.NDVI <- as.Date("2014-05-04")
ID.jour.max <- Y_2014[VAL.NDVI]
ID.jour.max
# calculer la période de déneigement en jour
period.neig <- (as.Date("2014-05-04"))-(as.Date("2014-04-22"))
period.neig
# calculer la Période de verdissement en jour
period.verdi <- (as.Date("2014-06-10"))-(as.Date("2014-05-07"))
period.verdi
### identifier le 1er decrochage avec un seuil de delta = -0.05
# Afficher objet delta_roll pour savoir à quelle date le delta atteint -0.05
delta_roll
# indiquer la date à laquelle le seuil est dépassé
ID.regain <- as.Date("2014-07-21")
# Récupérer la valeur NDVI associée à cette date
ID.zero <- Y_2014[ID.regain]
ID.zero
# lignes de codes 2019 à 2017 utiles pour afficher la valeur NDVI selon les seuils de delta que l'on cherche
# decrochage, regain, etc..
#Période 1er décrochage des 2 seuils :
# 1ere date : date de fin de rupture
# 2eme date : date d'un des 2 seuils
period.decro.1 <- (as.Date("2014-07-21"))-(as.Date("2014-07-13"))
period.decro.1
#Même methode que la rupture
# identifier dans l'objet delta_roll le moment où la valeur de delta devient positive
delta_roll
# récupérer la date associée
ID.regain <- as.Date("2014-08-15")#afficher la valeur NDVI associée à cette date
ID.zero <- Y_2014[ID.regain]
ID.zero
delta_roll
# calculer la période de regain en jour
#identifier quand valeur de delta devient positive et quand elle atteint un pic
period.regain <- (as.Date("2014-08-10"))-(as.Date("2014-08-15"))
period.regain
# Identifier la date de senescence, cad le premier jour où delta devient negatif definitivement dans delta_roll
#utiliser cette date pour récupérer la valeur NDVI dan l'objet Y_2014
ID.senes <- as.Date("2014-09-04")
ID.zero <- Y_2014[ID.senes]
ID.zero
### creer l'abscisse d'un plot qui affiche les mois de l'année à l'intervalle et fréquence souhaitée
# identifier les dates de toutes les valeurs de l'objet Y_2014
t <- time(Y_2014)
# Récupérer la position de la première et dernière valeur de l'objet t
# Identifier un intervalle de temps de la position de la première et dernière valeur de t
# cad la premiere et dernière valeure de l'année 2014
# by = indiquer la fréquence de l'intervalle, un trait d'abscisse = un mois
t <- seq(t[1], t[287], by="month")
t
#indique que je veux deux plots dans la même fenêtre
# mfcol indique que je veux une colonne à 2 lignes
#pour les avoir en ligen : par(mfrow=c(1,2))
# mfcol=c(2,1) : Afficher 2 plots en colonne (l'un en-dessous de l'autre)
# oma pour les marges hors plot, pour plus de détails : https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet.html

# oma=c(0,0,0,2)
par(mfcol=c(3,2), mar=c(4,4,2,8)+1, xpd=F)
plot(window(Y_2014, start="2014-04-01", end="2014-11-30"),plot.type="single", ylab="Indice NDVI", xlab="mois de l'année",main = "NDVI Crouzet 2014 (PNE), rollmean = 12", yaxt="n", xaxt="n")
axis(2, las = 1, at = seq(-1,1,0.1))
axis(side=1,t, at = t, labels=format(t, "%m"))
par(new=TRUE)
abline(h="0", lty=2)
# NDVI qui dépasse une valeur de 0
abline(v=as.Date("2014-04-22"), col="#66CCCC", lwd=2)
# Pic valeur delta
abline(v=as.Date("2014-05-04"), col="#663300",lwd=2)
# Date de demarrage
abline(v=as.Date("2014-05-07"), col="#99FF66",lwd=2)
# Pic valeur max
abline(v=as.Date("2014-06-10"), col="#009900", lwd=2)
#Seuil : delta = -0.05
abline(v=as.Date("2014-07-13"), col="#FFCC00", lwd=2)
# Fin du 2e decrochage ou phase de rupture 2
abline(v=as.Date("2014-07-21"), col="#FFCC00", lwd=2, lty=2)
# Debut période de regain NDVI
abline(v=as.Date("2014-08-10"), col="#99FF66", lwd=2)
# Fin du regain
abline(v=as.Date("2014-08-15"), col="#99FF66", lwd=2, lty=2)
#début senescence
abline(v=as.Date("2014-09-01"), col="#FF6600", lwd=2)
# Neige
abline(v=as.Date("2014-11-02"), col="#999999", lwd=2)
# xpd=TRUE indique que la legende est hors du plot
#inset : le pourcentage d'éloignement du graph, plus on le veut loin plus il est hors du plot

#ncol= 1 : une colonne pour toute notre légende
legend("left",inset=c(1,1), ncol=1,title="Légende", xpd=TRUE ,
       legend=c("NDVI= 0", "Delta max", "Demarrage", "Pic NDVI", "Rupture (seuil à -0.05)", "Fin rupture", "Regain", "Fin Regain", "Senescence", "Neige"),
       col=c("#66CCCC", "#663300","#99FF66", "#009900", "#FFCC00", "#FFCC00", "#99FF66","#99FF66","#FF6600","#999999"), lty=1,cex=0.7,lwd=2)
plot(window(delta_roll,start="2014-04-01",end="2014-11-30"), main="Delta, crouzet 2014, lag=20 + rollmean = 12", plot.type="single", xlab="mois de l'année", ylab="Variations Indice NDVI", xaxt="n")
axis(side=1,t, at = t, labels=format(t, "%m"))
par(new=TRUE)
abline(h="0", lty=2)
# NDVI qui dépasse une valeur de 0
abline(v=as.Date("2014-04-22"), col="#66CCCC", lwd=2)
# Pic valeur delta
abline(v=as.Date("2014-05-04"), col="#663300",lwd=2)
# Date de demarrage
abline(v=as.Date("2014-05-07"), col="#00CC66",lwd=2)
# Pic valeur max
abline(v=as.Date("2014-06-10"), col="#009900", lwd=2)
#Seuil : delta = -0.05
abline(v=as.Date("2014-07-13"), col="#FFCC00", lwd=2)
# Fin du 2e decrochage ou phase de rupture 2
abline(v=as.Date("2014-07-21"), col="#FFCC00", lwd=2)
# Debut période de regain NDVI
abline(v=as.Date("2014-08-10"), col="#99FF66", lwd=2)
# Fin du regain
abline(v=as.Date("2014-08-15"), col="#99FF66", lwd=2)
#début senescence
abline(v=as.Date("2014-09-01"), col="#FF6600", lwd=2)
# Neige
abline(v=as.Date("2014-11-02"), col="#999999", lwd=2)