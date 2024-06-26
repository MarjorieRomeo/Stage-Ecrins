# r�pertoire contenant les fichiers de sortie des capteurs
DIR.END <- "C:/Users/Marjo/OneDrive/Documents/Maitrise Biologie/�t� 2021/Stage/gestion/CalculNDVI/NDVI-ESE-PNE/ESE_final/"
# liste des fichiers ESE avec : DATE/HEURE/Indice NDVI
allfiles <- list.files(DIR.END, pattern=".csv", full=T)
# r�cup�rer le nom des fichiers pour l'appliquer aux fichiers en sortie de boucle
allfiles.s <- list.files(DIR.END, pattern=".csv", full=F)
allfiles.s
# Idenfitier dans allfiles.s les diff�rentes sites gr�ce aux noms des fichiers
SUR.id <- which(substr(allfiles.s,11,13)=="SUR")
54
SUR.id
CRO.id <- which(substr(allfiles.s,11,13)=="CRO")
DIS_INT.id <- which(substr(allfiles.s,19,21)=="INT")
DIS_EXT.id <- which(substr(allfiles.s,19,21)=="EXT")
#liste vide qui o� les fichiers zoo cr�� dans la boucle ci-dessous seront enregistr�s
NDVIz <-list()
NDVIz
# Aplliquer le format date et manipuler des donn�es par rapport � leurs date
library(lubridate)
# Afficher les valeurs ndvi par rapport aux dates : package zoo facilite la manipulation de donn�es dat�es
library(zoo)
# Manipuler des donn�es NDVI
library(phenex)
# Boucle pour calculer l'indice NDVI moyen par jour et transformer le dataframe en objet zoo
for (i in 1:length(allfiles)){
    PLOT <- read.csv2(file=allfiles[i], sep=",", dec=".")
    #fusionner la colonne temps et date, mettre �a dans un objet et s�parer ces 2 infos par un espace
    tmp <- paste(PLOT$Date,PLOT$Time,sep=" ")
    tmp
    # Changer le format de l'objet tmp au format date
    # ATTENTION : la partie du code format = se code en fonction du format de date de d�part et non la forme que l'on souhaite lui donner
    DateTime <- as.POSIXct(tmp,format = '%d/%m/%Y %H:%M:%S',tz="UTC")
    # identifier la position dans DateTime les donn�es comprises entre 11h et 13h
    MIDI.id <- which(hour(DateTime)>=11 & hour(DateTime)<=13)
    55
    # Verifier le nombre de donn�es r�cup�rer. nombre de donn�es Suffisantes ou pas ?
    length(MIDI.id)
    # la fonction date (issu du package lubridate) sert � s�lectionner les donn�es du m�me jour
    # PLOT$NDVI[MIDI.id] : identifier toutes les donn�es NDVI comprises entre 11h et 13h
    # aggregate(...,mean) : calculer la moyenne des donn�es journali�res NDVI
    tmp <- aggregate(PLOT$NDVI[MIDI.id],list(DAY=date(DateTime)[MIDI.id]),mean)
    # Verifier l'objet (fonctionne si on run ligne par ligne et non toute la boucle d'un seul coup)
    head(tmp)
    
    # Arrondir la moyenne que l'on vient de calculer � 2 chiffres apr�s la virgule et cr�er une nouvelle colonne dans l'objet tmp
    tmp$NDVI <- round(tmp$x, 2)
    # transformer la colonne cr��e ci-dessus au format zoo
    tmpz <- zoo(tmp$NDVI)
    # la fonction time (package zoo) indique que la colonne DAY de l'objet tmp est identifi� comme �tant la date
    time(tmpz) <- tmp$DAY
    # enregistre l'objet zoo dans la liste cr��e ligne 19
    NDVIz[[i]]<- tmpz
    NDVIz[[i]]
}
# Mettre dans un objet toutes les donn�es qui appartiennent � un site
# ligne 12 � 16 utilis�es
CROZ <- do.call(merge, NDVIz[CRO.id])
head(CROZ)

SUR <- do.call(merge, NDVIz[SUR.id])
DIST_INT <- do.call(merge, NDVIz[DIS_INT.id ])
DIST_EXT <- do.call(merge, NDVIz[DIS_EXT.id ])
########################### Afficher toutes les ann�es par site
# Total ann�es - Crouzet (CRO)
plot(CROZ, plot.type="single", xlab="Ann�es", ylim=c(-1,1), ylab="Indice NDVI", main = "NDVI sur le site de Crouzet, slidingperiod=5", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1)) + geom_line()
# Total ann�es - Vallonpierre (SUR)
plot(SUR, plot.type="single", xlab="Ann�es", ylab="Indice NDVI", main = "NDVI sur le site de Vallonpierre (PNE)", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1))
# Total ann�es - Distroit Int�rieur (DIST_INT)
plot(DIST_INT, plot.type="single", xlab="Ann�es", ylab="Indice NDVI", main = "NDVI sur le site de Distroit Int�rieur (PNE)", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1))
# Total ann�es - Distroit Ext�rieur (DIST_EXT)
plot(DIST_EXT, plot.type="single", xlab="Ann�es", ylab="Indice NDVI", main = "NDVI sur le site de Distroit Ext�rieur (PNE)", yaxt="n")
axis(2, las = 1, at = seq(-1, 1, 0.1))
########################### trier les donn�es par site
# Mettre dans un objet toutes les donn�es qui appartiennent � un site
# ligne 12 � 16 utilis�es
CROZ <- NDVIz[CRO.id]
head(CROZ)
SUR <- NDVIz[SUR.id]
DIST_INT <- NDVIz[DIS_INT.id]
DIST_EXT <- NDVIz[DIS_EXT.id]
57
########################### Ann�e 2013 - CROUZET
# Cette ligne est utile quand il y a plusieurs ann�es dans un objet CROZ[[1]]
YEAR_2013 <- which(year(CROZ[[1]])==2013)
YEAR_2013
# Mettre dans un objet une ann�e d'un site
YEAR_CRO <- CROZ[[1]][YEAR_2013]
YEAR_CRO
# D�bruiter les donn�es avec la fonction rollmean
# il est possible de changer l'intensit� du lissage en modifiant le "12"
# align indique la mani�re dont on lisse est ce que l'on prend sur 12 valeurs celle de : right / left / center
YEAR_RM = rollmean(YEAR_CRO, 12, fill = NA, align = 'right')
# Calculer la difference d'une valeur � l'autre
# lag = indique sur combien de jour la diference se calcule
# fonction diff() utilis�e pour des objets zoo avec des dates
delta_2013 <- diff(YEAR_RM, lag=5)
########################### Ann�e 2014 - Crouzet
# plusieurs objets zoo contiennent une seule ann�e
# Indexer les valeurs associ�es � 2014 dans les objets Y_2014
Y1_2014 <- which(year(CROZ[[1]])==2014)
Y2_2014 <- which(year(CROZ[[2]])==2014)
Y3_2014 <- which(year(CROZ[[3]])==2014)
Y4_2014 <- which(year(CROZ[[4]])==2014)
# associ� l'indexation ci-dessus et int�grer dans un objet les valeurs associ�s � 2014
Y1_2014 <- CROZ[[1]][Y1_2014]
Y2_2014 <- CROZ[[2]][Y2_2014]
Y3_2014 <- CROZ[[3]][Y3_2014]
Y4_2014 <- CROZ[[4]][Y4_2014]
# rbind.zoo: chaque objet ci-dessus va �tre int�gr� ligne par ligne dans un seul objet
58
# But: avoir toutes les valeurs pour une seule ann�e dans un objet
TOTAL <- rbind.zoo(Y1_2014,Y2_2014,Y3_2014,Y4_2014)
# Arrondir les valeurs NDVI apr�s avoir effectu� un rollmean � 12
Y_2014 <- round(rollmean(TOTAL, 12, fill= NA),2)
Y_2014
# Calculer la difference entre deux valeurs, le lag indique la grandeur de cette difference
delta_2014 <- diff(Y_2014, lag=20)
# lisser les r�sultats de ce calcul de diff�rence et arrondir ces valeurs
delta_roll <- round(rollmean(delta_2014, 12, fill=NA),2)
delta_roll
### identifier la date de deneigement
# quelles sont les valeurs sup�rieures � 0
id.zero <- which(Y_2014>0)
#afficher la position de ces valeurs
id.zero
# prendre la position de la premiere valeur de id.zero
id.zero <- Y_2014[79]
id.zero
# verifier l'information manuellement en affichant l'objet 2014
Y_2014
# identifier les Valeurs manquantes pour une ann�e
NA_2014 <- which(is.na(Y_2014))
#nombre de valeurs manquantes
length(NA_2014)
59
### Extraire la valeur maximale entre le 1er avril et le 31 novembre : saison ph�nologique
MAX_2014 <- window(Y_2014, start="2014-04-01", end="2014-11-30")
ID.MAX <- which.max(MAX_2014)
#afficher la valeur et la date
PIC_2014 <- MAX_2014[ID.MAX]
PIC_2014
### Trouver la date et valeur de demarrage du verdissemeent : valeur � 50% de la valeur max NDVI
# identifier l'intervalle de temps entre le moment o� le NDVI d�passe 0 � start
# et le moment o� le NDVI atteint sa valeur maximale � end
DEM_2014 <- window(Y_2014, start="2014-04-22", end="2014-06-10")
# calculer la moiti� de la valeur du ndvi max
PIC_2014/2
# chercher quelles sont les valeurs = � ce r�sultat dans la p�riode DEM_2014
id.DEM <- which(DEM_2014==0.37)
#selectionner la valeur et la date dans DEM_2014
value.dem <- DEM_2014[id.DEM]
value.dem
### Idenfifier la dtae t la valeur o� la croissance du NDVI est la plus importante
MAX.delta <- which.max(delta_roll)
PIC_delta <- delta_roll[MAX.delta]
PIC_delta
#je veux conna�tre quelle est la valeur du NDVI le jour du pic de delta
VAL.NDVI <- as.Date("2014-05-04")
ID.jour.max <- Y_2014[VAL.NDVI]
ID.jour.max
# calculer la p�riode de d�neigement en jour
period.neig <- (as.Date("2014-05-04"))-(as.Date("2014-04-22"))
period.neig
# calculer la P�riode de verdissement en jour
period.verdi <- (as.Date("2014-06-10"))-(as.Date("2014-05-07"))
period.verdi
### identifier le 1er decrochage avec un seuil de delta = -0.05
# Afficher objet delta_roll pour savoir � quelle date le delta atteint -0.05
delta_roll
# indiquer la date � laquelle le seuil est d�pass�
ID.regain <- as.Date("2014-07-21")
# R�cup�rer la valeur NDVI associ�e � cette date
ID.zero <- Y_2014[ID.regain]
ID.zero
# lignes de codes 2019 � 2017 utiles pour afficher la valeur NDVI selon les seuils de delta que l'on cherche
# decrochage, regain, etc..
#P�riode 1er d�crochage des 2 seuils :
# 1ere date : date de fin de rupture
# 2eme date : date d'un des 2 seuils
period.decro.1 <- (as.Date("2014-07-21"))-(as.Date("2014-07-13"))
period.decro.1
#M�me methode que la rupture
# identifier dans l'objet delta_roll le moment o� la valeur de delta devient positive
delta_roll
# r�cup�rer la date associ�e
ID.regain <- as.Date("2014-08-15")#afficher la valeur NDVI associ�e � cette date
ID.zero <- Y_2014[ID.regain]
ID.zero
delta_roll
# calculer la p�riode de regain en jour
#identifier quand valeur de delta devient positive et quand elle atteint un pic
period.regain <- (as.Date("2014-08-10"))-(as.Date("2014-08-15"))
period.regain
# Identifier la date de senescence, cad le premier jour o� delta devient negatif definitivement dans delta_roll
#utiliser cette date pour r�cup�rer la valeur NDVI dan l'objet Y_2014
ID.senes <- as.Date("2014-09-04")
ID.zero <- Y_2014[ID.senes]
ID.zero
### creer l'abscisse d'un plot qui affiche les mois de l'ann�e � l'intervalle et fr�quence souhait�e
# identifier les dates de toutes les valeurs de l'objet Y_2014
t <- time(Y_2014)
# R�cup�rer la position de la premi�re et derni�re valeur de l'objet t
# Identifier un intervalle de temps de la position de la premi�re et derni�re valeur de t
# cad la premiere et derni�re valeure de l'ann�e 2014
# by = indiquer la fr�quence de l'intervalle, un trait d'abscisse = un mois
t <- seq(t[1], t[287], by="month")
t
#indique que je veux deux plots dans la m�me fen�tre
# mfcol indique que je veux une colonne � 2 lignes
#pour les avoir en ligen : par(mfrow=c(1,2))
# mfcol=c(2,1) : Afficher 2 plots en colonne (l'un en-dessous de l'autre)
# oma pour les marges hors plot, pour plus de d�tails : https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet.html

# oma=c(0,0,0,2)
par(mfcol=c(3,2), mar=c(4,4,2,8)+1, xpd=F)
plot(window(Y_2014, start="2014-04-01", end="2014-11-30"),plot.type="single", ylab="Indice NDVI", xlab="mois de l'ann�e",main = "NDVI Crouzet 2014 (PNE), rollmean = 12", yaxt="n", xaxt="n")
axis(2, las = 1, at = seq(-1,1,0.1))
axis(side=1,t, at = t, labels=format(t, "%m"))
par(new=TRUE)
abline(h="0", lty=2)
# NDVI qui d�passe une valeur de 0
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
# Debut p�riode de regain NDVI
abline(v=as.Date("2014-08-10"), col="#99FF66", lwd=2)
# Fin du regain
abline(v=as.Date("2014-08-15"), col="#99FF66", lwd=2, lty=2)
#d�but senescence
abline(v=as.Date("2014-09-01"), col="#FF6600", lwd=2)
# Neige
abline(v=as.Date("2014-11-02"), col="#999999", lwd=2)
# xpd=TRUE indique que la legende est hors du plot
#inset : le pourcentage d'�loignement du graph, plus on le veut loin plus il est hors du plot

#ncol= 1 : une colonne pour toute notre l�gende
legend("left",inset=c(1,1), ncol=1,title="L�gende", xpd=TRUE ,
       legend=c("NDVI= 0", "Delta max", "Demarrage", "Pic NDVI", "Rupture (seuil � -0.05)", "Fin rupture", "Regain", "Fin Regain", "Senescence", "Neige"),
       col=c("#66CCCC", "#663300","#99FF66", "#009900", "#FFCC00", "#FFCC00", "#99FF66","#99FF66","#FF6600","#999999"), lty=1,cex=0.7,lwd=2)
plot(window(delta_roll,start="2014-04-01",end="2014-11-30"), main="Delta, crouzet 2014, lag=20 + rollmean = 12", plot.type="single", xlab="mois de l'ann�e", ylab="Variations Indice NDVI", xaxt="n")
axis(side=1,t, at = t, labels=format(t, "%m"))
par(new=TRUE)
abline(h="0", lty=2)
# NDVI qui d�passe une valeur de 0
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
# Debut p�riode de regain NDVI
abline(v=as.Date("2014-08-10"), col="#99FF66", lwd=2)
# Fin du regain
abline(v=as.Date("2014-08-15"), col="#99FF66", lwd=2)
#d�but senescence
abline(v=as.Date("2014-09-01"), col="#FF6600", lwd=2)
# Neige
abline(v=as.Date("2014-11-02"), col="#999999", lwd=2)