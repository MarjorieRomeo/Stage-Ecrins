Script pour calculer le greenness index � partir des images pour le site de Crouzet
# raster pour importer une image dans la fen�tre plot et travailler dessus
library(raster)
# phenopix pour dessiner les polygones
library(phenopix)
library(lubridate)
# ligne 9 � 22 pour dessiner le(s) polygone(s) sur image de reference et enregistrer les coordonn�es de ces polygones
# utiliser des lignes de code une seule fois par site si la camera ne bouge pas trop
# Definir path pour stocker les coordonn�es des polygones que l'on va dessiner avec la fonction DrawMULTIROI
DIR.ROI <- "D:/gestion datas/images/Crouzet/ROI/"
# importer le path de l'image de reference sur laquelle on souhaite dessiner le ROI
# l'image du 15 juillet 2015 prise � 12h13 a �t� utilis� comme image de r�f�rence pour tracer les polygones
# pour site Crouzet
IMG.REF <- "E:/Stage PNE/gestion datas/images/Crouzet/IMG/2015/Crouzet_2015-07-15_08-26-29.JPG"
#Dessiner le ROI (package phenopix)
# 1er argument de la fonction DrawMULTIROI : image de reference (IMG.REF)
# 2e argument de la fonction DrawMULTIROI : o� l'on souhaite enregistrer les coordonn�es (DIR.ROI)
# 3e argument : nom du fichier
DrawMULTIROI(IMG.REF, DIR.ROI,nroi = 1, "ROI1", file.type='.jpg')
## POUR DESSINER LE ROI
# clic gauche avec le curseur sur la photo qui s'affiche dans "Plots"
# pour dessiner le p�rim�tre du polygone(ROI)
# Une fois le premier ROI dessin� cliquer sur "finish" en haut � droite de la photo
# si plusieurs ROI souhait�s, taper n dans la console (pour no) � la question : la cr�ation de ROI est-elle finie ?
# recommencer cette operation autant que de ROI souhait�
# une fois qu'on a le nombre de ROI souhait�, taper y dans la console
# Importer le fichier dans lequel se trouvent les cordonn�es des ROI (fichier au format RData)
load("E:/Stage PNE/gestion datas/images/ROI_crouzet/roi.data.Rdata")
roi
#lister des images pour une ann�e sur le site de vallonpierre
## ATTENTION A CHANGER LE PATH chaque fois qu'on change de dossier d'images
allfiles <- list.files("E:/Stage PNE/gestion datas/images/Crouzet/IMG/2015", pattern=".JPG",full=T)
length(allfiles)
# fonction strsplit() s�pare les caract�res du nom du fichier
# lapply: recup�re le troisieme �l�ment dans le nom du fichier
# cette ligne recupere l'heure � laquelle la photo a �t� prise pour toutes les images de la liste allfiles
id.hour <- unlist(lapply(strsplit(allfiles ,"_"), function(x) x[3]))
id.hour
#enlever la terminaison .JPG de la partie recuperee
id.hour <- gsub(".JPG","",id.hour)
id.hour
# dans le m�me principe : r�cup�rer la date dans le nommage du fichier pour toutes les images de la liste allfiles
id.Date <- unlist(lapply(strsplit(allfiles ,"_"), function(x) x[2]))
id.Date
# Coller la date et l'heure et les s�parer par un espace
DateTime <- paste(id.Date, id.hour,sep=" ")
DateTime
# Mettre l'objet avec la date et l'heure au format date
DateTime <- as.POSIXct(DateTime,format = "%Y-%m-%d %H-%M-%S",tz="UTC")
DateTime
# Recuperer les images entre le mois d'avril et le mois de novembre
MIDI.id <- which(month(DateTime)>=4 & month(DateTime)<=11)
length(MIDI.id)
# creer un objet avec la date
Date.IMG <- as.data.frame.Date(DateTime[MIDI.id])
Date.IMG
# Selectionner les images entre avril et novembre dans la liste allfiles
# Rappel : allfiles = toutes les images de l'ann�e
IMG.id <- allfiles[MIDI.id]
IMG.id
# MG pour indice de vert des prairies
# MT pour indice de vert des m�l�zes
103
list.MG <- NULL
list.MT <- NULL
for (i in 1:length(IMG.id)){
    # Verifier qu'il n'y a pas de message d'erreur � chaque fin d'iteration
    # parfois un warning message peut s'afficher mais ne pas en tenir en compte car ne semble pas poser probl�me
    print (i)
    #importer l'image sous forme de raster (fonction stack() du package raster)
    IMG <- stack(IMG.id[i])
    # Des qu'il y a 3 ### �a affiche la ligne de code pr�c�dente produit sur une image mais prends trop de temps quand on calcule toutes les images
    ###plotRGB(IMG)
    # faire la somme de la quantit� de chaque couleur (rouge, bleue et vert)
    # (p.39 pr�sentation epheno)
    BR <- stackApply(IMG,rep(1,3),sum)
    ###plot(BR)
    # Calculer la quantit� relative de rouge, de bleue et de vert
    # au denominateur : la quantitt� total de couleur (BR)
    # au num�rateur : l'objet IMG r�cup�re une quantit� de couleur apr�s l'autre
    RGBi <- IMG/BR
    ### Afficher l'indice de brightness
    ###plot(RGBi)
    # Calcul l'intensit� du vert pr�sent sur l'image en soustrayant le nombre digital du bleu et du rouge au vert
    # (RGBi,2): Canal vert
    # (RGBi,1): canal rouge
    # (RGBi,3): canal bleue
    # equation 8 de Richardson (2007) simplifi�e :
    # EXG <- (RGBi,2 - RGBi,1) + (RGBi,2 - RGBi,3)
    # ca montre le changement d'�tat de la prairie dans sa ph�nologie (Richardson, 2007)
    EXG <- 2*raster(RGBi,2)-(raster(RGBi,1)+raster(RGBi,3))
    EXG
    ### zlim : ajuster la legende entre 0 et 2 et afficher
    ### plot(EXG,zlim=c(0,2))
    ### superposition des ROI dessin�es sur le plot
    ### lines(myroi <- roi.data$ROI$polygons)
    # mettre les coordonn�es des polygones dessin�s dans l'objet myroi
    myroi <- roi.data$ROI$polygons
    # gr�ce aux ROI dessin�es sur la photo de ref,
    # on applique l'�quation EXG (greenness index ou indice de vert) aux 1er, 2e et 3e polygones dessin�s sur les prairies de la photo
    # que l'on est en train de traiter
    masked_GRASS <- mask(x = EXG, mask = myroi[1:3])
    # on applique l'�quation EXG (greenness index ou indice de vert) aux 4e et 5e polygones dessin�s sur les m�l�zes de la photo
    masked_TREE <- mask(x = EXG, mask = myroi[4:5])
    ### plot(masked_GRASS)
    ###plot(masked_TREE)
    # calculer le greenness index moyen pour les prairies (utile si plusieurs polygones dessin�s sur images de r�f�rence)
    MG <- round(mean(masked_GRASS[],na.rm=T),2)
    MG
    # calculer le greenness index moyen pour les m�l�zes
    MT <- round(mean(masked_TREE[],na.rm=T),2)
    MT
    # Mettre les moyennes calculees dans une liste respective � chaque fin d'iteration
    list.MG[i] <- rbind(MG)
    list.MT[i] <- rbind(MT)
}
# verifier qu'il y a le m�me nombre de donn�es que d'images trait�es dans la boucle
# Pour cela, verifier la longueur de l'objet IMG.id et celui de list.MG
length(list.MG)
length(list.MT)
length(IMG.id)
# objet avec la date, l'heure et l'indice de vert pour les prairies et les m�l�zes
FINAL.file <- cbind(Date.IMG, list.MG, list.MT)
# calculer le greenness index moyen par jour car il y a plusieurs images par jour qui ont �t� trait� dans la boucle
MDT.G <- aggregate(FINAL.file$list.MG,list(DAY=date(FINAL.file$DateTime[MIDI.id])),mean)
MDT.G$x <- round(MDT.G$x, 2)
MDT.G
# calculer le greenness index moyen par jour car il y a plusieurs images par jour qui ont �t� trait� dans la boucle
MDT.T <- aggregate(FINAL.file$list.MT,list(DAY=date(FINAL.file$DateTime[MIDI.id])),mean)
MDT.T$x <- round(MDT.T$x, 2)
MDT.T
#changer le nom des colonnes utile pour l'exportation des donn�es dans un fichier csv
colnames(FINAL.file)[2] <- "GREENNESS_GRASS"
colnames(FINAL.file)[3] <- "GREENNESS_TREES"
#path pour enregistrer les donn�es
DIR.END <- "D:/gestion datas/images/Crouzet/IMG/GREENNESS/all_data/"
# selectionner la date de la premi�re photo trait�e en avril et de la derni�re trait�e en novembre
# ATTENTION : Il arrive qu'il manque des donn�es apr�s le calcul de brigthness par jour ligne 161.
# �a vient souvent de cette partie de la ligne : FINAL.file$DateTime[MIDI.id]
INITIAL <- FINAL.file[1,1]
END <- FINAL.file[118,1]
NAME.END <- paste0(DIR.END, "Crouzet_",INITIAL,"_",END,"_v1.csv")
NAME.END
# exporter les donnees de l'objet FINAL.file au format csv dans le dossier pr�vu � cet effet (l.180)
write.csv(FINAL.file,NAME.END,row.names=F)