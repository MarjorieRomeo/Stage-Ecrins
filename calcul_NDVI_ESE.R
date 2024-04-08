Script R pour le calcul NDVI des fichiers des capteurs ESE
# répertoire contenant les fichiers de sortie des capteurs
DIR.RAW <- "D:/gestion datas/CalculNDVI/NDVI-ESE-PNE/RAW-ESE/"
# répertoire pour diriger les fichiers intermédiaires de calculs en fin de boucle
DIR.CALC <- "D:/gestion datas/CalculNDVI/NDVI-ESE-PNE/Calc_ESE_PNE/"
# répertoire pour diriger les fichiers finaux avec les indices NDVI seulement en fin de boucle
DIR.END <- "D:/gestion datas/CalculNDVI/NDVI-ESE-PNE/ESE_final/"
#indiquer le path du fichier metadata capteur
METAFILE <- "D:/gestion datas/CalculNDVI/NDVI_Metadonnees_PNE_capteurs_2021-05-18.csv"
#importer du fichier metadata capteur
META <- read.csv2(file=METAFILE, sep=",", dec=".")
# contrôler la structure du fichier importé
head(META)
# contrôle dimension (Nb lignes et Nb colonnes) du fichier

dim(META)
# creer une liste qui contient tous les fichiers de sorties des capteurs ESE
# utile pour la boucle
allfiles <- list.files(DIR.RAW, pattern=".csv",full=T)
# dans le nom des fichiers de sorties des capteurs il y a le numero de serie du capteur associé (NS)
# ligne 32 a 48 recuperent cette information du nom de chaque fichier
# Creer une deuxieme liste des mêmes fichiers
allfiles.s <- list.files(DIR.RAW, pattern=".csv",full=F)
#vérifier si tous les fichiers ont ete importé sous forme de liste
allfiles.s
#remplacer .csv par rien ("") avec la fonction gsub
allfiles.s <- gsub(".csv","",allfiles.s)
allfiles.s
# lapply realise 2 operations aux noms des fichiers
# 1 : avec la fonction strsplit separe les informations contenu dans le nom des fichiers separes par "_".
# 2 : recuperer le 2eme terme du nom des fichiers qui correspond au numero de serie des capteurs de lummiere reflechie
# Information aussi présente dans le fichier de METAdonnees capteur dans la colonne "Sensor_R_NS" (colonne E)
# unlist : les informations recuperees ne sont plus sous forme de liste
CAPTEUR <- unlist(lapply(strsplit(allfiles.s,"_"), function(x) x[2]))
CAPTEUR
#boucle pour calculer l'indice NDVI a partir de la liste de fichier (allfiles)
for (i in 1:length(allfiles)){
    # affiche dans la console a chaque fois que la boucle a fini le traitement d'un fichier
    print(i)
    50
    SITE <- read.csv2(file=allfiles[i], sep=",", dec=".")
    # nommer les 2 fichiers de sortie de boucle avec le même nom que le fichier utilisé en entrée (SITE) de boucle :
    # 1) fichier pour les calculs (NAME.CALC)
    # 2) fichier pour l'indice NDVI seulement (NAME.END)
    NAME.CALC <- paste0(DIR.CALC, allfiles.s[i],"_calc.csv")
    NAME.END <- paste0(DIR.END, allfiles.s[i],"_date_NDVI.csv")
    # ligne 45 utilisees ici, dans l'objet capteur on a le numero de serie du capteur
    #dans l'objet META à la colonne sensor on a tous les numeros de series
    #l'objet ID recueille toutes les positions où les infos de ces 2 objets sont les mêmes
    ID <- which(META$Sensor_R_NS==CAPTEUR[i])
    # l'objet META receuille en fonction du numero de serie recupere ci-dessus (ID) l'offset et coefficient associés
    offset_NIR <- META$Offset_NIR[ID[1]]
    offset_RED <- META$Offset_RED[ID[1]]
    coeff_Z <- META$Coefficient_calibration_Z[ID[1]]
    coeff_W <- META$Coefficient_calibration_W[ID[1]]
    #calcul de l'indice NDVI : équation capteur ESE
    SITE$NIR_calc <- (coeff_Z*(SITE$NIR_mV-offset_NIR))
    SITE$RED_calc <- (coeff_W*(SITE$RED_mV-offset_RED))
    SITE$NDVI <- round((SITE$NIR_calc-SITE$RED_calc)/(SITE$NIR_calc+SITE$RED_calc), 2)
    # remplacer les valeurs NIR ou RED = 0, supérieures ou égales à 60 par NA
    # BUT : Enlever les donnees aberrantes
    SITE$NDVI[SITE$NIR_calc==0 | SITE$RED_calc==0 | SITE$NIR >=60 | SITE$Red>=60] <- NA
    51
    # remplacer les donnees hors de l'intervalle de l'indice NDVI par NA
    SITE$NDVI[SITE$NDVI>=1 | SITE$NDVI<=-1] <- NA
    # exporter objet SITE vers un format csv : fichier avec resultat des calcules effectués dans la boucle
    # ligne 60 utilisée
    write.csv(SITE,NAME.CALC,row.names=F)
    # Recuperer et concatener les colonnes : date, time et NDVI depuis l'objet SITE
    final.file <- SITE[,c("Date","Time","NDVI")]
    # exporter objet finale.file vers un format csv : fichier final
    # ligne 61 utilise ici
    write.csv(final.file, NAME.END,row.names=F)
}