# Modeliser courbe phénologique à partir de 2 indices de verdures
Ces travaux s'inscrivent dans le cadre d'un stage en collaboration avec le laboratoire d'écologie alpine (LECA) en 2017. L'objectif était de modéliser deux indices de verdures greenness index et NDVI pour observer les cycles phénologiques des communautés de prairies de trois sites situés entre 1800 et 2000m d'altitude dans l'enceinte du Parc National des Ecrins. Ce projet automatise le traitement des données de capteurs photos et NDVI pour modéliser les courbes pour chaque année. 

#### Ces scripts ont été réalisés avec l'aide de P. Choler Directeur de recherche CNRS (LECA)
- Traitement et calcul des indices :
    - Script R pour le calcul NDVI à partir de longueurs d'ondes de capteurs ESE : calcul_NDVI_ESE.R
    - Script R pour le calcul de Greenness index à partir de capteurs images sur un site (crouzet) et plusieurs années : calcul_GRENNESS.R
- Modéliser les longueurs d'ondes des différents capteurs utilisés :
    - Script R pour modéliser d'une courbe phénologiques et ses différentes phases pour un site (crouzet) et une année (2014) : modelisation_NDVI_ESE_Crouzet_2014.R
