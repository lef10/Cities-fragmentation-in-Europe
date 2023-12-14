library(sf)
library(dplyr)
###################################################
##### Sortir plusieurs fichiers gpkg ##############
dossier_geopackages <- "data/shp/UA_2018_3035_eu"

# mettre les données gpkg depuis la fonction recursive pour qu'elle cherche les données dans le sous repertoire
files_gpkg <- list.files(path = dossier_geopackages, recursive = TRUE, pattern = paste0("\\.gpkg$"), full.names = TRUE)

# appliquer une basename sur les gpkg villes 
code_city<-substr(basename(files_gpkg),1,12)
names(files_gpkg)<-code_city

# lancer la boucle pour faire la liste les différentes villes gpkg
UA_gpkg<-lapply(files_gpkg[1:788],function(gpkg){
  
  # Lire toutes les données du GeoPackage
  sf <- st_read(gpkg,quiet=TRUE)
  
  # Ajouter les données dans la liste avec le nom du fichier
  cityname<-st_drop_geometry(sf)[1,"fua_name"]
  citycode<-st_drop_geometry(sf)[1,"fua_code"]
  
  message(paste(cityname, citycode))
  
  return(sf) 
})


############################################## process ##########################

# Fonction pour appliquer le traitement à un fichier GeoPackage
process_city <- lapply(UA_gpkg,function(s){
  
  # Reorganiser la classification du sol urbain
  s$classif <- as.factor(case_when((s$code_2018 == "11100" ~ "urban"),
                                   (s$code_2018 == "11210" ~ "urban"),
                                   (s$code_2018 == "11220" ~ "urban"),
                                   (s$code_2018 == "11230" ~ "urban"),
                                   (s$code_2018 == "11240" ~ "urban"),
                                   (s$code_2018 == "11300" ~ "urban"),
                                   (s$code_2018 == "12100" ~ "activity zone"),
                                   (s$code_2018 == "12300" ~ "other"),
                                   (s$code_2018 == "12400" ~ "other"),
                                   (s$code_2018 == "12210" ~ "other"),
                                   (s$code_2018 == "12220" ~ "other"),
                                   (s$code_2018 == "12230" ~ "other"),
                                   (s$code_2018 == "13100" ~ "other"),
                                   (s$code_2018 == "13300" ~ "other"),
                                   (s$code_2018 == "13400" ~ "other"),
                                   (s$code_2018 == "14200" ~ "green urban"),
                                   (s$code_2018 == "14100" ~ "green urban"),
                                   (s$code_2018 == "21000" ~ "farming"),
                                   (s$code_2018 == "22000" ~ "farming"),
                                   (s$code_2018 == "23000" ~ "farming"),
                                   (s$code_2018 == "24000" ~ "farming"),
                                   (s$code_2018 == "25000" ~ "farming"),
                                   (s$code_2018 == "31000" ~ "natural"),
                                   (s$code_2018 == "32000" ~ "natural"),
                                   (s$code_2018 == "33000" ~ "natural"),
                                   (s$code_2018 == "40000" ~ "natural"),
                                   (s$code_2018 == "50000" ~ "water"),
                                   TRUE ~ "NA"))
  
 # calcule de la superficie de la couverture des sols 

classif_area<-data.frame(aggregate(s$area, list(s$classif), sum)) 
  
  names(classif_area)<-c("classif", "sum_area")

classif_area$total_area<-sum(classif_area$sum_area)
classif_area$percentage_area<-classif_area$sum_area/classif_area$total_area

  
classif_area$cityname<-st_drop_geometry(s)[1,"fua_name"]
classif_area$citycode<-st_drop_geometry(s)[1,"fua_code"]
  

  return(classif_area)
})
process_city


# assembler la list par un do.call
long_sums<-do.call(rbind,process_city)
write.csv(long_sums, file = "output/process_city_area.csv", row.names = FALSE)

# conserver les données sur R
saveRDS(long_sums,"output/long_sums_2018.rds")
