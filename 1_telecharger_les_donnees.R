################################################################################
###### SUIVI DE L'OBJECTIF 8 ###################################################

# Creation d'un vecteur contenant toutes les dates dans une annéee
dates <- paste0("2022-", stringr::str_sub(paste0("0",1:12),-2,-1))

# creation de l'url pour chaque date.
url <- paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/",dates,"-CSV.zip")

# apres verifications (les fichiers tous uniques, et les fichiers qui ont le meme
# nom sont en fait le meme fichier), on peut mettre tous les fichiers dans un seul
# et meme dossier. (JANV. A SEPT.)
as.list(url[1:9]) %>% 
  lapply(function(x) download_online_data(url = x, output_folder = "input_files/"))

