################################################################################
###### MISE EN CORRESPONDANCE: CREATION DES DICTIONNAIRES DONNEE ###############


# creation d'un index de tous les fichiers contenus dans le dossier input
index_files <- fabR::file_index_create('input_files/')

# Apres investigation, les fichiers sont en Francais, On ajoute l'argument
# encoding = 'latin' pour lire correctement le fichier. On corrige manuellement 
# dans l'index l'information pour comment lire le fichier.
index_files <-
  fabR::file_index_search(index_files, extension = "csv") %>%
  mutate(to_eval = paste0("read_csv('",file_path,"',locale=locale(encoding='latin1'),show_col_types = FALSE)"))

# lecture des fichiers: utiliser l'index pour lire les CSV et les charger dans 
# l'environnement sous forme de tibble
key_file  <- file_index_read(index_files,file_name = "LFS_PUMF_EPA_FGMD_variables")[[1]]
all_files <- file_index_read(index_files,file_name = "^pub")

# creation of the data diction to ensure correspondance between data and meta data.
data_dict <-
  key_file %>%
  shape_key_file() %>%
  add_variables_valueType(dataset = all_files[[1]]) 

# modification of the dataset to safely handle the data type and handle NAs
dataset <- 
  all_files %>%
  bind_rows() %>%
  replace_missing_values(data_dict,replacement = -77)

# a partir du data dictionary et du dataset, on peut extraire toute information 
# pertinente. Exemple tache2 : naics_21, prov, educ, noc_40 et age_12

query <- c("'NAICS_21','PROV','EDUC','NOC_40','AGE_12'")
data_dict_tache2 <- data_dict

data_dict_tache2 <-
  data_dict %>%
  data_dict_filter(filter_var = paste0("name %in% c(",query,")")) %>%
  data_dict_flatten() %>% .[[1]] %>% 
  select(
    name,
    label_en = `label:en`,
    label_fr = `label:fr`,
    data_type = valueType,
    code_en = `Categories::label:en`,
    code_fr = `Categories::label:fr`,
    missing = `Categories::missing`)

# generer l'output
write_excel_allsheets(data_dict_tache2,"output_files/TACHE_2/correspondance.xlsx")
write_excel_allsheets(data_dict,       "output_files/TACHE_1/dictionaire_de_donnee.xlsx")

  
  



