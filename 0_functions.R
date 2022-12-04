# telecharger les librairies et dependances


if(TRUE){
  devtools::install_github(
    repo = "https://github.com/guifabre/tinyPackage",
    force = TRUE)
  
  devtools::install_github(
    repo = "https://github.com/guifabre/fabR",
    force = TRUE)
}else{
  
  library(magrittr)
  library(rlang)
  library(lubridate)
  library(ggplot2)
  library(digest)
  library(fs)
  library(janitor)
  library(purrr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(writexl)
  library(crayon)
  library(DT)
  library(bookdown)
  library(ggplot2)
  library(haven)
  
  source("package_source_bkp/package_source_fabR/00-lazy.R")
  source("package_source_bkp/package_source_fabR/01-functions_template.R")
  source("package_source_bkp/package_source_fabR/02-file_index.R")
  source("package_source_bkp/package_source_fabR/03-dates.R")
  source("package_source_bkp/package_source_fabR/04-viz.R")
  source("package_source_bkp/package_source_fabR/05-check_table_values.R")
  
  source("package_source_bkp/package_source_tinyPackage/00-utils.R")
  source("package_source_bkp/package_source_tinyPackage/03-dictionaries_functions.R")
  source("package_source_bkp/package_source_tinyPackage/04-data_QA.R")
  source("package_source_bkp/package_source_tinyPackage/05-summary_functions.R")
  source("package_source_bkp/package_source_tinyPackage/06-visualize.R")
  source("package_source_bkp/package_source_tinyPackage/08-catalogue_functions.R")
  }

# si cela ne fonctionne pas:


library(downloader)
library(tidyverse)
library(fabR)
library(tinyPackage)
library(stringr)
library(stats)

sessionInfo()

###### functions #########


download_online_data <- function(url, output_folder = "output_files"){
  
  # verification des input. Si le fichier est de un element, on le repete autant 
  # de fois qu'il y a d'url.
  if(length(output_folder) != 1){
    stop("Votre dossier 'output_folder' doit etre une chaine de caractere")}
  if(length(url) != 1){
    stop("Votre url doit etre une chaine de caractere")}

  download(url, dest = "dataset.zip", mode="wb")
  unzip("dataset.zip", exdir = output_folder[url == url])
  file.remove("dataset.zip")

  message("Vos documents sont bien telechargés dans ", output_folder)  

}

# Transformation du key file pour le rendre interpretable plus facilement.
# La structure etant determinée pour un key file, on peut le brasser pour obtenir
# un format plus facile a interpreter. 
# C'est une liste deux tibble 'Variables' et 'Categories'. Variables contient une
# colonne 'name' dont chaque observation est une variable du dataset. 
# 'Categories' contient 'variable' et 'name' qui sont la codification des variables
# categorielles donc chaque observation possible est donnée dans 'name'.
# Le format choisis est compatible avec SPSS

shape_key_file <- function(key_file){
  
  # verification des input
  if(!is.data.frame(key_file)) stop("Your object must be a data.frame or any compatible extension (tibble)")
  
  # renommage des colomns pour plus de facilité pour la suite
  names(key_file) <- 
    make.names(names(key_file)) %>%
    str_replace_all("\\.+","_") %>%
    tolower
  
  # verification que l'object en question est bien un key file.  
  if(sum(names(key_file) %in% c("position_position", "variable_variable")) != 2){
    stop("\n\nCe fichier n'est pas un key file. Veuillez proposer un fichier qui contient au \n",
         "moins `position_position` et `variable_variable` comme nom de colonnes.")
  }
  
  tidy_key_file <-
    key_file %>%
    mutate(
      across(c(note_1_french,note_2_french,note_1_english,note_2_english), 
             ~ ifelse(!is.na(position_position),"__NO_NOTE__",.))) %>%
    fill(position_position:note_2_french,.direction = "down") %>%
    filter(!is.na(code_code)) %>%
    mutate(
      across(c(note_1_french,note_2_french,note_1_english,note_2_english), 
             ~ na_if(.,"__NO_NOTE__"))) %>% 
    mutate(
      range = ifelse(is.na(`label_english_étiquette_anglais`),code_code,NA),
      range = ifelse(code_code %in% c('1-990','0-990','1-999999'),code_code,range),
      code_code = ifelse(!is.na(range),NA,code_code))
  
  data_dict <- list(Variables=tibble(), Categories=tibble())
  
  data_dict[['Variables']] <- 
    tidy_key_file %>% 
    select(
      name = variable_variable, 
      `label:en` = variable_name_english_nom_de_variable_anglais,
      `label:fr` = variable_name_french_nom_de_variable_francais,
      length_longueur, 
      start_début, 
      end_fin, 
      position_position,
      range) %>% distinct %>% 
    group_by(name) %>% slice(1) %>% ungroup %>%
    arrange(position_position) 
  
  data_dict[['Categories']] <- 
    tidy_key_file %>% 
    filter(!is.na(code_code)) %>%
    select(
      variable     = variable_variable,
      name         = code_code,
      `label:en`   = label_english_étiquette_anglais,
      `label:fr`   = label_french_étiquette_francais)
  
  
  # the names in the variables in the dataset are upper case, so will be the 
  # variables in the data dictionary.
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    mutate(name = toupper(name))
  
  # manual addition of criteria of exclusion and replacement of blank by NA 
  # NA must be replaced by an integer to ensure data type integrity.
  data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    mutate(missing = ifelse(`label:en`== "Not applicable", TRUE, FALSE),
           name = str_replace(name,'blank','-77')) %>%
    mutate(variable = toupper(variable)) %>%
    mutate(name = toupper(name))
  
  return(data_dict) 
}

# the last element of the data dictionary needed is the data type of the variable.
# this data type is taken directly from the dataset typeof and translated into a 
# more generic type called valueType
add_variables_valueType <- function(data_dict, dataset){
  
  # verification des input
  if(!is_mlstr_data_dict(data_dict)) stop("Your key file must be at a data_dictionary format.")
  if(!is_dataset(dataset)) stop("Your key file must be at a data_dictionary format.")

  data_dict_from_data <- data_dict_extract(dataset)

  data_dict[['Variables']] <- 
    data_dict_from_data[['Variables']] %>%
    select(name, valueType) %>%
    full_join(data_dict$Variables %>% select(-matches('valueType')), by = "name") %>%
    add_index(.force = TRUE) %>%
    arrange(index)

  # verification de l'output grace a la fonction as_mlstr_data_dict()
  data_dict <- as_mlstr_data_dict(object = data_dict) 

  return(data_dict)
  
}

# to be consistent through the data and preserve both data type and TRUE missings
# (NAs that have categories assocated in the data dictionary). -77 or any replacement 
# can replace the values in the dataset. the replacement can be typed-casted 
# without affecting the valueType of the colum.
replace_missing_values <- function(dataset, data_dict, replacement = -77){
  
  na_var <- 
    data_dict$Categories %>% 
    filter(name == replacement) %>%
    pull(variable)
    
  valueType_groups <-
    data_dict$Variables %>%
    filter(name %in% na_var) %>%
    select(name,valueType) %>%
    group_by(valueType) %>%
    group_split() %>% as.list() %>%
    setNames(sort(unique(data_dict$Variables$valueType))) %>%
    lapply(function(x) x$name)
  
  for(i in 1:length(valueType_groups)){
    dataset <- 
      dataset %>%
      mutate(across(all_of(valueType_groups[[i]]), ~ {
        replace_na(.,as_valueType(replacement,names(valueType_groups[i])))}))
  }
  
  return(dataset)
}


