################################################################################
###### SUIVI DE L'OBJECTIF 8 ###################################################


# pour des raisons d'espace memoire, on travaille sur 2% du dataset
dataset_tache4 <-
  dataset %>% sample_frac(0.02) %>% 
  select(REC_NUM,LFSSTAT,QUARTER, EDUC,AGE_6,PROV,VOLUNTARY_PT,NAICS_21) %>%
  mutate(LFSSTAT = as.integer(LFSSTAT)) %>%
  mutate(AGE_6 = as.integer(AGE_6)) %>%
  mutate(NAICS_21 = as.integer(NAICS_21))

data_dict_tache4 <- 
  data_dict %>% 
  data_dict_filter(
    filter_var = 
      paste0("name %in% 
      c('REC_NUM','LFSSTAT'     ,'QUARTER' , 'EDUC'  ,'AGE_6',
        'PROV'   ,'VOLUNTARY_PT','NAICS_21')")) %>%
  add_variables_valueType(dataset_tache4)

# Reponses aux questions posees par l'exercice
dataset_tache4 <- 
  dataset_tache4 %>%
  data_dict_apply(data_dict_tache4)

# 1. Quel est le taux de chômage à chaque trimestre de l’année donnée?
#    n'ayant pas d'indication supplentaire, j'ai supposé que le poids final
#    etait un indicateur de poids du participant dans l'etude, mais je ne l'ai pas 
#    pris en compte
tx_chom <-
  dataset_tache4 %>%
  group_by(QUARTER,LFSSTAT) %>%
  count() %>%
  group_by(QUARTER) %>%
  mutate(n = round(100*n/sum(n),2)) %>%
  filter(LFSSTAT == 3) %>% ungroup() %>% 
  select(
    trimestre = QUARTER,
    `Taux de chomage par trim. (2022)` = n)
  
# 2. Quel est le taux moyen d’achèvement des études supérieures (postsecondaires ou
#    un niveau supérieur) chez les jeunes (15 à 29 ans) pour chaque province?
tx_etud_prov <-
  dataset_tache4 %>%
  select(PROV,EDUC,AGE_6) %>%
  filter(AGE_6 != -77) %>%
  group_by(PROV,EDUC) %>% count() %>%
  group_by(PROV) %>%
  mutate(nn = sum(n)) %>%
  filter(EDUC >= 4) %>%
  mutate(n = round(100*sum(n)/nn,2)) %>%
  select(-EDUC,-nn) %>% distinct %>% ungroup %>%
  mutate(
    PROV = as.character(PROV)) %>%
  left_join(
    data_dict$Categories %>% filter(variable == "PROV") %>% 
     select(PROV = name, `label:fr`), by = "PROV") %>%
  select(province = `label:fr`, `Taux d'achev. d'etude sup (15-29 ans)` = n)


# 3. Quelles sont les cinq principales industries qui ont le taux le plus élevé de 
# travail à temps partiel involontaire?
rank_5_industry_VPT <-
  dataset_tache4 %>%
  select(NAICS_21, VOLUNTARY_PT) %>%
  filter(NAICS_21 != -77) %>%
  group_by(NAICS_21, VOLUNTARY_PT) %>% count() %>%
  group_by(NAICS_21) %>%
  mutate(n = round(100*n/sum(n),2)) %>% ungroup() %>%
  mutate(VOLUNTARY_PT = as.character(VOLUNTARY_PT)) %>%
  filter(VOLUNTARY_PT == 'FALSE') %>%
  arrange(-n) %>% slice(1:5) %>%
  mutate(
    NAICS_21 = as.character(NAICS_21)) %>%
  left_join(
    data_dict$Categories %>% filter(variable == "NAICS_21") %>% 
      select(NAICS_21 = name, `label:fr`), by = "NAICS_21") %>%
  select(industry = `label:fr`, `Taux de travail non volontaire a TP` = n)

# output
write_excel_allsheets(tx_chom,            "output_files/TACHE_4/tx_chom_trimestre.csv")
write_excel_allsheets(tx_etud_prov,       "output_files/TACHE_4/tx_etud_prov.csv")
write_excel_allsheets(rank_5_industry_VPT,"output_files/TACHE_4/rank_5_industry_VPT.csv")



