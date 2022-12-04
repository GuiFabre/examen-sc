################################################################################
###### SUIVI DE L'OBJECTIF 5 ###################################################


# proposition d'un indicateur pour l'objectif 5
# propostion des femmes a l'emploi en temps partiel non volontaire
# par rapport aux hommes

dataset_tache5 <- 
  dataset %>% 
  sample_frac(0.02) %>% 
  select(REC_NUM,SEX,WHYPT, VOLUNTARY_PT)

data_dict_tache5 <- 
  data_dict %>% 
  data_dict_filter(
    filter_var = 
      paste0("name %in% 
      c('REC_NUM','SEX','VOLUNTARY_PT','WHYPT')")) %>%
  add_variables_valueType(dataset_tache5)

dataset_tache5 <- 
  dataset_tache5 %>%
  data_dict_apply(data_dict_tache5)

tx_difference_hm_VPT <-
  dataset_tache5 %>%
  filter(VOLUNTARY_PT != "-77") %>%
  group_by(SEX,VOLUNTARY_PT) %>% count() %>%
  group_by(SEX) %>%
  mutate(n = round(100*n/sum(n),2)) %>% ungroup() %>%
  mutate(VOLUNTARY_PT = as.character(VOLUNTARY_PT)) %>%
  filter(VOLUNTARY_PT == "FALSE") %>%
  arrange(-n) %>% slice(1:5) %>%
  mutate(
    SEX = as.character(SEX)) %>%
  left_join(
    data_dict$Categories %>% filter(variable == "SEX") %>% 
      select(SEX = name, `label:fr`), by = "SEX") %>%
  select('Individu (Sexe)' = `label:fr`, `Taux de travail non volontaire a TP` = n)

# Les femmes semblent plus exposées que les hommes (il faudrait faire un test statistique
# pour savoir si la difference est ou non significative) au temps partiel involontaire.
# on peut regarder les raisons pour lesquelles les hommes et les femmes se retrovent
# en temps partiel dans un second indicateur.

tx_difference_hm_WHYPT <-
  dataset_tache5 %>%
  filter(WHYPT != -77) %>%
  group_by(SEX,WHYPT) %>% count() %>%
  group_by(SEX) %>%
  mutate(n = round(100*n/sum(n),2)) %>% ungroup() %>%
  pivot_wider(names_from = SEX,values_from = n) %>%
  mutate(
    WHYPT = as.character(WHYPT)) %>%
  left_join(
    data_dict$Categories %>% filter(variable == "WHYPT") %>% 
      select(WHYPT = name, `label:fr`), by = "WHYPT") %>%
  select(`Raison pour le travail à temps partiel` = `label:fr`, Male = `1`,Female = `2`)
  
write_excel_allsheets(tx_difference_hm_VPT,   "output_files/TACHE_5/tx_difference_hm_VPT.csv")
write_excel_allsheets(tx_difference_hm_WHYPT, "output_files/TACHE_5/tx_difference_hm_WHYPT.csv")

