################################################################################
###### EXTRAIRE ET FUSIONNER LES DONNEES #######################################

query_tache3 <-
  c('REC_NUM','SURVMNTH','LFSSTAT','PROV','AGE_12','SEX','EDUC','NAICS_21','NOC_10',
    'NOC_40','COWMAIN','FTPTMAIN','QUARTER','VOLUNTARY_PT','WHYPT')

# creation of derived variables and valueType updates
# integer : SURVMNTH , PROV   , AGE_12   , EDUC
# decimal : NAICS_21 , NOC_10 , NOC_40   , COWMAIN
# text    : LFSSTAT  , SEX    , FTPTMAIN , QUARTER,
# boolean : VOLUNTARY_PT

vT_intg <- c('SURVMNTH'    ,'PROV'  ,'AGE_12'  ,'EDUC'   ,'WHYPT')
vT_decm <- c('NAICS_21'    ,'NOC_10','NOC_40'  ,'COWMAIN')
vT_text <- c('LFSSTAT'     ,'SEX'   ,'FTPTMAIN','QUARTER','VOLUNTARY_PT')

dataset_tache3 <- 
  dataset %>% 
  select(any_of(query_tache3)) %>%
  mutate(
    
    # Gather months into quarters
    QUARTER = 
      case_when(
        SURVMNTH %in% c("01","02","03") ~ "Q_1",
        SURVMNTH %in% c("04","05","06") ~ "Q_2",
        SURVMNTH %in% c("07","08","09") ~ "Q_3",
        SURVMNTH %in% c("10","11","12") ~ "Q_4",
      ),
    
    # recode HWHYPT : Reason for part-time work -  5 | 'Personal preference'
    VOLUNTARY_PT = recode(WHYPT, `5` = 'TRUE', `-77` = '-77', .default = 'FALSE')) %>%
  
  # change valueType 
  
  mutate(across(all_of(vT_intg), as.integer)) %>%
  mutate(across(all_of(vT_decm), as.numeric)) %>%
  mutate(across(all_of(vT_text), as.character)) 
  
# rajout des nouvelles meta donnees dans le dictionaire.
data_dict_updated <- list(
  
  Variables = data_dict[['Variables']] %>% bind_rows(
    tribble(
      ~name         , ~`label:fr`,                        ~`label:en`,                           ~valueType, ~source   , 
      'QUARTER'     ,'Regroupement des mois en trimestre','Grouping months into quarters'        ,'text','SURVMNTH', 
      'VOLUNTARY_PT','temps partiel volontaire ou non'   ,'Whether Part time is volontary or not','text'  , 'WHYPT')),
  
  
  Categories = data_dict[['Categories']] %>% bind_rows( 
    tribble(
      ~variable     , ~name , ~`label:fr`           , ~`label:en`          , ~missing , 
      'QUARTER'     ,'Q_1'  ,'Trim. 1'              ,'Quarter 1'           ,FALSE     , 
      'QUARTER'     ,'Q_2'  ,'Trim. 2'              ,'Quarter 2'           ,FALSE     , 
      'QUARTER'     ,'Q_3'  ,'Trim. 3'              ,'Quarter 3'           ,FALSE     , 
      'QUARTER'     ,'Q_4'  ,'Trim. 4'              ,'Quarter 4'           ,FALSE     , 
      'VOLUNTARY_PT','TRUE' ,'TP est volontaire'    ,'PT is volontary'     ,FALSE,
      'VOLUNTARY_PT','FALSE','TP est not volontaire','PT is not volontary' ,FALSE,
      'VOLUNTARY_PT','-77'  ,'Indisponible'         ,'Not applicable'      ,FALSE))
)

data_dict_tache3 <- 
  data_dict_updated %>%
  data_dict_filter(
    filter_var = paste0("name %in% c(",toString(paste0('"',query_tache3,'"')),")")) %>%
  add_variables_valueType(dataset_tache3)

# Rapprochement des donnees et verifications
annotated_dataset_tache3 <- 
  data_dict_apply(
    data = dataset_tache3 %>% sample_frac(0.01),
    data_dict = data_dict_tache3)

# verifications Quarter
annotated_dataset_tache3 %>%
  group_by(QUARTER,SURVMNTH) %>%
  tally

# verifications Vontary PT
annotated_dataset_tache3 %>%
  group_by(VOLUNTARY_PT,WHYPT) %>%
  tally

# gather all data
dataset <- 
  dataset %>%
  select(-any_of(query_tache3[-1])) %>%
  bind_cols(dataset_tache3[-1])

data_dict <- data_dict_updated


# generer l'output (5% des donnnes participants pour eviter d'encombrer en volume)
write_excel_allsheets(dataset_tache3 %>% sample_frac(0.05),"output_files/TACHE_3/donnees_update.csv")
write_excel_allsheets(data_dict_tache3,                    "output_files/TACHE_3/dictionaire_de_donnee_update.xlsx")
