################################################################################
###### ANNEXE : UTILISATION DU PACKAGE TINYPACKAGE #############################


# il est possible d'adapter le package que je suis en train de developper pour 
# des besoins de visualisation et d'aide a la decision (nettoyage, structuration,
# controle de la qualité des données)

# exemple 1 : faire un rapport des données avec leur metadonnées. Le fichier est 
# un tableur excel resumant les donnnes fournies en entrée.
report_tache4 <- 
  study_assessement_report(
    dataset = dataset_tache4,
    data_dict = data_dict_tache4)[[1]]
write_excel_allsheets(report_tache4,"output_files/ANNEXE_6/summary.xlsx")

# exemple 2 : faire un rapport visuel (bookdown, equivalent a une appli web portable)
# ce bookdown se presente sous la forme d'un dossier qui faire le sommaire des 
# donnees fournies en entree, variable par variable. Il fournit des indicateurs
# et figures. Pour l'ouvrir a nouveau il suffit d'aller dans report/docs/ et de 
# cliquer sur index.html , ce qui ouvrira un navigateur internet. (NE REQUIERE PAS
# DE CONNEXION INTERNET).

if(TRUE){
  try({study_visual_report(
    dataset = dataset_tache4, 
    data_dict = data_dict_tache4,to = "output_files/ANNEXE_6/report")})
}

if(TRUE){
  try({study_visual_report(
    dataset = dataset_tache4, 
    data_dict = data_dict_tache4,to = "output_files/ANNEXE_6/report_BY_QUARTER",
    group_by = "QUARTER",
    out = "plotly")})
}
