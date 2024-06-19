library(tidyverse)

fxn_std_tx<-function(df){
  df%>%
    #test<-test_df_tx%>%
    mutate(
      std_tx = case_when(
        str_detect(src_treatment_upper, 'ADVOC.')~'Danofloxacin',
        str_detect(src_treatment_upper, 'AROVY.|DRAXXI.|MACROSY.')~'Tulathromycin',
        str_detect(src_treatment_upper, 'BAYTRI.')~'Enrofloxacin',
        str_detect(src_treatment_upper, 'BIOMYCI.|DURAMYCI.|LA20.|OXY2|VETRIMYCI.|BIO-MYCIN|LA-20.')~'Oxytetracycline 200',
        str_detect(src_treatment_upper, 'LA30.')~'Oxytetracycline 300',
        
        str_detect(src_treatment_upper, 'EXCED.')~'Ceftiofur CFA',
        str_detect(src_treatment_upper, 'MICOTI.')~'Tilmicosin',
        str_detect(src_treatment_upper, 'NUFLO.|RESFLO.|NORFENICO.')~'Florfenicol',
        
        str_detect(src_treatment_upper, 'PENICILLI.|PEN G|PEN G.')~'Penicillin',
        str_detect(src_treatment_upper, 'ZACTRA.')~'Gamithromycin',
        str_detect(src_treatment_upper, 'ZUPREV.')~'Tildipirosin',
        
        str_detect(src_treatment_upper, 'EXCENE.')~'Ceftiofur HCl',
        str_detect(src_treatment_upper, 'POLYFLE.')~'Ampicillin',
        
        str_detect(src_treatment_upper, 'SUSTAI.')~'Sulfamethazine',
        str_detect(src_treatment_upper, 'SULFADIMETH.')~'Sulfadimethoxine',
        
        str_detect(src_treatment_upper, 'TOMORRO.')~'Cefapirin',
        str_detect(src_treatment_upper, 'NITROFURI.')~'Nitrofurizone',
        str_detect(src_treatment_upper, 'TYLAN|TYLAN.')~'Tylosin',
        
        str_detect(src_treatment_upper, 'SULFASUR|SULFASUR.|VS-ANTIBIOTIC ADMIN')~'!!!ASK!!!',
        
        #-----------------------------------------------
          str_detect(treatment_upper, 'ADVOC.')~'Danofloxacin',
          str_detect(treatment_upper, 'AROVY.|DRAXXI.|MACROSY.')~'Tulathromycin',
          str_detect(treatment_upper, 'BAYTRI.')~'Enrofloxacin',
          str_detect(treatment_upper, 'BIOMYCI.|DURAMYCI.|LA20.|OXY2|VETRIMYCI.|BIO-MYCIN|LA-20.')~'Oxytetracycline 200',
          str_detect(treatment_upper, 'LA30.')~'Oxytetracycline 300',
          
          str_detect(treatment_upper, 'EXCED.')~'Ceftiofur CFA',
          str_detect(treatment_upper, 'MICOTI.')~'Tilmicosin',
          str_detect(treatment_upper, 'NUFLO.|RESFLO.|NORFENICO.')~'Florfenicol',
          
          str_detect(treatment_upper, 'PENICILLI.|PEN G|PEN G.')~'Penicillin',
          str_detect(treatment_upper, 'ZACTRA.')~'Gamithromycin',
          str_detect(treatment_upper, 'ZUPREV.')~'Tildipirosin',
          
          str_detect(treatment_upper, 'EXCENE.')~'Ceftiofur HCl',
          str_detect(treatment_upper, 'POLYFLE.')~'Ampicillin',
          
          str_detect(treatment_upper, 'SUSTAI.')~'Sulfamethazine',
          str_detect(treatment_upper, 'SULFADIMETH.')~'Sulfadimethoxine',
          
          str_detect(treatment_upper, 'TOMORRO.')~'Cefapirin',
          str_detect(treatment_upper, 'NITROFURI.')~'Nitrofurizone',
          str_detect(treatment_upper, 'TYLAN|TYLAN.')~'Tylosin',
          
          str_detect(src_treatment_upper, 'SULFASUR|SULFASUR.|VS-ANTIBIOTIC ADMIN')~'!!!ASK!!!',
        
        TRUE~'Not Standardized')
      )%>%
    
    #add conc---------------
     mutate(mg_per_amt = case_when(
       (std_tx %in% "!!!ASK!!!")~0, 
       (std_tx %in% "Ampicillin")~400,
       (std_tx %in% "Cefapirin")~300,
       (std_tx %in% "Ceftiofur CFA" )~200, 
       (std_tx %in% "Ceftiofur HCl")~50, 
       (std_tx %in% "Danofloxacin")~180, 
       (std_tx %in% "Enrofloxacin")~100, 
       (std_tx %in% "Florfenicol")~300, 
       (std_tx %in% "Gamithromycin")~150, 
       (std_tx %in% "Nitrofurizone" )~as.numeric(NA), 
       (std_tx %in% "Not Standardized")~0, 
       (std_tx %in% "Oxytetracycline 200")~200, 
       (std_tx %in% "Oxytetracycline 300")~200, 
       (std_tx %in% "Penicillin")~1666.67,  #per first dairy paper
       (std_tx %in% "Sulfadimethoxine")~15000, 
       (std_tx %in% "Sulfamethazine")~8020, 
       (std_tx %in% "Tildipirosin")~180, 
       (std_tx %in% "Tilmicosin")~300, 
       (std_tx %in% "Tulathromycin")~100, 
       (std_tx %in% "Tylosin")~50, 
       TRUE~as.numeric(NA)
     ))%>%
    
  #add Active stubstances----------------
  mutate(ActiveSubstance = case_when(
      (std_tx %in% c("Oxytetracycline 200", "Oxytetracycline 300"))~'Oxytetracycline', 
      TRUE~std_tx) 
      )%>%
    
  #add class ----------------------
  mutate(AntimicrobialClass = case_when(
    ActiveSubstance %in% c("Ampicillin", "Penicillin")~'Penicillins',
    ActiveSubstance %in% c("Cefapirin", "Ceftiofur CFA", "Ceftiofur HCl")~'Cephalopsprins',
    ActiveSubstance %in% c("Danofloxacin", "Enrofloxacin")~'Fluoroquinolones',
    ActiveSubstance %in% c("Florfenicol")~'Amphenicols',
    ActiveSubstance %in% c("Gamithromycin", "Tildipirosin",
                           "Tilmicosin",  "Tulathromycin", "Tylosin")~'Macrolides',
    ActiveSubstance %in% c("Nitrofurizone")~'Nitrofurans',
    ActiveSubstance %in% c('Oxytetracycline')~'Tetracyclines',
    ActiveSubstance %in% c("Sulfadimethoxine","Sulfamethazine")~'Sulfonamides',
    #ActiveSubstance %in% c("!!!ASK!!!")~"!!!ASK!!!", 
    ActiveSubstance %in% c("Not Standardized")~'Non-Antimicrobial', 
    TRUE~'ERROR-no class assigned')
    )
}
