library(tidyverse)

fxn_std_dx<-function(df){
  df%>%
    mutate(std_dx = case_when(
      (hd_tx>1)~'Processing',
      (diagnosis %in% c("ACUTE INT. PNEUMONIA", "ACUTE PNEUMONIA", "AIP", 
                        "CHRONIC PNEUMONIA", "CHRONIC", "FIBRINOUS PNEUMONIA", "HARD BREATHER", "HONKER", "HEAVY RESP", "HEAVY RESPIRATORY", 
                        "PNEUMONIA", "PNEUMONIA PASTURE", "RESP OBSERVE", "RESPIRATORY", "RESPIRATORY FAT",
                        "RESPIRATORY OBSERVE"))~'Respiritory', 
      (diagnosis %in% c("CRIPPLE", "CRIPPLED", "Digital Dermatitis", "FOOT ROT", "FOOTROT", "FOOTROT PASTURE", 
                        "FOUNDER/LAMINITIS", "HV FOOT ROT", "LAME", 
                        "INJURY", "INJURY MECHANICAL", "INJURY NERVOUS", "INJURY SHARPS", "INJURY TRAUMA"))~'Lameness', 
      (diagnosis %in% c("ABCESS", "ABORT", "ABORTION", "ABORTION AT PROCESSING",
                        "ABSCESS", "ACIDOSIS", 
                        "BAND BULL", "BLEEDER", "BLEEDING", "BLOAT", "BRAIN FEVER", "BRAINER", "BUFFALO INJURY",
                        "BULL CASTRATION", "BULLER", "BULLER INJURY", "CALVER", "CALVING", "CASTRATE", "CASTRATE INF",
                        "CASTRATION",            
                        "CELLULITIS", "CHOKE", 
                        "CLEANER" , "CNS INFECT", "CNS TRAUMA", "COCCI", "COCCIDIOSIS", "COLIC",
                        "CONTAMINATED CALVER", "Cripple Respiratory",
                        "DEHORN", "DIARRHEA", "DIGESTIVE",
                        "DOWNER", "DOWNHILL", "ENCEPHALITIS", "ENCEPHALITIS-BRAINER", "EXTERNAL ABCESS", 
                        "FROTHY BLOAT", "GUT INFECTION", "H SOMNUS","HARDWARE", "HEART FAILURE", "HEAT STRESS", 
                        "HEIFER BULLER",                          
                        "HOT HEIFER", 
                        "IBR", "IMPLANT AT HOSPITAL", "INFECTIONS DIARRHEA", "INFECTIOUS DIARRHEA", "INFECTIOUS HEART", 
                        "KIDNEY FAILURE", "KIDNEY STONE", 
                        "LIVER FAILURE" , "LONG  BRED", "LONG BRED", "LUMP JAW", "MECHANICAL", "MECHANICAL HEART",
                        "MISCELLANEOU", "No post mortem exam", "NON EATER", 
                        "OBSERVATION", "OBSERVE", "OTHER", "OTHER-DIGESTIVE", "OTHER-MUSCLE/SKELETAL",
                        "OTHER-URINARY/GENITAL", "OTHER/UNKNOWN", "OVERBALANCE", "OVERLOAD", "PEN DEATH",
                        "PERICARDITIS", "PERITONITIS", "PINK EYE", "PLEURITIS", 
                        "PREG CHECK", "PREG CHECK2", 
                        "PROLAPSE", "PROLAPSE REC", "PROLAPSE RECTAL", "PROLAPSE VAG", "PROLAPSE VAGINAL", 
                        "RAILER", "RECHECK OPEN", "SALMONELLA", "SCOURS", "SHORT BRED", "STRAY", "TA TOEABSCES", 
                        "TOE ABCESS", "TOE ABSCESS", "TWISTED GUT", "UNKNOWN", "UTER INFECT", "UTERINE INFECTION", 
                        "WATERBELLY",
                        
                        "DEAD CHARGE", "DECOMPOSED",
                        "DIP HEAVY", "DIPHTHERIA", "DIPTHERIA", "DIPTHERIA/HONKER",
                        "DOWN EAR", "EAR INFECTIO", "EAR INFECTION"))~'Other', 
      # (diagnosis %in% c(  
      #                   ))~'!!!Ask!!!', 
      (diagnosis %in% c("Processing", "PROCESSING"))~'Processing', 
      TRUE~diagnosis)
      )
}


         