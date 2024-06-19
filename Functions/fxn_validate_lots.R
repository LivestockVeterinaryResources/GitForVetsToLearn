
library(tidyverse)

fxn_validate_lots<-function(df){
  #note: df_wrangle_lot_filtered must be in the environment
  get_valid_lots<-df_wrangle_lot_filtered%>%
    select(Yard, lot_id)%>%
    distinct()%>%
    mutate(lot_validation_id = paste0(Yard, lot_id))%>%
    mutate(valid_lot = TRUE)%>%
    select(lot_validation_id, valid_lot)
  
  
  df%>%
    mutate(lot_validation_id = paste0(Yard, lot_id))%>%
    left_join(get_valid_lots)%>%
    filter(valid_lot == TRUE)
}
