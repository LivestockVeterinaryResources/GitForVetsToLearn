library(tidyverse)

#fucntion to rename variables used within lag function
mutfxn = function(x, vars, prefix){
  
  map_dfc(vars, ~
            x %>% 
            #mutate(key = as.character(NA))%>%
            transmute(!! paste(prefix,"lag",.x,sep = "_") := lag(!!as.name(.x))==!!as.name(.x))) %>% 
    bind_cols(x,.) 
  
}


# ------------------------------------------------------------------------------

#function to create grouping variables
test_fxn1 <- function(x,arrange_var, mutate_var, prefix, gap){
  arrange_var1 <- quos(!!! arrange_var) # quos returns a list of quosures - can handle (...), !!! allows more than 1
  var_ct<-length(arrange_var1) # Calculate the number of variables
  
  x %>%
    arrange(!!!arrange_var1) %>% 
    
    mutfxn(mutate_var, prefix) %>%   # call mutfxn
    
    mutate(date_gap = (difftime(date_admin, lag(date_admin), units = 'days')),  # for long format data: data_last_admin is replaced by Date
           lag_date = ((date_gap > (gap*(-1))) & (date_gap < gap))) %>%
    
    mutate(count_lags = reduce(select(., contains("lag")), `+`)) %>%  #Identify rows where a new lag group starts
    
    mutate(lag_ct = (count_lags) < (var_ct))%>%
    
    mutate(lag_ct = case_when(is.na(lag_ct)~TRUE,
                              TRUE~lag_ct)) %>% #this is necessary to fill the first row in the dataframe, which cannot be NA 
    
    mutate(key = case_when(is.na(lag_ct)>0~paste0(prefix, rowid),
                           lag_ct>0~paste0(prefix, rowid),
                           TRUE~as.character(NA))) %>%
    fill(key)
}