library(tidyverse)

#read csv with all columns as characters
fxn_read_csv_character<-function(file_location){
  read_csv(file_location, col_types = cols(.default = col_character()))
}