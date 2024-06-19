library(tidyverse)

df<-read_csv("PathToFile.csv", 
             col_types = cols(.default = "c"))

#feedyard from microbeef------------------------------
scramble_owner<-df%>%
  select(Owner)%>%
  distinct()%>%
  rowid_to_column()%>%
    mutate(owner_anon = paste0(rowid, str_sub(Owner, -3, -1)))%>%
  select(-rowid)


scramble_origin<-df%>%
  select(Origin)%>%
  distinct()%>%
  rowid_to_column()%>%
  mutate(origin_anon = paste0(rowid, str_sub(Origin, -3, -1)))%>%
  select(-rowid)

df2<-df%>%
  left_join(scramble_origin)%>%
  left_join(scramble_owner)%>%
  mutate(Origin = origin_anon,
         Owner = owner_anon)%>%
  select(-contains('_anon'))

sk<-skim(df)
sk2<-skim(df2)
# write_csv(df2,
#           'Data/FeedlotData/Yardsheet_2024_06_11.csv')

#