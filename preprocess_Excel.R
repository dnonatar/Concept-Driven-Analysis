library(tidyverse)
library(readxl)

path <- "./Analysis_Template.xlsx"

all_sheets = path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)

sheet_list = excel_sheets(path)[-1:-8]  # pick particular sheets
phase1_list = sheet_list[1:14][-3]
phase2_list = sheet_list[15:length(sheet_list)]

merged_df_1 = tibble()
for (data_name in phase1_list){
  print(data_name)
  data = all_sheets[data_name][[1]][,1:10]
  data = fill(data ,colnames(data))       # fill missing data with values from previous row
  data = mutate(data, TIME = as.character(TIME))  # convert to char type
  merged_df_1 = rbind(merged_df_1, data)
  print(data_name)
}

merged_df_2 = tibble()
for (data_name in phase2_list){
  print(data_name)
  data = all_sheets[data_name][[1]][,1:10]
  data = fill(data ,colnames(data))
  data = mutate(data, TIME = as.character(TIME))
  merged_df_2 = rbind(merged_df_2, data)
  print(data_name)
}



