library(dplyr)
library(tidyr)
library(reshape2)

## pick between phase 1 and 2
data = merged_df_1 
data = merged_df_2 

data = data[c('USER','TIME','DATASET','PREDMATCH','REACT')]

data$DATASET = sapply(data$DATASET,FUN=trimws )
data$REACT = sapply(data$REACT,FUN=trimws )

## For ACCMATCH
accmatch = data[data['PREDMATCH']=='ACCMATCH',]
accmatch = accmatch %>% drop_na()

react_before_ACC = c()
react_after_ACC = c()
react_before_hypo = c()

for (dataset in unique(accmatch$DATASET)){
  for (user in unique(accmatch$USER)){
    sub_df = accmatch[accmatch$DATASET==dataset & accmatch$USER==user , ]
    for (time in unique(sub_df$TIME)){  # for each user and dataset, loop through each action
      sub_df_2 = sub_df[sub_df$DATASET==dataset & sub_df$USER==user & sub_df$TIME == time, ]
         
      ACC_pos = match("EXPECT",sub_df_2$REACT)  
      if (is.na(ACC_pos)){
        react_before_ACC = append(react_before_ACC,"NoKeyWord")
        react_after_ACC = append(react_after_ACC,"NoKeyWord")
      } else if (ACC_pos == 1){
        react_before_ACC = append(react_before_ACC,"NoPriorReact")
        if (length(sub_df_2$REACT)==1){
          react_after_ACC = append(react_after_ACC,"NoAfterReact")
        } else{
          react_after_ACC = append(react_after_ACC,toString(sub_df_2$REACT[ACC_pos+1]))
        }
      } else if (ACC_pos == length(sub_df_2$REACT)) {
        react_before_ACC = append(react_before_ACC,toString(sub_df_2$REACT[ACC_pos-1]))
        if (sub_df_2$REACT[ACC_pos-1] == "HYPOTHESIS"){
          react_before_hypo = append(react_before_hypo,toString(sub_df_2$REACT[ACC_pos-2]))
        }
        react_after_ACC = append(react_after_ACC,"NoAfterReact")
      } else {
        react_before_ACC = append(react_before_ACC,toString(sub_df_2$REACT[ACC_pos-1]))
        if (sub_df_2$REACT[ACC_pos-1] == "HYPOTHESIS"){
          react_before_hypo = append(react_before_hypo,toString(sub_df_2$REACT[ACC_pos-2]))
        }
        react_after_ACC = append(react_after_ACC,toString(sub_df_2$REACT[ACC_pos+1]))
        
      }
      
    }
  }
}

sort(table(react_before_ACC))/sum(sort(table(react_before_ACC)))*100
sort(table(react_after_ACC))/sum(sort(table(react_after_ACC)))*100
sort(table(react_before_hypo))/sum(sort(table(react_before_hypo)))*100

## For NOTMATCH
notmatch = data[data['PREDMATCH']=='NOTMATCH',]
notmatch = notmatch %>% drop_na()

react_before_NOT = c()
react_after_NOT = c()
react_before_hypo = c()

for (dataset in unique(notmatch$DATASET)){
  for (user in unique(notmatch$USER)){
    sub_df = notmatch[notmatch$DATASET==dataset & notmatch$USER==user , ]
    for (time in unique(sub_df$TIME)){
      sub_df_2 = sub_df[sub_df$DATASET==dataset & sub_df$USER==user & sub_df$TIME == time, ]
      
      NOT_pos = which(sub_df_2$REACT %in% c("NOTEXPECT","NOTBELIEVE","SURPRISE"))[1]
      if (is.na(NOT_pos)){
        react_before_NOT = append(react_before_NOT,"NoKeyWord")
        react_after_NOT = append(react_after_NOT,"NoKeyWord")
      } else if (NOT_pos == 1){
        react_before_NOT = append(react_before_NOT,"NoPriorReact")
        if (length(sub_df_2$REACT)==1){
          react_after_NOT = append(react_after_NOT,"NoAfterReact")
        } else{
          react_after_NOT = append(react_after_NOT,toString(sub_df_2$REACT[NOT_pos+1]))
        }
      } else if (NOT_pos == length(sub_df_2$REACT)) {
        react_before_NOT = append(react_before_NOT,toString(sub_df_2$REACT[NOT_pos-1]))
        if (sub_df_2$REACT[NOT_pos-1] == "HYPOTHESIS"){
          react_before_hypo = append(react_before_hypo,toString(sub_df_2$REACT[NOT_pos-2]))
        }
        react_after_NOT = append(react_after_NOT,"NoAfterReact")
      } else {
        react_before_NOT = append(react_before_NOT,toString(sub_df_2$REACT[NOT_pos-1]))
        if (sub_df_2$REACT[NOT_pos-1] == "HYPOTHESIS"){
          react_before_hypo = append(react_before_hypo,toString(sub_df_2$REACT[NOT_pos-2]))
        }
        react_after_NOT = append(react_after_NOT,toString(sub_df_2$REACT[NOT_pos+1]))
      }
    }
  }
}

sort(table(react_before_NOT))/sum(sort(table(react_before_NOT)))*100
sort(table(react_after_NOT))/sum(sort(table(react_after_NOT)))*100
sort(table(react_before_hypo))/sum(sort(table(react_before_hypo)))*100



## For MODMATCH
modmatch = data[data['PREDMATCH']=='MODMATCH',]
modmatch = modmatch %>% drop_na()

react_before_MOD = c()
react_after_MOD = c()


## For CANTMATCH
cantmatch = data[data['PREDMATCH']=='CANTMATCH',]
cantmatch = cantmatch %>% drop_na()
