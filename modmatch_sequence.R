library(dplyr)
library(tidyr)
library(reshape2)

data = merged_df_1 
data = merged_df_2 

modmatch = data[data['PREDMATCH']=='MODMATCH',]
modmatch = modmatch %>% drop_na()

modmatch = modmatch[c("USER","TIME","DATASET","IFPRED","REACT")]
modmatch = modmatch[modmatch$IFPRED!="PREDNO",]

keywords = c('EXPECT','NOTEXPECT','NOTBELIEVE','SURPRISE','REVBELIEF','NOTKNOW')

all_sequence = c()
for (time in unique(modmatch$TIME)) {
  this_action = modmatch[modmatch$TIME == time,] 
  matching = match(keywords, this_action$REACT)
  matching = matching[!is.na(matching)]
  first_key = min(matching)
  last_key = max(matching)
  sequence = paste(this_action$REACT[first_key:last_key], collapse = '-->')
  all_sequence = append(all_sequence, sequence)
}


sort(table(all_sequence)/length(all_sequence)*100)
length(all_sequence)
