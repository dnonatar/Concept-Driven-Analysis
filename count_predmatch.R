library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


## phase 1

data = merged_df_1[c('USER','IFPRED','PREDMATCH','DATASET')]
data = data[data$IFPRED!="PREDNO",]   ## exclude cases without prediction

count_data = data.frame(summarise(group_by(data,USER,PREDMATCH),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100
count_by_predmatch = data.frame(count_data %>% group_by(PREDMATCH) %>% summarise(normalized_frequency = mean(percent)))

count_by_predmatch = count_by_predmatch[order(-count_by_predmatch$normalized_frequency),]
count_by_predmatch$PREDMATCH <- factor(count_by_predmatch$PREDMATCH,levels = count_by_predmatch$PREDMATCH)

count_by_predmatch_1 <- count_by_predmatch
count_by_predmatch_1$phase = "Prediction"


## phase 2
data = merged_df_2[c('USER','IFPRED','PREDMATCH','DATASET')]
data = data[data$IFPRED!="PREDNO",]   ## exclude cases without prediction

count_data = data.frame(summarise(group_by(data,USER,PREDMATCH),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100
count_by_predmatch = data.frame(count_data %>% group_by(PREDMATCH) %>% summarise(normalized_frequency = mean(percent)))

count_by_predmatch = count_by_predmatch[order(-count_by_predmatch$normalized_frequency),]
count_by_predmatch$PREDMATCH <- factor(count_by_predmatch$PREDMATCH,levels = count_by_predmatch$PREDMATCH)

count_by_predmatch_2 <- count_by_predmatch
count_by_predmatch_2$phase = "No Prediction"


count_by_predmatch_all = rbind(count_by_predmatch_1,count_by_predmatch_2)

p <- ggplot(count_by_predmatch_all, aes(x=PREDMATCH, y=normalized_frequency, fill=phase)) + 
  geom_bar(stat="identity", position=position_dodge()) +theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

p + scale_fill_brewer(palette="Paired") 
