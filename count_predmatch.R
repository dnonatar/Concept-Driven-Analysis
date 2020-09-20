library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


## Condition 1

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
CI_by_predmatch = data.frame(count_data %>% group_by(PREDMATCH) %>% summarise(normalized_CI = sd(percent)*1.96/sqrt(12)))

count_by_predmatch$CI = CI_by_predmatch$normalized_CI
count_by_predmatch = count_by_predmatch[order(-count_by_predmatch$normalized_frequency),]
count_by_predmatch$PREDMATCH <- factor(count_by_predmatch$PREDMATCH,levels = count_by_predmatch$PREDMATCH)

count_by_predmatch_1 <- count_by_predmatch
count_by_predmatch_1$Condition = "P"


## Condition 2
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
CI_by_predmatch = data.frame(count_data %>% group_by(PREDMATCH) %>% summarise(normalized_CI = sd(percent)*1.96/sqrt(12)))

count_by_predmatch$CI = CI_by_predmatch$normalized_CI
count_by_predmatch = count_by_predmatch[order(-count_by_predmatch$normalized_frequency),]
count_by_predmatch$PREDMATCH <- factor(count_by_predmatch$PREDMATCH,levels = count_by_predmatch$PREDMATCH)

count_by_predmatch_2 <- count_by_predmatch
count_by_predmatch_2$Condition = "S"


count_by_predmatch_all = rbind(count_by_predmatch_1,count_by_predmatch_2)

count_by_predmatch_ordered <- factor(count_by_predmatch_all$PREDMATCH, 
                                levels = c('NOTMATCH','ACCMATCH','MODMATCH','CANTMATCH'))

ggplot(count_by_predmatch_all, aes(x=c("Not Match", "Accurate", "Moderate", "None", "Accurate", "Moderate", "Not Match", "None"), y=normalized_frequency, fill=Condition)) + 
  geom_bar(stat="identity", position=position_dodge(),color="black", width =0.6) +
  geom_errorbar(aes(ymin=normalized_frequency-CI, ymax=normalized_frequency+CI), width=.2, position=position_dodge(0.6)) +
  scale_fill_manual(values=c("#edb89f","#9fcbed")) +
  ylab("% of Queries") +
  xlab("Matching Justification") +
  theme_minimal() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14),
      axis.title.y = element_text(color="black", size=14),
      axis.text=element_text(size=12))  +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5)) 

ggplot(count_by_predmatch_all, aes(x=count_by_predmatch_ordered, y=normalized_frequency, fill=Condition)) + 
  geom_bar(stat="identity", position=position_dodge(0.75),color="black", width =0.75) +
  geom_errorbar(aes(ymin=normalized_frequency-CI, ymax=normalized_frequency+CI), width=.2, position=position_dodge(0.75)) +
  scale_fill_manual(values=c("#edb89f","#9fcbed")) +
  ylab("% of Queries") +
  xlab("Matching Justification") +
  theme_minimal() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12))  +
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) 



count_by_predmatch_all$low = count_by_predmatch_all$normalized_frequency-count_by_predmatch_all$CI
count_by_predmatch_all$high = count_by_predmatch_all$normalized_frequency+count_by_predmatch_all$CI
