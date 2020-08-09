library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


###################################################
#### normalized count by REACT column
###################################################

## phase 1

data = merged_df_1 

data$REACT = sapply(data$REACT,FUN=trimws )

count_data = data.frame(summarise(group_by(data,USER,REACT),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100
count_by_react= data.frame(count_data %>% group_by(REACT) %>% summarise(normalized_frequency = mean(percent)))

count_by_react = count_by_react[order(-count_by_react$normalized_frequency),]
count_by_react$REACT <- factor(count_by_react$REACT,levels = count_by_react$REACT)

count_by_react_1 <- count_by_react
count_by_react_1$phase = "Prediction"

# if a REACT only appears in the other phase, fill in 0
REACT = c('AMBIG')
normalized_frequency = c (0)
phase = c(count_by_react_1$phase[1])
sub_df = data.frame(REACT, normalized_frequency, phase) 
count_by_react_1 = rbind(count_by_react_1,sub_df)

#ggplot(data=count_by_chart, aes(x=REACT, y=normalized_frequency)) +
#  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###########


##### phase 2

data = merged_df_2
data$REACT = sapply(data$REACT,FUN=trimws )

count_data = data.frame(summarise(group_by(data,USER,REACT),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100
count_by_react = data.frame(count_data %>% group_by(REACT) %>% summarise(normalized_frequency = mean(percent)))

count_by_react = count_by_react[order(-count_by_react$normalized_frequency),]
count_by_react$REACT <- factor(count_by_react$REACT,levels = count_by_react$REACT)
count_by_react_2 <- count_by_react
count_by_react_2$phase = "No Prediction"

# if a REACT only appears in the other phase, fill in 0
REACT = c('INVSTGT')
normalized_frequency = c (0)
phase = c(count_by_react_2$phase[1])
sub_df = data.frame(REACT, normalized_frequency, phase) 
count_by_react_2 = rbind(count_by_react_2,sub_df)

#ggplot(data=count_by_chart, aes(x=REACT, y=normalized_frequency)) +
#  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###########

count_by_react_all = rbind(count_by_react_1,count_by_react_2)

## stacked
p <- ggplot(data=count_by_react_all, aes(x=REACT, y=normalized_frequency,fill=phase)) +
  theme(axis.text.x = element_text(angle = 90))+geom_bar(stat="identity") 
p + scale_fill_brewer(palette="Paired") 

## side by side
p <- ggplot(count_by_react_all, aes(x=REACT, y=normalized_frequency, fill=phase)) + 
  geom_bar(stat="identity", position=position_dodge()) +theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

p + scale_fill_brewer(palette="Paired") 



