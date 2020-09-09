library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


###################################################
#### User distribution plots (hypothesis, posthypothesis, observation)
###################################################

## pick react

this_react = "HYPOTHESIS"

## Prediction

data = merged_df_1 

data$REACT = sapply(data$REACT,FUN=trimws )

count_data = data.frame(summarise(group_by(data,USER,REACT),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100

df_1 = count_data[count_data$REACT== this_react,]
df_1 = df_1[c('USER','REACT','percent')]


### Filling in for missing values
if (this_react == 'HYPOTHESIS'){
  USER = c('1.1')
  REACT = c('HYPOTHESIS')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

if (this_react == 'POSTHYPOTHESIS'){
  USER = c('1.1','1.7','1.9')
  REACT = c('POSTHYPOTHESIS')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

if (this_react == 'REVBELIEF'){
  USER = c('1.4')
  REACT = c('REVBELIEF')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

if (this_react == 'SURPRISE'){
  USER = c('1.4','1.15')
  REACT = c('SURPRISE')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

if (this_react == 'GOAL'){
  USER = c('1.1','1.4')
  REACT = this_react
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

if (this_react == 'REASON'){
  USER = c('1.1')
  REACT = this_react
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

## Standard

data = merged_df_2

data$REACT = sapply(data$REACT,FUN=trimws )

count_data = data.frame(summarise(group_by(data,USER,REACT),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100

df_2 = count_data[count_data$REACT==this_react,]
df_2 = df_2[c('USER','REACT','percent')]

if (this_react == 'REVBELIEF'){
  USER = c('2.4','2.10')
  REACT = c('REVBELIEF')
  percent = c(0)
  df_2 = rbind(data.frame(USER,REACT,percent),df_2)
}

if (this_react == 'SURPRISE'){
  USER = c('2.4','2.6','2.12')
  REACT = this_react
  percent = c(0)
  df_2 = rbind(data.frame(USER,REACT,percent),df_2)
}

if (this_react == 'GOAL'){
  USER = c('2.12')
  REACT = this_react
  percent = c(0)
  df_2 = rbind(data.frame(USER,REACT,percent),df_2)
}

##########

df_both = data.frame(Group = c('P', 'S'), 
                     percentage = c(mean(df_1$percent),mean(df_2$percent)), 
                     CI = c(1.96*sd(df_1$percent)/sqrt(12), 1.96*sd(df_2$percent)/sqrt(12)))

ggplot(data = df_both, aes(x=Group, y=percentage))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=percentage-CI, ymax=percentage+CI), width=.2, position=position_dodge(.9)) +
  ggtitle("BELIEF UPDATE") + 
  ylab("% of All Reactions")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))
