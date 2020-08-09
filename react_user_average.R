library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


###################################################
#### User distribution plots (hypothesis, posthypothesis, observation)
###################################################

## pick react

this_react = "REASON"

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

df_1 = count_data[count_data$REACT== this_react,]
df_1 = df_1[c('USER','REACT','percent')]


### Filling in for Hyposthesis
if (this_react == 'HYPOTHESIS'){
  USER = c('1.1')
  REACT = c('HYPOTHESIS')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
  #ggplot(data = df_1, aes(x=USER, y=percent))+geom_col()+ylim(0,45)
}

if (this_react == 'POSTHYPOTHESIS'){
  USER = c('1.1','1.7','1.9')
  REACT = c('POSTHYPOTHESIS')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
  
  #ggplot(data = df_1, aes(x=USER, y=percent))+geom_col()+ylim(0,21)
}

if (this_react == 'OBSERVATION'){
  #ggplot(data = df_1, aes(x=USER, y=percent))+geom_col()+ylim(0,40)
}

if (this_react == 'REVBELIEF'){
  USER = c('1.4','1.9')
  REACT = c('REVBELIEF')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

## phase 2

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

if (this_react == 'POSTHYPOTHESIS'){
  #ggplot(data = df_2, aes(x=USER, y=percent))+geom_col()+ylim(0,21)
}
  

if (this_react == 'HYPOTHESIS'){
  #ggplot(data = df_2, aes(x=USER, y=percent))+geom_col()+ylim(0,45)
}

if (this_react == 'OBSERVATION'){
  #ggplot(data = df_2, aes(x=USER, y=percent))+geom_col()+ylim(0,40)
}

if (this_react == 'REVBELIEF'){
  USER = c('2.4','2.10')
  REACT = c('REVBELIEF')
  percent = c(0)
  df_1 = rbind(data.frame(USER,REACT,percent),df_1)
}

df_both = data.frame(with_predictions = c("Yes","No"), percentage = c(mean(df_1$percent),mean(df_2$percent)))
ggplot(data = df_both, aes(x=with_predictions, y=percentage))+geom_col()+ggtitle(this_react)
