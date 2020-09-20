library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

## Prediction Condition

data = merged_df_1[c('USER','IFPRED','PREDMATCH','DATASET')]

count_data = data.frame(summarise(group_by(data,USER,IFPRED),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100
count_ifpred_1 = count_data

#### PREDINT
count_predint_1 = count_ifpred_1[count_ifpred_1$IFPRED=="PREDINT",][c("USER","IFPRED","percent")]
USER = c('1.1','1,4','1.6','1.7','1.8','1.9','1.10','1.15')
IFPRED = "PREDINT"
percent = c(0)
count_predint_1 = rbind(data.frame(USER,IFPRED,percent),count_predint_1)

#### PREDEXT
count_predext_1 = count_ifpred_1[count_ifpred_1$IFPRED=="PREDEXT",][c("USER","IFPRED","percent")]

#### PREDNO
count_predno_1 = count_ifpred_1[count_ifpred_1$IFPRED=="PREDNO",][c("USER","IFPRED","percent")]
USER = c('1.1','1,4','1.6','1.7','1.13','1.15')
IFPRED = "PREDNO"
percent = c(0)
count_predno_1 = rbind(data.frame(USER,IFPRED,percent),count_predno_1)

## Standard Condition

data = merged_df_2[c('USER','IFPRED','PREDMATCH','DATASET')]

count_data = data.frame(summarise(group_by(data,USER,IFPRED),count =n()))
count_by_user = data.frame(count_data %>% group_by(USER) %>% summarise(Frequency = sum(count)))

count_data$total = 0

for (user in unique(count_by_user$USER)) {
  count_data[count_data$USER == user,'total'] = count_by_user[count_by_user$USER==user,'Frequency']
}

count_data$percent = count_data$count / count_data$total * 100

count_ifpred_2 = count_data

#### PREDINT
count_predint_2 = count_ifpred_2[count_ifpred_2$IFPRED=="PREDINT",][c("USER","IFPRED","percent")]
USER = c('2.1','2.8','2.11')
IFPRED = "PREDINT"
percent = c(0)
count_predint_2 = rbind(data.frame(USER,IFPRED,percent),count_predint_2)

#### PREDEXT
count_predext_2 = count_ifpred_2[count_ifpred_2$IFPRED=="PREDEXT",][c("USER","IFPRED","percent")]

#### PREDNO
count_predno_2 = count_ifpred_2[count_ifpred_2$IFPRED=="PREDNO",][c("USER","IFPRED","percent")]
USER = c('2.11')
IFPRED = "PREDNO"
percent = c(0)
count_predno_2 = rbind(data.frame(USER,IFPRED,percent),count_predno_2)
###############



predint_both = data.frame(Group = c('P', 'S'), 
                          percentage = c(mean(count_predint_1$percent),mean(count_predint_2$percent)),
                          CI = c(sd(count_predint_1$percent)*1.96/sqrt(12),sd(count_predint_2$percent)*1.96/sqrt(12))
                          )

ggplot(data = predint_both, aes(x=Group, y=percentage))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=percentage-CI, ymax=percentage+CI), width=.2, position=position_dodge(.9)) +
  ggtitle("Predict After Seeing Data") + 
  ylab("% of Queries")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))


predext_both = data.frame(Group = c('P', 'S'), 
                          percentage = c(mean(count_predext_1$percent),mean(count_predext_2$percent)),
                          CI = c(sd(count_predext_1$percent)*1.96/sqrt(12),sd(count_predext_2$percent)*1.96/sqrt(12))
                          )

ggplot(data = predext_both, aes(x=Group, y=percentage))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=percentage-CI, ymax=percentage+CI), width=.2, position=position_dodge(.9)) +
  ggtitle("Predict Before Seeing Data") + 
  ylab("% of Queries")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))


predno_both = data.frame(Group = c('P', 'S'), 
                         percentage = c(mean(count_predno_1$percent),mean(count_predno_2$percent)),
                         CI = c(sd(count_predno_1$percent)*1.96/sqrt(12),sd(count_predno_2$percent)*1.96/sqrt(12))
                         )

ggplot(data = predno_both, aes(x=Group, y=percentage))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=percentage-CI, ymax=percentage+CI), width=.2, position=position_dodge(.9)) +
  ggtitle("No Prediction") + 
  ylab("% of Queries")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))
