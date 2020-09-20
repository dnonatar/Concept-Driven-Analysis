## no separate action

data_1 = read.csv("./timespent/timespent_1.csv")
data_2 = read.csv("./timespent/combined_total_2.csv")[c("User","mode","timespent")]


timespent_sum_1 = data_1 %>% group_by(User) %>% summarise_at(vars(timespent),funs(sum(.,na.rm=TRUE)))
timespent_sum_1$queries_count = c(22,22,28,22,24,27,15,21,21,40,17,12)

timespent_sum_2 = data_2 %>% group_by(User) %>% summarise_at(vars(timespent),funs(sum(.,na.rm=TRUE)))
timespent_sum_2$queries_count = c(31,36,34,24,17,24,31,57,23,21,49,50)

timespent_avg_1 = mean(timespent_sum_1$timespent/timespent_sum_1$queries_count)
timespent_CI_1 = sd(timespent_sum_1$timespent/timespent_sum_1$queries_count)*1.96/12

timespent_avg_2 = mean(timespent_sum_2$timespent/timespent_sum_2$queries_count)
timespent_CI_2 = sd(timespent_sum_2$timespent/timespent_sum_2$queries_count)*1.96/12

timespent_total = data.frame(Group = c('P','S'), 
                             time = c(timespent_avg_1,timespent_avg_2),
                             CI = c(timespent_CI_1,timespent_CI_2) 
                             )


## predict only
data_1 = read.csv("./timespent/timespent_1.csv")
data_2 = read.csv("./timespent/combined_total_2.csv")[c("User","mode","timespent")]

data_1 = data_1[data_1$mode=='predict',]
data_2 = data_2[data_2$mode=='predict',]

timespent_sum_1 = data_1 %>% group_by(User) %>% summarise_at(vars(timespent),funs(sum(.,na.rm=TRUE)))
timespent_sum_1$queries_count = c(22,22,28,22,24,27,15,21,21,40,17,12)

timespent_sum_2 = data_2 %>% group_by(User) %>% summarise_at(vars(timespent),funs(sum(.,na.rm=TRUE)))
timespent_sum_2$queries_count = c(31,36,34,24,17,24,31,57,23,21,49,50)

timespent_avg_1 = mean(timespent_sum_1$timespent/timespent_sum_1$queries_count)
timespent_CI_1 = sd(timespent_sum_1$timespent/timespent_sum_1$queries_count)*1.96/12

timespent_avg_2 = mean(timespent_sum_2$timespent/timespent_sum_2$queries_count)
timespent_CI_2 = sd(timespent_sum_2$timespent/timespent_sum_2$queries_count)*1.96/12

timespent_predict = data.frame(Group = c('P','S'), 
                             time = c(timespent_avg_1,timespent_avg_2),
                             CI = c(timespent_CI_1,timespent_CI_2),
                             Action = "Making Prediction"
                             )


## explore only (see data + brushing)
data_1 = read.csv("./timespent/timespent_1.csv")
data_2 = read.csv("./timespent/combined_total_2.csv")[c("User","mode","timespent")]

data_1 = data_1[data_1$mode!='predict',]
data_2 = data_2[data_2$mode!='predict',]

timespent_sum_1 = data_1 %>% group_by(User) %>% summarise_at(vars(timespent),funs(sum(.,na.rm=TRUE)))
timespent_sum_1$queries_count = c(22,22,28,22,24,27,15,21,21,40,17,12)

timespent_sum_2 = data_2 %>% group_by(User) %>% summarise_at(vars(timespent),funs(sum(.,na.rm=TRUE)))
timespent_sum_2$queries_count = c(31,36,34,24,17,24,31,57,23,21,49,50)

timespent_avg_1 = mean(timespent_sum_1$timespent/timespent_sum_1$queries_count)
timespent_CI_1 = sd(timespent_sum_1$timespent/timespent_sum_1$queries_count)*1.96/12

timespent_avg_2 = mean(timespent_sum_2$timespent/timespent_sum_2$queries_count)
timespent_CI_2 = sd(timespent_sum_2$timespent/timespent_sum_2$queries_count)*1.96/12

timespent_explore = data.frame(Group = c('P','S'), 
                               time = c(timespent_avg_1,timespent_avg_2),
                               CI = c(timespent_CI_1,timespent_CI_2),
                               Action = "Looking at Data"
)

#### plot
ggplot(data = timespent_total, aes(x=Group, y=time))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=time-CI, ymax=time+CI), width=.2, position=position_dodge(.9)) +
  ggtitle("Average Time Spent") + 
  ylab("Average Seconds per Query")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))


timespent_total$low = timespent_total$time - timespent_total$CI
timespent_total$high = timespent_total$time + timespent_total$CI


time_by_mode = rbind(timespent_predict,timespent_explore)

ggplot(time_by_mode, aes(Action, y=time, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge(),color="black", width = 0.6) +
  geom_errorbar(aes(ymin=time-CI, ymax=time+CI), width=.2, position=position_dodge(0.6)) +
  scale_fill_manual(values=c("#edb89f","#9fcbed")) +
  theme_minimal() +
  ggtitle("Average Time Spent by Actions") + 
  ylab("Average Seconds per Query") +
  xlab("Action")  + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold",angle = 0, hjust = 0.5))

time_by_mode$low = time_by_mode$time - time_by_mode$CI
time_by_mode$high = time_by_mode$time + time_by_mode$CI

time_by_mode