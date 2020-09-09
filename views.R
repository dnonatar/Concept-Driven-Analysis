#### number of views

library(dplyr)

sub_df1 <- unique(merged_df_1[,c('USER','TIME','GRAPHTYP','ATTRIBUTE')])
sub_df2 <- unique(merged_df_2[,c('USER','TIME','GRAPHTYP','ATTRIBUTE')])

view_count <- function(row){
  view = 1
  count_and = table(strsplit(as.character(row["ATTRIBUTE"]), " "))["and"]
  if (!is.na(count_and)) {
    view = count_and + 1
  } 
  return(as.numeric(view)) 
}

sub_df1["VIEW"] = apply(sub_df1, 1, view_count)
sub_df2["VIEW"] = apply(sub_df2, 1, view_count)

view_by_user_1 = as.data.frame(sub_df1 %>% group_by(USER) %>% summarise_at(vars(VIEW),funs(mean(.,na.rm=TRUE))))["VIEW"]
view_by_user_2 = as.data.frame(sub_df2 %>% group_by(USER) %>% summarise_at(vars(VIEW),funs(mean(.,na.rm=TRUE))))["VIEW"]

### user average
user_ave_1 = colMeans(view_by_user_1)
user_ave_2 = colMeans(view_by_user_2)

view_by_group = data.frame(Group=c('P','S'),
                           view = c(user_ave_1,user_ave_2),
                           CI=c(1.96*sd(view_by_user_1$VIEW)/sqrt(12),1.96*sd(view_by_user_2$VIEW)/sqrt(12))
                           )


ggplot(data = view_by_group, aes(x=Group, y=view))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=view-CI, ymax=view+CI), width=.2, position=position_dodge(.9)) +
  ggtitle('Number of Views') + 
  ylab("Average View Count")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))


### query average
query_ave_1 = mean(sub_df1$VIEW)
query_ave_2 = mean(sub_df2$VIEW)

table(sub_df1$VIEW) / sum(table(sub_df1$VIEW)) *100
table(sub_df2$VIEW) / sum(table(sub_df2$VIEW)) *100
