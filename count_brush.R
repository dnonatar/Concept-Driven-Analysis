library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

sub_df1 = unique(merged_df_1[c("USER","TIME","DATASET","ATTRIBUTE","GRAPHTYP")])
sub_df2 = unique(merged_df_2[c("USER","TIME","DATASET","ATTRIBUTE","GRAPHTYP")])

sub_df1_brush = unique(sub_df1[grep("BRUSHING",sub_df1$GRAPHTYP),])
sub_df2_brush = unique(sub_df2[grep("BRUSHING",sub_df2$GRAPHTYP),])

query_count_1 = summarise(group_by(sub_df1,USER),count =n())
query_count_2 = summarise(group_by(sub_df2,USER),count =n())

brush_count_1 = summarise(group_by(sub_df1_brush,USER),count =n())
brush_count_2 = summarise(group_by(sub_df2_brush,USER),count =n())

for (user in unique(query_count_1$USER)) {
  if (user %in% brush_count_1$USER){
    query_count_1[query_count_1$USER == user,'brush_count'] = brush_count_1[brush_count_1$USER==user,'count']
  } else
    query_count_1[query_count_1$USER == user,'brush_count'] = 0
}

for (user in unique(query_count_2$USER)) {
  if (user %in% brush_count_2$USER){
    query_count_2[query_count_2$USER == user,'brush_count'] = brush_count_2[brush_count_2$USER==user,'count']
  } else
    query_count_2[query_count_2$USER == user,'brush_count'] = 0
}

brush_count_1 = query_count_1
brush_count_2 = query_count_2

brush_count_1$percent = brush_count_1$brush_count/brush_count_1$count *100
brush_count_2$percent = brush_count_2$brush_count/brush_count_2$count *100

count_by_group = data.frame(Group=c('P','S'),
                            percentage=c(mean(brush_count_1$percent),mean(brush_count_2$percent)),
                            CI = c(sd(brush_count_1$percent)*1.96/sqrt(12),sd(brush_count_2$percent)*1.96/sqrt(12))    
                            )


ggplot(data = count_by_group, aes(x=Group, y=percentage))+
  geom_col(width = 0.5, fill = c("#edb89f","#9fcbed"), color="black")+
  geom_errorbar(aes(ymin=percentage-CI, ymax=percentage+CI), width=.2, position=position_dodge(.9)) +
  ggtitle('Brushing') + 
  ylab("% of Queries")  +
  theme_minimal() +
  theme(aspect.ratio = 1.5/1) + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=14),
        axis.text=element_text(size=12, face="bold"))
