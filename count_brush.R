percent_1 = nrow(merged_df_1[grep("BRUSHING",merged_df_1$GRAPHTYP),]) / nrow(merged_df_1) * 100
percent_2 = nrow(merged_df_2[grep("BRUSHING",merged_df_2$GRAPHTYP),]) / nrow(merged_df_2) * 100

count_brush = data.frame(Group = c('Prediction', 'Standard'), Frequency = c(percent_1, percent_2))

ggplot(data = count_brush, aes(x=Group, y=Frequency))+geom_col() + ylab("Brushing Count (%)")
