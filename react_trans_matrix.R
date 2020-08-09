library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

###################################################
### Creating a transition matrix 
###################################################

## pick between phase 1 and 2
data = merged_df_1 
data = merged_df_2 

data= data[c('USER','DATASET','REACT')]
data = data[data['REACT']!="-",]


data$REACT = sapply(data$REACT,FUN=trimws )
dataset_list = c("GRE","Music")
user_list = unique(data$USER)
new_data = data.frame()[0,0]

for (i in user_list){
  for (j in dataset_list) {
    sub_data = data[data$USER==i & data$DATASET==j,]
    sub_list =  rep(NA, dim(sub_data)[1] )
    for (k in 2:dim(sub_data)[1] ){
      sub_list[k] = paste(sub_data$REACT[k-1],"-->",sub_data$REACT[k])
    }
    sub_data$transition = sub_list
    new_data = rbind(new_data,sub_data)
  }
}

new_data$dataset_user = paste(new_data$DATASET,new_data$USER)
drops = c("USER","REACT","DATASET")
new_data = new_data[,!names(new_data) %in%drops]
new_data = na.omit(new_data)

new_data = data.frame(new_data %>% group_by(dataset_user) %>% count(transition))

new_data = data.frame(new_data %>% spread(dataset_user, n))
new_data[is.na(new_data)] <- 0

sum_each_col = colSums(new_data[,-1], na.rm = FALSE, dims = 1)

for (i in 2:dim(new_data)[2]){
  new_data[,i] = new_data[,i] / sum_each_col[i-1] * 100
}

new_data$normalized_freq = rowMeans(new_data[,-1], na.rm = FALSE, dims = 1)
new_data = new_data[,c("transition", "normalized_freq")]
new_data = new_data %>% separate(transition, c("transition1","transition2"))

new_dataframe = as.data.frame(matrix(0, nrow = 17, ncol = 17))

# react_names = c("SURPRISE","EXPECT","NOTEXPECT","NOTBELIEVE","ACK","NOREACT","REVBELIEF",
#                 "OBSERVATION","REASON","CONCLUSION","NOTKNOW","NOCONCLUSION","AMBIG","HYPOTHESIS",
#                 "POSTHYPOTHESIS","GOAL")

react_names = unique(data$REACT)
row.names(new_dataframe) <- react_names
colnames(new_dataframe) <- react_names

for (row in 1:dim(new_data)[1]){
  current_row = new_data[row,]
  new_dataframe[current_row$transition1,current_row$transition2] = current_row[['normalized_freq']]
}


new_dataframe[is.na(new_dataframe)] <- 0

# Dissimilarity matrix
#d <- dist(new_dataframe, method = "euclidean")

# Hierarchical clustering using Complete Linkage
#hc1 <- hclust(d, method = "complete" )

#order = hc1$labels[hc1$order]
# Plot the obtained dendrogram
#plot(hc1, cex = 0.6, hang = -1)

#new_dataframe = new_dataframe[order,order]

melted_df = melt(data.matrix(new_dataframe))
melted_df = with(melted_df,  melted_df[order(Var2) , ])

ggplot(data = melted_df, aes(x=Var1, y = reorder(Var2, desc(Var2)), fill=value)) +
  geom_tile()+theme(axis.text.x = element_text(angle = 90))+
  labs(x="First Reaction", y="Second Reaction")+ scale_x_discrete(position = "top")+
  scale_fill_distiller(palette = "RdPu",direction=1,limits = c(0,7))

ggplot(data = melted_df, aes(x=Var1, y = Var2, fill=value)) + 
  geom_tile()+theme(axis.text.x = element_text(angle = 90))+
  labs(x="First Reaction", y="Second Reaction")+ scale_x_discrete(position = "top")+
  scale_fill_distiller(palette = "RdPu",direction=1,limits = c(0,7))

#scale_fill_gradient(low="white", high="blue")
############################################################################


