#### attributes coverage

### GRE dataset
sub_df1_gre <- unique(merged_df_1[merged_df_1$DATASET=='GRE',c('USER','TIME','DATASET','ATTRIBUTE')])
sub_df2_gre <- unique(merged_df_2[merged_df_2$DATASET=='GRE',c('USER','TIME','DATASET','ATTRIBUTE')])

## phase 1 (prediction)
all_attributes_1 <- character()
for (i in 1:nrow(sub_df1_gre)){
  row = sub_df1_gre[i,]
  word_list = strsplit(as.character(row["ATTRIBUTE"]), " ")[[1]]
  word_count = length(word_list)
  for (i in 1:word_count){
    if ((tolower(word_list[i]) != "vs") & (word_list[i] != "and")){
      all_attributes_1 = c(all_attributes_1, tolower(word_list[i]))
    }
  }
}
table(all_attributes_1)

gpa = as.numeric(table(all_attributes_1)["cgpa"]) 
coa = as.numeric(table(all_attributes_1)["coa"]) 
gre = as.numeric(table(all_attributes_1)["gre"]) 
toefl = as.numeric(table(all_attributes_1)["toefl"]) 
lor = as.numeric(table(all_attributes_1)["lor"])
research = as.numeric(table(all_attributes_1)["research"]) 
sop = as.numeric(table(all_attributes_1)["sop"]) 
prevUni = as.numeric(table(all_attributes_1)["prevunivrating"] + table(all_attributes_1)["universityrating"] + 
  table(all_attributes_1)["univrat"] + table(all_attributes_1)["univrating"]) 

attribute = c('gpa','coa','gre','toefl','lor','research','sop','prevUni')
count = c(gpa,coa,gre,toefl,lor,research,sop,prevUni)

attribute_count_gre_1 <- tibble(attribute,count)
attribute_count_gre_1$normalized_count = attribute_count_gre_1$count/sum(attribute_count_gre_1$count)*100
attribute_count_gre_1$Condition = "P"
  
## phase 2 (standard)
all_attributes_2 <- character()
for (i in 1:nrow(sub_df2_gre)){
  row = sub_df2_gre[i,]
  word_list = strsplit(as.character(row["ATTRIBUTE"]), " ")[[1]]
  word_count = length(word_list)
  for (i in 1:word_count){
    if ((tolower(word_list[i]) != "vs") & (word_list[i] != "and")){
      all_attributes_2 = c(all_attributes_2, tolower(word_list[i]))
    }
  }
}
table(all_attributes_2)

gpa = as.numeric(table(all_attributes_2)["cgpa"] + table(all_attributes_2)["gpa"]) 
coa = as.numeric(table(all_attributes_2)["coa"]) 
gre = as.numeric(table(all_attributes_2)["gre"]) 
toefl = as.numeric(table(all_attributes_2)["toefl"]+table(all_attributes_2)["tofel"]) 
lor = as.numeric(table(all_attributes_2)["lor"])
research = as.numeric(table(all_attributes_2)["research"]) 
sop = as.numeric(table(all_attributes_2)["sop"]) 
prevUni = as.numeric(table(all_attributes_2)["previousunivrating"] + table(all_attributes_2)["previousyearrating"] + 
                       table(all_attributes_2)["prevunivrating"] + table(all_attributes_2)["univranking"] + 
                       table(all_attributes_2)["univrat"]) 

attribute = c('gpa','coa','gre','toefl','lor','research','sop','prevUni')
count = c(gpa,coa,gre,toefl,lor,research,sop,prevUni)

attribute_count_gre_2 <- tibble(attribute,count)
attribute_count_gre_2$normalized_count = attribute_count_gre_2$count/sum(attribute_count_gre_2$count)*100
attribute_count_gre_2$Condition = "S"

attribute_count_gre_combined <- rbind(attribute_count_gre_1,attribute_count_gre_2)




### Music dataset
sub_df1_music <- unique(merged_df_1[merged_df_1$DATASET=='Music',c('USER','TIME','DATASET','ATTRIBUTE')])
sub_df2_music <- unique(merged_df_2[merged_df_2$DATASET=='Music',c('USER','TIME','DATASET','ATTRIBUTE')])

## phase 1 (prediction)
all_attributes_1 <- character()
for (i in 1:nrow(sub_df1_music)){
  row = sub_df1_music[i,]
  word_list = strsplit(as.character(row["ATTRIBUTE"]), " ")[[1]]
  word_count = length(word_list)
  for (i in 1:word_count){
    if ((tolower(word_list[i]) != "vs") & (word_list[i] != "and")){
      all_attributes_1 = c(all_attributes_1, tolower(word_list[i]))
    }
  }
}
table(all_attributes_1)

acousticness = as.numeric(table(all_attributes_1)["accousticness"]+table(all_attributes_1)["acousticness"])
bpm = as.numeric(table(all_attributes_1)["bpm"])
danceability = as.numeric(table(all_attributes_1)["danceability"])
duration = as.numeric(table(all_attributes_1)["duration"])
energy = as.numeric(table(all_attributes_1)["energy"])
gender = as.numeric(table(all_attributes_1)["gender"])
genre = as.numeric(table(all_attributes_1)["genre"])
loudness = as.numeric(table(all_attributes_1)["loudness"])
speechiness = as.numeric(table(all_attributes_1)["speechiness"])
year = as.numeric(table(all_attributes_1)["year"]+table(all_attributes_1)["years"])
positivemood = as.numeric(table(all_attributes_1)["positivemood"]+table(all_attributes_1)["positive"])
liverecording = as.numeric(table(all_attributes_1)["liverecording"]+table(all_attributes_1)["liveness"]+
                             table(all_attributes_1)["live"] )
popularity = as.numeric(table(all_attributes_1)["polpularity"]+table(all_attributes_1)["popularity"])
  
attribute = c('acousticness','bpm','danceability','duration','energy','gender','genre','loudness','speechiness','year','positivemood','liverecording','popularity')
count = c(acousticness,bpm,danceability,duration,energy,gender,genre,loudness,speechiness,year,positivemood,liverecording,popularity)

attribute_count_music_1 <- tibble(attribute,count)
attribute_count_music_1$normalized_count = attribute_count_music_1$count/sum(attribute_count_music_1$count)*100
attribute_count_music_1$Condition = "P"

## phase 2 (standard)
all_attributes_2 <- character()
for (i in 1:nrow(sub_df2_music)){
  row = sub_df2_music[i,]
  word_list = strsplit(as.character(row["ATTRIBUTE"]), " ")[[1]]
  word_count = length(word_list)
  for (i in 1:word_count){
    if ((tolower(word_list[i]) != "vs") & (word_list[i] != "and")){
      all_attributes_2 = c(all_attributes_2, tolower(word_list[i]))
    }
  }
}
table(all_attributes_2)

acousticness = as.numeric(table(all_attributes_2)["accousticness"]+table(all_attributes_2)["acousticness"])
bpm = as.numeric(table(all_attributes_2)["bpm"])
danceability = as.numeric(table(all_attributes_2)["danceability"]+table(all_attributes_2)["danceabilty"])
duration = as.numeric(table(all_attributes_2)["duration"])
energy = as.numeric(table(all_attributes_2)["energy"])
gender = as.numeric(table(all_attributes_2)["gender"])
genre = as.numeric(table(all_attributes_2)["genre"])
loudness = as.numeric(table(all_attributes_2)["loudness"])
speechiness = as.numeric(table(all_attributes_2)["speechiness"])
year = as.numeric(table(all_attributes_2)["year"]+table(all_attributes_1)["years"])
positivemood = as.numeric(table(all_attributes_2)["positivemood"]+table(all_attributes_2)["postivemood"])
liverecording = as.numeric(table(all_attributes_2)["liverecording"]+table(all_attributes_2)["liveness"]+
                             table(all_attributes_2)["live"]+table(all_attributes_2)["liverecordings"] )
popularity = as.numeric(table(all_attributes_2)["popularity"])

attribute = c('acousticness','bpm','danceability','duration','energy','gender','genre','loudness','speechiness','year','positivemood','liverecording','popularity')
count = c(acousticness,bpm,danceability,duration,energy,gender,genre,loudness,speechiness,year,positivemood,liverecording,popularity)

attribute_count_music_2 <- tibble(attribute,count)
attribute_count_music_2$normalized_count = attribute_count_music_2$count/sum(attribute_count_music_2$count)*100
attribute_count_music_2$Condition = "S"

attribute_count_music_combined <- rbind(attribute_count_music_1,attribute_count_music_2)



### plot bar charts
ggplot(data=attribute_count_gre_combined, aes(x=attribute, y=normalized_count, fill=Condition)) +
  geom_bar(stat="identity", position=position_dodge(),color="black", width =0.6) +
  scale_fill_manual(values=c("#edb89f","#9fcbed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5)) + 
  ylab("Coverage %") +
  xlab("Attributes") 

ggplot(data=attribute_count_music_combined, aes(x=attribute, y=normalized_count, fill=Condition)) +
  geom_bar(stat="identity", position=position_dodge(),color="black", width =0.6) +
  scale_fill_manual(values=c("#edb89f","#9fcbed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  ylab("Coverage %") +
  xlab("Attributes") 



