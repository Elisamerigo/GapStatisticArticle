library(cluster)
library(MASS)
library(clusterSim)
library(ggplot2)
library(factoextra)
library(gridExtra)
library(scales)



n_cluster<-2
bootstrap_clus<-10
gap_ind<-numeric(8)
gap_values<-numeric(9)
df1_list<-list()
df2_list<-list()

#Number points for side
n_points_per_side <- 3

#Coordinates of the first cluster's points
OnefirstOriginal_x<- 4
OnesecondOriginal_x<- 6
OnefirstOriginal_y<- 1
OnesecondOriginal_y<- 3
clusterOriginal_df<-data.frame()

#Coordinates of the second cluster's points
SecfirstOriginal_x<- 3
SecsecondOriginal_x<- 5
SecfirstOriginal_y<- 5
SecsecondOriginal_y<- 7
clusterOriginal_df2<-data.frame()

#First Cluster
for (j in 1:ncol(cluster_df)) {
  points_bottom <- data.frame(x = seq(OnefirstOriginal_x, OnesecondOriginal_x, length.out = n_points_per_side), y = rep(OnefirstOriginal_y, n_points_per_side))
  points_right <- data.frame(x = rep(OnesecondOriginal_x, n_points_per_side), y = seq(OnefirstOriginal_y, OnesecondOriginal_y, length.out = n_points_per_side))
  points_top <- data.frame(x = seq(OnesecondOriginal_x, OnefirstOriginal_x, length.out = n_points_per_side), y = rep(OnesecondOriginal_y, n_points_per_side))
  points_left <- data.frame(x = rep(OnefirstOriginal_x, n_points_per_side), y = seq(OnesecondOriginal_y, OnefirstOriginal_y, length.out = n_points_per_side))
  clusterOriginal_df<-rbind(points_bottom, points_left, points_right, points_top)
}

#Second cluster
for (k in 1:ncol(cluster_df2)) {
  points_bottom <- data.frame(x = seq(SecfirstOriginal_x, SecsecondOriginal_x, length.out = n_points_per_side), y = rep(SecfirstOriginal_y, n_points_per_side))
  points_right <- data.frame(x = rep(SecsecondOriginal_x, n_points_per_side), y = seq(SecfirstOriginal_y, SecsecondOriginal_y, length.out = n_points_per_side))
  points_top <- data.frame(x = seq(SecsecondOriginal_x, SecfirstOriginal_x, length.out = n_points_per_side), y = rep(SecsecondOriginal_y, n_points_per_side))
  points_left <- data.frame(x = rep(SecfirstOriginal_x, n_points_per_side), y = seq(SecsecondOriginal_y, SecfirstOriginal_y, length.out = n_points_per_side))
  clusterOriginal_df2<-rbind(points_bottom, points_left, points_right, points_top)
}

clusterOriginalAll<-rbind(clusterOriginal_df, clusterOriginal_df2)

#kmeans and Gap Statistic
kmeans1<-kmeans(clusterOriginalAll, centers = n_cluster)
kmeans2<-kmeans(clusterOriginalAll, centers = n_cluster)
clustertot<-cbind(kmeans1$cluster, kmeans2$cluster)
gap_stat1<-index.Gap(clusterOriginalAll, clustertot, reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
cat("valore della gap con Index.Gap = ", round(gap_stat1$gap, 4), "\n", sep = "")

#Create a dataframe with all the points and the value of Gap Statistic
data1<-data.frame(clusterOriginalAll, Gap=round(gap_stat1$gap, 4))

#Create a loop for to create eight cases 
for(i in 1:8){

#Coordinates of the first cluster's points
OnefirstPoint_x<- 4+i
OnesecondPoint_x<- 6+i
OnefirstPoint_y<- 1
OnesecondPoint_y<- 3
cluster_df<-data.frame()

#Coordinates of the second cluster's points
SecfirstPoint_x<- 3
SecsecondPoint_x<- 5
SecfirstPoint_y<- 5+i
SecsecondPoint_y<- 7+i
cluster_df2<-data.frame()

#First Cluster
for (j in 1:ncol(cluster_df)) {
  points_bottom <- data.frame(x = seq(OnefirstPoint_x, OnesecondPoint_x, length.out = n_points_per_side), y = rep(OnefirstPoint_y, n_points_per_side))
  points_right <- data.frame(x = rep(OnesecondPoint_x, n_points_per_side), y = seq(OnefirstPoint_y, OnesecondPoint_y, length.out = n_points_per_side))
  points_top <- data.frame(x = seq(OnesecondPoint_x, OnefirstPoint_x, length.out = n_points_per_side), y = rep(OnesecondPoint_y, n_points_per_side))
  points_left <- data.frame(x = rep(OnefirstPoint_x, n_points_per_side), y = seq(OnesecondPoint_y, OnefirstPoint_y, length.out = n_points_per_side))
  cluster_df<-rbind(points_bottom, points_left, points_right, points_top)
  df1_list[[i]]<-cluster_df
}
  
#Second cluster
for (k in 1:ncol(cluster_df2)) {
  points_bottom <- data.frame(x = seq(SecfirstPoint_x, SecsecondPoint_x, length.out = n_points_per_side), y = rep(SecfirstPoint_y, n_points_per_side))
  points_right <- data.frame(x = rep(SecsecondPoint_x, n_points_per_side), y = seq(SecfirstPoint_y, SecsecondPoint_y, length.out = n_points_per_side))
  points_top <- data.frame(x = seq(SecsecondPoint_x, SecfirstPoint_x, length.out = n_points_per_side), y = rep(SecsecondPoint_y, n_points_per_side))
  points_left <- data.frame(x = rep(SecfirstPoint_x, n_points_per_side), y = seq(SecsecondPoint_y, SecfirstPoint_y, length.out = n_points_per_side))
  cluster_df2<-rbind(points_bottom, points_left, points_right, points_top)
  df2_list[[i]]<-cluster_df2
}

#kmeans and Gap Statistic
  kmeans1<-kmeans(clusterAll, centers = n_cluster)
  kmeans2<-kmeans(clusterAll, centers = n_cluster)
  clustertot<-cbind(kmeans1$cluster, kmeans2$cluster)
  gap_stat<-index.Gap(clusterAll, clustertot, reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
  gap_ind[i]<- round(gap_stat$gap, 4)
  cat("valore della gap con Index.Gap = ", round(gap_stat$gap, 4), "\n", sep = "")
  
}

#Create a list and add list1 and list2 alternately
df_merge <- list()
for (i in 1:length(df1_list)) {
  df_merge[[length(df_merge) + 1]] <- df1_list[[i]]
  df_merge[[length(df_merge) + 1]] <- df2_list[[i]]
}

#df_finale contains all the dataframes present in df_merge
df_finale <- do.call(rbind, df_merge)

#Create a new dataframe with all the points and the value Gap Statistic
data2 <- data.frame(df_finale, Gap=rep(gap_ind, each=24))

#Add data1 and data2 in a finale dataframe
dataFinale<- rbind(data1, data2)
#Add a new column in the dataframe that cointains the format values of Gap Statistic
GapFormat<- ifelse(dataFinale$Gap >= 0, paste0("+", format(round(dataFinale$Gap, 4), nsmall = 4)), 
                  format(round(dataFinale$Gap, 4), nsmall = 4))
dataFinaleForm<-data.frame(dataFinale, GapFormat)

#Finale graphic
ggplot(dataFinaleForm, aes(x = x, y = y, color=Gap)) +
  geom_point(size=1) +
  facet_wrap("Gap =" ~ GapFormat)+ 
  coord_cartesian(xlim=c(-1, 16), ylim=c(-1, 16))
  

