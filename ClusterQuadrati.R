library(cluster)
library(MASS)
library(clusterSim)
library(ggplot2)
library(factoextra)
library(NbClust)

n_cluster<-2
bootstrap_clus<-10

#Number points for side
n_points_per_side <- 3
y_extension<- 1
#To create Figure 1 replace 6 with 7, the first 3 with 4, the first 5 with 6 and 7 with 8
#Coordinates of the first cluster's points
OnefirstPoint_x<- 4
OnesecondPoint_x<- 6
OnefirstPoint_y<- 1
OnesecondPoint_y<- 3
cluster_df<-data.frame()

#Coordinates of the second cluster's points
SecfirstPoint_x<- 3
SecsecondPoint_x<- 5
SecfirstPoint_y<- 5+y_extension
SecsecondPoint_y<- 7+y_extension
cluster_df2<-data.frame()



#First Cluster
for (i in 1:ncol(cluster_df)) {
  points_bottom <- data.frame(x = seq(OnefirstPoint_x, OnesecondPoint_x, length.out = n_points_per_side), y = rep(OnefirstPoint_y, n_points_per_side))
  points_right <- data.frame(x = rep(OnesecondPoint_x, n_points_per_side), y = seq(OnefirstPoint_y, OnesecondPoint_y, length.out = n_points_per_side))
  points_top <- data.frame(x = seq(OnesecondPoint_x, OnefirstPoint_x, length.out = n_points_per_side), y = rep(OnesecondPoint_y, n_points_per_side))
  points_left <- data.frame(x = rep(OnefirstPoint_x, n_points_per_side), y = seq(OnesecondPoint_y, OnefirstPoint_y, length.out = n_points_per_side))
  cluster_df<-rbind(points_bottom, points_left, points_right, points_top)
}

#Second cluster

for (i in 1:ncol(cluster_df2)) {
  points_bottom <- data.frame(x = seq(SecfirstPoint_x, SecsecondPoint_x, length.out = n_points_per_side), y = rep(SecfirstPoint_y, n_points_per_side))
  points_right <- data.frame(x = rep(SecsecondPoint_x, n_points_per_side), y = seq(SecfirstPoint_y, SecsecondPoint_y, length.out = n_points_per_side))
  points_top <- data.frame(x = seq(SecsecondPoint_x, SecfirstPoint_x, length.out = n_points_per_side), y = rep(SecsecondPoint_y, n_points_per_side))
  points_left <- data.frame(x = rep(SecfirstPoint_x, n_points_per_side), y = seq(SecsecondPoint_y, SecfirstPoint_y, length.out = n_points_per_side))
  cluster_df2<-rbind(points_bottom, points_left, points_right, points_top)
}

#kmeans and Gap Statistic
kmeans1<-kmeans(rbind(cluster_df,cluster_df2), centers = n_cluster)
kmeans2<-kmeans(rbind(cluster_df,cluster_df2), centers = n_cluster)
clustertot<-cbind(kmeans1$cluster, kmeans2$cluster)
gap_stat<-index.Gap(rbind(cluster_df, cluster_df2), clustertot, reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
cat("valore della gap con Index.Gap = ", round(gap_stat$gap, 4), "\n", sep = "")

######FIGURE1 and FIGURE2######
# Graph with ggplot2
ggplot() +
  geom_point(data=cluster_df, aes(x = x, y = y), color = "blue", size = 3) +# Disegna i punti
  geom_point(data=cluster_df2, aes(x = x, y = y), color = "red", size = 3) +
  coord_fixed(ratio = 1) +  # Imposta il rapporto 1:1 per mantenere il quadrato
  labs(title = paste0("k-Means with k = 2, Gap = ", round(gap_stat$gap,4)), x = "X", y = "Y") +  # Aggiungi titolo e etichette
  theme(plot.title = element_text(hjust = 0.5))+  # Centra il titolo
  xlim(-1, 25) +  # Imposta i limiti per l'asse X
  ylim(-1, 25)  

#Calculate the centroids of each cluster
centroidsFirstClu_x<- mean(cluster_df$x)
centroidsFirstClu_y<- mean(cluster_df$y)
centroidsSecondClu_x<- mean(cluster_df2$x)
centroidsSecondClu_y<- mean(cluster_df2$y)
centroidFirstClu<- data.frame(x=centroidsFirstClu_x, y=centroidsFirstClu_y)
centroidSecondClu<- data.frame(x=centroidsSecondClu_x, y=centroidsSecondClu_y)

ggplot()+
  geom_point(data=cluster_df, aes(x = x, y = y), color = "blue", size = 3) +# Disegna i punti
  geom_point(data=cluster_df2, aes(x = x, y = y), color = "red", size = 3) +
  geom_point(data = centroidFirstClu, aes(x=x, y=y), color="green", size=3)+
  geom_point(data=centroidSecondClu, aes(x=x, y=y), color="yellow", size=3)+
  coord_fixed(ratio = 1)+
  xlim(-1, 10) +  # Imposta i limiti per l'asse X
  ylim(-1, 10)  
  
  

#Add points around the two cluster with normal distribution and standard deviation(sigma) = 1
num_points<- 3
sigma<- 4
firstRandom_points_x<- rnorm(num_points, mean=centroidsFirstClu_x, sd=sigma)
dist_Euc_x<- sqrt(sum((firstRandom_points_x - centroidFirstClu)^2))
  if(dist_Euc_x < 2)
    firstRandom_points_x
dist_EucAlternate_x<- sqrt(sum((firstRandom_points_x-centroidSecondClu)^2))
  if(dist_EucAlternate_x<=1)
    firstRandom_points_x
firstRandom_points_y<- rnorm(num_points, mean=centroidsFirstClu_y, sd=sigma)
dist_Euc_y<- sqrt(sum((firstRandom_points_y - centroidFirstClu)^2))
if(dist_Euc_y < 2)
  firstRandom_points_y
dist_EucAlternate_y<- sqrt(sum((firstRandom_points_y-centroidSecondClu)^2))
if(dist_EucAlternate_y<=1)
  firstRandom_points_y
OneRandom_points<- data.frame(x=firstRandom_points_x, y=firstRandom_points_y)
firstClusterWithPoints<- rbind(cluster_df, OneRandom_points)

secondRandom_points_x<- rnorm(num_points, mean=centroidsSecondClu_x, sd=sigma)
dist2_Euc_x<- sqrt(sum((secondRandom_points_x - centroidSecondClu)^2))
if(dist2_Euc_x < 2)
  secondRandom_points_x
dist2_EucAlternate_x<- sqrt(sum((secondRandom_points_x-centroidFirstClu)^2))
if(dist2_EucAlternate_x<1)
  secondRandom_points_x
secondRandom_points_y<- rnorm(num_points, mean=centroidsSecondClu_y, sd=sigma)
dist2_Euc_y<- sqrt(sum((secondRandom_points_y - centroidSecondClu)^2))
if(dist2_Euc_y < 2)
  secondRandom_points_y
dist2_EucAlternate_y<- sqrt(sum((secondRandom_points_y-centroidFirstClu)^2))
if(dist2_EucAlternate_y<1)
  secondRandom_points_y
TwoRandom_points<- data.frame(x=secondRandom_points_x, y=secondRandom_points_y)
secondClusterWithPoints<- rbind(cluster_df2, TwoRandom_points)

#Calculate kmeans and Gap Statistic
kmeans_withPoints1<-kmeans(rbind(firstClusterWithPoints,secondClusterWithPoints), centers = n_cluster)
kmeans_withPoints2<-kmeans(rbind(firstClusterWithPoints,secondClusterWithPoints), centers = n_cluster)
clustertot_withPoints<-cbind(kmeans_withPoints1$cluster, kmeans_withPoints2$cluster)
gap_stat_withPoints<-index.Gap(rbind(firstClusterWithPoints, secondClusterWithPoints), clustertot_withPoints, reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
cat("valore della gap con Index.Gap = ", round(gap_stat_withPoints$gap, 4), "\n", sep = "")

##########FIGURE3######
ggplot() +
  geom_point(data=cluster_df, aes(x = x, y = y), color = "blue", size = 3) +# Disegna i punti
  geom_point(data=OneRandom_points, aes(x = x, y = y), color="blue", size=3)+
  geom_point(data=cluster_df2, aes(x = x, y = y), color = "red", size = 3) +
  geom_point(data=TwoRandom_points, aes(x = x, y = y), color="red", size=3)+
  coord_fixed(ratio = 1) +  # Imposta il rapporto 1:1 per mantenere il quadrato
  labs(title = paste0("k-Means with k = 2, Gap = ", round(gap_stat_withPoints$gap,4)), x = "X", y = "Y") +  # Aggiungi titolo e etichette
  theme(plot.title = element_text(hjust = 0.5))+  # Centra il titolo
  xlim(-10, 15) +  # Imposta i limiti per l'asse X
  ylim(-10, 15)  

#Add points inside the clusters
num_points<- 2
sigma<- 0.5
firstRandom_points2_x<- rnorm(num_points, mean=centroidsFirstClu_x, sd=sigma)
dist_Euc2_x<- sqrt(sum((firstRandom_points2_x - centroidFirstClu)^2))
if(dist_Euc2_x > 0.60)
  firstRandom_points2_x
firstRandom_points2_y<- rnorm(num_points, mean=centroidsFirstClu_y, sd=sigma)
dist_Euc2_y<- sqrt(sum((firstRandom_points2_y - centroidFirstClu)^2))
if(dist_Euc2_y > 0.60)
  firstRandom_points2_y
OneRandom_points2<- data.frame(x=firstRandom_points2_x, y=firstRandom_points2_y)
firstClusterWithPoints2<- rbind(cluster_df, OneRandom_points2)

secondRandom_points2_x<- rnorm(num_points, mean=centroidsSecondClu_x, sd=sigma)
dist2_Euc2_x<- sqrt(sum((secondRandom_points2_x - centroidSecondClu)^2))
if(dist2_Euc2_x > 0.60)
  secondRandom_points2_x
secondRandom_points2_y<- rnorm(num_points, mean=centroidsSecondClu_y, sd=sigma)
dist2_Euc2_y<- sqrt(sum((secondRandom_points2_y - centroidSecondClu)^2))
if(dist2_Euc2_y > 0.60)
  secondRandom_points2_y
TwoRandom_points2<- data.frame(x=secondRandom_points2_x, y=secondRandom_points2_y)
secondClusterWithPoints2<- rbind(cluster_df2, TwoRandom_points2)

#Calculate kmeans and Gap Statistic
kmeans_withPoints_inside1<-kmeans(rbind(firstClusterWithPoints2,secondClusterWithPoints2), centers = n_cluster)
kmeans_withPoints_inside2<-kmeans(rbind(firstClusterWithPoints2,secondClusterWithPoints2), centers = n_cluster)
clustertot_withPoints_inside<-cbind(kmeans_withPoints_inside1$cluster, kmeans_withPoints_inside2$cluster)
gap_stat_withPoints_inside<-index.Gap(rbind(firstClusterWithPoints2, secondClusterWithPoints2), clustertot_withPoints_inside, reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
cat("valore della gap con Index.Gap = ", round(gap_stat_withPoints_inside$gap, 4), "\n", sep = "")

######FIGURE4######
ggplot() +
  geom_point(data=cluster_df, aes(x = x, y = y), color = "blue", size = 3) +# Disegna i punti
  geom_point(data=OneRandom_points2, aes(x=x, y=y), color="blue", size=3)+
  geom_point(data=cluster_df2, aes(x = x, y = y), color = "red", size = 3) +
  geom_point(data=TwoRandom_points2, aes(x=x, y=y), color="red", size=3)+
  coord_fixed(ratio = 1) +  # Imposta il rapporto 1:1 per mantenere il quadrato
  labs(title = paste0("k-Means with k = 2, Gap = ", round(gap_stat_withPoints_inside$gap,4)), x = "X", y = "Y") +  # Aggiungi titolo e etichette
  theme(plot.title = element_text(hjust = 0.5))+  # Centra il titolo
  xlim(-1, 10) +  # Imposta i limiti per l'asse X
  ylim(-1, 10)  

######FIGURE5####
#Number points for side
n_points_per_side_lastcase <- 2

#First Cluster
for (i in 1:ncol(cluster_df)) {
  points_bottom <- data.frame(x = seq(OnefirstPoint_x, OnesecondPoint_x, length.out = n_points_per_side_lastcase), y = rep(OnefirstPoint_y, n_points_per_side_lastcase))
  points_right <- data.frame(x = rep(OnesecondPoint_x, n_points_per_side_lastcase), y = seq(OnefirstPoint_y, OnesecondPoint_y, length.out = n_points_per_side_lastcase))
  points_top <- data.frame(x = seq(OnesecondPoint_x, OnefirstPoint_x, length.out = n_points_per_side_lastcase), y = rep(OnesecondPoint_y, n_points_per_side_lastcase))
  points_left <- data.frame(x = rep(OnefirstPoint_x, n_points_per_side_lastcase), y = seq(OnesecondPoint_y, OnefirstPoint_y, length.out = n_points_per_side_lastcase))
  cluster_df_lastcase<-rbind(points_bottom, points_left, points_right, points_top)
}

#Second cluster

for (i in 1:ncol(cluster_df2)) {
  points_bottom <- data.frame(x = seq(SecfirstPoint_x, SecsecondPoint_x, length.out = n_points_per_side_lastcase), y = rep(SecfirstPoint_y, n_points_per_side_lastcase))
  points_right <- data.frame(x = rep(SecsecondPoint_x, n_points_per_side_lastcase), y = seq(SecfirstPoint_y, SecsecondPoint_y, length.out = n_points_per_side_lastcase))
  points_top <- data.frame(x = seq(SecsecondPoint_x, SecfirstPoint_x, length.out = n_points_per_side_lastcase), y = rep(SecsecondPoint_y, n_points_per_side_lastcase))
  points_left <- data.frame(x = rep(SecfirstPoint_x, n_points_per_side_lastcase), y = seq(SecsecondPoint_y, SecfirstPoint_y, length.out = n_points_per_side_lastcase))
  cluster_df2_lastcase<-rbind(points_bottom, points_left, points_right, points_top)
}

#kmeans and Gap Statistic
kmeans1_lastcase<-kmeans(rbind(cluster_df_lastcase,cluster_df2_lastcase), centers = n_cluster)
kmeans2_lastcase<-kmeans(rbind(cluster_df_lastcase,cluster_df2_lastcase), centers = n_cluster)
clustertot_lastcase<-cbind(kmeans1_lastcase$cluster, kmeans2_lastcase$cluster)
gap_stat_lastcase<-index.Gap(rbind(cluster_df_lastcase, cluster_df2_lastcase), clustertot_lastcase, reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
cat("valore della gap con Index.Gap = ", round(gap_stat_lastcase$gap, 4), "\n", sep = "")

######FIGURE5######
# Graph with ggplot2
ggplot() +
  geom_point(data=cluster_df_lastcase, aes(x = x, y = y), color = "blue", size = 3) +# Disegna i punti
  geom_point(data=cluster_df2_lastcase, aes(x = x, y = y), color = "red", size = 3) +
  coord_fixed(ratio = 1) +  # Imposta il rapporto 1:1 per mantenere il quadrato
  labs(title = paste0("k-Means with k = 2, Gap = ", round(gap_stat_lastcase$gap,4)), x = "X", y = "Y") +  # Aggiungi titolo e etichette
  theme(plot.title = element_text(hjust = 0.5))+  # Centra il titolo
  xlim(-1, 10) +  # Imposta i limiti per l'asse X
  ylim(-1, 10)  



