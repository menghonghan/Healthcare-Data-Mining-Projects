library(dplyr)
library(sqldf)
library(reshape2)
VTREVCODE16 <- read.csv("~/Desktop/healthcare/03/VTREVCODE16.TXT")
VTINP16_upd <- read.csv("~/Desktop/healthcare/03/VTINP16_upd.TXT")
`FILE_LAYOUT_and_CODES_MSDRG2007+_20.977` <- read.csv("~/Desktop/healthcare/03/FILE_LAYOUT_and_CODES_MSDRG2007+_20-977.csv", sep=";")
REVCODE_FILE_LAYOUT_and_CODES_PCCR <- read.csv("~/Desktop/healthcare/03/REVCODE_FILE_LAYOUT_and_CODES_PCCR.csv", sep=";")

# Filter DRGs between 20 and 977
VTREVCODE16_new<-VTREVCODE16%>%select(Uniq,REVCODE,REVCHRGS,PCCR)
DRG0 = subset(VTINP16_upd, VTINP16_upd$DRG >=20&VTINP16_upd$DRG <=977,select=names(VTINP16_upd))

# Merge filtered DRG to the Revenue Code file on UNIQ 
dt1<-merge(VTREVCODE16_new,DRG0, by.x = "Uniq", by.y = "UNIQ")
dt1_new<-dt1%>% select(Uniq,PCCR,REVCHRGS,DRG)

# Exclude the low dollar value services (less than $100)
dt2 = subset(dt1_new, dt1_new$REVCHRGS >=100,select=names(dt1_new))

# Sum all charges group by DRG, PCCR categories
PCCR_groupby <- dt2 %>% group_by(Uniq, DRG, PCCR) %>% summarise(revchrgs = sum(REVCHRGS))
PCCR <- merge(PCCR_groupby, REVCODE_FILE_LAYOUT_and_CODES_PCCR, by = "PCCR")
DRG<-merge(PCCR, `FILE_LAYOUT_and_CODES_MSDRG2007+_20.977`, by.x = "DRG", by.y = "MSDRG")
dt3<-DRG%>% select(MSDRG_DESC,PCCR.NAME,revchrgs)

# Tabulate
tablulate <- dcast(dt3,MSDRG_DESC~PCCR.NAME,mean)

# Combining the PCCR 3700 Operating Room & PCCR 4000 Anesthesiology
tablulate$PCCR_OR_and_Anesth_Costs <- tablulate$`3700-Operating Room` + tablulate$`4000-Anesthesiology`

rownames(tablulate) <- tablulate[,1]
tablulate <- tablulate[,-1]

# Turn NA to 0
tablulate[is.na(tablulate)] = 0
View(tablulate) #687 rows and 55 columns

setwd("/Users/tmh/desktop") 
write.csv(tablulate,"PCCR_DRG_HMH.csv")

  






  ###########cluster
  
  
  
  install.packages("rattle")
  library(cluster)
  library(rattle)
  data(PCCR_DRG, package="rattle")
  head(PCCR_DRG)
  dim(PCCR_DRG)
  
  
  PCCR_DRG.stand  = scale(PCCR_DRG[-1])  # To standarize the variables
  
  # K-Means
  k.means.fit = kmeans(PCCR_DRG.stand, 3) # k = 3
  attributes(k.means.fit)
  k.means.fit$centers
  k.means.fit$cluster
  k.means.fit$size
  
  
  wssplot = function(data, nc=15, seed=1234){
    wss   = (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] = sum(kmeans(data, centers=i)$withinss)
    }
    plot(x    = 1:nc,
         y    = wss, 
         type = "b",
         xlab = "Number of Clusters",ylab="Within groups sum of squares")
  }
  wssplot(PCCR_DRG.stand, nc=6) 
  
  
  
  clusplot(x      = PCCR_DRG.stand,
           clus   = k.means.fit$cluster,
           main   = '2D representation of the Cluster solution',
           color  = TRUE,
           shade  = TRUE,
           labels = 2,
           lines  = 0)
  
  
  table(PCCR_DRG[,1],k.means.fit$cluster)
  
  
  d     =  dist(PCCR_DRG.stand, method = "euclidean") # Euclidean distance matrix.
  H.fit =  hclust(d, method="ward")
  plot(H.fit)                                     # display dendogram
  groups = cutree(H.fit, k=3)                     # cut tree into 5 clusters
  
  
  # draw dendogram with red borders around the 5 clusters
  rect.hclust(H.fit, k=3, border="red") 
  table(PCCR_DRG[,1],groups)
  
  d     = dist(PCCR_DRG.stand, method = "euclidean") # Euclidean distance matrix.
  H.fit = hclust(d, method="ward.D")
  
  plot(H.fit) # display dendogram
  groups = cutree(H.fit, k=3) # cut tree into 5 clusters
  
  
  # draw dendogram with red borders around the 5 clusters
  rect.hclust(H.fit, k=3, border="red") 
  table(PCCR_DRG[,1],groups)
  
  
  
  
  
  
  url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
  food <- read.csv(url)
  head(food)
  set.seed(123456789) ## to fix the random starting clusters
  grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
  grpMeat
  
  ## list of cluster assignments
  o=order(grpMeat$cluster)
  data.frame(food$Country[o],grpMeat$cluster[o])
  
  
  plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
  text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)
  
  
  ## same analysis, but now with clustering on all
  ## protein groups change the number of clusters to 7
  set.seed(123456789)
  grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
  o=order(grpProtein$cluster)
  data.frame(food$Country[o],grpProtein$cluster[o])
  
  
  library(cluster)
  clusplot(food[,-1], grpProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
  foodagg=agnes(food,diss=FALSE,metric="euclidian")
  plot(foodagg, main='Dendrogram') ## dendrogram
  
  
  
  