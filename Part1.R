setwd("/Users/seeyanyu/Desktop/SIM - UOL (2)/ST 3189 (Machine Learning)/Coursework/Part 1")
data1 <- read.csv("EWCS_2016.csv")
ncol(data1)
nrow(data1)
summary(data1)
data1[data1 < 0] <- NA
summary(data1)
data2 <- na.omit(data1)
summary(data2)

### Plot Matrix --------------------------------------------------------------
pairs(~., panel=panel.smooth, data=data2)


### PCA ---------------------------------------------------------------------
pc <- prcomp(data2, scale. = T)
summary(pc)
pc$rotation


### K Means Clustering ---------------------------------------------------------
set.seed(2021)
data2.scaled <- scale(data2)

# set k = 2 to see natural clusters of 2 (Happy or Unhappy with working conditions)
k2 <- kmeans(data2.scaled, centers = 2) 
summary(k2)


# K-means (Q87d ~ Q90b)
k2results1 <- data.frame(data2$Q87d, data2$Q90b, k2$cluster)
cluster1 <- subset(k2results1, k2$cluster==1)
cluster2 <- subset(k2results1, k2$cluster==2)

cluster1$data2.Q90b <- factor(cluster1$data2.Q90b )
cluster2$data2.Q90b <- factor(cluster2$data2.Q90b )
summary(cluster1$data2.Q90b)
summary(cluster2$data2.Q90b)
round(prop.table(table(cluster1$data2.Q90b)),2)
round(prop.table(table(cluster2$data2.Q90b)),2)

cluster1$data2.Q87d <- factor(cluster1$data2.Q87d )
cluster2$data2.Q87d <- factor(cluster2$data2.Q87d )
summary(cluster1$data2.Q87d)
summary(cluster2$data2.Q87d)
round(prop.table(table(cluster1$data2.Q87d)),2)
round(prop.table(table(cluster2$data2.Q87d)),2)


# K-means (Q87d ~ Q90a)
k2results2 <- data.frame(data2$Q87d, data2$Q90a, k2$cluster)
cluster1a <- subset(k2results2, k2$cluster==1)
cluster2a <- subset(k2results2, k2$cluster==2)

cluster1a$data2.Q87d <- factor(cluster1a$data2.Q87d )
cluster2a$data2.Q87d <- factor(cluster2a$data2.Q87d )
summary(cluster1a$data2.Q87d)
summary(cluster2a$data2.Q87d)
round(prop.table(table(cluster1a$data2.Q87d)),2)
round(prop.table(table(cluster2a$data2.Q87d)),2)

cluster1a$data2.Q90a <- factor(cluster1a$data2.Q90a )
cluster2a$data2.Q90a <- factor(cluster2a$data2.Q90a )
summary(cluster1a$data2.Q90a)
summary(cluster2a$data2.Q90a)
round(prop.table(table(cluster1a$data2.Q90a)),2)
round(prop.table(table(cluster2a$data2.Q90a)),2)


### Hierarchical Clustering --------------------------------------------------
hc.average <- hclust(dist(data2.scaled), method = "average")
plot(hc.average, main = "Average Linkage", xlab="", sub="", cex=.9)
sum(cutree(hc.average,2)==2) #29
sum(cutree(hc.average,2)==1) #7618

# Q87d vs Q90b
hc.complete <- hclust(dist(data2.scaled), method = "complete")
plot(hc.complete, main = "Complete Linkage", xlab="", sub="", cex=.9)
sum(cutree(hc.complete, 2)==2)
sum(cutree(hc.complete, 2)==1)
hc.cluster1 <- subset(k2results1, cutree(hc.complete, 2)==1)
hc.cluster2 <- subset(k2results1, cutree(hc.complete, 2)==2)

summary(hc.cluster1$data2.Q90b)
summary(hc.cluster2$data2.Q90b)

round(prop.table(table(hc.cluster1$data2.Q87d)),2)
round(prop.table(table(hc.cluster2$data2.Q87d)),2)

round(prop.table(table(hc.cluster1$data2.Q90b)),2)
round(prop.table(table(hc.cluster2$data2.Q90b)),2)

##Goodness of fit test
M1 <- as.matrix(table(hc.cluster2$data2.Q87d))
p.null <- as.vector(prop.table(table(hc.cluster1$data2.Q87d)))
chisq.test(M,p=p.null)


#Q87d vs Q90a
hc.cluster1a <- subset(k2results2, cutree(hc.complete, 2)==1)
hc.cluster2a <- subset(k2results2, cutree(hc.complete, 2)==2)
summary(hc.cluster1a$data2.Q90a)
summary(hc.cluster2a$data2.Q90a)

round(prop.table(table(hc.cluster1a$data2.Q87d)),2)
round(prop.table(table(hc.cluster2a$data2.Q87d)),2)

round(prop.table(table(hc.cluster1a$data2.Q90a)),2)
round(prop.table(table(hc.cluster2a$data2.Q90a)),2)

##Goodness of fit test
M <- as.matrix(table(hc.cluster2a$data2.Q87d))
p.null <- as.vector(prop.table(table(hc.cluster1a$data2.Q87d)))
chisq.test(M,p=p.null)
