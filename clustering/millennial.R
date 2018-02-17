# ---- Read in the data ----

data <- read.table('C:/Data/millennial')
col <- read.table('C:/Data/millennial_column')
colnames(data) <- col$V2


# ---- Sample/separate data ----

n <- dim(data)[1]
p <- dim(data)[2]

visit <- which( 1:p %% 2 == 0 )
spend <- which( 1:p %% 2 == 1 )[-1]
data_v <- na.omit(data[,visit])
data_s <- na.omit(data[,spend])

View(data_v)
names(data_v)

scaleDataV=scale(data_v[,-1])
View(scaleDataV)

# === visit ===
# find the best k from SSE
find_v <- scaleDataV
wss <- (nrow(find_v)-1)*sum(apply(find_v,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(find_v,centers=i,500)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#best_v 12
best_v <- 12
k_v <- kmeans(scaleDataV, best_v, 500, 15)
centers<-k_v$center
table(k_v$cluster)

neg_v = which.max(k_v$size)
mil_v = !is.element(k_v$cluster, neg_v)
total_v = apply(data_v, 1, sum)
tapply(total_v, mil_v, summary)

millennial = data[ is.element(k_v$cluster, (1:best_v)[-neg_v]), 1 ]
View(millennial)

# further break down the biggest one
row = is.element(k_v$cluster, neg_v)
col = which(apply(data_v[row, ], 2, sum) != 0)
data2_v = data_v[row, col]
#View(data2_v)
find2_v <- scale(data2_v)
wss2 <- (nrow(find2_v)-1)*sum(apply(find2_v,2,var))
for (i in 2:15) wss2[i] <- sum(kmeans(find2_v,centers=i,500)$withinss)
plot(1:15, wss2, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#best2_v 13
best2_v = 13

k2_v = kmeans(scale(data2_v[,-1]), best2_v, 500, 15)
centers2<-k2_v$center
table(k2_v$cluster)

neg2_v = which.max(k2_v$size)
mil2_v = !is.element(k2_v$cluster, neg2_v)
total2_v = apply(data2_v, 1, sum)
tapply(total2_v, mil2_v, summary)

user2 = data[row, 1]
#View(user2)
millennial2 = user2[ is.element(k2_v$cluster, (1:best2_v)[-neg2_v]) ]

sort(total2_v[ k2_v$cluster == neg2_v ], decreasing = T)[1:100]

View(millennial2)





