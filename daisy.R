#only needed the first time
#install.packages("dendextend")
setwd('/home/mohit/Documents/My Writeups/Analytics/daisy/')
library(cluster)
library(dendextend)
library(colorspace)

bank <- read.csv('bank.csv', sep = ';')
bank2 <- bank[c('age', 'job' ,'marital', 'education',
                'default', 'balance', 'housing', 'loan')]
summary(bank2)
table(bank2$job)
table(bank2$marital)
table(bank2$education)
table(bank2$default)
table(bank2$housing)
table(bank2$loan)

bank2$job <- factor(bank2$job)
bank2$marital <- factor(bank2$marital)
bank2$education <- factor(bank2$education,
                            levels = c('primary', 'secondary', 'tertiary'),
                            ordered = TRUE)
bank2$default <- factor(bank2$default, levels = c('no', 'yes'), labels = c(0, 1),
                            ordered = TRUE)
bank2$housing <- factor(bank2$housing, levels = c('no', 'yes'), labels = c(0, 1),
                            ordered = TRUE)
bank2$loan <- factor(bank2$loan, levels = c('no', 'yes'), labels = c(0, 1),
                            ordered = TRUE)

dmat1 <- daisy(bank2,
                metric = 'gower',
                type = list('asymm' = c('default', 'housing', 'loan'),
                            'factor' = c('job', 'marital'),
                            'ordered' = c('education'),
                            'numeric' = c('age', 'balance')))

bank3 <- bank[c('age', 'job' ,'marital', 'education',
                'default', 'balance', 'housing', 'loan')]

bank3$job <- as.numeric(factor(bank2$job))
bank3$marital <- as.numeric(factor(bank2$marital))
bank3$education <- as.numeric(factor(bank2$education))
bank3$default <- as.numeric(factor(bank2$default))
bank3$housing <- as.numeric(factor(bank2$housing))
bank3$loan <- as.numeric(factor(bank2$loan))

dmat2 <- daisy(bank3)

tree1 <- hclust(dmat1)
tree2 <- hclust(dmat2)
par(mfrow = c(2,1))
plot(tree1)
plot(tree2)

dend1 <- as.dendrogram(tree1)
dend2 <- as.dendrogram(tree2)
dend1 <- color_branches(dend1, k = 4)
dend2 <- color_branches(dend2, k = 4)
par(mfrow = c(2,1))
plot(dend1)
plot(dend2)

clus1 <- cutree(tree1, k = 4)
clus2 <- cutree(tree2, k = 4)

sil1 = silhouette (clus1, dmat1)
sil2 = silhouette (clus2, dmat2)
sil2a = silhouette (clus2, dmat1)

dev.new()
par(mfrow = c(1, 2))
plot(sil1)
plot(sil2)
