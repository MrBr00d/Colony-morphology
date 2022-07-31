library(tidyverse)
library(class)
#clean data

lowest <- 0.1*mean(FM145ColonyData$Area) # Lowest area which is still considerd a valid datapoint
FM145 <- FM145ColonyData[FM145ColonyData$Area > lowest,]


lowest <- 0.1*mean(LivColonyData$Area) # Lowest area which is still considerd a valid datapoint
Liv <- LivColonyData[LivColonyData$Area > lowest,]

lowest <- 0.1*mean(M145AColonyData$Area) # Lowest area which is still considerd a valid datapoint
M145A <- M145AColonyData[M145AColonyData$Area > lowest,]

df <- rbind(FM145, Liv, M145A) # combine dataframes
df$Name <- paste(rep("Colony", times = nrow(df)), 1:nrow(df), sep = "") #name each colony
df <- df %>% column_to_rownames(var = "Name") # Set rownames so only vars in df
df <- df %>% select(-Slice) # remove slice as it contains no info


#PCA
pca <- prcomp(df[,1:6], scale. = TRUE)
pcafm145 <- prcomp(FM145[2:7], scale. = TRUE)
pcaLiv <- prcomp(Liv[2:7], scale. = TRUE)
pcaM145A <- prcomp(M145A[,2:34], scale. = TRUE)

summary(pca)
summary(pcaM145A)
summary(pcafm145)
summary(pcaLiv)

label <- c(rep("FM145", times = nrow(FM145)), rep("Liv", times = nrow(Liv)), rep("M145A", times = nrow(M145A)))
df$label <- label

#Calculate the amount each components contributes to the total
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per)

loading_scores <- abs(pca$rotation[,1:2])
PCA1_top10 <- names(sort(loading_scores[,1], decreasing = TRUE)[1:10])
PCA2_top10 <- names(sort(loading_scores[,2], decreasing = TRUE)[1:10])
pca$rotation[PCA1_top10,1]
pca$rotation[PCA2_top10,1]

pca$x <- as.data.frame(pca$x)
pca$x <- rownames_to_column(pca$x, "Name")

ggplot(pca$x, aes(x= PC1, y = PC2, color = label, fill = label, label = c(1:89)))+
  geom_text()

#PCA of FM145 alone
pcaLiv$x <- as.data.frame(pcaLiv$x)
pcaLiv$x <- rownames_to_column(pcaLiv$x, "Name")

ggplot(pcaLiv$x, aes(x= PC1, y = PC2, label = c(1:34)))+
  geom_text() + 
  ggtitle("PCA Liv shape")

new_df <- pca$x[,1:5]
new_df <- cbind(new_df, label)
new_df$label <- as.factor(new_df$label)
new_df$label <- as.numeric(new_df$label)
data <- new_df

# machine learning section

size <- floor(0.6*nrow(df))
train_set <- df[sample(seq_len(nrow(df)), size = size),]
test_set <- df[-sample(seq_len(nrow(df)), size = size),]

predictions <- knn(train = train_set[,1:32],
                   test = test_set[,1:32],
                   cl = train_set[,33],
                   k= 9, prob = TRUE)

results <- as.data.frame(table(test_set$label, predictions))
#62% FM145
#67% liv
#75% 

