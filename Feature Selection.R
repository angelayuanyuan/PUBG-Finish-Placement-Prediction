library(h2o)
library(dplyr)
library(dummies)
library(devtools)
library(ggbiplot)

###########################################
################# PCA #####################
###########################################

# exclude ids ("Id","groupId","matchId")

train.pca <- train.raw%>%
  select(-Id,-groupId,-matchId)

train.pca <- train.pca%>%
  select(-winPlacePerc)
tr.pca <- prcomp(train.pca,
                 center = TRUE,
                 scale. = TRUE) 
print(tr.pca)

# scree plot
plot(tr.pca, type = "l")

tr.pca$rotation[1:5,1:5]

