library(h2o)
library(dplyr)
library(dummies)
library(devtools)
library(ggbiplot)
library(factoextra)
library(corrplot)

# split training set
train.raw <- readRDS("train.rds") 

set.seed(1234)

s=sample(1:nrow(train.raw),3000000)
train <- train.raw[s,]
test <- train.raw[-s,]

train.pca <- train%>%
  select(-Id,-groupId,-matchId)

train.pca <- train.pca%>%
  select(-winPlacePerc)

###########################################
############## PCA (PCR) ##################
###########################################

# exclude ids ("Id","groupId","matchId")

tr.pca <- prcomp(train.pca,
                 center = TRUE,
                 scale. = TRUE) 
print(tr.pca)

# eigenvalues
eig.val <- get_eigenvalue(tr.pca)

# scree plot
fviz_eig(tr.pca, addlabels = TRUE, ylim = c(0, 50)) # first five components explained approximately 80% variance

# variable importance
var <- get_pca_var(tr.pca)

fviz_pca_var(tr.pca, alpha.var="contrib")

corrplot(var$contrib, is.corr=FALSE)  

# Contributions of variables to PC1
fviz_contrib(tr.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(tr.pca, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC3
fviz_contrib(tr.pca, choice = "var", axes = 3, top = 10)

# Contributions of variables to PC4
fviz_contrib(tr.pca, choice = "var", axes = 4, top = 10)

# Contributions of variables to PC5
fviz_contrib(tr.pca, choice = "var", axes = 5, top = 10)

# correlation by contribution
fviz_pca_var(tr.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

