library(data.table)
library(ggplot2)
library(dplyr)

###########################################
############## Read Data ##################
###########################################

train.raw <- fread("../all/train.csv")
test.raw <- fread("../all/test.csv")

# compare train and test sets
setdiff(names(train.raw), names(test.raw)) 

# variables' distinct values
var_value.tr <- data.frame(apply(train.raw,2, n_distinct),row.names = colnames(train.raw))
colnames(var_value.tr) <- "count"
var_value.te <- data.frame(apply(test.raw,2, n_distinct),row.names = colnames(test.raw))
colnames(var_value.te) <- "count"


###########################################
################## EDA ####################
###########################################

# 4357336 records, 1888732 teams, 47734 matches

train.raw <- train.raw%>%
  group_by(matchId)%>%
  arrange(groupId)

# number of groups in matches with records
tr.match <- train.raw%>%
  summarise(n=n()) # makes sense to have minimum 1 group, maximum 100 groups on record

# group-level stats
tr.players <- train.raw%>%
  select(groupId,Id)%>%
  group_by(groupId)%>%
  summarise(n=n())

tr.group <- train.raw%>%
  filter(groupId=="1193309")

quantile(tr.players$n) # having more than 4 players in a team is weird (maximum 8 players is possible but nothing above)

# does this happen in test set
te.players <- test.raw%>%
  select(groupId,Id)%>%
  group_by(groupId)%>%
  summarise(n=n())

quantile(te.players$n) # yes, then we'll keep these data for now

# response variable
ggplot(train.raw)+
  geom_histogram(aes(winPlacePerc),fill="#9999CC") 


