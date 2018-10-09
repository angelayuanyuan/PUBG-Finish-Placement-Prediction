library(data.table)
library(ggplot2)
library(dplyr)

###########################################
############## Read Data ##################
###########################################

# train.raw <- fread("../all/train.csv")
# test.raw <- fread("../all/test.csv")

# train <- saveRDS(train.raw,file = "train.rds")
# test <- saveRDS(test.raw,file = "test.rds")
train.raw <- readRDS("train.rds") 
test.raw <- readRDS("test.rds")

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

# independant variables
# walk distance
ggplot(train.raw)+
  geom_histogram(aes(walkDistance),fill="#66CC99")
quantile(train.raw$walkDistance) # outliers

# ride distance
ggplot(train.raw)+
  geom_histogram(aes(rideDistance),fill="#66CC99")
quantile(train.raw$rideDistance) # outliers

ggplot(train.raw[train.raw$rideDistance<5000,])+
  geom_histogram(aes(rideDistance),fill="#66CC99")

sum(train.raw$rideDistance==0) # 3439985 records have zero ride distance (never used a car in the match)

# swim distance
ggplot(train.raw)+
  geom_histogram(aes(swimDistance),fill="#66CC99")
quantile(train.raw$swimDistance)

sum(train.raw$swimDistance==0) # 4076544 records have zero swim distance (am I the only one who like to swim??)

# weapons aquired
ggplot(train.raw)+
  geom_histogram(aes(weaponsAcquired),fill="#CC6666")
quantile(train.raw$weaponsAcquired)

ggplot(train.raw[train.raw$weaponsAcquired<20,])+
  geom_histogram(aes(weaponsAcquired),fill="#CC6666")

median(train.raw$weaponsAcquired) # 3 weapons

# knocked downs
ggplot(train.raw)+
  geom_histogram(aes(DBNOs),fill="#CC6666")
quantile(train.raw$DBNOs,0.99999) # more than 30 knock downs is really unlikely to happen

ggplot(train.raw[train.raw$DBNOs<20,])+
  geom_histogram(aes(DBNOs),fill="#CC6666") # most players have zero knock downs

# kills
ggplot(train.raw)+
  geom_histogram(aes(kills),fill="#CC6666")
quantile(train.raw$kills,0.99999) # more than 30 kills is really unlikely to happen too

ggplot(train.raw[train.raw$kills<20,])+
  geom_histogram(aes(kills),fill="#CC6666") # most players have zero kills

# longest kills
ggplot(train.raw)+
  geom_histogram(aes(longestKill),fill="#CC6666")
quantile(train.raw$longestKill,0.99999)

ggplot(train.raw[train.raw$longestKill<300,])+
  geom_histogram(aes(longestKill),fill="#CC6666")

# head shots kill
ggplot(train.raw)+
  geom_histogram(aes(headshotKills),fill="#CC6666")
quantile(train.raw$headshotKills,0.99999)

ggplot(train.raw[train.raw$headshotKills<10,])+
  geom_histogram(aes(headshotKills),fill="#CC6666")
###########################################
############## Sample EDA #################
###########################################
set.seed(1234)
tr.sample <- sample_n(train.raw, 50000)

# kills versus rank
ggplot(tr.sample)+
  geom_jitter(aes(winPlacePerc,kills,alpha=0.2))
  
# assists versus rank
ggplot(tr.sample)+
  geom_jitter(aes(winPlacePerc,assists,alpha=0.2))

# head shots versus rank
ggplot(tr.sample)+
  geom_jitter(aes(winPlacePerc,headshotKills,alpha=0.2))

# weapons versus rank
ggplot(tr.sample)+
  geom_jitter(aes(winPlacePerc,weaponsAcquired,alpha=0.2))
