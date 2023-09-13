## VERSION 1 ##

# I'm Editing this shit

ufc_fights <- read.csv("R/ufc_fights.csv")
ufc_fighters <- read.csv('R/ufc_fighters.csv')
ufc_stats <- read.csv('R/ufc_fight_stats.csv')
head(ufc_fighters)
head(ufc_fights)
head(ufc_stats)

my.string <- '5:06'
strsplit(my.string, ':')

time_parts <- strsplit(ufc_stats$ctrl_time, ":")
ufc_stats$ctrl_time[ufc_stats$ctrl_time == "NULL"] <- "0:00"

# Convert minutes and seconds to numeric and calculate control time in seconds
sapply(time_parts, my.function)
ufc_stats |>
  mutate(ctrl_time = as.numeric(strsplit(ctrl_split, ':')))


#### PREPARING DATA ####

head(ufc_stats)
first.join <- ufc_fights |>
  full_join(ufc_stats, by = 'fight_id')

head(ufc_fighters)

second.join <- first.join |>
  full_join(ufc_fighters, by = 'fighter_id')
second.join <- second.join[, -c(16,43)]
head(second.join, 20)
str(second.join)
as.numeric(second.join[,18])
second.join[,c(18:28, 33:35, 38:41)] <- sapply(second.join[,c(18:28, 33:35, 38:41)], as.numeric)
second.join <- separate(second.join, col = finish_time, into =c('finish_minute', 'finish_second'), sep = ':')
second.join$finish_time <- 60*as.numeric(second.join$finish_minute) + as.numeric(second.join$finish_second)
second.join <- second.join[,-c(15,16)]
str(second.join)
second.join <- separate(second.join, col = ctrl_time, into =c('ctrl_minute', 'ctrl_second'), sep = ':')
second.join$ctrl_time <- 60*as.numeric(second.join$ctrl_minute) + as.numeric(second.join$ctrl_second)
second.join <- second.join[, -c(26,27)]
second.join[,c('weight_class', 'gender', 'result', 'finish_round', 'winner.y', 'fighter_stance')] <- sapply(second.join[,c('weight_class', 'gender', 'result', 'finish_round', 'winner.y', 'fighter_stance')], factor)

second.join$title_fight <- factor(second.join$title_fight)
second.join$weight_class <- factor(second.join$weight_class)
second.join$gender <- factor(second.join$gender)
second.join$result <- factor(second.join$result)
second.join$finish_round <- factor(second.join$finish_round)
second.join$winner.y <- factor(second.join$winner.y)
second.join$fighter_stance <- factor(second.join$fighter_stance)
second.join$weight_class_rank <- factor(second.join$weight_class_rank)

full.name <- function(x,y){
  second.join$fighter.fullname <- paste0(x, ' ', y)
} # Getting full names for fighters instead of ID
sapply(second.join$fighter_f_name[120], full.name, y = second.join$fighter_l_name[120])
for(i in 1:nrow(second.join)){
  second.join$fighter.fullname[i] <- paste0(second.join$fighter_f_name[i], ' ', second.join$fighter_l_name[i])
}
second.join <- second.join[, c(1,2,15,16,42, 30:34,26, 36:39,3:5, 8:11, 17:25, 41, 7, 27, 6 , 12:14, 40)]
vector <- c() 
for( i in 1:nrow(second.join)){
  if(i%%2 != 0){
    vector[i+1] <- second.join$fighter.fullname[i]
  }else{
    vector[i-1] <- second.join$fighter.fullname[i]
  }
} # creating an opponent name vector
vector <- vector[-15841]
second.join$opponent <- vector
vector <- c()
for( i in 1:nrow(second.join)){
  if(i%%2 != 0){
    vector[i+1] <- second.join$fighter_id[i]
  }else{
    vector[i-1] <- second.join$fighter_id[i]
  }
} # creating opponent id vector
vector <- vector[-15841]
second.join$opponent_id <- vector
dim(second.join)

head(second.join)
colnames(second.join) <- c('FIGHTID', 'EVENTID', 'FIGHTERFIGHTID', 'FIGHTERID', 'FULLNAME', 'NICKNAME', 'HEIGHT', 'WEIGHT', 'REACH', 'STANCE', 'AGE', 'W', 'L', 'D', 'NC.DQ', 'REF', 'f_1', 'f_2', 'TITLEFIGHT', 'WEIGHTCLASS', 'RANK', 'GENDER', 'KNOCKDOWNS', 'ATT.STRIKES', 'SUCC.STRIKES', 'ATT.SIGSTRIKEs', 'SUCC.SIGSTRIKES', 'TD.ATT', 'TD.SUCC', 'SUB.ATT', 'REVERSALS', 'CONTROLTIME', 'NUMROUNDS', 'WIN?', 'WINNERID', 'RESULT', 'DETAILS', 'FINISHROUND', 'FINISHTIME', 'OPPONENT', 'OPPONENTID')
second.join <- second.join[, c(2,1,3:15,40:41,16,19:39)] #Renaming and Structuring the data how I want it
data <- second.join
data <- data[!is.na(data$FIGHTID),] #removing rows without a fight ID, these are probably just fighters on the roster with no fights
head(data)
missmap(data, col = c('yellow', 'black')) # We see main issues are Control Time, NC/DQ, RANK, REACh. Only one which is weird is the control time, so have to revisit this, mustve made a mistake
ufc_stats <- read.csv('R/ufc_fight_stats.csv')
ufc_stats$ctrl_time[ufc_stats$ctrl_time == 'NULL'] <- '0:00'
ufc_stats <- separate(ufc_stats, col = ctrl_time, into =c('ctrl_minute', 'ctrl_second'), sep = ':')
CTRLTIME <- 60*as.numeric(ufc_stats$ctrl_minute) + as.numeric(ufc_stats$ctrl_second)
data$NC.DQ[is.na(data$NC.DQ)] <- 0 #Done
data$CONTROLTIME <- CTRLTIME # Sorted, now lets sort the NC/DQ
data$CONTROLTIME[1:396] <- NA # first fights did not record control time
data$NICKNAME[data$NICKNAME == 'NULL'] <- ''
data$STANCE[is.na(data$STANCE)] <- ''
dim(data[is.na(data$WINNERID),]) # 100 fights have no winner, lets remove those
data <- data[!is.na(data$WINNERID),]
dim(data[is.na(data$STANCE),]) # 42 rows without fight stats, a lot without reach and rank. Unfortunately not much I can do about RANK, but will engineer some reach data
data <- data[!is.na(data$HEIGHT),] # REMOVE rows with neither weight nor height

data$`WIN?`<- as.character(data$`WIN?`)
data$`WIN?`<-as.factor(data$`WIN?`)
data$WEIGHTCLASS[data$WEIGHTCLASS == 'NULL'] <- NA
data$WEIGHTCLASS <- as.character(data$WEIGHTCLASS)
data$WEIGHTCLASS[is.na(data$WEIGHTCLASS)] <- 'Unknown'
data$WEIGHTCLASS <- as.factor(data$WEIGHTCLASS)
data$STANCE[data$STANCE == 'NULL'] <- NA
data$STANCE <- as.character(data$STANCE)
data$STANCE[is.na(data$STANCE)] <- 'Unknown'
data$STANCE <- as.factor(data$STANCE) # Made a mess but trynna get it right, replacing NAs with Unknown for these less important variables
subset(data, RANK == '')
#RANK is actually the Weight class RANK as compared to other weightclasses
data$RANK <- as.numeric(data$RANK)
str(data)
for(i in 1:nrow(data)){
  if(data$WEIGHTCLASS[i] == 'Light Heavyweight'){
    data$RANK[i] <- 11
  }else{
  }
}
data$RANK <- factor(data$RANK)
unique(data$WEIGHTCLASS)
data <- data[!is.na(data$ATT.STRIKES),] # Removing fights with stats
colSums(data == 'NULL')
data[150:160,]
data <- data[, -c(33)] # Remving number of rounds for fight, kind of useless
#All that's left that I want to do is to sort out the reach issue
# To do this, I will take the average reach of fighters who are within 5cm in height to the fighter
summary(data$HEIGHT)
for(i in 1:nrow(data)){
  if(is.na(data$REACH[i])){
    fighter.height <- data$HEIGHT[i]
    fighters <- subset(data, HEIGHT >= (fighter.height-5) & HEIGHT <= (fighter.height +5))
    mean.reach <- mean(fighters$REACH, na.rm = TRUE)
    data$REACH[i] <- round(mean.reach,2)
  }else{
  }
}
head(data)
head(data)
str(data)
data$RANK <- as.numeric(data$RANK)
data$RANK[is.na(data$RANK)] <- 'Unknown'
data$RANK <- factor(data$RANK)
#Realised I messed up on the finish time thing
data$FINISHROUND <- as.numeric(data$FINISHROUND)
str(data)
data$FINISHTIME <- data$FINISHTIME + 300*(data$FINISHROUND - 1)
data$FINISHTIME
subset(data, FINISHTIME > 1000)
df

# I will now create a new dataframe where I will get their percentages and stuff, to see how well we can forecast
basic.data <- data
df <- data
str(df)
dim(df)
# for the new data, I will use recent data mostly, will remove any NA rows (although barely exist anyway)
missmap(df, col = c('yellow', 'black')) # Beautiful, will keep NAs for now
fighter.adv.stats <- data.frame(FIGHTERID = 1:nrow(ufc_fighters))
adv.stat <- function(a,b,c,d){
  for(i in 1:nrow(ufc_fighters)){
    fighter.data <- subset(df, FIGHTERID == i)
    if(nrow(fighter.data)>0){
      fighter.data[,a] <- fighter.data[,b]/fighter.data[,c]
      fighter.adv.stats[i,d] <- mean(fighter.data[,a], na.rm = TRUE)
      if(is.nan(fighter.adv.stats[i,d])){
        fighter.adv.stats[i,d] <- 0
      }else{
      }
    }else {
    }
  }
  return(fighter.adv.stats)
} # THESE ARE FOR AVERAGES
fighter.adv.stats <- adv.stat('STRIKEACC', 'SUCC.STRIKES', 'ATT.STRIKES', 'STRIKEPERC')
fighter.adv.stats <- adv.stat('SIGSTRIKEACC', 'SUCC.SIGSTRIKES', 'ATT.SIGSTRIKEs', 'SIGSTRIKEPERC')
fighter.adv.stats <- adv.stat('TDACC', 'TD.SUCC', 'TD.ATT', 'TDPERC')
head(fighter.adv.stats,20)
sum(is.nan(fighter.adv.stats$TDPERC))
adv.stat.avg <- function(a,b){
  for(i in 1:nrow(ufc_fighters)){
    fighter.data <- subset(df, FIGHTERID == i)
    if(nrow(fighter.data) > 0){
      fighter.adv.stats[i,a] <- mean(fighter.data[,b], na.rm = TRUE)
      if(is.nan(fighter.adv.stats[i,a])){
        fighter.adv.stats[i,a] <- NA
      }else{
      }
    }else{
    }
  }
  return(fighter.adv.stats)
}
fighter.adv.stats <- adv.stat.avg('STRIKEAVG', 'SUCC.STRIKES')
fighter.adv.stats <- adv.stat.avg('SIGSTRIKEAVG', 'SUCC.SIGSTRIKES')
fighter.adv.stats <- adv.stat.avg('KNOCKDOWNAVG' ,'KNOCKDOWNS')
fighter.adv.stats <- adv.stat.avg('TDAVG', 'TD.SUCC')
fighter.adv.stats <- adv.stat.avg('SUBAVG', 'SUB.ATT')
fighter.adv.stats <- adv.stat.avg('REVERSALAVG', 'REVERSALS')
fighter.adv.stats <- adv.stat.avg('CONTROLAVG', 'CONTROLTIME')
# I now have my adv stats created
active.fighters <- unique(df$FIGHTERID)
active.fighter.adv.stats <- fighter.adv.stats[fighter.adv.stats$FIGHTERID%in%active.fighters,]
head(active.fighter.adv.stats) # Now we have adv stats for active fighters
opp.fighter.adv.stats <- active.fighter.adv.stats
for(i in 1:ncol(active.fighter.adv.stats)){
  colnames(opp.fighter.adv.stats)[i] <- paste0('OPP.', colnames(active.fighter.adv.stats)[i])
}
missmap(active.fighter.adv.stats, col = c('yellow', 'black')) # Looking good, the Control NA values probably correspond to fighters in first fights where control time was not recorded
#Created new column names for the opponent data, this is coming together
#Lets try to join stuff up
dim(df)
df <- df |>
  full_join(active.fighter.adv.stats, by = 'FIGHTERID')
df <- df |>
  full_join(opp.fighter.adv.stats, by = c('OPPONENTID' = 'OPP.FIGHTERID'))
colnames(df)
missmap(df, col = c('yellow', 'black'))
df <- df |>
  mutate(WINNER = case_when(
    WINNERID == FIGHTERID ~ FULLNAME,
    TRUE ~ OPPONENT
  ))
df <- df[,c(1:22, 33, 59, 34:38, 23:32, 39:58)]
df <- df |>
  rename(VICTORY = `WIN?`)
# We have some NA values, but 467/13943 (3.3%) rows would get removed, it is sensible to get rid of it, most is from early fights anyway so data wpouldnt be representative of curret trends
# I will still save current dataframe just in case
df.current <- df
df <- df.current
df <- na.omit(df)
dim(df)
head(df)
#Check everything looks good
subset(df, FULLNAME == 'Amanda Nunes')
# There is just one issue, division rank is wrong for some reason, but tbh its kind of useless, so will remove
df <- df[, -21]
str(df)
is.ordered(df$WEIGHTCLASS) # I think my factors are all unordered, which is good
# I think my data is ready to be used
subset(df, FULLNAME == 'Conor McGregor') # Everything Lines up I think
data.dummy <- df
df$WEIGHTCLASS <- as.character(df$WEIGHTCLASS)
df$WEIGHTCLASS <- factor(df$WEIGHTCLASS, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 'Heavyweight', 'Catch Weight', 'Open Weight'))
unique(df$WEIGHTCLASS)

# Thinking baout it agin, it should be interesting to have differential stats, so looking at the differences in control, strike, sigstrike avgs etc
df <- df |>
  mutate(STRIKEDIFF = STRIKEAVG - OPP.STRIKEAVG, SIGSTRIKEDIFF = SIGSTRIKEAVG - OPP.SIGSTRIKEAVG, TDDIFF = TDAVG - OPP.TDAVG, SUBDIFF= SUBAVG - OPP.SUBAVG, CONTROLDIFF = CONTROLAVG - OPP.CONTROLAVG, WINPERC = (W)/(W+D+L+NC.DQ))
ufc_fighters$fighter_nc_dq[ufc_fighters$fighter_nc_dq == 'NULL'] <- 0
ufc_fighters$FIGHTERID <- ufc_fighters$fighter_id
str(ufc_fighters)
ufc_fighters <- ufc_fighters |>
  mutate(WINPERC = fighter_w/(fighter_l +fighter_d + as.numeric(fighter_nc_dq) + fighter_w))
mini.df <- ufc_fighters[, c('FIGHTERID', 'WINPERC')]
df <- df |>
  full_join(mini.df, by = c('OPPONENTID' = 'FIGHTERID'))
df <- df |>
  rename(WINPERC = WINPERC.x, OPP.WINPERC = WINPERC.y)


head(df)
#### DATA VISUALISATION ####

df |>
  distinct(FIGHTID, .keep_all = TRUE)|>
  filter(RESULT != 'DQ')|>
  ggplot() + geom_bar(aes(x = TITLEFIGHT, fill = RESULT), position = 'fill') + facet_wrap(~TITLEFIGHT, scales= 'free')+ scale_fill_manual(values = terrain.colors(4), guide = guide_legend('Result')) + theme_bw() + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5)) + ggtitle('Result Type by Weight Category') + xlab('Weight Class') + ylab('Count')
#This Graph shows us that Title fights actually see more finishes
df |>
  distinct(FIGHTID, .keep_all = TRUE)|>
  filter(RESULT != 'DQ')|>
  ggplot() + geom_bar(aes(x = WEIGHTCLASS, fill = RESULT), position = 'dodge') + scale_fill_manual(values = terrain.colors(4), guide = guide_legend('Result')) + theme_bw() + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5)) + ggtitle('Result Type by Weight Category') + xlab('Weight Class') + ylab('Count')
#The Heavier the division, the more KO/TKOs you get

df |>
  group_by(WEIGHTCLASS)|>
  mutate(MEAN.REACH = mean(REACH))|>
  ungroup()|>
  distinct(FIGHTERID, .keep_all = TRUE)|>
  filter(WEIGHTCLASS != 'Open Weight')|>
  ggplot(aes(y = WEIGHTCLASS, x = REACH)) + geom_jitter(col = 'black')  + stat_summary(aes(col = WEIGHTCLASS),geom = 'point', size = 6, fun = mean) + geom_vline(xintercept = mean(df$REACH)) + geom_segment(aes(x = MEAN.REACH, xend = mean(REACH), y = WEIGHTCLASS, yend = WEIGHTCLASS, col = WEIGHTCLASS), size = 1) + theme_bw() + scale_color_manual(values = terrain.colors(13),guide = guide_legend('Weight Class')) + theme(legend.position = 'top')
# Just a nice aesthetic
df|>
  distinct(FIGHTERID, .keep_all = TRUE)|>
  ggplot(aes(x = SIGSTRIKEAVG, y = CONTROLAVG, col = WEIGHTCLASS)) + geom_point()


dim(distinct(df, FIGHTID))


df |>
  distinct(FIGHTID, .keep_all = TRUE)|>
  ggplot() + geom_bar(aes(x = VICTORY)) # We could use this dataframe for tests later in order to avoid repeat readings
df |>
  distinct(FIGHTID, .keep_all = TRUE)|>
  ggplot(aes(x = CONTROLDIFF, y = STRIKEDIFF, col = VICTORY)) + geom_point(alpha=0.2) + geom_density_2d()
missmap(df, col = c('yellow', 'black'))
dim(df)
df <- na.omit(df)
df |>
  distinct(FIGHTERID, .keep_all = TRUE)|>
  ggplot(aes(x = REACH, y= WEIGHTCLASS, fill = ..x..)) + geom_density_ridges_gradient()
df |>
  distinct(FIGHTID, .keep_all = TRUE)|>
  ggplot(aes(x = SUCC.SIGSTRIKES, col = VICTORY)) + geom_point(aes(y = CONTROLTIME)) + geom_density(aes(x = SUCC.SIGSTRIKES))
df |>
  filter(EVENTID == 618) |>
  distinct(FIGHTID, .keep_all = T)|>
  ggplot(aes(x = WEIGHTCLASS, fill = WEIGHTCLASS)) + geom_bar()
pl1 <- df |>
  filter(EVENTID == 618)|>
  ggplot(aes(x = SUCC.SIGSTRIKES , y = CONTROLTIME, col = RESULT)) + geom_point(aes(alpha = VICTORY), size = 6, show.legend = FALSE) + geom_point(aes(col = GENDER), show.legend = FALSE) + scale_color_manual(values = c('salmon', 'grey50', 'turquoise', 'forestgreen', 'black')) + scale_alpha_manual('Gender' ,values = c(0.5, 1))
pl1 + geom_text(aes(label = FULLNAME), col = 'black', check_overlap = TRUE)
df |>
  filter(EVENTID == 618)|>
  ggplot(aes(x = STRIKEAVG, y = WINPERC, size = W)) + geom_text(aes(label = FULLNAME, col = VICTORY)) + scale_size(trans = 'reverse')
df |>
  group_by(EVENTID)|>
  summarize(SIGSTRIKES = mean(SIGSTRIKEAVG))|>
  ggplot(aes(x = EVENTID, y = SIGSTRIKES)) + geom_smooth() + geom_line() # SIG STRIKES HAVE INCREASED

#### MACHINE LEARNING MODELS ####

# I feel like a RANDOM FOREST model will woprk best, maybe KNN although there may be some issues due to the number of dimensions.
# I want to focus on the predictipons based on baseline stats before fights, nopt after fight stats
# So I must select the global stats
head(df)
ML.DF <- df[, c(2,5,7:11,16,19:22, 39:ncol(df))]
head(ML.DF)
#Now we need to only use 1 row of data per fight
ML.DF <- ML.DF |>
  distinct(FIGHTID, .keep_all = T)
dim(ML.DF) #We still have 6770 fights to analyse
# Data into training and test data
set.seed(101)
split <- sample.split(ML.DF$VICTORY, SplitRatio = 0.7)
training <- subset(ML.DF, split == TRUE)
test <- subset(ML.DF, split == FALSE)
str(test)

# TREES and RANDOM FOREST #
tree <- tree(data = training, VICTORY~ . - FULLNAME - OPPONENT - FIGHTID)
head(training)
tree.rp <- rpart(data = training, VICTORY ~ .-FULLNAME - OPPONENT - FIGHTID)
plot(tree.rp)
text(tree.rp)
prp(tree.rp)
plot(tree)
text(tree) # This one is model
summary(tree.rp)
tree.pred <- predict(tree, test, type = 'class')
tree.rp.pred <- predict(tree.rp, test, type = 'class') # Idk why its annoying with the fullname stuff
table(tree.pred, test$VICTORY) # 66.0% accuracy, depends on the run
(1462)/2031
table(tree.rp.pred, test$VICTORY) # 68.8% accuracy

forest <- randomForest(data = training, VICTORY ~ .- FULLNAME - OPPONENT - FIGHTID, importance = TRUE, method = 'class')
forest$importance
forest.pred <- predict(forest, test, method = 'class')
table(forest.pred, test$VICTORY) # here we have a 72.7% accuracy

# KNN #
# First we have to scale the data
head(ML.DF)
scaled.df <- scale(ML.DF[,-c(1,2,6, 8:12)])
head(scaled.df)
scaled.df <- cbind(scaled.df, ML.DF[,c(1,2,6,8:12)])
set.seed(101)
split <- sample.split(scaled.df$VICTORY, 0.7)
training <- subset(scaled.df, split == TRUE)
training.results <- training$VICTORY
head(training)
training <- training[,-c(32,33,35,39)]
training <- training[, -c(32:35)]
test <- subset(scaled.df, split == FALSE)
test <- na.omit(test)
test.results <- test$VICTORY
test <- test[, -c(32,33,35,39)]
test<- test[, -c(32:35)] # Removing factors
missmap(training, col = c('yellow', 'black'))
test.pred <- knn(training, test, training.results, k = 200)
table(Prediction = test.pred, Actual = test.results)
(1230+183)/2031 # 69.6% accuracy
#Let's use what the random forest technique finds to be the most important factors
training <- training[, c('SIGSTRIKEAVG', 'STRIKEDIFF', 'SIGSTRIKEDIFF', 'WINPERC', 'OPP.WINPERC')]
test <- test[, c('SIGSTRIKEAVG', 'STRIKEDIFF', 'SIGSTRIKEDIFF', 'WINPERC', 'OPP.WINPERC')]
test.pred <- knn(training, test, training.results, k = 1000)
table(Prediction = test.pred, Actual = test.results)
error.rate <- c()
for(i in seq(min(1), max(500), by = 20)){
  test.pred <- knn(training, test, training.results, k = i)
  error.rate[i] <- mean(test.pred != test.results)
}
error.rate <- error.rate[!is.na(error.rate)]
error <- data.frame(error.rate, k = seq(min(1), max(500), by = 20))
ggplot(error, aes(x = k, y  = error.rate)) + geom_line()
1- min(error.rate) # 71.3% accuracy
which.min(error.rate)
# minimum error rate around k = 200, lets investigate further.
error.rate <- c()
for(i in seq(min(175), max(225), by = 5)){
  test.pred <- knn(training, test, training.results, k = i)
  error.rate[i] <- mean(test.pred != test.results)
}
error.rate <- error.rate[!is.na(error.rate)]
error <- data.frame(error.rate, k = seq(min(175), max(225), by = 5))
ggplot(error, aes(x = k, y  = error.rate)) + geom_line()
1- min(error.rate) # min at k = 200, accuracy = 71.4%

# Support Vector Machines #
set.seed(101)
split <- sample.split(ML.DF, 0.7)
training <- subset(ML.DF, split == TRUE)
test <- subset(ML.DF, split == FALSE)
test <- na.omit(test)
test.results <- test$VICTORY
length(test.results)
str(test)
svm.df <- svm(data = training, VICTORY ~. - FULLNAME - OPPONENT - FIGHTID, kernel = 'radial')
summary(svm.df) # uses 2959 support vector, out of roughly 4000 observations in training set. This means it should be quite robust
svm.pred <- predict(svm.df, test[,-12])
table(svm.pred, test.results)
mean(svm.pred == test.results) #73.5% accuracy, this seems promising, lets try polynomial kernel
svm.df <- svm(data = training, VICTORY ~. - FULLNAME - OPPONENT - FIGHTID, kernel = 'polynomial')
summary(svm.df) # uses 2935 support vector, out of roughly 4000 observations in training set. This means it should be quite robust
svm.pred <- predict(svm.df, test[,-12])
table(svm.pred, test.results)
mean(svm.pred == test.results) # 70.2% accuracy, also seems promising, lets optimise these
set.seed(101)
tuned.svm <- tune(svm, train.x = VICTORY ~ . - FULLNAME - OPPONENT - FIGHTID, data = training, kernel = 'radial', ranges = list(cost = c(0.01,0.1,1, 10, 100), gamma = c(0.1,0.5,1,2)))
tuned.svm$best.parameters # gives us best parameters to use
svm.df <- svm(data = training, VICTORY ~ . - FULLNAME - OPPONENT - FIGHTID, kernel = 'radial', cost = 1, gamma = 0.1)
svm.pred <- predict(svm.df, test[,-12])
table(svm.pred, test$VICTORY)
mean(svm.pred == test$VICTORY) # 70.5% accuracy, so somehow performed worse than when not tuned lol

# Neural Net #
library(neuralnet)
ML.DF <- na.omit(ML.DF)
scaled.df <- scale(ML.DF[,-c(1,2,6,8,9,10,11,12)])
scaled.df <- data.frame(scaled.df,ML.DF[, c(1,2,6,8:12)])
head(scaled.df)
scaled.df <- scaled.df[,-c(29:35)]
set.seed(101)
split <- sample.split(scaled.df$VICTORY, 0.7)
training <- subset(scaled.df, split == TRUE)
test <- subset(scaled.df, split == FALSE)
names <- names(training)
f <- as.formula(paste("VICTORY ~", paste(names[!names %in% "VICTORY"], collapse = " + ")))
nn <- neuralnet(data = training, f, hidden = c(12, 15, 20), threshold = 0.01, linear.output = FALSE)
nn.pred <- compute(nn, test[, 1:28])
# Need to work on this because its bugging out #

names(df)
names(ML.DF)
head(df)
str(df)
subset(df, FULLNAME == "Sean O'Malley")


fighter.1 <- df|>
  filter(FULLNAME == "Sean O'Malley")|>
  distinct(FIGHTERID, .keep_all = TRUE)|>
  select(5,7:11,20:21, 39:48, 64)
fighter.2 <- df |>
  filter(OPPONENT == 'Aljamain Sterling')|>
  distinct(OPP.WINPERC, .keep_all = TRUE)|>
  select(16,49:58,65)

fighter.comp <- data.frame(fighter.1, fighter.2, TITLEFIGHT = TRUE)
fighter.comp$TITLEFIGHT <- factor(fighter.comp$TITLEFIGHT, levels = c(FALSE, TRUE))
fighter.comp <- fighter.comp |>
  mutate(STRIKEDIFF = STRIKEAVG - OPP.STRIKEAVG, SIGSTRIKEDIFF = SIGSTRIKEAVG - OPP.SIGSTRIKEAVG, TDDIFF = TDAVG - OPP.TDAVG, SUBDIFF = SUBAVG - OPP.SUBAVG, CONTROLDIFF = CONTROLAVG - OPP.CONTROLAVG, FIGHTID = 1)
str(fighter.comp)
fight.outcome <- predict(forest, fighter.comp)
