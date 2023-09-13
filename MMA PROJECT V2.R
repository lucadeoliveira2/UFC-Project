# Version 2 #

ufc_fights <- read.csv("R/ufc_fights.csv")
ufc_fighters <- read.csv('R/ufc_fighters.csv')
ufc_stats <- read.csv('R/ufc_fight_stats.csv')

##### DATA CLEANING/ENGINEERING ####

ufc_fighters <- ufc_fighters |>
  mutate(fighter_fullname = paste(fighter_f_name, fighter_l_name), fighter_f_name = NULL, fighter_l_name = NULL, fighter_dob = NULL)|>
  mutate(win_perc = (ufc_fighters$fighter_w)/(rowSums(ufc_fighters[,10:12])))

df <- ufc_fights |>
  full_join(ufc_stats, by = 'fight_id') |>
  full_join(ufc_fighters, by = 'fighter_id')|>
  select(!'fight_url' & !'fighter_url') # Removing URLs

df$ctrl_time[df$ctrl_time == 'NULL'] <- '0:00'
df <- df %>%
  mutate(
    finish_split = strsplit(finish_time, ':', fixed = TRUE),
    ctrl_split = strsplit(ctrl_time, ':', fixed = TRUE),
    finish_time = 60 * as.numeric(sapply(finish_split, `[`, 1)) +
      as.numeric(sapply(finish_split, `[`, 2)) + 300*finish_round,
    ctrl_time = 60 * as.numeric(sapply(ctrl_split, `[`, 1)) +
      as.numeric(sapply(ctrl_split, `[`, 2))
  ) %>%
  select(-finish_split, -ctrl_split) # getting control and finish times

df$opponent_id <- NA
df$opponent_fullname <- NA
na_or_null <- is.na(df$f_1) | is.null(df$f_1)
df$opponent_id[!na_or_null] <- ifelse(df$f_1[!na_or_null] == df$fighter_id[!na_or_null], df$f_2[!na_or_null], df$f_1[!na_or_null])
df$opponent_fullname <- ufc_fighters$fighter_fullname[match(df$opponent_id, ufc_fighters$fighter_id)]
# Potentially remove this after, but creating opponent ID and fullnames

head(df)
df <- df |>
  mutate(across(18:27, as.numeric))
full_df <- data.frame()
adv.stat <- function(a,b,c){
  for(name in unique(df$fighter_fullname)){
    fighter_df <- subset(df, fighter_fullname == name)
    for(j in 1:nrow(fighter_df)){
      if(j == 1){
        fighter_df[j, a] <- sum(fighter_df[,b])/sum(fighter_df[,c])
      }else if(j <5 & j>1){
        fighter_df[j,a] <- sum(fighter_df[0:(j-1), b])/sum(fighter_df[0:(j-1),c])
      }else{
        fighter_df[j,a] <- sum(fighter_df[(j-5):(j-1), b])/sum(fighter_df[(j-5):(j-1),c])
      }
    }
    full_df <- rbind(full_df, fighter_df)
  }
  return(full_df)
}
df <- adv.stat('strike_perc', 'total_strikes_succ', 'total_strikes_att')
df$strike_perc[is.nan(df$strike_perc)] <- 0
df <- adv.stat('sigstrike_perc', 'sig_strikes_succ', 'sig_strikes_att')
df$sigstrike_perc[is.nan(df$sigstrike_perc)] <- 0
df <- adv.stat('takedown_perc', 'takedown_succ', 'takedown_att')
df$takedown_perc[is.nan(df$takedown_perc)] <- 0

full_df <- data.frame()
adv.stat.2 <- function(a,b){
  for(name in unique(df$fighter_fullname)){
    fighter_df <- subset(df, fighter_fullname == name)
    for(j in 1:nrow(fighter_df)){
      if(j == 1){
        fighter_df[j, a] <- mean(fighter_df[,b])
      }else if(j <5 & j>1){
        fighter_df[j,a] <- mean(fighter_df[0:(j-1), b])
      }else{
        fighter_df[j,a] <- mean(fighter_df[(j-5):(j-1), b])
      }
    }
    full_df <- rbind(full_df, fighter_df)
  }
  return(full_df)
}
df <- adv.stat.2('strike_avg', 'total_strikes_succ')
df <- adv.stat.2('strike_avg', 'total_strikes_succ')
df <- adv.stat.2('sig_strike_avg', 'sig_strikes_succ')
df <- adv.stat.2('knockdown_avg', 'knockdowns')
df <- adv.stat.2('takedown_avg', 'takedown_succ')
df <- adv.stat.2('sub_avg', 'submission_att')
df <- adv.stat.2('reversal_avg', 'reversals')
df <- adv.stat.2('control_avg', 'ctrl_time')
df <- adv.stat.2('finish_avg', 'finish_time') #I've engineered my own stats which I think will be relevant

df.save <- df

df <- df |>
  mutate(strike_per_minute = strike_avg/(finish_avg/60), sig_strike_per_minute = sig_strike_avg/(finish_avg/60))
my_stats_df <- df |>
  select(fighter_fight_id, strike_perc:sig_strike_per_minute, win_perc)|>
  arrange(fighter_fight_id)
opp_stats_df <- my_stats_df
for(i in 2:ncol(opp_stats_df)){
  colnames(opp_stats_df)[i] <- paste0('opp_', colnames(my_stats_df[i]))
}
for(i in 1:nrow(opp_stats_df)){
  if(i%%2 == 0){
    opp_stats_df$fighter_fight_id[i] <- i-1
  }else{
    opp_stats_df$fighter_fight_id[i] <- i+1
  }
}

df <- df |>
  select(-c(win_perc, strike_perc:sig_strike_per_minute))
df <- na.omit(df)
df <- df|>
  arrange(fighter_fight_id)
df <- df |>
  full_join(my_stats_df, by = 'fighter_fight_id')|>
  full_join(opp_stats_df, by = 'fighter_fight_id')

df <- df %>%
  mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
         spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
         knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
         sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg)

df <- df |>
  select(-c('referee', 'f_1', 'f_2', 'winner.x', 'num_rounds', 'weight_class_rank', 'result_details', 'finish_round',
            'fighter_fight_id', 'fighter_nickname', 'fighter_d', 'fighter_l', 'fighter_nc_dq'))|>
  select(event_id, fight_id, fighter_id, fighter_fullname, fighter_age, fighter_height_cm:fighter_stance, 
         title_fight, weight_class, gender, winner.y, result: finish_time, total_strikes_att:ctrl_time, strike_perc:win_perc,
         fighter_w, opponent_id, opponent_fullname, opp_strike_perc:control_diff) # Reordering my dataframe

df <- df |>
  mutate(across(fighter_age:fighter_reach_cm, as.numeric))
df <- na.omit(df)# 11319 x 63 dimension
df$fighter_stance <- ifelse(df$fighter_stance == 'NULL', 'Unknown', df$fighter_stance)
df$weight_class <- ifelse(df$weight_class == 'NULL' | is.na(df$weight_class), 'Unknown', df$weight_class)
df <- df |>
  mutate(weight_class = factor(weight_class, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                        "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                        'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                        'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown')))|>
  mutate(across(c('fighter_stance', 'title_fight', 'gender', 'winner.y', 'result'), as.factor))
df.save.2 <- df
df <- df.save.2

full_df <- data.frame()
names(df)
for(name in unique(df$fighter_fullname)){
  fighter_df <- df |>
    filter(fighter_fullname == name)
  for(i in 1:nrow(fighter_df)){
    fighter_df$fighter_w[i] <- sum(fighter_df$winner.y[0:(i-1)] == 'T')
  }
  full_df <- rbind(full_df, fighter_df)
}
df <- full_df # Actual fighter_w

full_df <- data.frame()
for(name in unique(df$fighter_fullname)){
  fighter_df <- df |>
    filter(fighter_fullname == name)
  for(i in 1:nrow(fighter_df)){
    if(i <5){
      last5 <- sum(fighter_df[0:(i-1), 'winner.y'] == 'T') - sum(fighter_df[0:(i-1), 'winner.y'] == 'F')
    }else{
      last5 <- sum(fighter_df[(i-5):(i-1), 'winner.y'] == 'T') - sum(fighter_df[(i-5):(i-1), 'winner.y'] == 'F')
    }
    fighter_df$last5[i] <- last5
  }
  full_df <- rbind(full_df, fighter_df)
}
df <- full_df # Last 5 fights form

full_df <- data.frame()
for(name in unique(df$fighter_fullname)){
  fighter_name <- name
  streak <- 0
  full_streak <- c()
  fighter_df <- df|>
    filter(fighter_fullname == fighter_name)
  for(j in 1:nrow(fighter_df)){
    if(streak == 0){
      if(fighter_df$winner.y[j] == 'T'){
        streak <- 1
      }else{
        streak <- -1
      }
    }else if(streak<0){
      if(fighter_df$winner.y[j] == 'T'){
        streak <- 1
      }else{
        streak <- streak - 1
      }
    }else{
      if(fighter_df$winner.y[j] == 'T'){
        streak <- streak + 1
      }else{
        streak <- -1
      }
    }
    full_streak <- append(full_streak, streak)
  }
  full_streak <- append(full_streak, 0, after = 0)
  full_streak <- full_streak[-length(full_streak)]
  fighter_df$streak <- full_streak
  full_df <- rbind(full_df, fighter_df)
}
df <- full_df # Current streak

full_df <- df
set.seed(1000)
duplicated_fights <- duplicated(df$fight_id) | duplicated(df$fight_id, fromLast = T)
df.unique.fights <- df[!duplicated_fights,] # Here we have included all the fights which are not duplicated
df.duplicated.fights <- df[duplicated_fights,]
sample <- sample(unique(df.duplicated.fights$fight_id), ((length(unique(df$fight_id))/2)-sum(df.unique.fights$winner.y == 'T')), replace = FALSE)
df.duplicated.winners <- df.duplicated.fights |>
  filter(winner.y == 'T')|>
  subset(fight_id%in%sample)
df.duplicated.losers <- df.duplicated.fights |>
  filter(winner.y == 'F')|>
  subset(!fight_id%in%sample)
df <- rbind(df.unique.fights,df.duplicated.winners, df.duplicated.losers)|>
  arrange(fight_id)
sum(df$winner.y == 'T') # I finally have an equal spread of wins and losses.

df <- df|>
  select(-event_id, -(total_strikes_att:ctrl_time), -finish_time)

names(df)
head(df)
dim(df)
##### DATA VISUALIZATION ####

df |>
  group_by(event_id, winner.y)|>
  summarise(mean.sub.att = mean(submission_att, na.rm = TRUE))|>
  ggplot(aes(x = event_id, y = mean.sub.att, col = winner.y)) + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + scale_color_manual(values = c('salmon', 'white'))

df |>
  ggplot(aes(x = fighter_stance, fill = gender)) + geom_bar(col = 'black', position = 'dodge') + scale_fill_manual(values = c('salmon', 'turquoise'),guide = guide_legend('Gender', title.position = 'bottom',label.position = 'top')) + theme(legend.position = 'top')

mean.age <- df|>
  distinct(fighter_id, .keep_all = TRUE)|>
  mutate(mean.age = mean(fighter_age))|>
  distinct(mean.age)|>
  as.numeric()
df|>
  group_by(weight_class)|>
  mutate(mean_age = mean(fighter_age))|>
  ungroup()|>
  mutate(overall.mean.age = mean(fighter_age))|>
  filter(weight_class != 'Open Weight' & weight_class != 'Unknown')|>
  ggplot(aes(x = fighter_age, y = weight_class)) + geom_jitter(aes(col = weight_class), alpha = 0.5) + 
  geom_vline(xintercept = mean(df$fighter_age),lty = 2, lwd = 1) + 
  geom_segment(aes(x = mean_age, xend = mean(fighter_age), y = weight_class, yend = weight_class)) +
  stat_summary(geom = 'point', size = 6, fun = mean, col = 'black')

df|>
  filter(fighter_stance != 'Unknown' & fighter_stance != 'Open Stance')|>
  ggplot(aes(x = strike_perc, y = fighter_stance, fill = ..x..)) + geom_density_ridges_gradient(alpha = 0.5) + scale_fill_viridis_c()

##### MACHINE LEARNING ####



set.seed(123)
split <- sample.split(df$winner.y, SplitRatio = 0.7)
training <- subset(df, split == TRUE)
training <- training |>
  select(-c(fight_id:fighter_fullname, result, opponent_id, opponent_fullname))
test <- subset(df, split == FALSE)
test <- test |>
  select(-c(fight_id:fighter_fullname, result, opponent_id, opponent_fullname))

names(training)

###
### TREE METHODS ###
###

tree <- tree(data = training, formula = winner.y ~.)
summary(tree)
tree.rp <- rpart(data = training, formula = winner.y ~.)
plot(tree)
text(tree)
prp(tree.rp)
tree.pred <- predict(tree.rp, test, type = 'class')
table(tree.pred, test$winner.y)
mean(tree.pred == test$winner.y) # 63.2% accuracy

set.seed(101)
forest <- randomForest(data = training, winner.y ~.  + I(fighter_w)^2 + I(control_diff)^2 - fighter_stance - fighter_age -
                         fighter_height_cm - finish_avg, importance = TRUE, method = 'class')
forest$importance
forest.pred <- predict(forest, test, method = 'class')
table(forest.pred, test$winner.y)
mean(forest.pred == test$winner.y) # 69.6% accuracy, 70.1% precision


###
### SVM ###
###

names(training)
set.seed(123)
mysvm <- svm(data = training, winner.y ~. + I(fighter_w)^2 + I(control_diff)^2 - fighter_stance - fighter_age -
               fighter_height_cm - finish_avg, kernel = 'radial')
svm.pred <- predict(mysvm, test)
table(svm.pred, test$winner.y)
confidence <- mean(svm.pred == test$winner.y)*100
precision <-  table(svm.pred, test$winner.y)[2,2]/(rowSums(table(svm.pred, test$winner.y))[2])*100 #73.6% accuracy, 74.8% precision
confidence
precision


# Tuning it, helped by GPT
param_grid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100),
                          gamma = c(0.001, 0.01, 0.1, 1, 10))
best_accuracy <- 0
best_svm <- NULL
for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  svm_model <- svm(winner.y ~ .,data = training, kernel = 'radial',
                   cost = params$C, gamma = params$gamma)
  # Make predictions on the validation set
  svm.pred <- predict(svm_model, newdata = test)
  # Calculate accuracy
  accuracy <- mean(svm.pred == test$winner.y)
  # Check if this model is the best one so far
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_svm <- svm_model
  }
}
# Print the best SVM model and its accuracy
print(best_svm)
print(best_svm$gamma) # cost = 1, gamma = 0.01
print(paste("Best accuracy:", best_accuracy)) # 72.2% accuracy, 72.5% precision

# Trying with polynomial kernel
mypolysvm <- svm(data = training, winner.y ~., kernel = 'polynomial')
polysvm.pred <- predict(mypolysvm, test)
table(polysvm.pred, test$winner.y)
mean(polysvm.pred == test$winner.y) # 70.7% accuracy, with 73.2 % precision, interesting

# Tuning it
param_grid <- expand.grid(C = c(0.01,0.1,1,10,100), degree = c(0.001,0.01,0.1,1,10))
best_polysvm <- NA
best_accuracy <- 0
for(i in 1:nrow(param_grid)){
  params <- param_grid[i,]
  polysvm <- svm(data = training, winner.y ~., kernel = 'polynomial',
                 cost = params$C, degree = params$degree)
  polysvm.pred <- predict(polysvm, newdata = test)
  accuracy <- mean(polysvm.pred == test$winner.y)
  precision <- table(polysvm.pred, test$winner.y)[2,2]/(rowSums(table(polysvm.pred, test$winner.y))[2])
  if(accuracy > best_accuracy){
    best_polysvm <- polysvm
    best_accuracy <- accuracy
    best_polysvm_precision <- precision
  }
}
print(best_polysvm)
print(paste0('Best accuracy: ', round(best_accuracy*100,1), '% , with precision: ', round(best_polysvm_precision*100,1), '%'))
plot(df[, c('sig_strike_diff', 'opp_win_perc')], col = ifelse(df$winner.y == 'T', 'blue', 'red'))


###
### KNN ###
###

names(df)
scaled.df <- df|>
  select(-c(fight_id:fighter_fullname, result, opponent_id, opponent_fullname))|>
  select(-c(fighter_stance:gender, winner.y))|> # removing factors
  scale()|>
  data.frame()|>
  mutate(winner.y = df$winner.y)
head(scaled.df)
set.seed(101)
split <- sample.split(scaled.df$winner.y, 0.7)
training <- subset(scaled.df, split == TRUE)
training_winner <- training$winner.y
training <- training|>
  select(-winner.y)
test <- subset(scaled.df, split == FALSE)
test_winner <- test$winner.y
test <- test |>
  select(-winner.y)
test_pred <- knn(training, test, training_winner, k = 450)
table(test_pred, test_winner)
mean(test_pred == test_winner)
accuracy <- c()
for(i in seq(min(1), max(451), by = 10)){
  test_pred <- knn(training, test, training_winner, k = i)
  correct.rate <- mean(test_pred == test_winner)
  accuracy <- append(accuracy, correct.rate)
}
plot(seq(min(1), max(451), by =10), accuracy)
max(accuracy) # 70.1% accuracy at k = 101
accuracy <- c()
for(i in seq(min(90), max(110), by = 1)){
  test_pred <- knn(training, test, training_winner, k = i)
  correct.rate <- mean(test_pred == test_winner)
  accuracy <- append(accuracy, correct.rate)
}
plot(seq(min(90), max(110), by =1), accuracy)
lines(seq(min(90), max(110), by =1), accuracy) # max is still at k = 101


### CONCLUSIONS ###

# Most accurate model is the Support Vector Machine, using a radial kernel
# Will use this as my model.

predict.fight <- function(a,b, title, gender, category){
  fighter_names <- unique(full_df$fighter_fullname)
  if(a%in%fighter_names & b%in%fighter_names){
    fighter1 <- full_df |>
      filter(fighter_fullname == a)|>
      slice_max(fight_id)|>
      select(fighter_age:fighter_stance, strike_perc:fighter_w)
    fighter2 <- full_df |>
      filter(opponent_fullname == b)|>
      slice_max(fight_id)|>
      select(opp_strike_perc:opp_win_perc)
    fighter1.bis <- full_df |>
      filter(fighter_fullname == b)|>
      slice_max(fight_id)|>
      select(fighter_age:fighter_stance, strike_perc:fighter_w)
    fighter2.bis <- full_df |>
      filter(opponent_fullname == a)|>
      slice_max(fight_id)|>
      select(opp_strike_perc:opp_win_perc)
    
    df.svm <- cbind(fighter1, fighter2)
    df.svm <- df.svm %>%
      mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
             spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
             knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
             sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg)
    df.svm$title_fight <- factor(title, levels = c('F', 'T'))
    df.svm$gender <- factor(gender, levels = c('F', 'M'))
    df.svm$weight_class <- factor(category, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                       "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                       'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                       'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown'))
    prediction <- predict(mysvm, df.svm)
    
    df.svm.bis <- cbind(fighter1.bis, fighter2.bis)
    df.svm.bis <- df.svm.bis %>%
      mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
             spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
             knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
             sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg)
    df.svm.bis$title_fight <- factor(title, levels = c('FALSE', 'TRUE'))
    df.svm.bis$gender <- factor(gender, levels = c('F', 'M'))
    df.svm.bis$weight_class <- factor(category, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                           "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                           'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                           'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown'))
    prediction.bis <- predict(mysvm, df.svm.bis)
    print(df.svm)
    print(df.svm.bis)
    if(prediction == prediction.bis){
      if(prediction == 'T'){
        print("Sorry, I can't accurately predict this fight, I feel like both should win ;)")
      }else{
        print("Sorry, I can't accurately predict this fight, I feel like both will lose :(")
      }
    }else if(prediction == 'T'){
      paste0('I predict ', a,  ' wins this fight with ', round(confidence, 1), '% confidence and ', round(precision, 1), '% precision')
    }else{
      paste0('I predict ', b,  ' wins this fight with ', round(confidence, 1), '% confidence and ', round(precision, 1), '% precision')
    }
  }else{
    if(!a%in%fighter_names & !b%in%fighter_names){
      cat('Sorry, I do not have data on either fighters, did you spell them correctly?')
    }else if(a%in%fighter_names){
      cat('Sorry I do not have data on', b, 'unfortunately, have you spelt it correctly?')
    }else{
      cat('Sorry, I do not have data on', a, 'unfortunately, have you spelt it correctly?')
    }
  }
}
predict.fight("Ciryl Gane", "Serghei Spivac", 'FALSE', 'M', "Heavyweight")

predict.fight.forest <- function(a,b, title, gender, category){
  fighter_names <- unique(full_df$fighter_fullname)
  if(a%in%fighter_names & b%in%fighter_names){
    fighter1 <- full_df |>
      filter(fighter_fullname == a)|>
      slice_max(fight_id)|>
      select(fighter_age:fighter_stance, strike_perc:fighter_w, last5, streak)
    fighter2 <- full_df |>
      filter(opponent_fullname == b)|>
      slice_max(fight_id)|>
      select(opp_strike_perc:opp_win_perc)
    fighter1.bis <- full_df |>
      filter(fighter_fullname == b)|>
      slice_max(fight_id)|>
      select(fighter_age:fighter_stance, strike_perc:fighter_w, last5, streak)
    fighter2.bis <- full_df |>
      filter(opponent_fullname == a)|>
      slice_max(fight_id)|>
      select(opp_strike_perc:opp_win_perc)
    
    df.svm <- cbind(fighter1, fighter2)
    df.svm <- df.svm %>%
      mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
             spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
             knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
             sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg)
    df.svm$title_fight <- factor(title, levels = c('FALSE', 'TRUE'))
    df.svm$gender <- factor(gender, levels = c('F', 'M'))
    df.svm$weight_class <- factor(category, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                       "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                       'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                       'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown'))
    prediction <- predict(forest, df.svm, method = 'class')
    
    df.svm.bis <- cbind(fighter1.bis, fighter2.bis)
    df.svm.bis <- df.svm.bis %>%
      mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
             spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
             knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
             sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg)
    df.svm.bis$title_fight <- factor(title, levels = c('FALSE', 'TRUE'))
    df.svm.bis$gender <- factor(gender, levels = c('F', 'M'))
    df.svm.bis$weight_class <- factor(category, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                           "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                           'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                           'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown'))
    prediction.bis <- predict(forest, df.svm.bis, method = 'class')
    print(df.svm)
    print(df.svm.bis)
    if(prediction == prediction.bis){
      if(prediction == 'T'){
        print("Sorry, I can't accurately predict this fight, I feel like both should win ;)")
      }else{
        print("Sorry, I can't accurately predict this fight, I feel like both will lose :(")
      }
    }else if(prediction == 'T'){
      paste0('I predict ', a,  ' wins this fight with ', round(confidence, 1), '% confidence and ', round(precision, 1), '% precision')
    }else{
      paste0('I predict ', b,  ' wins this fight with ', round(confidence, 1), '% confidence and ', round(precision, 1), '% precision')
    }
  }else{
    if(!a%in%fighter_names & !b%in%fighter_names){
      cat('Sorry, I do not have data on either fighters, did you spell them correctly?')
    }else if(a%in%fighter_names){
      cat('Sorry I do not have data on', b, 'unfortunately, have you spelt it correctly?')
    }else{
      cat('Sorry, I do not have data on', a, 'unfortunately, have you spelt it correctly?')
    }
  }
}
predict.fight.forest("Beneil Dariush", "Charles Oliveira", 'FALSE', 'M', "Lightweight")


