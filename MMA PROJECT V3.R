# FINAL VERSION #

ufc_fighters <- read.csv('~/UFC Project/ufc_fighters.csv')
ufc_fights <- read.csv('~/UFC Project/ufc_fights.csv')
ufc_stats <- read.csv("~/UFC Project/ufc_fight_stats.csv")

#### DATA CLEANING/ENGINEERING ####

ufc_fighters <- ufc_fighters |>
  mutate(fighter_fullname = paste(fighter_f_name, fighter_l_name), fighter_f_name = NULL, fighter_l_name = NULL, fighter_dob = NULL)

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

# Creating my own relevant Stats
my_stats_df <- data.frame(fighter_id = 1:nrow(ufc_fighters))
df <- df|>
  mutate(across(c(knockdowns:ctrl_time), as.numeric))
adv.stat <- function(a,b,c,d){
  for(i in 1:nrow(ufc_fighters)){
    fighter.data <- subset(df, fighter_id == i)
    for(j in 1:nrow(fighter.data)){
      if(nrow(fighter.data)>0){
        fighter.data[j,a] <- fighter.data[j,b]/fighter.data[j,c]
        my_stats_df[i,d] <- mean(fighter.data[,a])
        if(is.nan(my_stats_df[i,d])){
          my_stats_df[i,d] <- 0
        }else{
        }
      }else {
      }
    }
  }
  return(my_stats_df)
}
my_stats_df <- adv.stat('acc_strike', 'total_strikes_succ', 'total_strikes_att', 'strike_perc')
my_stats_df <- adv.stat('acc_sigstrike', 'sig_strikes_succ', 'sig_strikes_att', 'sigstrike_perc')
my_stats_df <- adv.stat('acc_td', 'takedown_succ', 'takedown_att', 'takedown_perc')


adv.stat.2 <- function(a,b){
  for(i in 1:nrow(ufc_fighters)){
    fighter.data <- subset(df, fighter_id == i)
    if(nrow(fighter.data) > 0){
      my_stats_df[i,a] <- mean(fighter.data[,b], na.rm = TRUE)
      if(is.nan(my_stats_df[i,a])){
        my_stats_df[i,a] <- NA
      }else{
      }
    }else{
    }
  }
  return(my_stats_df)
}
my_stats_df <- adv.stat.2('strike_avg', 'total_strikes_succ')
my_stats_df <- adv.stat.2('sig_strike_avg', 'sig_strikes_succ')
my_stats_df <- adv.stat.2('knockdown_avg', 'knockdowns')
my_stats_df <- adv.stat.2('takedown_avg', 'takedown_succ')
my_stats_df <- adv.stat.2('sub_avg', 'submission_att')
my_stats_df <- adv.stat.2('reversal_avg', 'reversals')
my_stats_df <- adv.stat.2('control_avg', 'ctrl_time')
my_stats_df <- adv.stat.2('finish_avg', 'finish_time')#I've engineered my own stats which I think will be relevant

my_stats_df <- my_stats_df |>
  mutate(strike_per_minute = strike_avg/(finish_avg/60), sig_strike_per_minute = sig_strike_avg/(finish_avg/60))
my_stats_df <- my_stats_df |>
  mutate(win_perc = (ufc_fighters$fighter_w)/(rowSums(ufc_fighters[,7:9])))
opp_stats_df <- my_stats_df
for(i in 2:ncol(opp_stats_df)){
  colnames(opp_stats_df)[i] <- paste0('opp_', colnames(my_stats_df[i]))
} # made one for opposition to make it easier to merge later

df <- df |>
  full_join(my_stats_df, by = 'fighter_id')|>
  full_join(opp_stats_df, by = c('opponent_id' = 'fighter_id')) # Merging my data, now we have opponent stats as well

df <- df %>%
  mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
         spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
         knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
         sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg) # Adding differential stats

df <- df |>
  select(-c('referee', 'f_1', 'f_2', 'winner.x', 'num_rounds', 'weight_class_rank', 'result_details', 'finish_round',
            'fighter_fight_id', 'fighter_nickname', 'fighter_d', 'fighter_l', 'fighter_nc_dq'))|>
  select(event_id, fight_id, fighter_id, fighter_fullname, fighter_age, fighter_height_cm:fighter_stance, 
         title_fight, weight_class, gender, winner.y, result: finish_time, total_strikes_att:ctrl_time, strike_perc:win_perc,
         fighter_w, opponent_id, opponent_fullname, opp_strike_perc:control_diff) # Reordering my dataframe

df <- df |>
  mutate(across(fighter_age:fighter_reach_cm, as.numeric))
df <- na.omit(df)
df$fighter_stance <- ifelse(df$fighter_stance == 'NULL', 'Unknown', df$fighter_stance)
df$weight_class <- ifelse(df$weight_class == 'NULL' | is.na(df$weight_class), 'Unknown', df$weight_class)
df <- df |>
  mutate(weight_class = factor(weight_class, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                        "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                        'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                        'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown')))|>
  mutate(across(c('fighter_stance', 'title_fight', 'gender', 'winner.y', 'result'), as.factor))

full_df <- data.frame()
for(name in unique(df$fighter_fullname)){
  fighter_df <- df |>
    filter(fighter_fullname == name)
  for(i in 1:nrow(fighter_df)){
    fighter_df$fighter_w[i] <- sum(fighter_df$winner.y[0:(i-1)] == 'T')
  }
  full_df <- rbind(full_df, fighter_df)
}
df <- full_df # Actual fighter_w
head(full_df)

full_df <- data.frame()
for(name in unique(df$fighter_fullname)){
  vector <- c()
  fighter_df <- df |>
    filter(fighter_fullname == name)
  for(i in 1:nrow(fighter_df)){
    if(i <4){
      newlast5 <- sum(fighter_df[0:(i), 'winner.y'] == 'T') - sum(fighter_df[0:(i), 'winner.y'] == 'F')
    }else{
      newlast5 <- sum(fighter_df[(i-4):(i), 'winner.y'] == 'T') - sum(fighter_df[(i-4):(i), 'winner.y'] == 'F')
    }
    vector[i] <- newlast5
  }
  fighter_df$last5 <- append(vector[-length(vector)], 0, 0)
  fighter_df$newlast5 <- vector
  full_df <- rbind(full_df, fighter_df)
} # last 5 fight form (before and after fight)
df <- full_df

full_df <- data.frame()
for(name in unique(df$fighter_fullname)){
  streak <- 0
  full_streak <- c()
  fighter_df <- df|>
    filter(fighter_fullname == name)
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
  fighter_df$streak <- append(full_streak[-length(full_streak)],0,0)
  fighter_df$newstreak <- full_streak
  full_df <- rbind(full_df, fighter_df)
}
df <- full_df # fight streak befpre and after the fight (will also be useful later down the line)

full_df <- df
df <- full_df
set.seed(5550)
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
  select(-event_id, -(total_strikes_att:ctrl_time), -finish_time, -newlast5, -newstreak)
df

names(df)
head(df)
dim(df)

#### MACHINE LEARNING ####

df <- df|>
  select(-c(fight_id:fighter_fullname, result, opponent_id, opponent_fullname)) # Final selection of variables
df_save <- df # Making a copy, useful later

set.seed(123)
split <- sample.split(df$winner.y, SplitRatio = 0.7)
training <- subset(df, split == TRUE)
training <- training
test <- subset(df, split == FALSE)
test <- test # Splitting data into train and testing subsets

###
### TREE METHODS ###
###

set.seed(101)
forest <- randomForest(data = training, winner.y ~.  + I(fighter_w)^2 + I(control_diff)^2 - fighter_stance - fighter_age -
                         fighter_height_cm - finish_avg, importance = TRUE, method = 'class')
forest$importance
forest.pred <- predict(forest, test, method = 'class')
table(forest.pred, test$winner.y)
confidence.forest <- mean(forest.pred == test$winner.y)
precision.forest <- table(forest.pred, test$winner.y)[2,2]/(rowSums(table(forest.pred, test$winner.y))[2])
confidence.forest
precision.forest

###
### SVM ###
###

set.seed(123)
mysvm <- svm(data = training, winner.y ~. + I(fighter_w)^2 + I(control_diff)^2 - fighter_stance - fighter_age -
               fighter_height_cm, kernel = 'radial')
svm.pred <- predict(mysvm, test)
table(svm.pred, test$winner.y)
confidence.svm <- mean(svm.pred == test$winner.y)
precision.svm <-  table(svm.pred, test$winner.y)[2,2]/(rowSums(table(svm.pred, test$winner.y))[2])
confidence.svm
precision.svm

###
### LOGISTIC REGRESSION ###
###

set.seed(123)
logmodel <- glm(data = training, winner.y ~. + I(fighter_w)^2 + I(control_diff)^2, family = binomial(link = logit))
logmodel <- step(logmodel)
summary(logmodel)

confidence.log <- c()
for(i in seq(min(0.4), max(0.6), by = 0.01)){
  log.pred <- predict(logmodel, test, type = 'response')
  log.pred <- ifelse(log.pred > i, 'T', 'F')
  confidence.log <- append(confidence.log, mean(log.pred == test$winner.y))
}
plot(seq(min(0.4), max(0.6), by = 0.01),confidence.log)
lines(seq(min(0.4), max(0.6), by = 0.01),confidence.log) # max accuracy at i = 0.52

log.pred <- predict(logmodel, test, type = 'response')
log.pred <- ifelse(log.pred > 0.52, 'T', 'F')
confidence.log <- mean(log.pred == test$winner.y)
precision.log <- table(log.pred, test$winner.y)[2,2]/(rowSums(table(log.pred, test$winner.y))[2])

###
### NEURAL NETS ###
###

install.packages('neuralnet')
library(neuralnet)

scaled.df <- df|>
  select(-c(fighter_stance:gender, winner.y))|> # removing factors
  scale()|>
  data.frame()|>
  mutate(winner.y = df$winner.y)
str(scaled.df)

set.seed(123)
split <- sample.split(scaled.df$winner.y, SplitRatio = 0.7)
scaled.training <- subset(scaled.df, split == TRUE)
scaled.test <- subset(scaled.df, split == FALSE)
str(scaled.training)

n <- names(scaled.training)
f <- as.formula(paste("winner.y ~", paste(n[!n %in% "winner.y"], collapse = " + ")))
nn <- neuralnet(data = scaled.training, f, linear.output = FALSE)
nn$result.matrix
nn.pred <- predict(nn, scaled.test[,-ncol(scaled.test)])
nn.pred <- ifelse(nn.pred[,2] >0.5, 'T', 'F')
table(nn.pred, scaled.test$winner.y)
confidence.nn <- mean(nn.pred == scaled.test$winner.y)
precision.nn <- table(nn.pred, test$winner.y)[2,2]/(rowSums(table(nn.pred, test$winner.y))[2])
confidence.nn
precision.nn

library(caret) # optimization (Doesn't work, have to have a closer look)
params.grid <- expand.grid(layer1 = c(40,32,24), layer2 = c(16,12,8))
ctrl <- trainControl(method = 'cv', number = 5)
confidence.nn.vector <- c()
for(i in 2:9){
  params <- params.grid[i]
  nn <- neuralnet(data = scaled.training, f, linear.output = FALSE, hidden = c(params$layer1, params$layer2))
  nn.pred <- compute(nn, scaled.test[,-ncol(scaled.test)])
  nn.pred <- ifelse(nn.pred$net.result[,2] >0.5, 'T', 'F')
  confidence.nn.vector <- append(confidence.nn.vector, mean(nn.pred == scaled.test$winner.y))
}
confidence.nn.vector # This hasn't worked

#### CONCLUSION ####

my.function <- function(a, b, title, gender, category, model){
  fighter_names <- unique(full_df$fighter_fullname)
  get_fighter1_data <- function(name) {
    fighter_data <- full_df %>%
      filter(fighter_fullname == name) %>%
      slice_max(fight_id)
    return(fighter_data)
  }
  get_fighter2_data <- function(name){
    fighter_data <- full_df %>%
      filter(opponent_fullname == name) %>%
      slice_max(fight_id)
    return(fighter_data)
  }
  
  if(a%in%fighter_names & b%in%fighter_names){
    get_prediction <- function(a,b){
      fighter1 <- get_fighter1_data(a)|>
        select(fighter_age:fighter_stance, strike_perc:fighter_w, newstreak, newlast5)|>
        rename(last5 = newlast5, streak = newstreak)
      fighter2 <- get_fighter2_data(b)|>
        select(opp_strike_perc:opp_win_perc)
      
      df <- cbind(fighter1, fighter2)
      df <- df %>%
        mutate(strike_diff = strike_avg - opp_strike_avg, sig_strike_diff = sig_strike_avg - opp_sig_strike_avg,
               spm_diff = strike_per_minute - opp_strike_per_minute, sspm_diff = sig_strike_per_minute - opp_sig_strike_per_minute,
               knockdown_diff = knockdown_avg - opp_knockdown_avg, takedown_diff = takedown_avg - opp_takedown_avg,
               sub_diff = sub_avg - opp_sub_avg, control_diff = control_avg - opp_control_avg)
      df$title_fight <- factor(title, levels = c('FALSE', 'TRUE'))
      df$gender <- factor(gender, levels = c('F', 'M'))
      df$weight_class <- factor(category, levels = c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight", 
                                                     "Women's Featherweight", "Flyweight", 'Bantamweight', 'Featherweight', 
                                                     'Lightweight', 'Welterweight', 'Middleweight', 'Light Heavyweight', 
                                                     'Heavyweight', 'Catch Weight', 'Open Weight', 'Unknown'))
      
      if(identical(model, nn)){
        df <- df_save|>
          select(-winner.y)|>
          rbind(df)|>
          select(-c(fighter_stance:gender))|> # removing factors
          scale()|>
          data.frame()|>
          tail(1)
        prediction <- predict(nn, df)
        prediction <- ifelse(prediction[,2] >0.5, 'T', 'F')
      }else{
        prediction <- predict(model, df)
        if(identical(model, logmodel)){
          prediction <- ifelse(prediction>0.52, 'T', 'F')
        }
      }
      return(prediction)
    }
    
    prediction <- get_prediction(a,b)
    prediction.bis <- get_prediction(b,a)
    
    models <- list(
      mysvm = c(confidence.svm, precision.svm, 'Support Vector Machine Model'),
      forest = c(confidence.forest, precision.forest, 'Random Forest Model'),
      logmodel =c(confidence.log, precision.log, 'Logistic Regression Model'),
      nn = c(confidence.nn, precision.nn, 'Neural Network Model')
    )
    model <- deparse(substitute(model))
    confidence <- as.numeric(models[[model]][1])
    precision <- as.numeric(models[[model]][2])
    model <- models[[model]][3]
    
    if(prediction == prediction.bis){
      if(prediction == 'T'){ # 2 is the factor level of TRUE
        print("Sorry, I can't accurately predict this fight, I feel like both should win ;)")
      }else{
        print("Sorry, I can't accurately predict this fight, I feel like both will lose :(")
      }
    }else if(prediction == 'T'){
      paste0('I predict ', a,  ' wins this fight. This has been predicted using a ', model, ' with ', round(confidence*100), '% confidence and ', round(precision*100), '% precision')
    }else{
      paste0('I predict ', b,  ' wins this fight. This has been predicted using a ', model, ' with ', round(confidence*100), '% confidence and ', round(precision*100), '% precision')
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
my.function("Dan Argueta", "Miles Johns", 'FALSE', 'M', "Bantamweight", nn)

extra.function <- function(fighter1, fighter2, title, gender, weight) {
  results <- list()
  results[[1]] <- my.function(fighter1, fighter2, title, gender, weight, mysvm)
  results[[2]] <- my.function(fighter1, fighter2, title, gender, weight, logmodel)
  results[[3]] <- my.function(fighter1, fighter2, title, gender, weight, forest)
  results[[4]] <- my.function(fighter1, fighter2, title, gender, weight, nn)
  return(results)
}

# Here you enter your two fighters, followed by TRUE/FALSE for titlefight
# M or F for gender, the weightclass (please capitalise each word and use correct spelling) and finally the model you want to use
# Options are mysvm (Support Vector Machine), forest (Random Forest), logmodel (Logistic Regression)
# And nn (Neural Network)

