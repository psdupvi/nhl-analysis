if(!require(lattice)) install.packages("lattice")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(readxl)) install.packages("readxl")
if(!require(ggalt)) install.packages("ggalt")



raw_data_file <- "https://raw.github.com/psdupvi/nhl-analysis/master/raw-data.csv"
raw_data <- read_csv(raw_data_file)
raw_data <- raw_data[rowSums(is.na(raw_data)) != ncol(raw_data),]
## Download the data from my github repository
raw_data$playoff <- as.factor(raw_data$playoff)
## Turn the raw data into a factor for categorical predictions

raw_data_short <- raw_data %>% filter(gp < 82) %>% mutate(pts = pts*82/gp,w = w*82/gp, l = l*82/gp, 
                                                          ga = ga*82/gp,gf = gf*82/gp, sow = sow*82/gp,
                                                          sol = sol*82/gp, evgf = evgf*82/gp, evga = evga*82/gp,
                                                          pp = pp*82/gp, ppo = ppo*82/gp, ppoa = ppoa*82/gp,
                                                          ppa = ppa*82/gp,sh = sh*82/gp, sha = sha*82/gp,
                                                          shots = shots*82/gp, shots_against = shots_against*82/gp, gp = 82)  
## Normalize data for any years with fewer than 82 games
raw_data_not_short <- raw_data %>% filter(gp == 82)
## Select 
nhl_data <- rbind(raw_data_not_short,raw_data_short)

test_data_playoffs <- filter(nhl_data, year == 2017) %>% filter(team != "League Average") 
train_data_playoffs <- filter(nhl_data, year != 2017) %>% filter(team != "League Average")
rm(raw_data_not_short,raw_data,raw_data_short,raw_data_file)
## Eliminate the league average data and remove unnecessary files


train_data_playoffs %>% filter(playoff == "Y") %>%
  group_by(rank) %>% 
  summarize(mean = mean(pts), minimum = min(pts), maximum = max(pts))
## Table showing the points typically required to get each rank in the playoffs

train_data_playoffs %>% filter(playoff == "N") %>% 
  group_by(year) %>% 
  summarize(missed = max(pts))
## Table showing the maximum points each year that missed the playoffs

train_data_playoffs %>%  
  ggplot(aes(x = rank, y = w)) + 
  geom_boxplot(aes(group = rank))
## Visualization showing the number of points required for each rank

train_data_playoffs %>% filter(playoff == "Y") %>% 
  ggplot(aes(x = year, y = pts)) + 
  geom_point()
## Number of points required the make the playoffs each year

missed_pts <- train_data_playoffs %>% 
  filter(playoff == "N") %>% 
  group_by(year) %>% 
  summarize(missed = max(pts))
## Determine max missed playoffs points each year

missed_pts_avg <- mean(missed_pts$missed)
## Average missed points

train_data_playoffs %>% ggplot(aes(x = year, y = pts)) + 
  geom_point(aes(color = playoff)) + 
  geom_hline(yintercept = mean(missed_pts_avg))
## Plot showing teams that made the playoffs vs. teams that didn't by color, 
## with points and the average missed points as a horizontal line


train_data_playoffs %>% filter(team != "League Average") %>% 
  group_by(team) %>% summarize(avg = mean(pts), 
                               playoffs = sum(playoff == "Y"), 
                               finals = sum(champ == "Y" | runner == "Y"),
                               cups = sum(champ == "Y")) %>% arrange(desc(avg))
## See each team's success

train_data_playoffs %>% filter(team != "League Average") %>% 
  ggplot(aes(x = team, y = pts)) + 
  geom_boxplot() + 
  geom_point(aes(color = playoff)) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
## Plot each team's points over all the year


train_data_playoffs %>% filter(pts >= missed_pts_avg & playoff == "N") %>% select(team, year, rank)
## Show all the teams that missed the playoffs while doing better than the average


train_set <- train_data_playoffs %>% select(-year,-champ,-runner, -team,-rank,-pts_perc,-gp,-year,-pk_perc,-pp_perc,-w,-l,-sol,-sow)
## Eliminate columns that either don't matter (team name), 
## are results not causes (champ), 
## or are linearly dependent on others (pts_perc)

fit <- train(playoff ~ ., data = train_set, method = "rf")
pred <- predict(fit,newdata = test_data_playoffs)
round(mean(pred == test_data_playoffs$playoff),2)
## Test the random forest method

fit <- train(playoff ~ ., data = train_set, method = "rf")
pred <- predict(fit,newdata = test_data_playoffs, type = "prob")
head(pred)
## Do it again but this time return probabilities, not predictions
pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"
## Turn probabilities into playoff predictions, with top half in playoffs and bottom half out
acc <- mean(pred$Y == test_data_playoffs$playoff)
## Find accuracy

models <- c("rf","lda",'naive_bayes','kknn','loclda',
            'wsrf','avNNet','monmlp','adaboost','gbm',  'hda')
## List all models we will use
combined_preds <- setNames(data.frame(matrix(ncol = length(models) + 1,
                                             nrow = length(test_data_playoffs$w))),
                           c(models,"Overall"))
## Create data frame to store results in

fits <- lapply(models, function(model){ 
  print(model)
  train(playoff ~ . , method = model, data = train_set)
}) 
## Train each model on the training data

for(i in 1:length(models)){
  pred <- as.data.frame(predict(fits[i], newdata = test_data_playoffs, type = "prob"))
  pred$Y[pred$Y >= median(pred$Y)] <- "Y"
  pred$Y[pred$Y < median(pred$Y)] <- "N"
  combined_preds[i] <- pred$Y
}
## For each trained model, return probabilities on test set, turn them into predictions, and store in combined results
## Same as individual model above from 99-102

votes <- rowMeans(combined_preds == "Y", na.rm = TRUE)
combined_preds$Overall <- ifelse(votes > 0.5, "Y", "N")
## Create ensemble predictions
test_data_playoffs$playoff <- as.character(test_data_playoffs$playoff)
## Need to turn playoff column back into a character for comparison

acc <- colMeans(combined_preds == test_data_playoffs$playoff)
## Test accuracy

mean(acc)
## Find average accuracy

acc
