library(readxl)
library(tidyverse)
library(caret)
library(ggalt)



raw_data_file <- "https://raw.github.com/psdupvi/nhl-analysis/master/raw-data.csv"
raw_data <- read_csv(raw_data_file)
raw_data <- raw_data[rowSums(is.na(raw_data)) != ncol(raw_data),]
test_data_file <- "https://raw.github.com/psdupvi/nhl-analysis/master/test-data-2.csv"
test_data_w <- read_csv(test_data_file)
## Download the data from my github repository

test_data_w <- test_data_w[rowSums(is.na(test_data_w)) != ncol(test_data_w),]
test_data_w <- test_data_w %>% mutate(pts = pts*82/gp,w = w*82/gp, l = l*82/gp, 
       ga = ga*82/gp,gf = gf*82/gp, sow = sow*82/gp,
       sol = sol*82/gp, evgf = evgf*82/gp, evga = evga*82/gp,
       pp = pp*82/gp, ppo = ppo*82/gp, ppoa = ppoa*82/gp,
       ppa = ppa*82/gp,sh = sh*82/gp, sha = sha*82/gp,
       shots = shots*82/gp, shots_against = shots_against*82/gp, gp = 82)
## Make sure that test_data_pts is normalized to 82 poitnts

test_data_w <- na.omit(test_data_w)
## Tidy it up -- remove any rows which are only na

head(raw_data)
## View the data just to take a look

raw_data %>% ggplot(aes(x = ga, y = gf)) + geom_point() + geom_encircle(data = filter(raw_data, gp != 82), 
                                                                        color = "Blue", size = 4)
## Visualize goals for vs. goals against, the main predictors used in the baseline pts predictions
## You'll notice that one of the seasons has far fewer games, because there was a lockout
## In the next step we normalize that data to 82 games, then merge those changes back in
raw_data_2010 <- raw_data %>% filter(gp < 82) %>% mutate(pts = pts*82/gp,w = w*82/gp, l = l*82/gp, 
                                                    ga = ga*82/gp,gf = gf*82/gp, sow = sow*82/gp,
                                                    sol = sol*82/gp, evgf = evgf*82/gp, evga = evga*82/gp,
                                                    pp = pp*82/gp, ppo = ppo*82/gp, ppoa = ppoa*82/gp,
                                                    ppa = ppa*82/gp,sh = sh*82/gp, sha = sha*82/gp,
                                                    shots = shots*82/gp, shots_against = shots_against*82/gp)    
raw_data_not_2010 <- raw_data %>% filter(gp == 82)
nhl_data <- rbind(raw_data_not_2010,raw_data_2010)
## Combine the data sets back together

test_data_playoffs <- filter(nhl_data, year == 2017)
train_data_playoffs <- filter(nhl_data, year != 2017)
train_data_pts <- nhl_data %>% select(-team,-rank,-gp,-pts,-l,-ol,-sow,-sol, -pts_perc, -simp_rat_sys, -year, - playoff, -champ, -runner)
## Now all data is normalized to 82 games
head(train_data_pts)
train_data_pts <- na.omit(train_data_pts)

nhl_data %>% filter(playoff == T) %>% ggplot(aes(x = rank,y = w)) + geom_boxplot(aes(group = rank))

nhl_data %>% ggplot(aes(y = w, x = gf)) + geom_point() + geom_smooth()
nhl_data %>% ggplot(aes(y = w, x = ga)) + geom_point() + geom_smooth()
nhl_data %>% ggplot(aes(y = w, x = av_age)) + geom_point() + geom_smooth()
## Couple of fun visualizations


RMSE <- function(expected_value,true_value){
  sqrt(mean((expected_value - true_value)^2,na.rm = T))
}
## Create a RMSE function
pythag <- nhl_data %>% mutate(pythag_wins = gp*gf^2.2/(gf^2.2+ga^2.2)) %>% .$pythag_wins

pythag_rmse <- RMSE(pythag,nhl_data$w)
## The baseline RMSE comes from the pythagorean expectation, a formula originally designed for baseball
## by famed statistician Bill James.  It works for the NHL as well, with a slighly different value
## 2.2 as opposed to roughly 1.83. For additional information see https://web.williams.edu/Mathematics/sjmiller/public_html/math/papers/DayaratnaMiller_HockeyFinal.pdf
rmse_results <- data.frame(method = "Pythagorean Expectation", RMSE = pythag_rmse)
rmse_results %>% knitr::kable()

p <- train_data_pts %>% gather(-w, key = "key", value = "value")
p %>% ggplot(aes(x = value, y = w)) + 
  geom_point() +
  facet_wrap(~key, scales = "free")

## Obviously goals for, against are have strong correlation -- that's why they're in the pythag prediction
## Other apparently strong correlations are save percent, pp percent, shots, shots against, and pdo -- 
## which is the sum of a teams shooting percentage and save percentage
## Theoretically it should regress towards 100


