library(readxl)
library(tidyverse)
library(caret)
library(ggalt)



my_file <- "https://raw.github.com/psdupvi/nhl-analysis/master/raw-data.csv"
raw_data <- read_csv(my_file)
## Download the data from my github repository
head(raw_data)
## View the data just to take a look

raw_data %>% ggplot(aes(x = ga, y = gf)) + geom_point() + geom_encircle(data = filter(raw_data, gp != 82), 
                                                                        color = "Blue", size = 4)
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
nhl_data <- nhl_data %>% mutate(year = as.character(year))
## Now all data is normalized to 82 games, and we turn the year value into a character (more useful)
head(nhl_data)

nhl_data %>% filter(playoff == T) %>% ggplot(aes(x = rank,y = pts)) + geom_boxplot(aes(group = rank))

nhl_data %>% ggplot(aes(y = pts, x = gf)) + geom_point(aes(color = year)) + geom_smooth()
nhl_data %>% ggplot(aes(y = pts, x = ga)) + geom_point(aes(color = year)) + geom_smooth()
nhl_data %>% ggplot(aes(y = pts, x = av_age)) + geom_point() + geom_smooth()
## Couple of fun visualizations
RMSE <- function(expected_value,true_value){
  sqrt(mean((expected_value - true_value)^2,na.rm = T))
}
## Create a RMSE functions
nhl_data <- nhl_data %>% mutate(pythag_wins = gp*gf^2.2/(gf^2.2+ga^2.2))
RMSE(nhl_data$pythag_wins,nhl_data$w)
## The baseline RMSE comes from the pythagorean expectation, a formula originally designed for baseball
## by famed statistician Bill James.  It works for the NHL as well, with a slighly different value
## 2.2 as opposed to roughly 1.83. For additional information see https://web.williams.edu/Mathematics/sjmiller/public_html/math/papers/DayaratnaMiller_HockeyFinal.pdf




