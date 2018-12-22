library(readxl)
library(tidyverse)
library(caret)
library(ggalt)

raw_data <- read_xlsx("raw-data.xlsx")

head(raw_data)

raw_data %>% ggplot(aes(x = ga, y = gf)) + geom_point() + geom_encircle(data = filter(raw_data, gp != 82), 
                                                                        color = "Blue", size = 4)

raw_data_2010 <- raw_data %>% filter(gp < 82) %>% mutate(pts = pts*82/gp,w = w*82/gp, l = l*82/gp, 
                                                    ga = ga*82/gp,gf = gf*82/gp, sow = sow*82/gp,
                                                    sol = sol*82/gp, evgf = evgf*82/gp, evga = evga*82/gp,
                                                    pp = pp*82/gp, ppo = ppo*82/gp, ppoa = ppoa*82/gp,
                                                    ppa = ppa*82/gp,sh = sh*82/gp, sha = sha*82/gp,
                                                    shots = shots*82/gp, shots_against = shots_against*82/gp)    
raw_data_not_2010 <- raw_data %>% filter(gp == 82)
adjusted_data <- rbind(raw_data_not_2010,raw_data_2010)
adjusted_data <- adjusted_data %>% mutate(year = as.character(year), rank = as.integer(rank))
head(adjusted_data)

adjusted_data %>% filter(playoff == T) %>% ggplot(aes(x = rank,y = pts)) + geom_boxplot(aes(group = rank))

adjusted_data %>% ggplot(aes(y = pts, x = gf)) + geom_point(aes(color = year)) + geom_smooth()
adjusted_data %>% ggplot(aes(y = pts, x = ga)) + geom_point(aes(color = year)) + geom_smooth()
adjusted_data %>% ggplot(aes(y = pts, x = av_age)) + geom_point() + geom_smooth()


