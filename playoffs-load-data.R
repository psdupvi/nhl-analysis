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

raw_data_short <- raw_data %>% filter(gp < 82) %>% mutate(pts = pts*82/gp,w = w*82/gp, l = l*82/gp, 
                                                         ga = ga*82/gp,gf = gf*82/gp, sow = sow*82/gp,
                                                         sol = sol*82/gp, evgf = evgf*82/gp, evga = evga*82/gp,
                                                         pp = pp*82/gp, ppo = ppo*82/gp, ppoa = ppoa*82/gp,
                                                         ppa = ppa*82/gp,sh = sh*82/gp, sha = sha*82/gp,
                                                         shots = shots*82/gp, shots_against = shots_against*82/gp)  
## Normalize data for 2010 because it was shortened
raw_data_not_short <- raw_data %>% filter(gp == 82)
## Select 
nhl_data <- rbind(raw_data_not_short,raw_data_short)

test_data_playoffs <- filter(nhl_data, year == 2017) %>% filter(team != "League Average") 
train_data_playoffs <- filter(nhl_data, year != 2017) %>% filter(team != "League Average")
rm(raw_data_not_short,raw_data,raw_data_short,raw_data_file)



