nhl_data %>% filter(playoff == T) %>% ggplot(aes(x = rank,y = w)) + geom_boxplot(aes(group = rank))

nhl_data %>% ggplot(aes(y = w, x = gf)) + geom_point() + geom_smooth()
nhl_data %>% ggplot(aes(y = w, x = ga)) + geom_point() + geom_smooth()
nhl_data %>% ggplot(aes(y = w, x = av_age)) + geom_point() + geom_smooth()
## Couple of fun visualizations


p <- train_data_w %>% gather(-w, key = "key", value = "value")
p %>% ggplot(aes(x = value, y = w)) + 
  geom_point() +
  facet_wrap(~key, scales = "free")

## Obviously goals for, against are have strong correlation -- that's why they're in the pythag prediction
## Other apparently strong correlations are save percent, pp percent, shots, shots against, and pdo -- 
## which is the sum of a teams shooting percentage and save percentage
## Theoretically it should regress towards 100


