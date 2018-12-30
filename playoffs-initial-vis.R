train_data_playoffs$playoff <- as.factor(train_data_playoffs$playoff)

train_data_playoffs %>% filter(playoff == "Y") %>% group_by(rank) %>% summarize(mean = mean(pts), minimum = min(pts), n = n())
train_data_playoffs %>% filter(playoff == "N") %>% group_by(year) %>% summarize(missed = max(pts))

train_data_playoffs %>% filter(playoff == "Y") %>% ggplot(aes(x = rank, y = w)) + geom_boxplot(aes(group = rank))
train_data_playoffs %>% filter(playoff == "Y") %>% ggplot(aes(x = year, y = pts)) + geom_point()

missed_pts <- train_data_playoffs %>% filter(playoff == "N") %>% group_by(year) %>% summarize(missed = max(pts))
missed_pts_avg <- mean(missed_pts$missed)
train_data_playoffs %>% ggplot(aes(x = year, y = pts)) + geom_point(aes(color = playoff)) + geom_hline(yintercept = mean(missed_pts_avg))


train_data_playoffs %>% filter(team != "League Average") %>% group_by(team) %>% summarize(avg = mean(pts), 
                                                     playoffs = sum(playoff == "Y"), 
                                                     finals = sum(champ == "Y" | runner == "Y"),
                                                     cups = sum(champ == "Y")) %>% arrange(desc(avg))

train_data_playoffs %>% filter(team != "League Average") %>% ggplot(aes(x = team, y = pts)) + geom_boxplot() + 
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))



train_data_playoffs %>% filter(pts >= missed_pts_avg & playoff == "N") %>% select(team, year, rank)


train_data_playoffs %>% filter(playoff == "N") %>% group_by(year) %>% summarize(pts = max(pts), 
                                                                                av_age = max(av_age))

  
options(scipen=999)
playoffs <- train_data_playoffs %>% filter(team != "League Average") %>% filter(playoff == "Y") %>% select(-year,-playoff,-champ,-runner, -team,-rank,-pts,-w,-l,-ol,pts_perc,-gp)
no_play <- train_data_playoffs %>% filter(team != "League Average") %>% filter(playoff == "N") %>% select(-year,-playoff,-champ,-runner, -team,-rank,-pts,-w,-l,-ol,pts_perc,-gp)

diff <- round(sapply(playoffs,mean) - sapply(no_play,mean),3)
(sapply(playoffs,min) - sapply(no_play,max))/sapply(playoffs,min)

options(scipen=0)
