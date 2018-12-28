train_data_playoffs$playoff <- as.factor(train_data_playoffs$playoff)

train_data_playoffs %>% filter(playoff == "Y") %>% group_by(rank) %>% summarize(mean = mean(pts), minimum = min(pts), n = n())
train_data_playoffs %>% filter(playoff == "N") %>% group_by(year) %>% summarize(missed = max(pts))


train_data_playoffs %>% group_by(team) %>% summarize(avg = mean(pts), 
                                                     playoffs = sum(playoff == "Y"), 
                                                     finals = sum(champ == "Y" | runner == "Y"),
                                                     cups = sum(champ == "Y")) %>% arrange(desc(avg))
