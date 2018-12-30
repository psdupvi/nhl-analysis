train_set <- train_data_playoffs %>% select(-year,-champ,-runner, -team,-rank,-pts_perc,-gp,-year,-pk_perc,-pp_perc,-w,-l,-sol,-sow)

fit <- train(playoff ~ ., data = train_set, method = "rf")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- data.frame(method = "rf",
                          accuracy = acc)
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "lda")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="lda",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "naive_bayes")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="naive_bayes",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "kknn")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="kknn",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()

fit <- train(playoff ~ ., data = train_set, method = "loclda")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="loclda",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "wsrf")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="wsrf",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "avNNet")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="avNNet",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "mlp")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="mlp",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()


fit <- train(playoff ~ ., data = train_set, method = "monmlp")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="monmlp",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()

fit <- train(playoff ~ ., data = train_set, method = "adaboost")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="adaboost",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()

fit <- train(playoff ~ ., data = train_set, method = "gbm")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="gbm",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()




fit <- train(playoff ~ ., data = train_set, method = "hda")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="hda",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()



fit <- train(playoff ~ ., data = train_set, method = "lvq")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- bind_rows(acc_results,
                         data_frame(method="lvq",  
                                    accuracy = acc ))
acc_results %>% knitr::kable()






