# Testing all models at same time -----------------------------------
models <- c("rf","Rborist","lda",'naive_bayes','kknn','loclda',
            'wsrf','avNNet','monmlp','adaboost','gbm',  'hda')
        
train_set <- train_data_playoffs %>% select(-year,-champ,-runner, -team,-rank,-pts_perc,-gp,-year,-pk_perc,-pp_perc,-w,-l,-sol,-sow)
## Select useful variables for determining playoffs
## Also drop linearly dependent variables (eg pts vs pts_perc)
combined_preds <- setNames(data.frame(matrix(ncol = length(models) + 1,nrow = length(test_data_playoffs$w))),c(models,"Overall"))
## Create a data frame to hold predictions

fits <- lapply(models, function(model){ 
  print(model)
  train(playoff ~ . , method = model, data = train_set)
}) 
## Train a variety of models (listed above) with our train data

for(i in 1:length(models)){
  pred <- as.data.frame(predict(fits[i], newdata = test_data_playoffs, type = "prob"))
  ## Return probabilities of playoffs fr each team
  pred$Y[pred$Y >= median(pred$Y)] <- "Y"
  pred$Y[pred$Y < median(pred$Y)] <- "N"
  ## Select top half of teams as playoffs
  combined_preds[i] <- pred$Y
  ## Store results in combined_preds
}
## Using the probabilities, predict the playof teams
## Need to select only top half of teams based on probability

votes <- rowMeans(combined_preds == "Y", na.rm = TRUE)
combined_preds$Overall <- ifelse(votes > 0.5, "Y", "N")
## Average results for each prediction, put in overall
acc <- colMeans(combined_preds == test_data_playoffs$playoff)
## Test all columns to see which model works best

# Sample Individual Test -----------------------------------------
fit <- train(playoff ~ ., data = train_set, method = "rf")
pred <- predict(fit,newdata = test_data_playoffs, "prob") 

pred$Y[pred$Y >= median(pred$Y)] <- "Y"
pred$Y[pred$Y < median(pred$Y)] <- "N"

acc <- mean(pred$Y == test_data_playoffs$playoff)
acc_results <- data.frame(method = "rf",
                          accuracy = acc)
acc_results %>% knitr::kable()

pred_issue <- predict(fit, newdata = test_data_playoffs)
sum(pred_issue == "Y")
## Note that this has 19 teams -- but only 16 teams make the playoffs
## That's why we have to fix using the methods above (with probabilities)

