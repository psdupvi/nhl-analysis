lm <- train(w ~ .,nhl_data,method = "lm")

lm_predictions <- predict(lm,newdata = test_data)
lm_predictions <- unname(lm_predictions)
RMSE(lm_predictions,test_data$w)


rf <- train(w ~ . + gf,nhl_data,method = "rf")

rf_predictions <- predict(rf,newdata = test_data)

rf_predictions <- unname(rf_predictions)
RMSE(rf_predictions,test_data$w)


for(col in 1:ncol(nhl_data_train)){
 t <- train(colnames(nhl_data_train)[col],pts,data = nhl_data_train)
 pred <- unname(predict(t,newdata = test_data))
 print(RMSE(pred,test_data$pts),colnames(nhl_data_train,col))
}


ascolnames(nhl_data_train)[2]


assign()