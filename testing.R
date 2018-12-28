lm <- train(playoffs ~ .,nhl_data_train,method = "lm")

lm_predictions <- predict(lm,newdata = test_data)
lm_predictions <- unname(lm_predictions)
RMSE(lm_predictions,test_data$pts)


bridge <- train(pts ~ .,nhl_data_train,method = "bridge")

bridge_predictions <- predict(bridge,newdata = test_data)

bridge_predictions <- unname(bridge_predictions)
RMSE(bridge_predictions,test_data$pts)


