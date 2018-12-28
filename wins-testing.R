lm <- train(w ~ .,train_data_pts,method = "lm")

lm_predictions <- unname(predict(lm,newdata = test_data_w))
lm_rmse <- RMSE(lm_predictions,test_data_w$w)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Linear Model",  
                                     RMSE = lm_rmse ))
rmse_results %>% knitr::kable()
bridge <- train(w ~ .,train_data_pts,method = "bridge")

bridge_predictions <- unname(predict(bridge,newdata = test_data_w))

bridge_rmse <-RMSE(bridge_predictions,test_data_w$w)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Bridge Model",  
                                     RMSE = bridge_rmse ))
rmse_results %>% knitr::kable()

## But these are unfair.  The pythagorean expectation only takes two inputs
## So lets see if we can do that.w
