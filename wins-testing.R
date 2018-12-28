RMSE <- function(expected_value,true_value){
  sqrt(mean((expected_value - true_value)^2,na.rm = T))
}
## Create a RMSE function
pythag <- nhl_data %>% mutate(pythag_wins = gp*gf^2.2/(gf^2.2+ga^2.2)) %>% .$pythag_wins

pythag_rmse <- RMSE(pythag,nhl_data$w)
## The baseline RMSE comes from the pythagorean expectation, a formula originally designed for baseball
## by famed statistician Bill James.  It works for the NHL as well, with a slighly different value
## 2.2 as opposed to roughly 1.83. For additional information see https://web.williams.edu/Mathematics/sjmiller/public_html/math/papers/DayaratnaMiller_HockeyFinal.pdf
rmse_results <- data.frame(method = "Pythagorean Expectation", RMSE = pythag_rmse)
rmse_results %>% knitr::kable()



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
