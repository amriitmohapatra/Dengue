LASSO_Model <- function(YYY, YYY2){
  set.seed(0)
  X_train = YYY[,-ncol(YYY)]
  y_train = YYY[,ncol(YYY)]
  X_test = YYY2[,-ncol(YYY2)]
  cv_model <-cv.glmnet(x = as.matrix(X_train), y = y_train, alpha = 1, nfolds = 5)
  plot(cv_model)
  optimal_lambda <- cv_model$lambda.min
  final_lasso_model <- glmnet(as.matrix(X_train), y_train, alpha = 1, lambda = optimal_lambda)
  LASSO_forecast  = vector()
  for(ref_time in 1:nrow(X_test)){
    LASSO_forecast[ref_time] <- predict(final_lasso_model, newx = as.matrix(X_test[ref_time,]))
  }
  return(LASSO_forecast)
}


SVR1_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  
  # op=tune(svm, target_variable ~ . , data = training_set_scaled_df, kernel="radial", type ='eps-regression',ranges=list(cost=c(0.001,0.01),gamma=c(0.5,0.1)))
  op=tune(svm, target_variable ~ . , data = training_set_scaled_df, kernel="linear", type ='eps-regression',ranges=list(cost=c(0.001,0.01)))
  regressor = op$best.model
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}


SVR2_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  
  op=tune(svm, target_variable ~ . , data = training_set_scaled_df, kernel="radial", type ='eps-regression',ranges=list(cost=c(0.001,0.01),gamma=c(0.5,0.1)))
  regressor = op$best.model
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}




SVR3_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  
  op=tune(svm, target_variable ~ . , data = training_set_scaled_df, kernel="linear", type ='eps-regression')
  regressor = op$best.model
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}


SVR4_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  
  op=tune(svm, target_variable ~ . , data = training_set_scaled_df, kernel="radial", type ='eps-regression')
  regressor = op$best.model
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}



DT1_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  
  regressor = rpart(target_variable ~ . , data = training_set_scaled_df, method = "anova", minsplit = 5, cp = 0.0005) 
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}


DT2_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  
  regressor = rpart(target_variable ~ . , data = training_set_scaled_df, method = "anova") 
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}


RF_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  mty = floor((ncol(training_set_scaled_df) - 1)/3)
  regressor = randomForest(target_variable ~ . , data = training_set_scaled_df, ntree = 5000, mtry = mty, maxnodes = 10) 
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}


Bagging_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  mty = floor((ncol(training_set_scaled_df) - 1)/3)
  regressor = randomForest(target_variable ~ . , data = training_set_scaled_df, ntree = 5000, mtry = mty*3, maxnodes = 10)
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, test_scaled[ref_time,])
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}

Boosting1_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  regressor = gbm(target_variable ~ ., data=training_set_scaled_df, distribution='gaussian',
                  interaction.depth=2,n.trees=10000,shrinkage=.01, cv.folds = 10)
  
  # best = gbm.perf(boostfit, method="OOB")
  best_fit = gbm.perf(regressor, method = "cv")
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, newdata = test_scaled[ref_time,], n.trees = best_fit, type = "response" )
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}

Boosting2_Model <- function(YYY, YYY2){
  training_set = YYY
  test_set = YYY2
  set.seed(0)
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  training_set_scaled_df = data.frame(training_set_scaled)
  colnames(training_set_scaled_df) = colnames(training_set)
  regressor = gbm(target_variable ~ ., data=training_set_scaled_df, distribution='gaussian',bag.fraction = .5,
                  interaction.depth=2,n.trees=10000,shrinkage=.01)
  
  best_fit = gbm.perf(regressor, method="OOB")
  
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  inputs_scaled_df = data.frame(inputs_scaled)
  colnames(inputs_scaled_df) = colnames(test_set)
  
  test_scaled = inputs_scaled_df[,1:(ncol(inputs_scaled_df) - 1)] 
  model_fit_values = vector()
  for(ref_time in 1:nrow(inputs_scaled_df)){
    predicted_dengue_cases = predict(regressor, newdata = test_scaled[ref_time,], n.trees = best_fit, type = "response" )
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    # Inverse transform the scaled inputs
    inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}



LSTM_Model <- function(YYY, YYY2){
  set.seed(0)
  n_total <- nrow(YYY)
  n_to_select <- round(0.25 * n_total)
  training_set = YYY
  validation_set <- YYY[(n_total - n_to_select + 1):n_total, ]
  test_set = YYY2
  
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  validation_set_scaled = matrix(0, nrow = nrow(validation_set), ncol = ncol(validation_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  for(i in 1:ncol(validation_set_scaled)){
    validation_set_scaled[,i] = as.vector(scale(validation_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  # Get X_train, y_train , X_val, y_val
  
  X_train = training_set_scaled[,-ncol(training_set_scaled)]
  y_train = training_set_scaled[,ncol(training_set_scaled)]
  
  X_validation = validation_set_scaled[,-ncol(validation_set_scaled)]
  y_validation = validation_set_scaled[,ncol(validation_set_scaled)]
  
  X_train <- array(X_train, dim = c(dim(X_train)[1], dim(X_train)[2], 1))
  X_validation <- array(X_validation, dim = c(dim(X_validation)[1], dim(X_validation)[2], 1))
  
  # Build LSTM
  
  batch_size = 20
  
  val_steps = nrow(validation_set_scaled)/ batch_size
  
  print(paste("Now Running Model for ", models, " week ahead.", sep = ""))

  regressor <- load_model_hdf5(paste(dir_path,"/Model/LSTM_","Model_",models,".hdf5", sep = ""))
  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  X_test_master = inputs_scaled[,-ncol(inputs_scaled)]  
  model_fit_values = vector()
  for(ref_time in 1:nrow(X_test_master)){
    X_test = matrix(X_test_master[ref_time,], 1 ,ncol = ncol(X_test_master))
    X_test <- array(X_test, dim = c(dim(X_test)[1], dim(X_test)[2], 1))
    
    
    predicted_dengue_cases = regressor %>% predict(X_test)
    
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    
    # Inverse transform the scaled inputs
    # inputs_inverse <- inputs_scaled[,ncol(inputs_scaled)] * (training_set_max - training_set_min) + training_set_min
    
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    
    model_fit_values[ref_time] = predicted_dengue_cases
    # This continues till all weeks have their forecasted values
  }
  return(model_fit_values)
}

GRU_Model <- function(YYY, YYY2){
  set.seed(0)
  n_total <- nrow(YYY)
  n_to_select <- round(0.25 * n_total)
  
  # latest_25_percent <- time_series_data[(n_total - n_to_select + 1):n_total]
  
  training_set = YYY
  validation_set <- YYY[(n_total - n_to_select + 1):n_total, ]
  test_set = YYY2
  # Feature Scaling
  
  training_set_scaled = matrix(0, nrow = nrow(training_set), ncol = ncol(training_set))
  validation_set_scaled = matrix(0, nrow = nrow(validation_set), ncol = ncol(validation_set))
  for(i in 1:ncol(training_set_scaled)){
    training_set_scaled[,i] = as.vector(scale(training_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  for(i in 1:ncol(validation_set_scaled)){
    validation_set_scaled[,i] = as.vector(scale(validation_set[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  # Get X_train, y_train , X_val, y_val
  
  X_train = training_set_scaled[,-ncol(training_set_scaled)]
  y_train = training_set_scaled[,ncol(training_set_scaled)]
  
  X_validation = validation_set_scaled[,-ncol(validation_set_scaled)]
  y_validation = validation_set_scaled[,ncol(validation_set_scaled)]
  
  X_train <- array(X_train, dim = c(dim(X_train)[1], dim(X_train)[2], 1))
  X_validation <- array(X_validation, dim = c(dim(X_validation)[1], dim(X_validation)[2], 1))
  
  # Build GRU
  
  batch_size = 20
  
  val_steps = nrow(validation_set_scaled)/ batch_size

  
  print(paste("Now Running Model for ", models, " week ahead.", sep = ""))
  

  regressor <- load_model_hdf5(paste(dir_path,"/Model/GRU_","Model_",models,".hdf5", sep = ""))

  inputs = test_set
  
  inputs_scaled = matrix(0, nrow = nrow(inputs), ncol = ncol(inputs))
  for(i in 1:ncol(inputs)){
    inputs_scaled[,i] = as.vector(scale(inputs[,i], center = min(training_set[,i]), scale = max(training_set[,i]) - min(training_set[,i])))
  }
  
  X_test_master = inputs_scaled[,-ncol(inputs_scaled)]  
  model_fit_values = vector()
  for(ref_time in 1:nrow(X_test_master)){
    X_test = matrix(X_test_master[ref_time,], 1 ,ncol = ncol(X_test_master))
    X_test <- array(X_test, dim = c(dim(X_test)[1], dim(X_test)[2], 1))
    
    
    predicted_dengue_cases = regressor %>% predict(X_test)
    
    training_set_min <- min(training_set[,ncol(training_set)])
    training_set_max <- max(training_set[,ncol(training_set)])
    
    # Inverse transform the scaled inputs
    
    predicted_dengue_cases = predicted_dengue_cases * (training_set_max - training_set_min) + training_set_min
    
    model_fit_values[ref_time] = predicted_dengue_cases
  }
  return(model_fit_values)
}

