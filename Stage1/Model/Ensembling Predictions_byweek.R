rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
library(stats)
library(dplyr)
library(lubridate)
library(LaplacesDemon)
library(zoo)

dataset_total = read.csv("Dengue Weekly Total.csv")
dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")

n_models <- 12 # number of week-ahead predictions
n_lags <- 20 #number of lags of feature set being considered


all_dates <- dataset_total$Date
dataset_total <- subset(dataset_total, select = -c(1,9,10))
dataset_total_colnames <- colnames(dataset_total)

for(i in 1:length(dataset_total)){
  for(j in 1:n_lags){
    colname <- paste(dataset_total_colnames[i], ".lag_", j, sep = "")
    dataset_total <- dataset_total %>% mutate(!!colname := lag(.[[i]], j))
  }
}

for(i in 1:n_models){
  colname <- paste(dataset_total_colnames[1], ".lead_", i, sep = "")
  dataset_total <- dataset_total %>% mutate(!!colname := lead(.[[1]], i))
}


dataset_total <- cbind(dataset_total, all_dates)
dataset_total <- dataset_total %>%
  filter(all_dates <= as.Date(end_date_input_train))

sub_df = dataset_total[-1:-n_lags, -length(dataset_total)]


n_rows <- nrow(sub_df)

n_models <- 12

n_lags <- 20
####################################### Preprocessing #############################################
if(!file.exists("True_Data.RData")){
  print("True_Data.RData does not exist. Create the RData")
  dataset_total = read.csv("Dengue Weekly Total.csv")
  dataset_total = read.csv("Dengue Weekly Total.csv")
  
  ### This block of code needs to be run only once, when creating the .RData of the True_Data
  dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")
  
  
  # actual = list()
  true_data = matrix(0, nrow = n_rows, ncol = 12)
  for(rows in 1:n_rows){
    true_data[rows,] <- dataset_total[(n_lags + 1 + rows):(n_lags + 12 +rows), 2]
  }
  
  myfilename = paste("True_Data",".RData", sep = '')
  save(true_data, file = myfilename)
}else{
  print("True_Data.RData exists. Load the RData file.")
  load("True_Data.RData")
}

load("All_Forecasts.RData")

rm(dataset_total, sub_df, all_dates, colname, dataset_total_colnames, i, j)
#################################### Bayesian Combination Predictors ################

# A list that calculates the deviation of each predicted value from the true value observed for all individual Machine Learning models.
e_hat = list()

for(i in 1:length(All_Predictions_Master)){
  e_hat[[i]] <- All_Predictions_Master[[i]] - true_data
}

# A list that stores sampled weights for each reference week for all 12 week ahead predictions from the 13 machine learning models.
Weights_Matrix_List <- list()

for(ref_week in 1:n_rows){
  Weights_Matrix = matrix(NA, nrow = length(All_Predictions_Master), ncol = n_models)
  for(week_num in 1:n_models){
    print(paste("Now Running Model for ", ref_week," row and " ,week_num, " week(s) ahead prediction.", sep = ""))
    model_variances <- vector() # a vector that calculates the error variances for a given week ahead prediction window for all ML models.
    
    for(i in 1:length(All_Predictions_Master)){
      model_variances[i] <- var(e_hat[[i]][,week_num])
    }

    
    #Prior for weights
    theta_prior = function(theta_vec){
      ddirichlet(theta_vec,rep(1, length(e_hat)), log = TRUE)
    }
    
    # Likelihood (Different for each model)
    likelihood = function(theta, variance){
      sum(dnorm(theta, mean = 0, sd = sqrt(variance), log = TRUE))
    }
    
    
    # Proposal
    proposal = function(theta,sd){
      x1 = rnorm(n=1,mean = theta[1], sd = sd[1])
      x2 = rnorm(n=1,mean = theta[2], sd = sd[2])
      x3 = rnorm(n=1,mean = theta[3], sd = sd[3])
      x4 = rnorm(n=1,mean = theta[4], sd = sd[4])
      x5 = rnorm(n=1,mean = theta[5], sd = sd[5])
      x6 = rnorm(n=1,mean = theta[6], sd = sd[6])
      x7 = rnorm(n=1,mean = theta[7], sd = sd[7])
      x8 = rnorm(n=1,mean = theta[8], sd = sd[8])
      x9 = rnorm(n=1,mean = theta[9], sd = sd[9])
      x10 = rnorm(n=1,mean = theta[10], sd = sd[10])
      x11 = rnorm(n=1,mean = theta[11], sd = sd[11])
      x12 = rnorm(n=1,mean = theta[12], sd = sd[12])
      x13 = 1-x1-x2-x3-x4-x5-x6-x7-x8-x9-x10-x11-x12
      vec = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
      return(vec)
    }
    
    iter = 20000 # Number of iterations
    store = list(theta_vals = matrix(0,nrow = iter,ncol = length(e_hat)),e_hats = matrix(0, nrow = iter, ncol = n_rows) ,logpost_vals = rep(0,iter))
    
    # First Run
    x1 = x2 = x3 = x4 = x5 = x6 = x7 = x8 = x9 = x10 = x11 = x12 = 0.08
    x13 = 0.04
    
    
    theta_0 = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
    
    w1_all_models_reg_1 <- vector() # A vector that extracts, from each ML model prediction, the predicted value in the ref_week row and the week_num column. 
    for(columns in 1:length(All_Predictions_Master)){
      w1_all_models_reg_1[columns] <- All_Predictions_Master[[columns]][ref_week ,week_num]
    }
    
    w1_wt_sum <- sum(w1_all_models_reg_1 * theta_0) # The extracted data from the previous step is then subjected to a dot multiplication with the vector of proposed weights.
    
    e_hat_wt = w1_wt_sum - true_data[ref_week, week_num] # The deviation from the true data 
    
    
    sum_likelihood = 0 # a variable that calculates the likelihood, initialized to 0 for each round of the sampling exercise.
    
    for(i in 1:length(model_variances)){
      sum_likelihood = sum_likelihood + likelihood(e_hat_wt, model_variances[i])
    }
    
    store$theta_vals[1,] = theta_0
    store$e_hats[1] = e_hat_wt
    store$logpost_vals[1] = sum_likelihood + theta_prior(theta_0)
    print("MCMC Iterations starting!")
    set.seed(19)
    # Perform the Metropolis-Hastings algorithm
    for (t in 2:iter) {
      
      if(t %% 100 == 0){print(t)}
      # Sample a proposed value from the proposal distribution
      theta_star <- proposal(store$theta_vals[t-1,], rep(0.0005 ,14))
      
      REJECT = FALSE
      if(min(theta_star)<0){REJECT = TRUE}
      if(!REJECT)
      { # Compute the weighted sum of all estimates and find the likelihood
        w1_wt_sum_star <- sum(w1_all_models_reg_1 * theta_star)
        e_hat_wt_star <- w1_wt_sum_star - true_data[ref_week, week_num]
        
        sum_likelihood_star = 0
        sum_likelihood_prev = 0
        for(i in 1:length(model_variances)){
          sum_likelihood_star = sum_likelihood_star + likelihood(e_hat_wt_star, model_variances[i])
          sum_likelihood_prev = sum_likelihood_prev + likelihood(store$e_hats[(t-1)], model_variances[i])
        }
        
        
        # Calculate the acceptance probability
        
        log_alpha <- sum_likelihood_star + theta_prior(theta_star) - sum_likelihood_prev - theta_prior(store$theta_vals[(t-1),])
        logaccprob = min(0,log_alpha)
        # Sample a uniform random variable
        u <- runif(1)
        if(log(u)>logaccprob)REJECT=TRUE
      }
      if(REJECT){store$theta_vals[t,] <- store$theta_vals[(t-1), ] ; store$e_hats[t] <- store$e_hats[(t-1)] ; store$logpost_vals[t] <- sum_likelihood_prev + theta_prior(store$theta_vals[(t-1),]) }
      else{store$theta_vals[t,] <- theta_star ; store$e_hats[t] <- e_hat_wt_star ; store$logpost_vals[t] <- sum_likelihood_star + theta_prior(theta_star) }
    }
    print("MCMC Iterations Completed Successfully!")
    # plot(store$logpost_vals)
    ################################## Sampling from Posterior ################
    burnin = iter/2
    wts = store$theta_vals[(burnin+1):iter, ]
    e_hats_converged = store$e_hats[(burnin+1):iter]
    avg_wts = apply(wts, MARGIN = 2, FUN = mean)
    Weights_Matrix[,week_num] = avg_wts
  }
  Weights_Matrix_List[[ref_week]] <- Weights_Matrix
}

########################################### End of Train Set BCP Exercise ########################################################################

save(Weights_Matrix_List, file = "Sampled Weights WS.RData")
rm(Weights_Matrix_List)
#################################### Postprocessing ########################
load("Sampled Weights WS.RData")

ML_models <- 13

num_lists <- length(All_Predictions_Master)
num_rows <- dim(All_Predictions_Master[[1]])[1]
num_cols <- dim(All_Predictions_Master[[1]])[2]

# Initialize a 3D array with the specified dimensions
all_predictions_array <- array(NA, dim = c(num_lists, num_rows, num_cols))
# Fill the 3D array with your data
for (i in 1:num_lists) {
  all_predictions_array[i, ,] <- All_Predictions_Master[[i]]
}


Predictions_Ensemble_True <- matrix(0, nrow = n_rows, ncol = n_models)
for(rows in 1:n_rows){
  for(week_num in 1:n_models){
    for(ML_model in 1:ML_models){
      Predictions_Ensemble_True[rows,week_num] <- Predictions_Ensemble_True[rows, week_num] + all_predictions_array[ML_model, rows, week_num] * Weights_Matrix_List[[rows]][ML_model, week_num]
    }
  }
}

Predictions_Ensemble_List <- list()
for(counter in 1:length(Weights_Matrix_List)){
  Predictions_Ensemble <- matrix(0, nrow = n_rows, ncol = n_models)
  for(rows in 1:n_rows){
    for(week_num in 1:12){
      for( ML_model in 1:ML_models){
        # Predictions_Ensemble_Array[counter, rows, week_num] <- Predictions_Ensemble_Array[counter, rows, week_num] + all_predictions_array[ML_model, rows, week_num] * Weights_Matrix_List[[counter]][ML_model, week_num]
        Predictions_Ensemble[rows,week_num] <- Predictions_Ensemble[rows , week_num] + all_predictions_array[ML_model, rows, week_num] * Weights_Matrix_List[[counter]][ML_model, week_num]
      }
    }
  }
  Predictions_Ensemble_List[[counter]] <- Predictions_Ensemble
}

# Assuming Predictions_Ensemble_List is your list
Predictions_Ensemble_Array <- array(NA, dim = c(length(Predictions_Ensemble_List), n_rows, n_models))
for (x in 1:length(Predictions_Ensemble_List)) {
  for(y in 1:n_rows){
    for(z in 1:n_models){
      Predictions_Ensemble_Array[x,y,z] <- Predictions_Ensemble_List[[x]][y,z]
    }
  }
}

Predictions_Ensemble_Results <- array(NA, dim = c(3, n_rows, n_models))
# Predictions_Ensemble_Results[1, , ] <- apply(Predictions_Ensemble_Array, c(2,3), mean, na.rm = TRUE) #1
Predictions_Ensemble_Results[1, , ] <- Predictions_Ensemble_True
Predictions_Ensemble_Results[2, , ] <- apply(Predictions_Ensemble_Array, c(2,3), quantile, probs = 0.025, na.rm = TRUE) #2
Predictions_Ensemble_Results[3, , ] <- apply(Predictions_Ensemble_Array, c(2,3), quantile, probs = 0.975, na.rm = TRUE) #3

View(Predictions_Ensemble_Results[1, , ])
View(Predictions_Ensemble_Results[2, , ])
View(Predictions_Ensemble_Results[3, , ])

save(Predictions_Ensemble_Results, file = "Predictions Ensemble.RData")
























