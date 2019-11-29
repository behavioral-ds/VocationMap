## trying to predict with only subsets of features

library(reticulate)
# use_condaenv(condaenv = "py3k") ## on Hephaestos
use_condaenv(condaenv = "vocationcompass") ## on Hephaestos
# use_condaenv(condaenv = "base") ## on Donald
require(parallel)
require(caret)
library(ggplot2)
require(dplyr)
source("scripts/utils.R")

## MAR: I could to it liks this, but it would take forever to translate the entire notebook
# pd <- import(module = "pandas")
# np <- import(module = "numpy")
# dill <- import(module = "dill")
# 
# dl <- dill$load_session("study3_experiments/rand_optimised_all_R.db")

scripts <- c("study3_experiments/donald_base_env/load_rand_optimised_all.py", 
             "study3_experiments/donald_base_env/load_rand_optimised_big5.py", 
             "study3_experiments/donald_base_env/load_rand_optimised_values.py")
.cl <- makeCluster(spec = min(detectCores(), length(scripts)), type = "PSOCK")
res <- parSapply(cl = .cl, X = scripts, FUN = function(myscript) {
  library(reticulate)
  use_condaenv(condaenv = "vocationcompass") ## on Hephaestos and Donald
  require(caret)
  
  ## now, we execute the python script which simply loads the dill with everything saved in it!
  ## note that it might take a while to load the dill file
  source_python(myscript)
  
  ## now, simple process it!
  models <- c("logistic_regression", "KNN", "random_forest", "gradient_boosting", "xgboost") # "linear_model" -- Stochastic Gradient Descent -- only for large datasets, not here.
  # models <- c("KNN", "lr")
  algo_names <- c("KNN", "Logistic\nRegression", "Random\nForests", "Gradient\nBoosting", "XGboost")
  myLabels <- c("True", "Predicted")
  results <- list()
  for (model in models) {
    # model <- models[1]
    
    ## for each fold
    results[[model]] <- data.frame(t(sapply(X = 1:length(predicted_labels[[model]]), FUN = function(foldno) {
      pred <- data.frame(True = unlist(true_labels[[model]][[foldno]]), Predicted = unlist(predicted_labels[[model]][[foldno]]))
      names(pred) <- myLabels
      
      ## transform into factors
      pred$True <- as.factor(pred$True)
      pred$Predicted <- factor(x = pred$Predicted, levels = levels(pred$True))
      
      ## compute measures
      result <- confusionMatrix(data = pred$Predicted, reference = pred$True, mode="prec_recall")
      
      apply(X = result$byClass, MARGIN = 2, FUN = mean, na.rm = T) #
    })))
  }
  return(list(results))
})
stopCluster(cl = .cl)
names(res) <- c("All", "Big5", "Values")

save(res, file = "study3_experiments/ALL_BIG5_Values_performances.dat", compress = "xz")
