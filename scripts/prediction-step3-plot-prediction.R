## this script plots the results of the ablation study (boxplots of performances)

## load the ablation study results (if you can't find them, re-run the ablation study aggregation in "2019-05-28-ablation-study.R")
load(file = "study3_experiments/ALL_BIG5_Values_performances.dat")

## now, simple process it!
models <- c("logistic_regression", "KNN", "random_forest", "gradient_boosting", "xgboost") # "linear_model" -- Stochastic Gradient Descent -- only for large datasets, not here.
# models <- c("KNN", "lr")
algo_names <- c("Logistic\nRegression", "KNN", "Random\nForests", "Gradient\nBoosting", "XGboost")
myLabels <- c("True", "Predicted")

## from here on it is only plotting, taken from the 2018-11-03-study3.R file
valPlot <- c("Precision", "Recall", "F1", "Balanced.Accuracy") ## MAR: , "Balanced.Accuracy" needs to be a separate graphic, not on the same scale
measure_names <- c("Precision", "Recall", "F1", "Accuracy")
names(measure_names) <- valPlot

foo <- lapply(X = names(res), FUN = function(feat){
  # feat <- "All"
  results <- res[[feat]]
  tmp <- lapply(X = valPlot, FUN = function(valp) {
    toPlot <- sapply(X = results, FUN = function(myres) {
      # print(myres)
      return(myres[,valp])
    })
    myres <- stack(data.frame(toPlot))
    myres$measure <- valp
    return(myres)
  })
  toPlot <- do.call(what = rbind, tmp)
  toPlot$measure <- factor(x = toPlot$measure, levels = valPlot)
  toPlot$features <- feat
  toPlot <- data.frame(toPlot)
  
  return(toPlot)
})
toPlot <- do.call(what = rbind, foo)
toPlot$measure <- factor(x = toPlot$measure, levels = valPlot)
toPlot$models <- factor(x = toPlot$ind, levels = models)
toPlot$ind <- NULL
toPlot$features <- as.factor(toPlot$features)

## actually plotting
pdf(file = "plots/measures-different-features-boxplots.pdf", width = 8, height = 6)
# meas <- "Balanced.Accuracy"
for (meas in valPlot) {
  p <- ggplot(toPlot %>% filter(measure == meas), aes(x = models, y = values, fill = features)) +
    geom_boxplot(alpha=0.7) + #, outlier.shape = NA
    scale_y_continuous(name = "Value of measure") + #limits=c(0.475, 0.565)
    scale_x_discrete(name = element_blank(), labels=algo_names) + #name = "Classifier algorithms"
    ggtitle(sprintf("Prediction performance: %s", measure_names[meas])) +
    theme_bw() +
    theme(plot.title = element_text(size = 22, face = "bold"), #family = "Tahoma", -- not supported by PDF
          text = element_text(size = 18), #family = "Tahoma", -- not supported by PDF
          axis.title = element_text(face="bold"),
          axis.text.x = element_text(size = 16),
          legend.position = "bottom") +
    # scale_fill_brewer(palette = "Paired") +
    labs(fill = "Features:")
  show(p)
}
dev.off()

## trying a barplot instead
toPlot_summary <- toPlot %>%
  group_by(measure, features, models) %>%
  summarise(mean_values = mean(values, na.rm = T), median_values = median(values, na.rm = T), sd_values = sd(values, na.rm = T), count = n())

pdf(file = "plots/measures-different-features-barplots.pdf", width = 8, height = 6)
# meas <- "Balanced.Accuracy"
for (meas in valPlot) {
  lims <- c(.2, .57)
  if (meas == "Balanced.Accuracy")
    lims <- c(.575, .755)
  p <- ggplot(toPlot_summary %>% filter(measure == meas), aes(x = models, y = mean_values, fill = features)) + 
    geom_bar(stat="identity", position="dodge") +  
    geom_errorbar(aes(ymin = mean_values - sd_values, ymax = mean_values + sd_values), width=0.1, position = position_dodge(width = 0.9)) +
    scale_x_discrete(name = element_blank(), labels=algo_names) + #name = "Classifier algorithms"
    scale_y_continuous(name = "Value of measure") + #limits=c(0.475, 0.565)
    ggtitle(sprintf("Prediction performance: %s", measure_names[meas])) +
    theme_bw() +
    theme(plot.title = element_text(size = 22, face = "bold"), #family = "Tahoma", -- not supported by PDF
          text = element_text(size = 18), #family = "Tahoma", -- not supported by PDF
          axis.title = element_text(face="bold"),
          axis.text.x = element_text(size = 16),
          legend.position = "bottom") +
    scale_fill_brewer(palette = "Paired") +
    coord_cartesian(ylim = lims) +
    labs(fill = "Features:")
  show(p)
}
dev.off()


## compute the actual measure to put in paper
toPlot %>%
  group_by(measure, features, models) %>%
  summarise(mean = mean(values), sd = sd(values) ) %>%
  arrange(desc(mean)) %>%
  filter(measure == "Balanced.Accuracy")
