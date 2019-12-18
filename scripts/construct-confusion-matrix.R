require(readr)
require(caret)
source("scripts/utils.R")

myNames <- c("Handle", "Profession", "Openness", "Conscientousness", "Extraversion", "Agreeableness", "Emotional_Range",
             "Conversation", "Openness to Change", "Hedonism", "Self-enhancement", "Self-transcendence")

files <- list.files(path = "data/prediction-results", pattern = "xgboost", full.names = T)
confMatrix <- NA

for (file in files) {
  myLabels <- c("True", "Predicted")
  ## load the prediction
  pred <- read.csv(file, stringsAsFactors = F)
  names(pred) <- myLabels
  
  print(c(file, nrow(pred)))
  
  ## transform into factors
  pred$True <- as.factor(pred$True)
  pred$Predicted <- factor(x = pred$Predicted, levels = levels(pred$True))
  
  ## compute measures
  result <- confusionMatrix(data = pred$Predicted, reference = pred$True, mode="prec_recall")
  if (!is.na(confMatrix))
    confMatrix <- confMatrix + result$table
  else confMatrix <- result$table
}

## scale the confusion matrix by the total size of the classes
confMatrix <- confMatrix / colSums(confMatrix, na.rm = T)
diag(confMatrix) <- NA

# pdf(file = "plots/confusion-heatmap.pdf", width = 8, height = 6)
# par(mar = c(8, 8, 4, 2) + 0.1)
# image.plot(x = 1:10, y = 1:10, z = confMatrix, cex.lab = 2, cex.main = 2.2, axes = F, 
#            xlab = "", ylab = "", main = "Confusion between top 10 professions",
#            col = myColors(n = 30, color_scheme = "data/blues.csv"), las = 2)
# axis(side = 1, at = 1:10, labels = colnames(confMatrix), cex = 2, las = 2)
# axis(side = 2, at = 1:10, labels = colnames(confMatrix), cex = 2, las = 2)
# dev.off()

pdf(file = "plots/confusion-heatmap-dendogram.pdf", width = 8, height = 8)
par(mar = c(10, 8, 4, 2) + 0.1)
heatmap(x = confMatrix, cex.lab = 2, cex.main = 2.2, Colv = "Rowv", 
        xlab = "", ylab = "", main = "Confusion between top 10 professions", margin = c(9, 9),
        col = myColors(n = 30, color_scheme = "data/blues.csv") )

dev.off()