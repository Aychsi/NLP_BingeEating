ratios <- c(.40, .45, 0.50, 0.55, 0.60)

hold <- 0
iProp <- 0
gProp <- 0
for (i in ratios) {
  for (ii in ratios) {
    total <- getPredictionMatrix()$overall["Accuracy"] + 
      getPredictionMatrix()$byClass["Specificity"] + 
      getPredictionMatrix()$byClass["Sensitivity"]
    getGroupModels(7, i, ii)
    if (getPredictionMatrix()$overall["Accuracy"] > hold) {
      hold <- getPredictionMatrix()$overall["Accuracy"]
      iProp <- i
      gProp <- ii
    } else {
      next
    }
  }
}

# Highest Sensitivity
getGroupModels(7, 0.45, 0.60)
getPredictionMatrix()

# Highest Accuracy
getGroupModels(7, 0.40, 0.40)
getPredictionMatrix()


getPredictionMatrix()$overall["Accuracy"] + 
  getPredictionMatrix()$byClass["Specificity"] + 
  getPredictionMatrix()$byClass["Sensitivity"]
