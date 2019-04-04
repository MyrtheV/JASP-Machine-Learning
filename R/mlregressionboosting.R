#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

MLRegressionBoosting <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Boosting Regression"
  
  # Read dataset
  dataset <- .regBoostReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regBoostErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regBoostResults <- .regBoostComputeResults(jaspResults, dataset, options)
  
  # Output tables
  .regBoostTable(      jaspResults, options, regBoostResults, ready)
  .regBoostRelInfTable(jaspResults, options, regBoostResults, ready)
  .regBoostApplyTable( jaspResults, options, regBoostResults, ready)
  
  # Output plots
  if (ready) .regBoostRelInfPlot(          jaspResults, options, regBoostResults)
  if (ready) .regBoostPlotDeviance(        jaspResults, options, regBoostResults)
  if (ready) .regBoostPlotOOBChangeDev(    jaspResults, options, regBoostResults)
  if (ready) .regBoostPlotPredPerformance( jaspResults, options, regBoostResults)
  
  return()
}

# Read dataset
.regBoostReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors,
                            columns.as.factor = options$indicator)
  
  return(data)
}

# Error checking
.regBoostErrorHandling <- function(dataset, options) {
  
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: Apply indicator should not have any missing values (consist of 0s and 1s)
  if (options$indicator != "") {
    .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
               all.target = options$indicator, observations.amount = nrow(dataset), exitAnalysisIfErrors = TRUE)
  }
  
  # Error Check 3: There should be at least 2 predictors; otherwise gbm() complains
  if (options$target != "" && ncol(dataset) < 3L){
    JASP:::.quitAnalysis("Please provide at least 2 predictors.")
  }
  
  # Error Check 5: If target values should be imputed, there have to be missing values in the target
  # if (options$applyModel == "applyImpute" && sum(is.na(dataset[, .v(options$target)])) < 1) {
  #   JASP:::.quitAnalysis("To apply model to missing values in target, please provide observations that have missing 
  #   values in the target variable.")
  # }
  
}

# Compute results
.regBoostComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateClassBoostResults"]])) return (jaspResults[["stateClassBoostResults"]]$object)
  
  # Create results object and add options
  results <- list()
  results[["spec"]] <- .regBoostCalcSpecs(dataset, options)
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs: apply roughfix or omit NA rows
  if (sum(is.na(dataset)) > 0) {
    
    if (options$applyModel == "applyImpute") {
      
      idxApply <- which(is.na(dataset[, target]))
      
      if (options$NAs == "roughfix") {
        predImpute <- randomForest::na.roughfix(dataset[idxApply, preds])
      } else {
        predImpute <- na.omit(dataset[idxApply, preds])
      }
      
    }
    
    if (options$NAs == "roughfix") dataset <- randomForest::na.roughfix(dataset) else dataset <- na.omit(dataset)
    
  }
  
  # Splitting the data into training set, test set, and application set
  if (options$applyModel == "applyIndicator" && options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1)
    idxModel <- which(dataset[, indicator] == 0)
    
    applyData <- dataset[idxApply, preds, drop = FALSE]
    modelData <- dataset[idxModel, ]
    
  } else {
    
    modelData <- dataset
    
  }
  
  # Set seed	
  if (options$seedBox == "manual") set.seed(options$seed) else set.seed(Sys.time())
  
  # Compile training and test data
  idxTrain <- sample(1:nrow(modelData), floor(results$spec$dataTrain * nrow(modelData)))
  idxTest <- (1:nrow(modelData))[-idxTrain]
  
  trainData <- modelData[idxTrain, c(preds, target), drop = FALSE]
  testData <- modelData[idxTest, preds, drop = FALSE]
  testTarget <- as.numeric(modelData[idxTest, target])
  
  # Prepare Boosting
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  # Run Boosting
  results[["res"]] <- gbm::gbm(formula = formula, data = trainData, n.trees = results$spec$noOfTrees,
                               shrinkage = results$spec$shrinkage, interaction.depth = results$spec$int.depth,
                               cv.folds = results$spec$modelOptimization, bag.fraction = results$spec$bag.fraction,
                               n.minobsinnode = results$spec$nNode, distribution = results$spec$dist)
  
  results[["data"]] <- list(trainData = trainData, testData = testData, testTarget = testTarget)
  results[["relInf"]] <- summary(results$res, plot = FALSE)
  
  if(options$modelOptimization == "cv") results[["method"]] <- "cv" else results[["method"]] <- "OOB"
  
  if (options$modelOptimization != "noOpt") {
    results[["optTrees"]] <- gbm::gbm.perf(results$res, plot.it = FALSE, method = results$method)[1]
  } else {
    results[["optTrees"]] <- results$spec$noOfTrees
  }
  
  # Derive test set predictions and calculate test error rate
  results[["preds"]] <- gbm::predict.gbm(results$res, testData, n.trees = results$optTrees, type = "response")
  results[["testError"]] <- mean((testTarget - results$preds)^2)
  
  # Apply model to new data if requested
  if(options$applyModel == "applyIndicator" && options$indicator != "") {
    
    applyPred <- gbm::predict.gbm(results$res, newdata = applyData, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- round(data.frame(case = idxApply, pred = applyPred), 2)
    
  } else if (options$applyModel == "applyImpute") {
    
    applyPred <- gbm::predict.gbm(results$res, newdata = predImpute, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- round(data.frame(case = idxApply, pred = applyPred), 2)
    
  }
  
  # Save results to state
  jaspResults[["stateClassBoostResults"]] <- createJaspState(results)
  jaspResults[["stateClassBoostResults"]]$dependOnOptions(c("target", "predictors", "indicator", "applyModel",
                                                            "noOfTrees", "numberOfTrees", "shrinkage", "shrinkPar",
                                                            "int.depth", "int.depth.parameter", "modelOptimization",
                                                            "cvFolds", "nNode", "nNodeSpec", "dataTrain",
                                                            "percentageDataTraining", "bag.fraction",
                                                            "bag.fraction.spec", "dist", "seedBox", "seed", "NAs"))
  
  return(results)
}

.regBoostCalcSpecs <- function(modelData, options) {
  specs <- list()
  
  # Setting the number of trees
  if (options$noOfTrees == "manual") specs$noOfTrees <- as.integer(options$numberOfTrees) else specs$noOfTrees <- 100
  
  # Setting the number of variables considered at each split
  if (options$shrinkage == "manual") specs$shrinkage <- options$shrinkPar else specs$shrinkage <- .1
  
  # What percentage of the data should be used for training?
  if (options$int.depth == "manual") specs$int.depth <- options$int.depth.parameter else specs$int.depth <- 1
  
  # What percentage of the data should be used for training?
  if (options$dataTrain == "manual") specs$dataTrain <- options$percentageDataTraining else specs$dataTrain <- .8
  
  # What percentage of the training data should be used per tree?
  if (options$bag.fraction == "manual") specs$bag.fraction <- options$bag.fraction.spec else specs$bag.fraction <- .5
  
  # Minimum number of observations in the terminal nodes of every tree
  if (options$nNode == "manual") specs$nNode <- options$nNodeSpec else specs$nNode <- 10
  
  # Should cross-validation be performed?
  if (options$modelOptimization == "cv") specs$modelOptimization <- options$cvFolds else specs$modelOptimization <- 0
  
  # Which distribution should be used?
  if (options$dist == "tdist") {
    
    specs$dist <- "tdist"
    specs$distribution <- "t"
    
  } else if (options$dist == "laplace") {
    
    specs$dist <- "laplace"
    specs$distribution <- "Laplace"
    
  } else {
    
    specs$dist <- "gaussian"
    specs$distribution <- "Gaussian"
    
  }
  
  return(specs)
}

# Output functions
.regBoostTable <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regBoostTable"]])) return()
  
  # Create table and bind to jaspResults
  regBoostTable <- createJaspTable(title = "Boosting Regression Model Summary")
  jaspResults[["regBoostTable"]] <- regBoostTable
  jaspResults[["regBoostTable"]]$dependOnOptions(c("target", "predictors", "indicator", "applyModel", "noOfTrees",
                                                   "numberOfTrees", "shrinkage", "shrinkPar", "int.depth",
                                                   "int.depth.parameter", "modelOptimization", "cvFolds", "nNode",
                                                   "nNodeSpec", "dataTrain", "percentageDataTraining", "bag.fraction",
                                                   "bag.fraction.spec", "dist", "seedBox", "seed", "NAs"))
  
  # Add column info
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regBoostTable$addColumnInfo(name = "testError" ,  title = "Test Set MSE"  , type = "number", format = "sf:4")
  }
  regBoostTable$addColumnInfo(name = "ntrees"      ,  title = "Trees"         , type = "integer"                )
  regBoostTable$addColumnInfo(name = "shrinkage"   ,  title = "Shrinkage"     , type = "number", format = "sf:4")
  regBoostTable$addColumnInfo(name = "intDepth"    ,  title = "Int. Depth"    , type = "integer"                )
  regBoostTable$addColumnInfo(name = "minObsInNode",  title = "Min. Obs. Node", type = "integer"                )
  regBoostTable$addColumnInfo(name = "ntrain"      ,  title = "n (Train)"     , type = "integer"                )
  regBoostTable$addColumnInfo(name = "ntest"       ,  title = "n (Test)"      , type = "integer"                )
  
  # Add data per column
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regBoostTable[["testError"]]  <- if (ready) regBoostResults$testError else "."
  }
  regBoostTable[["ntrees"]]       <- if (ready) regBoostResults$optTrees                else "."
  regBoostTable[["shrinkage"]]    <- if (ready) regBoostResults$res$shrinkage           else "."
  regBoostTable[["intDepth"]]     <- if (ready) regBoostResults$res$interaction.depth   else "."
  regBoostTable[["minObsInNode"]] <- if (ready) regBoostResults$spec$nNode              else "."
  regBoostTable[["ntrain"]]       <- if (ready) regBoostResults$res$nTrain              else "."
  regBoostTable[["ntest"]]        <- if (ready) length(regBoostResults$data$testTarget) else "."
  
}

.regBoostRelInfTable <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$regBoostRelInfTable || !is.null(jaspResults[["regBoostRelInfTable"]])) return()
  
  # Create table
  regBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  jaspResults[["regBoostRelInfTable"]] <- regBoostRelInfTable
  jaspResults[["regBoostRelInfTable"]]$copyDependenciesFromJaspObject(jaspResults[["regBoostTable"]])
  jaspResults[["regBoostRelInfTable"]]$dependOnOptions("regBoostRelInfTable")
  
  # Add column info
  regBoostRelInfTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number", format = "sf:4")
  
  # Add data per column
  regBoostRelInfTable[["predictor"]] <- if(ready) .unv(regBoostResults$relInf$var) else "."
  regBoostRelInfTable[["relIn"]]     <- if(ready) regBoostResults$relInf$rel.inf   else "."
  
}

.regBoostApplyTable <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (options$applyModel == "noApp" || !is.null(jaspResults[["applyModel"]])) return()
  
  # Create table and bind to jaspResults
  regBoostApplyTable <- createJaspTable(title = "Boosting Model Predictions")
  jaspResults[["regBoostApplyTable"]] <- regBoostApplyTable
  jaspResults[["regBoostApplyTable"]]$copyDependenciesFromJaspObject(jaspResults[["regBoostTable"]])
  jaspResults[["regBoostApplyTable"]]$dependOnOptions("applyModel")
  
  # Add column info
  regBoostApplyTable$addColumnInfo(name = "case",  title = "Case"      , type = "integer")
  regBoostApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "string")
  
  # Add data per column
  regBoostApplyTable[["case"]]  <- if (ready) as.integer(regBoostResults$apply$case)   else "."
  regBoostApplyTable[["pred"]]  <- if (ready) as.character(regBoostResults$apply$pred) else "."
  
}

.regBoostRelInfPlot <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$plotRelInf || !is.null(jaspResults[["regBoostRelInfPlot"]])) return()
  
  relInfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(regBoostResults$relInf, ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Relative Influence"),
    horizontal = TRUE
  )
  
  # Create plot and bind to jaspResults
  regBoostRelInfPlot <- createJaspPlot(plot = relInfPlot, title = "Relative Influence Plot",
                                         width = 500, height = 20 * nrow(regBoostResults$relInf) + 60)
  jaspResults[["regBoostRelInfPlot"]] <- regBoostRelInfPlot
  jaspResults[["regBoostRelInfPlot"]]$dependOnOptions("plotRelInf")
}

.regBoostPlotDeviance <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$plotDeviance || !is.null(jaspResults[["plotDeviance"]])) return()

  deviance <- data.frame(
    trees = 1:regBoostResults$res$n.trees,
    trainError = c(regBoostResults$res$train.error, regBoostResults$res$cv.error),
    what = rep(c("OOB", "CV"), c(length(regBoostResults$res$train.error), length(regBoostResults$res$cv.error)))
    )
  
  plotDeviance <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError, group = what, color = what)) +
      ggplot2::geom_line(size = 1, show.legend = regBoostResults$method != "OOB") +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::ylab("Gaussian Deviance") +
      ggplot2::scale_color_manual(name = "", values = c("OOB" = "gray20", "CV" = "#99c454")) +
      ggplot2::geom_vline(xintercept = regBoostResults$optTrees, color = "lightgray", linetype = "dashed"),
    legend.position = "right"
  )

  # Create plot and bind to jaspResults
  plotDeviance <- createJaspPlot(plot = plotDeviance, title = "Deviance Plot", width = 500, height = 400)
  jaspResults[["plotDeviance"]] <- plotDeviance
  jaspResults[["plotDeviance"]]$copyDependenciesFromJaspObject(jaspResults[["regBoostTable"]])
  jaspResults[["plotDeviance"]]$dependOnOptions("plotDeviance")
}

.regBoostPlotOOBChangeDev <- function(jaspResults, options, regBoostResults, ready, analysisOptions) {
  if (!options$plotOOBChangeDev || !is.null(jaspResults[["regBoostPlotOOBChangeDev"]])) return()
  
  oobDev <- data.frame(trees = 1:regBoostResults$res$n.trees, oobImprove = regBoostResults$res$oobag.improve)
  
  plotOOBChangeDev <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::ylab(paste("OOB Change in ", regBoostResults$spec$distribution, " Deviance")) +
      ggplot2::geom_vline(xintercept = regBoostResults$optTrees, color = "gray20", linetype = "dashed")
  )
  
  # Create plot and bind to jaspResults
  regBoostPlotOOBChangeDev <- createJaspPlot(plot = plotOOBChangeDev,title = "OOB Improvement Plot",
                                               width = 400, height = 400)
  jaspResults[["regBoostPlotOOBChangeDev"]] <- regBoostPlotOOBChangeDev
  jaspResults[["regBoostPlotOOBChangeDev"]]$copyDependenciesFromJaspObject(jaspResults[["regBoostTable"]])
  jaspResults[["regBoostPlotOOBChangeDev"]]$dependOnOptions("plotOOBChangeDev")
}

.regBoostPlotPredPerformance <- function(jaspResults, options, regBoostResults, ready) {
  if (!options$plotPredPerformance || !is.null(jaspResults[["plotPredPerformance"]])) return()
  
  predPerformance <- data.frame(true = regBoostResults$data$testTarget, predicted = regBoostResults$preds)
  limits <- c(round(min(c(floor(predPerformance$true), floor(predPerformance$predicted)))),
              round(max(c(ceiling(predPerformance$true), ceiling(predPerformance$predicted)))))
  
  plotPredPerformance <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = predPerformance, mapping = ggplot2::aes(x = true, y = predicted)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_line(data = data.frame(x = limits, y = limits), mapping = ggplot2::aes(x = x, y = y), 
                         col = "darkred", size = 1) +
      ggplot2::scale_x_continuous("True", limits = limits, labels = scales::comma,
                                  breaks = seq(min(limits), max(limits), length.out = 6)) +
      ggplot2::scale_y_continuous("Predicted", limits = limits, labels = scales::comma,
                                  breaks = seq(min(limits), max(limits), length.out = 6))
  )
  
  plotPredPerformance <- createJaspPlot(plot = plotPredPerformance, title = "Predictive Performance",
                                        width = 400, height = 400)
  
  jaspResults[["plotPredPerformance"]] <- plotPredPerformance
  jaspResults[["plotPredPerformance"]]$position <- 7
  jaspResults[["plotPredPerformance"]]$copyDependenciesFromJaspObject(jaspResults[["regBoostTable"]])
  jaspResults[["plotPredPerformance"]]$dependOnOptions("plotPredPerformance")
}
