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

MLClassificationBoosting <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Boosting Classification"
  
  # Read dataset
  dataset <- .classBoostReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .classBoostErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) classBoostResults <- .classBoostComputeResults(jaspResults, dataset, options)
  
  # Output tables
  .classBoostTable(           jaspResults, options, classBoostResults, ready)
  .classBoostConfTable(       jaspResults, options, classBoostResults, ready, dataset)
  .classBoostRelInfTable(     jaspResults, options, classBoostResults, ready)
  .classBoostApplyTable(      jaspResults, options, classBoostResults, ready)
  
  # Output plots
  if (ready) .classBoostRelInfPlot(      jaspResults, options, classBoostResults)
  if (ready) .classBoostPlotDeviance(    jaspResults, options, classBoostResults)
  if (ready) .classBoostPlotOOBChangeDev(jaspResults, options, classBoostResults)
  
  return()
}

# Read dataset
.classBoostReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.factor = c(options$target, options$indicator), columns = options$predictors)
  
  return(data)
}

# Error checking
.classBoostErrorHandling <- function(dataset, options) {
  
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: Apply indicator should not have any missing values (consist of 0s and 1s)
  if (options$indicator != "") {
    .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
               all.target = options$indicator, observations.amount = nrow(dataset), exitAnalysisIfErrors = TRUE)
  }
  
  # Error Check 3: There should be at least 2 predictors (otherwise gbm() complains)
  if (options$target != "" && ncol(dataset) < 3L){
    JASP:::.quitAnalysis("Please provide at least 2 predictors.")
  }
  
  # Error Check 4: The target variable should have at least 2 classes
  if (nlevels(dataset[, .v(options$target)]) < 2){
    JASP:::.quitAnalysis("The target variable should have at least 2 classes.")
  }
  
  # Error Check 5: If target values should be imputed, there have to be missing values in the target
  # if (options$applyModel == "applyImpute" && sum(is.na(dataset[, .v(options$target)])) < 1) {
  #   JASP:::.quitAnalysis("To apply model to missing values in target, please provide observations that have missing 
  #   values in the target variable.")
  # }
  
}

# Compute results
.classBoostComputeResults <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["stateClassBoostResults"]])) return (jaspResults[["stateClassBoostResults"]]$object)
  
  # Create results object and add options
  results <- list()
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs: omit all rows that contain at least one missing value
  if (sum(is.na(dataset)) > 0) {
    
    # Save target NA observations to make predictions later
    if (options$applyModel == "applyImpute") {

      idxApply <- which(is.na(dataset[, target]))
      predImpute <- na.omit(dataset[idxApply, preds])

    } else {
      
      dataset <- na.omit(dataset) 
      
    }
    
  }
  
  # Splitting the data into training set, test set, and application set in case indicator is provided
  if (options$applyModel == "applyIndicator" && options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1)
    idxModel <- which(dataset[, indicator] == 0)
    
    applyData <- dataset[idxApply, preds, drop = FALSE]
    modelData <- dataset[idxModel, ]
    
    } else {
    
      modelData <- dataset
    
  }
  
  # Set seed	
  if (options$seedBox) set.seed(options$seed)
  
  # Compile training and test data
  idxTrain <- sample(1:nrow(modelData), floor(options$dataTrain * nrow(modelData)))
  idxTest <- (1:nrow(modelData))[-idxTrain]
  
  trainData <- modelData[idxTrain, c(preds, target), drop = FALSE]
  testData <- modelData[idxTest, preds, drop = FALSE]
  testTarget <- modelData[idxTest, target]
  
  # Prepare Boosting
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  if(options$modelOpt == "cv") {
    
    cv.folds <- options$cvFolds
    results[["method"]] <- "cv"
    
  } else {
    
    cv.folds <- 0
    results[["method"]] <- "OOB"
    
  }
  
  # Run Boosting
  results[["res"]] <- gbm::gbm(formula = formula, data = trainData, n.trees = options$noOfTrees,
                               shrinkage = options$shrinkage, interaction.depth = options$intDepth,
                               cv.folds = cv.folds, bag.fraction = options$bagFrac, n.minobsinnode = options$nNode,
                               distribution = "multinomial")

  results[["data"]] <- list(trainData = trainData, testData = testData, testTarget = testTarget)
  results[["relInf"]] <- summary(results$res, plot = FALSE)
  
  if (options$modelOpt != "noOpt") {
    results[["optTrees"]] <- gbm::gbm.perf(results$res, plot.it = FALSE, method = results$method)[1]
  } else {
    results[["optTrees"]] <- options$noOfTrees
  }
  
  # Derive test set predictions and calculate test error rate
  prob <- gbm::predict.gbm(results$res, newdata = testData, n.trees = results$optTrees, type = "response")
  results[["preds"]] <- colnames(prob)[apply(prob, 1, which.max)]
    
  results[["testError"]] <- mean(testTarget != as.character(results$preds))
  results[["confTable"]] <- table("Pred" = factor(results$preds, levels = levels(results$data$testTarget)),
                                  "True" = factor(results$data$testTarget))

  # Apply model to new data if requested
  if(options$applyModel == "applyIndicator" && options$indicator != "") {
    
    applyProb <- gbm::predict.gbm(results$res, newdata = applyData, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- data.frame(case = idxApply, pred = colnames(applyProb)[apply(applyProb, 1, which.max)])
    
  } else if (options$applyModel == "applyImpute") {

    applyProb <- gbm::predict.gbm(results$res, newdata = predImpute, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- data.frame(case = idxApply, pred = colnames(applyProb)[apply(applyProb, 1, which.max)])
    
  }
  
  # Save results to state
  jaspResults[["stateClassBoostResults"]] <- createJaspState(results)
  jaspResults[["stateClassBoostResults"]]$dependOnOptions(c("target", "predictors", "indicator", "applyModel",
                                                            "noOfTrees", "shrinkage", "intDepth", "modelOpt",
                                                            "cvFolds", "nNode", "dataTrain", "bagFrac", "seedBox",
                                                            "seed"))
  
  return(results)
}

# Output functions
.classBoostTable <- function(jaspResults, options, classBoostResults, ready) {
  if (!is.null(jaspResults[["classBoostTable"]])) return()
  
  # Create table and bind to jaspResults
  classBoostTable <- createJaspTable(title = "Boosting Classification Model Summary")
  jaspResults[["classBoostTable"]] <- classBoostTable
  jaspResults[["classBoostTable"]]$position <- 1
  jaspResults[["classBoostTable"]]$dependOnOptions(c("target", "predictors", "indicator", "applyModel", "noOfTrees",
                                                     "shrinkage", "int.depth", "modelOpt", "cvFolds", "nNode",
                                                     "dataTrain", "bagFrac", "seedBox", "seed"))
  
  # Add column info
  if (options$dataTrain != 1) {
    classBoostTable$addColumnInfo(name = "testError" ,  title = "Test Set Error", type = "number", format = "sf:4")
  }
  classBoostTable$addColumnInfo(name = "ntrees"      ,  title = "Trees"         , type = "integer"                )
  classBoostTable$addColumnInfo(name = "shrinkage"   ,  title = "Shrinkage"     , type = "number", format = "sf:4")
  classBoostTable$addColumnInfo(name = "intDepth"    ,  title = "Int. Depth"    , type = "integer"                )
  classBoostTable$addColumnInfo(name = "minObsInNode",  title = "Min. Obs. Node", type = "integer"                )
  classBoostTable$addColumnInfo(name = "ntrain"      ,  title = "n (Train)"     , type = "integer"                )
  classBoostTable$addColumnInfo(name = "ntest"       ,  title = "n (Test)"      , type = "integer"                )
  
  # Add data per column
  if (options$dataTrain != 1) {
    classBoostTable[["testError"]]  <- if (ready) classBoostResults$testError               else "."
  }
  classBoostTable[["ntrees"]]       <- if (ready) classBoostResults$optTrees                else "."
  classBoostTable[["shrinkage"]]    <- if (ready) classBoostResults$res$shrinkage           else "."
  classBoostTable[["intDepth"]]     <- if (ready) classBoostResults$res$interaction.depth   else "."
  classBoostTable[["minObsInNode"]] <- if (ready) options$nNode                             else "."
  classBoostTable[["ntrain"]]       <- if (ready) classBoostResults$res$nTrain              else "."
  classBoostTable[["ntest"]]        <- if (ready) length(classBoostResults$data$testTarget) else "."

}

.classBoostConfTable <- function(jaspResults, options, classBoostResults, ready, dataset) {
  if (!options$classBoostConfTable || !is.null(jaspResults[["classBoostConfTable"]])) return()
  
  # Create table and bind to jaspResults
  classBoostConfTable <- createJaspTable(title = "Confusion Table")
  jaspResults[["classBoostConfTable"]] <- classBoostConfTable
  jaspResults[["classBoostConfTable"]]$position <- 2
  jaspResults[["classBoostConfTable"]]$copyDependenciesFromJaspObject(jaspResults[["classBoostTable"]])
  jaspResults[["classBoostConfTable"]]$dependOnOptions("classBoostConfTable")
  
  target <- .v(options$target)

  if (ready) {

    classBoostConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    classBoostConfTable[["pred_name"]] <- c("Predicted", rep("", nrow(classBoostResults$confTable) - 1))
    classBoostConfTable[["varname_pred"]] <- colnames(classBoostResults$confTable)
    
    for (i in 1:length(rownames(classBoostResults$confTable))) {
      
      name <- paste("varname_obs", i, sep = "")
      classBoostConfTable$addColumnInfo(name = name, title = as.character(rownames(classBoostResults$confTable)[i]),
                                        type = "integer", overtitle = "Observed")
      classBoostConfTable[[name]] <- classBoostResults$confTable[, i] 
      
    }

  } else if (options$target != "" && !ready) {
    
    classBoostConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    classBoostConfTable[["pred_name"]] <- c("Predicted", rep("", length(unique(dataset[, target])) - 1))
    classBoostConfTable[["varname_pred"]] <- levels(dataset[, target])
    
    for (i in 1:length(unique(dataset[, target]))) {
      
      name <- paste("varname_obs", i, sep = "")
      classBoostConfTable$addColumnInfo(name = name, title = as.character(levels(dataset[, target])[i]),
                                        type = "integer", overtitle = "Observed")
      classBoostConfTable[[name]] <- rep(".", length(unique(dataset[, target])))
      
    }

  } else {

    classBoostConfTable$addColumnInfo(name = "pred_name"    , title = "" , type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_pred" , title = "" , type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_obs1", title = ".", type = "integer")
    classBoostConfTable$addColumnInfo(name = "varname_obs2", title = ".", type = 'integer')
    
    classBoostConfTable[["pred_name"]] <- c("Predicted", "")
    classBoostConfTable[["varname_pred"]] <- rep(".", 2)
    classBoostConfTable[["varname_obs1"]] <- rep("", 2)
    classBoostConfTable[["varname_obs2"]] <- rep("", 2)

  }
  
}

.classBoostRelInfTable <- function(jaspResults, options, classBoostResults, ready) {
  if (!options$classBoostRelInfTable || !is.null(jaspResults[["tableVarImp"]])) return()
  
  # Create table
  classBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  jaspResults[["classBoostRelInfTable"]] <- classBoostRelInfTable
  jaspResults[["classBoostRelInfTable"]]$position <- 3
  jaspResults[["classBoostRelInfTable"]]$copyDependenciesFromJaspObject(jaspResults[["classBoostTable"]])
  jaspResults[["classBoostRelInfTable"]]$dependOnOptions("classBoostRelInfTable")
  
  # Add column info
  classBoostRelInfTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  classBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number", format = "sf:4")
  
  # Add data per column
  classBoostRelInfTable[["predictor"]]  <- if(ready) .unv(classBoostResults$relInf$var) else "."
  classBoostRelInfTable[["relIn"]]  <- if(ready) classBoostResults$relInf$rel.inf else "."
  
}

.classBoostApplyTable <- function(jaspResults, options, classBoostResults, ready) {
  if (options$applyModel == "noApp" || !is.null(jaspResults[["applyModel"]])) return()
  
  # Create table and bind to jaspResults
  classBoostApplyTable <- createJaspTable(title = "Boosting Model Predictions")
  jaspResults[["classBoostApplyTable"]] <- classBoostApplyTable
  jaspResults[["classBoostApplyTable"]]$position <- 4
  jaspResults[["classBoostApplyTable"]]$copyDependenciesFromJaspObject(jaspResults[["classBoostTable"]])
  jaspResults[["classBoostApplyTable"]]$dependOnOptions("applyModel")
  
  # Add column info
  classBoostApplyTable$addColumnInfo(name = "case",  title = "Case", type = "integer")
  classBoostApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "string")
  
  # Add data per column
  classBoostApplyTable[["case"]]  <- if (ready) as.integer(classBoostResults$apply$case)   else "."
  classBoostApplyTable[["pred"]]  <- if (ready) as.character(classBoostResults$apply$pred) else "."
  
}

.classBoostRelInfPlot <- function(jaspResults, options, classBoostResults) {
  if (!options$plotRelInf || !is.null(jaspResults[["classBoostRelInfPlot"]])) return()
  
  relInfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classBoostResults$relInf, ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
      ggplot2::geom_bar(stat = "identity", fill = "gray", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Relative Influence"),
    horizontal = TRUE
  )
  
  # Create plot and bind to jaspResults
  classBoostRelInfPlot <- createJaspPlot(plot = relInfPlot, title = "Relative Influence Plot",
                                         width = 500, height = 20 * nrow(classBoostResults$relInf) + 60)
  
  jaspResults[["classBoostRelInfPlot"]] <- classBoostRelInfPlot
  jaspResults[["classBoostRelInfPlot"]]$position <- 5
  jaspResults[["classBoostRelInfPlot"]]$copyDependenciesFromJaspObject(jaspResults[["classBoostTable"]])
  jaspResults[["classBoostRelInfPlot"]]$dependOnOptions("plotRelInf")
}

.classBoostPlotDeviance <- function(jaspResults, options, classBoostResults) {
  if (!options$plotDeviance || !is.null(jaspResults[["plotDeviance"]])) return()
  
  deviance <- data.frame(
    trees = 1:classBoostResults$res$n.trees,
    trainError = c(classBoostResults$res$train.error, classBoostResults$res$cv.error),
    what = rep(c("OOB", "CV"), c(length(classBoostResults$res$train.error), length(classBoostResults$res$cv.error)))
    )
  
  if (nlevels(classBoostResults$data$testTarget) > 2L) {
    ylab <- "Multinomial Deviance"
  } else {
    ylab <- "Binomial Deviance"
  }
  
  plotDeviance <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError, group = what, color = what)) +
      ggplot2::geom_line(size = 1, show.legend = classBoostResults$method != "OOB") +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_color_manual(name = "", values = c("OOB" = "gray20", "CV" = "#99c454")) +
      ggplot2::geom_vline(xintercept = classBoostResults$optTrees, color = "gray20", linetype = "dashed"),
    legend.position = "right"
  )

  # Create plot and bind to jaspResults
  plotDeviance <- createJaspPlot(plot = plotDeviance, title = "Deviance Plot", width = 500, height = 400)
  jaspResults[["plotDeviance"]] <- plotDeviance
  jaspResults[["plotDeviance"]]$position <- 6
  jaspResults[["plotDeviance"]]$copyDependenciesFromJaspObject(jaspResults[["classBoostTable"]])
  jaspResults[["plotDeviance"]]$dependOnOptions("plotDeviance")
}

.classBoostPlotOOBChangeDev <- function(jaspResults, options, classBoostResults) {
  if (!options$plotOOBChangeDev || !is.null(jaspResults[["classBoostPlotOOBChangeDev"]])) return()
    
  oobDev <- data.frame(trees = 1:classBoostResults$res$n.trees, oobImprove = classBoostResults$res$oobag.improve)
  
  if (nlevels(classBoostResults$data$testTarget) > 2L) {
    ylab <- "OOB Change in \n Multinomial Deviance"
  } else {
    ylab <- "OOB Change in \n Binomial Deviance"
  }
  
  plotOOBChangeDev <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::ylab(ylab) +
      ggplot2::geom_vline(xintercept = classBoostResults$optTrees, color = "gray20", linetype = "dashed")
    )
  
  # Create plot and bind to jaspResults
  classBoostPlotOOBChangeDev <- createJaspPlot(plot = plotOOBChangeDev,title = "OOB Improvement Plot",
                                               width = 400, height = 400)
  
  jaspResults[["classBoostPlotOOBChangeDev"]] <- classBoostPlotOOBChangeDev
  jaspResults[["classBoostPlotOOBChangeDev"]]$position <- 7
  jaspResults[["classBoostPlotOOBChangeDev"]]$copyDependenciesFromJaspObject(jaspResults[["classBoostTable"]])
  jaspResults[["classBoostPlotOOBChangeDev"]]$dependOnOptions("plotOOBChangeDev")
}
