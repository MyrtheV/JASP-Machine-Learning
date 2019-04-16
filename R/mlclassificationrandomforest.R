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

MLClassificationRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  if (options$target == "") options$target <- NULL
  dataset <- .readDataSetToEnd(columns.as.factor = options$target, columns = options$predictors,
                               columns.as.nommnomm = options$indicator)
  
  # Check if results can be computed
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .classRanForErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) classRanForResults <- .classRanForComputeResults(jaspResults, dataset, options)
  
  # Output tables
  .classRanForTable(             jaspResults, options, classRanForResults, ready)
  .classRanForConfTable(         jaspResults, options, classRanForResults, ready, dataset)
  .classRanForTableVarImportance(jaspResults, options, classRanForResults, ready)
  .classRanForApplyTable(        jaspResults, options, classRanForResults, ready)
  
  # Output plots
  if (ready) .classRanForPlotVarImp1(          jaspResults, options, classRanForResults, ready)
  if (ready) .classRanForPlotVarImp2(          jaspResults, options, classRanForResults, ready)
  if (ready) .classRanForPlotTreesVsModelError(jaspResults, options, classRanForResults, ready)
  
  return()
}

# Check for errors
.classRanForErrorHandling <- function(dataset, options) {
  
  # Error Check 1: 0 observations for the target variable
  .hasErrors(
    dataset = dataset, 
    perform = "run", 
    type = c('observations', 'variance', 'infinity'),
    all.target = options$target,
    observations.amount = '< 1',
    exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: Apply indicator should not have any missing values (consist of 0s and 1s)
  .hasErrors(
    dataset = dataset, 
    perform = "run", 
    type = c('observations', 'variance', 'infinity'),
    all.target = options$indicator,
    observations.amount = nrow(dataset),
    exitAnalysisIfErrors = TRUE)
  
}

# Compute results
.classRanForComputeResults <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["stateRegRanForResults"]])) return (jaspResults[["stateRegRanForResults"]]$object)
  
  # Create results object
  results <- list()
  
  results[["spec"]] <- .classRanForCalcSpecs(dataset, options)
  results[["res"]] <- list()
  
  # Set seed	
  if (options$seedBox) set.seed(options$seed)
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  dataset <- na.omit(dataset)
  
  # Splitting the data into training set, test set, and application set
  if (options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1) # apply indicator doesn't work yet
    idxModel <- which(dataset[, indicator] == 0) # something goes wrong with making these indices
    
    applyData <- dataset[idxApply, preds, drop = FALSE]
    modelData <- dataset[idxModel, -indicator]
    
  } else {
    
    modelData <- dataset
    
  }
  
  idxTrain <- sample(1:nrow(modelData), floor(options$dataTrain * nrow(modelData)))
  idxTest <- (1:nrow(modelData))[-idxTrain]
  
  xTrain <- modelData[idxTrain, preds, drop = FALSE]
  yTrain <- modelData[idxTrain, target]
  xTest <- modelData[idxTest, preds, drop = FALSE]
  yTest <- modelData[idxTest, target]
  
  # Run Random Forest
  results[["res"]] <- randomForest::randomForest(x = xTrain, y = yTrain, xtest = xTest, ytest = yTest,
                                                 ntree = options$noOfTrees, mtry = results$spec$noOfPredictors,
                                                 sampsize = ceiling(options$bagFrac*nrow(dataset)),
                                                 importance = TRUE, keep.forest = TRUE)
  
  results[["data"]] <- list(xTrain = xTrain, yTrain = yTrain, xTest = xTest, yTest = yTest)
  
  # Calculating test error and making confusion table
  results[["preds"]]     <- results$res$test$predicted
  results[["testError"]] <- mean(yTest != as.character(results$preds))
  results[["confTable"]] <- table("Pred" = factor(results$preds, levels = levels(results$data$yTest)),
                                  "True" = factor(results$data$yTest))
  
  # Making a variable importance table
  results[["varImp"]] <- plyr::arrange(data.frame(
    Variable         = .unv(as.factor(names(results$res$importance[,1]))),
    MeanIncrMSE      = results$res$importance[, 1],
    TotalDecrNodeImp = results$res$importance[, 2]
  ), -TotalDecrNodeImp)
  
  if(options$indicator != "") results[["apply"]] <- predict(results$res, applyData, type = "class")
  
  # Save results to state
  jaspResults[["stateclassRanForResults"]] <- createJaspState(results)
  jaspResults[["stateclassRanForResults"]]$dependOn(options =c("target", "predictors", "indicator", "noOfTrees",
                                                             "noOfPredictors", "numberOfPredictors", "dataTrain",
                                                             "bagFrac", "seedBox", "seed", "applyModel"))
  
  return(results)
}

.classRanForCalcSpecs <- function(dataset, options) {
  specs <- list()
  
  # Setting the number of variables considered at each split
  if (options$noOfPredictors == "manual") {
    specs$noOfPredictors <- as.integer(options$numberOfPredictors)
  } else {
    specs$noOfPredictors <- if (!is.null(options$target) && !is.factor(options$target)) 
      max(floor(length(.v(options$predictors))/3), 1) else floor(sqrt(length(.v(options$predictors))))
  }
  
  return(specs)
}

# Output functions
.classRanForTable <- function(jaspResults, options, classRanForResults, ready) {
  if (!is.null(jaspResults[["classRanForTable"]])) return()
  
  # Create table and bind to jaspResults
  classRanForTable <- createJaspTable(title = "Random Forest Classification Model Summary")
  jaspResults[["classRanForTable"]] <- classRanForTable
  jaspResults[["classRanForTable"]]$position <- 1
  jaspResults[["classRanForTable"]]$dependOn(options =c("target", "predictors", "indicator", "noOfTrees",
                                                      "noOfPredictors", "numberOfPredictors", "dataTrain",
                                                      "bagFrac", "seedBox", "seed", "applyModel"))
  
  # Add column info
  if(options$dataTrain < 1){
    classRanForTable$addColumnInfo(name = "testError",  title = "Test Set Error", type = "number", format = "sf:4")
  }
  classRanForTable$addColumnInfo(name = "oobError", title = "OOB Error", type = "number", format = "sf:4")
  classRanForTable$addColumnInfo(name = "ntrees"  , title = "Trees"    , type = "integer")
  classRanForTable$addColumnInfo(name = "mtry"    , title = "m"        , type = "integer")
  classRanForTable$addColumnInfo(name = "nTrain"  , title = "n (Train)", type = "integer")
  classRanForTable$addColumnInfo(name = "nTest"   , title = "n (Test)" , type = "integer")
  
  # Add data per column
  classRanForTable[["testError"]]  <- if (ready && options$dataTrain < 1)
    mean(classRanForResults$preds != classRanForResults$data$yTest)                   else "."
  classRanForTable[["oobError"]]   <- if (ready)
    classRanForResults$res$err.rate[length(classRanForResults$res$err.rate)]          else "."
  classRanForTable[["ntrees"]]     <- if (ready) classRanForResults$res$ntree         else "."
  classRanForTable[["mtry"]]       <- if (ready) classRanForResults$res$mtry          else "."
  classRanForTable[["nTrain"]]     <- if (ready) nrow(classRanForResults$data$xTrain) else "."
  classRanForTable[["nTest"]]      <- if (ready) nrow(classRanForResults$data$xTest)  else "."
  
}

.classRanForConfTable <- function(jaspResults, options, classBoostResults, ready, dataset) {
  if (!options$classRanForConfTable || !is.null(jaspResults[["classRanForConfTable"]])) return()
  
  # Create table and bind to jaspResults
  classRanForConfTable <- createJaspTable(title = "Confusion Table")
  jaspResults[["classRanForConfTable"]] <- classRanForConfTable
  jaspResults[["classRanForConfTable"]]$position <- 2
  jaspResults[["classRanForConfTable"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["classRanForConfTable"]]$dependOn(options ="classRanForConfTable")
  
  target <- .v(options$target)
  
  if (ready) {
    
    classRanForConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    classRanForConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    classRanForConfTable[["pred_name"]] <- c("Predicted", rep("", nrow(classBoostResults$confTable) - 1))
    classRanForConfTable[["varname_pred"]] <- colnames(classBoostResults$confTable)
    
    for (i in 1:length(rownames(classBoostResults$confTable))) {
      
      name <- paste("varname_obs", i, sep = "")
      classRanForConfTable$addColumnInfo(name = name, title = as.character(rownames(classBoostResults$confTable)[i]),
                                        type = "integer", overtitle = "Observed")
      classRanForConfTable[[name]] <- classBoostResults$confTable[, i] 
      
    }
    
  } else if (!is.null(options$target) && !ready) {
    
    classRanForConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    classRanForConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    classRanForConfTable[["pred_name"]] <- c("Predicted", rep("", length(unique(dataset[, target])) - 1))
    classRanForConfTable[["varname_pred"]] <- levels(dataset[, target])
    
    for (i in 1:length(unique(dataset[, target]))) {
      
      name <- paste("varname_obs", i, sep = "")
      classRanForConfTable$addColumnInfo(name = name, title = as.character(levels(dataset[, target])[i]),
                                        type = "integer", overtitle = "Observed")
      classRanForConfTable[[name]] <- rep(".", length(unique(dataset[, target])))
      
    }
    
  } else {
    
    classRanForConfTable$addColumnInfo(name = "pred_name"    , title = "" , type = "string")
    classRanForConfTable$addColumnInfo(name = "varname_pred" , title = "" , type = "string")
    classRanForConfTable$addColumnInfo(name = "varname_obs1", title = ".", type = "integer")
    classRanForConfTable$addColumnInfo(name = "varname_obs2", title = ".", type = 'integer')
    
    classRanForConfTable[["pred_name"]] <- c("Predicted", "")
    classRanForConfTable[["varname_pred"]] <- rep(".", 2)
    classRanForConfTable[["varname_obs1"]] <- rep("", 2)
    classRanForConfTable[["varname_obs2"]] <- rep("", 2)
    
  }
  
}

.classRanForTableVarImportance <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$tableVariableImportance || !is.null(jaspResults[["tableVarImp"]])) return()
  
  # Create table
  classRanForTableVarImp <- createJaspTable(title = "Variable Importance")
  jaspResults[["tableVarImp"]] <- classRanForTableVarImp
  jaspResults[["tableVarImp"]]$position <- 3
  jaspResults[["tableVarImp"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["tableVarImp"]]$dependOn(options =c("tableVariableImportance"))
  
  # Add column info
  classRanForTableVarImp$addColumnInfo(name = "predictor",  title = " ", type = "string")
  classRanForTableVarImp$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number",
                                       format = "sf:4")
  classRanForTableVarImp$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity", type = "number",
                                       format = "sf:4")
  
  # Ordering the variables according to their mean decrease in accuracy
  if(ready) varImpOrder <- sort(classRanForResults$res$importance[,1], decr = TRUE, index.return = TRUE)$ix
  
  # Add data per column
  classRanForTableVarImp[["predictor"]] <- if(ready) .unv(.v(classRanForResults$varImp$Variable)) else "."
  classRanForTableVarImp[["MDiA"]]      <- if(ready) classRanForResults$varImp$MeanIncrMSE        else "."
  classRanForTableVarImp[["MDiNI"]]     <- if(ready) classRanForResults$varImp$TotalDecrNodeImp   else "."
  
}

.classRanForApplyTable <- function(jaspResults, options, classRanForResults, ready) {
  if (!is.null(jaspResults[["classRanForApplyTable"]])) return()
  if (options$indicator == "") return()
  
  # Create table and bind to jaspResults
  classRanForApplyTable <- createJaspTable(title = "Random Forest Model Predictions")
  jaspResults[["classRanForApplyTable"]] <- classRanForApplyTable
  jaspResults[["classRanForApplyTable"]]$position <- 4
  jaspResults[["classRanForApplyTable"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  
  # Add column info
  classRanForApplyTable$addColumnInfo(name = "row",  title = "Row", type = "integer")
  classRanForApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  classRanForApplyTable[["row"]]  <- if (ready) as.numeric(rownames(as.data.frame(classRanForResults$apply))) else "."
  classRanForApplyTable[["pred"]]  <- if (ready) as.numeric(classRanForResults$apply) else "."
  
}

.classRanForPlotVarImp1 <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotVarImp1) return()
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classRanForResults$varImp, ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Mean Decrease in Accuracy"),
    horizontal = TRUE
  )
  
  jaspResults[['varImpPlot1']] <- createJaspPlot(plot = varImpPlot1, title = "Mean Decrease in Accuracy per Variable",
                                                 width = 400, height = 20 * nrow(classRanForResults$varImp) + 60)
  
  jaspResults[["varImpPlot1"]]$position <- 5
  jaspResults[["varImpPlot1"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["varImpPlot1"]]$dependOn(options ="plotVarImp1")
}

.classRanForPlotVarImp2 <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotVarImp2) return()
  
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classRanForResults$varImp, ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), 
                                                            y = TotalDecrNodeImp)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Total Increase in Node Purity"),
    horizontal = TRUE
  )
  
  jaspResults[['varImpPlot2']] <- createJaspPlot(plot = varImpPlot2, 
                                                 title = "Total Increase in Node Purity per Variable",
                                                 width = 400, height = 20 * nrow(classRanForResults$varImp) + 60)
  
  jaspResults[["varImpPlot2"]]$position <- 6
  jaspResults[["varImpPlot2"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["varImpPlot2"]]$dependOn(options ="plotVarImp2")
}

.classRanForPlotTreesVsModelError <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotTreesVsModelError) return()
  
  treesMSE <- dplyr::tibble(
    trees = 1:length(classRanForResults$res$err.rate[,1]),
    error = classRanForResults$res$err.rate[,1]
  )
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::scale_x_continuous(name = "Trees", labels = scales::comma) +
      ggplot2::scale_y_continuous(name = "OOB Classification Error")
  )
  
  jaspResults[['plotTreesVsModelError']] <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs. Model Error",
                                                   width = 400, height = 400)
  
  jaspResults[["plotTreesVsModelError"]]$position <- 7
  jaspResults[["plotTreesVsModelError"]]$dependOn(optionsFromObject =jaspResults[["classRanForTable"]])
  jaspResults[["plotTreesVsModelError"]]$dependOn(options ="plotTreesVsModelError")
}
