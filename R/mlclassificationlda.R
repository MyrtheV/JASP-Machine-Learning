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

MLClassificationLDA <- function(jaspResults, dataset, options, ...) {
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  if (ready) {
    
    # Read dataset
    dataset <- .classLdaReadData(dataset, options)
    
    # Error checking
    #.classLdaCheckErrors(dataset, options, ready)
  }
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) classLdaResults <- .classLdaComputeResults(jaspResults, dataset, options)
  
  # Tables 
  .ldaTableMean(jaspResults, options, classLdaResults, dataset, ready)
  .ldaTableMain(jaspResults, options, classLdaResults, ready)
  .ldaConfTable(jaspResults, options, classLdaResults, dataset, ready) 
  .ldaCoefTable(jaspResults, options, classLdaResults, dataset, ready)
  .ldaPriorTable(jaspResults, options, classLdaResults, dataset, ready)
  .modelContainer(jaspResults, options, classLdaResults, dataset, ready)

  return()    
}

# Read dataset
.classLdaReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.factor = c(options$target, options$indicator), columns = options$predictors)
  
  return(data)
}

# Compute results 
.classLdaComputeResults <- function(jaspResults, dataset, options){
  
  if (!is.null(jaspResults[["stateClassLdaResults"]])) return (jaspResults[["stateClassLdaResults"]]$object)
  
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
  
  # Prepare LDA  
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  if (options$modelOpt == "validationLeaveOneOut") {
    
    CV <- TRUE
    results[["method"]] <- "Leave-one-out cross-validation"
    
  } else {
    
    CV <- FALSE
    results[["method"]] <- "None"
    
  }
  
  if (options$estimationMethod == "moment") {
    
    method <- "moment"
    results[["method"]] <- "Standard estimators of mean and variance"
    
  } else if (options$estimationMethod == "mle"){
    
    method <- "mle"
    results[["method"]] <- "MLE's"
    
  } else if (options$estimationMethod == "CovMve") {
    
    method <- "mve"
    results[["method"]] <- "cov.mve"
    
  } else {
    
    method <- "t"   # df should be added 
    results[["method"]] <- "Robust estimates based on a t distribution"
    
  }
  
  #if (options$priorSetting == "manual") {
  
  #  prior <- options$manualprior 
  #  results[["method"]] <- "Manual prior"
  
  #  } else {
  
  #   prior <- unspecified
  
  
  #  }
  
  # Run LDA 
  results[["res"]] <- MASS::lda(formula = formula, data = trainData, method = method, CV = CV) # prior is missing, nu too

  results[["data"]] <- list(trainData = trainData, testData = testData, testTarget = testTarget) 
  results[["relInf"]] <- summary(results$res, plot = FALSE)
  results[["meanTable"]] <- as.table(results$res$means) 
  
  # Predictions 
  prob <- stats::predict(results$res, newdata = testData)
  results[["preds"]] <- prob$class
  
  results[["ntest"]] <- length(results$preds)
  results[["testError"]] <- mean(testTarget != as.character(results$preds))
  results[["confTable"]] <- table("Pred" = factor(results$preds, levels = levels(results$data$testTarget)),
                                  "True" = factor(results$data$testTarget))
  
  # Apply model to new data (indicator)
  if(options$applyModel == "applyIndicator" && options$indicator != "") {
    
    applyProb <- stats::predict(results$res, newdata = applyData)
    results[["apply"]] <- data.frame(case = idxApply, pred = applyProb$class)
    
  } else if (options$applyModel == "applyImpute") {
    
    applyProb <- stats::predict(results$res, newdata = predImpute)
    results[["apply"]] <- data.frame(case = idxApply, pred = applyProb$class)
    
  }
  
  # Save results to state
  jaspResults[["stateClassLdaResults"]] <- createJaspState(results)
  jaspResults[["stateClassLdaResults"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel", # prior nog niet erbij
                                                             "modelOpt", "validationLeaveOneOut", "estimationMethod", "moment", "mle", "covMve", 
                                                             "dataTrain", "seedBox",
                                                             "seed"))
  
  return(results)
  
}

.modelContainer <- function(jaspResults, options, classLdaResults, dataset, ready){
  modelContainer <- createJaspContainer(title = "Training Information")  

  jaspResults[["modelContainer"]] <- modelContainer 
  modelContainer$dependOn(options = c("classLdaMeanTable"))
}
  
# Compute tables 
  .ldaTableMean <- function(jaspResults, options, classLdaResults, dataset, ready){
    if (!is.null(jaspResults[["ldaTableMean"]]) || !options$classLdaMeanTable) return()

    # Create table 
    ldaTableMean <- createJaspTable(title= "Group Means")
    ldaTableMean$dependOn(options = "classLdaMeanTable", optionsFromObject = jaspResults[["stateClassLdaResults"]])
    
    # Add column info 
    ldaTableMean$addColumnInfo(name = "target_level", title = "", type = "string")
    
    jaspResults[["modelContainer"]][["ldaTableMean"]] <- ldaTableMean
    jaspResults[["modelContainer"]][["ldaTableMean"]]$position <- 5
    
    target <- .v(options$target)

    if (ready) {
      
      for (predictor in options$predictors)
        ldaTableMean$addColumnInfo(name = predictor, type = "number")
      
      groupMeans <- classLdaResults[["res"]][["means"]]
      colnames(groupMeans) <- .unv(colnames(groupMeans))
      groupMeans <- cbind(target_level = rownames(groupMeans), as.data.frame(groupMeans))
      
      ldaTableMean$setData(groupMeans)
      
    }
  
}

.ldaTableMain <- function(jaspResults, options, classLdaResults, ready){
  if (!is.null(jaspResults[["ldaTableMain"]])) return()

  # Create table 
  ldaTableMain <- createJaspTable(title = "LDA Model Summary")
  jaspResults[["ldaTableMain"]] <- ldaTableMain
  jaspResults[["ldaTableMain"]]$position <- 1
  jaspResults[["ldaTableMain"]]$dependOn(options = c("target", "predictors", "indicator", "applyModel", # prior nog niet erbij
                                                     "modelOpt", "validationLeaveOneOut", "estimationMethod", "moment", "mle", "covMve", 
                                                     "dataTrain", "seedBox",
                                                     "seed"))
  
  # Add column info 
  ldaTableMain$addColumnInfo(name = "lvls", title = "", type = "string")
  ldaTableMain$addColumnInfo(name = "prior",  title = "Prior", type = "integer")
  ldaTableMain$addColumnInfo(name = "testerror", title = "Test Set Error", type = "integer")
  ldaTableMain$addColumnInfo(name = "ntrain", title = "n (Train)", type = "integer")
  ldaTableMain$addColumnInfo(name = "ntest", title = "n (Test)", type = "integer")
  
  # Add data per column 
  ldaTableMain[["prior"]] <- if (ready) classLdaResults$res$prior  else "."
  ldaTableMain[["lvls"]] <- if (ready) classLdaResults$res$lev  else "."
  ldaTableMain[["testerror"]] <- if (ready) classLdaResults$testError else "." 
  ldaTableMain[["ntrain"]] <- if (ready) classLdaResults$res$N  else "."
  ldaTableMain[["ntest"]] <- if (ready) classLdaResults$ntest  else "." 
  
}

.ldaConfTable <- function(jaspResults, options, classLdaResults, dataset, ready){
  if (!is.null(jaspResults[["ldaConfTable"]]) || !options$classLdaConfTable) return()
  
  # Create table 
  ldaConfTable <- createJaspTable(title = "Confusion Table")
  ldaConfTable$dependOn(options = "classLdaConfTable", optionsFromObject=jaspResults[["ldaTableMain"]])
  jaspResults[["ldaConfTable"]] <- ldaConfTable
  jaspResults[["ldaConfTable"]]$position <- 2
  
  target <- .v(options$target)
  
  if (ready) {
    
    ldaConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    ldaConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    ldaConfTable[["pred_name"]] <- c("Predicted", rep("", nrow(classLdaResults$confTable)-1))
    ldaConfTable[["varname_pred"]] <- colnames(classLdaResults$confTable)
    
    for (i in 1:length(rownames(classLdaResults$confTable))) {
      
      name <- paste("varname_obs", i, sep = "")
      ldaConfTable$addColumnInfo(name = name, title = as.character(rownames(classLdaResults$confTable)[i]),
                                 type = "integer", overtitle = "Observed")
      ldaConfTable[[name]] <- classLdaResults$confTable[, i]
      
    }
    
  } else if (options$target != "" && !ready) {
    
    ldaConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    ldaConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    ldaConfTable[["pred_name"]] <- c("Predicted", rep("", length(unique(dataset[, target])) - 1))
    ldaConfTable[["varname_pred"]] <- levels(dataset[, target])
    
    for (i in 1:length(unique(dataset[, target]))) {
      
      name <- paste("varname_obs", i, sep = "")
      ldaConfTable$addColumnInfo(name = name, title = as.character(levels(dataset[, target])[i]),
                                 type = "integer", overtitle = "Observed")
      ldaConfTable[[name]] <- rep(".", length(unique(dataset[, target])))
      
    }
    
  } else {
    
    ldaConfTable$addColumnInfo(name = "pred_name"    , title = "" , type = "string")
    ldaConfTable$addColumnInfo(name = "varname_pred" , title = "" , type = "string")
    ldaConfTable$addColumnInfo(name = "varname_obs1", title = ".", type = "integer")
    ldaConfTable$addColumnInfo(name = "varname_obs2", title = ".", type = 'integer')
    
    ldaConfTable[["pred_name"]] <- c("Predicted", "")
    ldaConfTable[["varname_pred"]] <- rep(".", 2)
    ldaConfTable[["varname_obs1"]] <- rep("", 2)
    ldaConfTable[["varname_obs2"]] <- rep("", 2)
    
  }
  
}

.ldaCoefTable <- function(jaspResults, options, classLdaResults, dataset, ready){
  if (!is.null(jaspResults[["ldaCoefTable"]]) || !options$classLdaCoefloadTable) return()
  
  # Create table 
  ldaCoefTable <- createJaspTable(title = "Coefficients of Linear Discriminants")
  ldaCoefTable$dependOn(options = "classLdaCoefloadTable", optionsFromObject = jaspResults[["stateClassLdaResults"]])
  
  jaspResults[["ldaCoefTable"]] <- ldaCoefTable
  jaspResults[["ldaCoefTable"]]$position <- 4
  
  # Add column info 
  ldaCoefTable$addColumnInfo(name = "pred_level", title = "", type = "string")
  
  target <- .v(options$target)
  
  if (ready) {
    
    for (ldacoef in colnames(classLdaResults[["res"]][["scaling"]]))
      ldaCoefTable$addColumnInfo(name = ldacoef, type = "number")

    Coeflda <- classLdaResults[["res"]][["scaling"]]
    colnames(Coeflda) <- colnames(Coeflda)
    Coeflda <- cbind(pred_level = .unv(rownames(Coeflda)), as.data.frame(Coeflda))
    
    ldaCoefTable$setData(Coeflda)
    
  }
  
  
}

.ldaPriorTable <- function(jaspResults, options, classLdaResults, dataset, ready){
  if (!is.null(jaspResults[["ldaPriorTable"]]) || !options$classLdaPriorTable) return()
  
  # Create table 
  ldaPriorTable <- createJaspTable(title = "Prior probabilities of groups")
  ldaPriorTable$dependOn(options = "classLdaPriorTable", optionsFromObject = jaspResults[["stateClassLdaResults"]])
  
  jaspResults[["ldaPriorTable"]] <- ldaPriorTable 
  jaspResults[["ldaPriorTable"]]$position <- 3
  
  # Add column info 
  
  if(ready) {
    
    
    levelPriors <- classLdaResults[["res"]][["prior"]]
    
    ldaPriorTable$setData(levelPriors)
    
  }
  
  
}