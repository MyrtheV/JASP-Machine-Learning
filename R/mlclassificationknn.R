#
# Copyright (C) 2017 University of Amsterdam
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

MLClassificationKNN <- function(jaspResults, dataset, options, ...) {
  
    jaspResults$title   <- 'K-Nearest Neighbors Classification'
    # read variables ##
    dataset             <- .knnClassificationReadData(dataset, options)
    # error handling ##
    .knnClassificationErrorHandling(dataset, options)
    ready <- length(options[["predictors"]][options[["predictors"]] != ""] > 0) && options[["target"]] != ""
    # Run the analysis
    res                 <- .knnClassification(dataset, options, jaspResults, ready)
    # create the evaluation table
    .knnClassificationSummaryTable(dataset, options, res, jaspResults, ready)
    # Create the confusion table
    .knnClassificationConfusionMatrix(dataset, options, res, jaspResults, ready)
    # Create the predictions table
    # .predictionsTableClassification(options, res, jaspResults, ready)
    # # Create the distances table
    # .distancesTableClassification(options, res, jaspResults, ready)
    # # Create the weights table ##
    # .weightsTableClassification(options, res, jaspResults, ready)
    # Create the Error vs K plot ##
    .knnClassificationTestSetErrorPlot(dataset, options, res, jaspResults, ready)
}

.knnClassificationReadData <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  if (is.null(dataset)) {
          dataset <- .readDataSetToEnd(columns.as.numeric=variables.to.read, exclude.na.listwise=variables.to.read)
  }
    return(dataset)
}

.knnClassificationErrorHandling <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                       all.target = variables.to.read,
                       observations.amount = "< 2",
                       exitAnalysisIfErrors = TRUE)
}

.makeformulaClassification <- function(options, ready){
    predictors <- .v(options[["predictors"]])
    target <- .v(options[["target"]])
    if( ready){
      formula <- paste(target, "~", paste(predictors, collapse=" + "))
    } else {
      formula <- "..."
    }
    return(formula)
}

.knnClassification <- function(dataset, options, jaspResults, ready){
  
  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)
  
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])

  formula                 <- .makeformulaClassification(options, ready)

  if(ready){
    if(options[["modelOpt"]] == "validationManual"){
      res <- .knnClassificationManual(dataset, options, formula, jaspResults)
    } else if(options[["modelOpt"]] == "validationLeaveOneOut"){
      res <- .knnClassificationLoocv(dataset, options, formula, jaspResults)
    } else if(options[["modelOpt"]] == "validationKFold"){
      res <- .knnClassificationKfold(dataset, options, formula, jaspResults)
    }
  } else {
    res                   <- list()
  }

  res[["formula"]] <- formula

  jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOnOptions(c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions"))

  return(jaspResults[["res"]]$object)
}

.knnClassificationManual <- function(dataset, options, formula, jaspResults){
  
  dataset                 <- na.omit(dataset)
  train.index             <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(options[['trainingDataManual']],1-options[['trainingDataManual']]))
  train                   <- dataset[train.index, ]
  test                    <- dataset[!train.index, ]

  knn.fit <- kknn::kknn(formula = formula,
                        train = train,
                        test = test,
                        k = options[['noOfNearestNeighbours']],
                        distance = options[['distanceParameterManual']],
                        kernel = options[['weights']],
                        scale = options[['scaleEqualSD']])
                          
    res <- list()
    if(is.numeric(knn.fit$fitted.values)){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,.v(options[["target"]])]),
            'Prediction' = round(knn.fit$fitted.values,0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,.v(options[["target"]])])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,.v(options[["target"]])]),
            'Prediction' = as.character(knn.fit$fitted.values))
        res[['confusion.table']] <- table('Pred'=knn.fit$fitted.values,'Real'=test[,.v(options[["target"]])])
    }
    if(options[["confusionProportions"]])
      res[['confusion.table']] <- round(res[['confusion.table']] / nrow(test), 2)
    res[['optimal.error']]      <- 1 - sum(diag(prop.table(res[['confusion.table']])))
    res[['optimal.k']]        <- options[['noOfNearestNeighbours']]
    res[['optimal.weights']]  <- options[["weights"]]
    res[["optimal.distance"]] <- options[["distanceParameterManual"]]
    res[['Weights']]          <- as.matrix(knn.fit$W)
    res[['Distances']]        <- as.matrix(knn.fit$D)
    res[['predictions']]      <- as.matrix(res[['predictions']])
    res[["ntrain"]]           <- nrow(train)
    res[["ntest"]]            <- nrow(test)
    return(res)
}

.knnClassificationLoocv <- function(dataset, options, formula, jaspResults){

    knn.fit <- kknn::train.kknn(formula = formula, data = dataset, ks = 1:options[["maxK"]], scale = options[["scaleEqualSD"]])
    
    res <- list()
    res[['error']] <- as.numeric(knn.fit$MISCLASS)
    res[['optimal.k']] <- knn.fit$best.parameters$k
    res[['optimal.weights']] <- knn.fit$best.parameters$kernel
    res[["optimal.distance"]] <- knn.fit$distance
    res[["optimal.error"]] <- min(res[["error"]])
    res[["ntrain"]] <- nrow(dataset)
    res[["ntest"]] <- nrow(dataset)
    if(is.numeric(knn.fit$fitted.values[[1]])){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(dataset),
            'Real' = as.character(dataset[,.v(options[["target"]])]),
            'Prediction' = round(knn.fit$fitted.values[[1]],0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit$fitted.values[[1]],0),'Real'=dataset[,.v(options[["target"]])])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(dataset),
            'Real' = as.character(dataset[,.v(options[["target"]])]),
            'Prediction' = as.character(knn.fit$fitted.values[[1]]))
        res[['confusion.table']] <- table('Pred'=knn.fit$fitted.values[[1]],'Real'=dataset[,.v(options[["target"]])])
    }
    if(options[["confusionProportions"]])
      res[['confusion.table']] <- round(res[['confusion.table']] / nrow(dataset), 3)

    return(res)
}

.knnClassificationKfold <- function(dataset, options, formula, jaspResults){

    knn.fit <- kknn::cv.kknn(formula = formula, data = dataset, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                             kcv = options[['noOfFolds']], k = options[['noOfNearestNeighbours']])
                             
    error <- 1 - length(which(knn.fit[[1]][,1] == knn.fit[[1]][,2]))/nrow(dataset)
    res <- list()
    res[['error']] <- error
    res[['optimal.weights']] <- options[["weights"]]
    res[["optimal.distance"]] <- options[["distanceParameterManual"]]
    res[['optimal.k']] <- options[['noOfNearestNeighbours']]
    res[["optimal.error"]] <- min(error)
    res[["ntrain"]] <- nrow(dataset)
    res[["ntest"]] <- nrow(dataset)
    if(is.numeric(knn.fit[[1]][, 2])){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(dataset),
            'Real' = as.character(dataset[,.v(options[["target"]])]),
            'Prediction' = round(knn.fit[[1]][, 2],0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit[[1]][, 2],0),'Real'=dataset[,.v(options[["target"]])])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(dataset),
            'Real' = as.character(dataset[,.v(options[["target"]])]),
            'Prediction' = as.character(knn.fit[[1]][, 2]))
        res[['confusion.table']] <- table('Pred'=as.character(knn.fit[[1]][, 2]),'Real'=dataset[,.v(options[["target"]])])
    }
    if(options[["confusionProportions"]])
      res[['confusion.table']] <- round(res[['confusion.table']] / nrow(dataset), 3)
    return(res)
}

.knnClassificationSummaryTable <- function(dataset, options, res, jaspResults, ready){

  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  evaluationTable                       <- createJaspTable("K-Nearest Neighbors Classification Summary")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  evaluationTable$dependOnOptions(c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "trainingError"))
  evaluationTable$position <- 1

  evaluationTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
  evaluationTable$addColumnInfo(name = 'testError', title = 'Test set error', type = 'number', format = 'dp:3')
  if(options[["trainingAccuracy"]])
    evaluationTable$addColumnInfo(name = 'trainingError', title = 'Training set error', type = 'number', format = 'dp:3')
  evaluationTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
  evaluationTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')
  evaluationTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'number')
  evaluationTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'number')
  
  if(!ready)
    return()
  
  distance  <- ifelse(res[["optimal.distance"]] == 1, yes = "Euclidian", no = "Manhattan")
  weights   <- res[["optimal.weights"]]
  nn        <- res[['optimal.k']]
  error     <- res[['optimal.error']]
  ntrain    <- res[["ntrain"]]
  ntest     <- res[["ntest"]]
    
  row <- data.frame(nn = nn, testError = error, distance = distance, weights = weights, ntrain = ntrain, ntest = ntest)
  evaluationTable$addRows(row)
}

.knnClassificationConfusionMatrix <- function(dataset, options, res, jaspResults, ready){
  
  if (!options$confusionTable || !is.null(jaspResults[["confusionTable"]])) return()

  confusionTable                       <- createJaspTable("Confusion table")
  jaspResults[["confusionTable"]]      <- confusionTable
  confusionTable$dependOnOptions(c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "confusionTable"))
  confusionTable$position <- 3
  
  target <- .v(options[["target"]])
  title_observed <- "Observed"
  
  if (ready) {

    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    row <- data.frame(pred_name = c("Predicted", rep("", nrow(res[["confusion.table"]]) - 1)),
                      varname_pred = colnames(res[["confusion.table"]]))
    
    for(i in 1:length(rownames(res[["confusion.table"]]))){
    
      name <- paste("varname_obs", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = as.character(rownames(res[["confusion.table"]])[i]),
                                        type = "integer", overtitle = "Observed")
      row <- cbind(row, tmp = res[["confusion.table"]][, i])
      names(row)[length(row)] <- name
    }
    
    confusionTable$addRows(row)

  } else if (options$target != "" && !ready) {
    
    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    
    confusionTable[["pred_name"]] <- c("Predicted", rep("", length(unique(dataset[, target])) - 1))
    confusionTable[["varname_pred"]] <- levels(dataset[, target])
    
    for (i in 1:length(unique(dataset[, target]))) {
      
      name <- paste("varname_obs", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = as.character(levels(dataset[, target])[i]),
                                        type = "integer", overtitle = "Observed")
      confusionTable[[name]] <- rep(".", length(unique(dataset[, target])))
      
    }

  } else {

    confusionTable$addColumnInfo(name = "pred_name"    , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_pred" , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_obs1", title = ".", type = "integer")
    confusionTable$addColumnInfo(name = "varname_obs2", title = ".", type = 'integer')
    
    row <- data.frame(pred_name = c("Predicted", ""), varname_pred = rep(".", 2), varname_obs1 = rep(".", 2), varname_obs2 = rep(".", 2))
    confusionTable$addRows(row)

  }
}

.knnClassificationTestSetErrorPlot <- function(dataset, options, res, jaspResults, ready){
  if(!ready && options[['plotErrorVsK']] && options[["modelOpt"]] != "validationManual"){
    p <- createJaspPlot(plot = NULL, title = "Classification Error Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(options[['plotErrorVsK']] && options[["modelOpt"]] != "validationManual"){
     if(is.null(jaspResults[["plotErrorVsK"]])){
       
       error <- 1:options[["maxK"]]
       dataset                 <- na.omit(dataset)
       train.index             <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(options[['trainingDataManual']],1-options[['trainingDataManual']]))
       train                   <- dataset[train.index, ]
       test                    <- dataset[!train.index, ]
      formula                 <- .makeformulaClassification(options, ready)
       for(i in 1:options[["maxK"]]){
         knn.fit <- kknn::kknn(formula = formula,
                               train = train,
                               test = test,
                               k = i,
                               distance = options[['distanceParameterManual']],
                               kernel = options[['weights']],
                               scale = options[['scaleEqualSD']])
         if(is.numeric(knn.fit$fitted.values)){
             predictions <- data.frame(
                 'Observation' = 1:nrow(test),
                 'Real' = as.character(test[,.v(options[["target"]])]),
                 'Prediction' = round(knn.fit$fitted.values,0))
             confusion.table <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,.v(options[["target"]])])
         } else {
            predictions <- data.frame(
                 'Observation' = 1:nrow(test),
                 'Real' = as.character(test[,.v(options[["target"]])]),
                 'Prediction' = as.character(knn.fit$fitted.values))
             confusion.table <- table('Pred'=knn.fit$fitted.values,'Real'=test[,.v(options[["target"]])])
         }
         error[i] <- 1 - sum(diag(prop.table(confusion.table)))
        }
        
        d <- data.frame(x = 1:options[["maxK"]], y = error)
        
        xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
        yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
    
        p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
          JASPgraphs::geom_point()
    
      	p <- p + ggplot2::scale_x_continuous(name = "Nearest neighbors", breaks = xBreaks, limits = range(xBreaks))
      	p <- p + ggplot2::scale_y_continuous(name = "Classification error", breaks = yBreaks, limits = range(yBreaks))
    
       p <- JASPgraphs::themeJasp(p)
       
       jaspResults[["plotErrorVsK"]] 		<- createJaspPlot(plot = p, title = "Classification Error Plot", width = 400, height = 300)
       jaspResults[["plotErrorVsK"]]		$dependOnOptions(c("plotErrorVsK","noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox"))
      jaspResults[["plotErrorVsK"]] 		$position <- 10
     }
  }
}

.distancesTableClassification <- function(options, res, jaspResults, ready){

  if(!is.null(jaspResults[["distancesTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if (options[['tableDistances']]){
      if(is.null(jaspResults[["distancesTable"]])){

  distancesTable                       <- createJaspTable("Distances Table")
  jaspResults[["distancesTable"]]      <- distancesTable
  distancesTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo",
                                    "scaleEqualSD", "predictors", "target", "tableDistances"))
  distancesTable$position <- 5

  from <- options[["predictionsFrom"]]
  to <- options[['predictionsTo']]
  predictors <- options[["predictors"]]
  target <- options[["target"]]
  
  if(!ready)
    return()

  distancesTable$addColumnInfo(name = "number", title = "Obs. number", type = "integer")
    if(!is.null(res)){
        for(i in 1:res[['Optimal.K']]){
            distancesTable$addColumnInfo(name =paste0('distance', i),title = paste0('Distance ',i), type = 'number', format = 'dp:2')
        }
    } else {
        distancesTable$addColumnInfo(name = 'distance1', title = "Distance", type = 'integer')
    }
    
    if(!ready)
      return()

    if(is.null(res)){
        row <- list(number = ".", distance = ".")
        distancesTable$addRows(row)
    } else {
      data <- as.data.frame(res[["Distances"]][from:to, ])
      number <- 1:nrow(data)
      data <- cbind(number, data)
      for(i in 2:ncol(data)){
        colnames(data)[i] <- paste0('distance', i-1)
      }
      distancesTable$setData(data)
    }

    if(options[['distanceParameterManual']]==1)
        message <- 'Distances shown are the Manhattan distances'
    if (options[['distanceParameterManual']] == 2)
      message <-'Distances shown are the Euclidian distances'
    distancesTable$addFootnote(message, symbol="<i>Note.</i>")

    }
  }
}

.weightsTableClassification <- function(options, res, jaspResults, ready){

  if(!is.null(jaspResults[["weightsTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if(options[['tableWeights']]){
    if(is.null(jaspResults[["weightsTable"]])){

  weightsTable                       <- createJaspTable("Weights Table")
  jaspResults[["weightsTable"]]      <- weightsTable
  weightsTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo",
                                    "scaleEqualSD", "predictors", "target", "tableWeights"))
  weightsTable$position <- 6

  from <- options[["predictionsFrom"]]
  to <- options[['predictionsTo']]
  predictors <- options[["predictors"]]
  target <- options[["target"]]
  
  if(!ready)
    return()

  weightsTable$addColumnInfo(name = "number", title = "Obs. number", type = "integer")
  if(!is.null(res)){
    for(i in 1:res[['Optimal.K']]){
      weightsTable$addColumnInfo(name = paste0('weight',i), title = paste('Weight',i,sep = ' '), type = 'number', format = 'dp:2')
    }
  } else {
    weightsTable$addColumnInfo(name = 'weight1', title = 'Weights', type = "integer")
  }
  
  if(!ready)
    return()

  if(is.null(res)){
    row <- list(number = ".", weights1 = ".")
    weightsTable$addRows(row)
  } else {
    data <- as.data.frame(res[["Weights"]][from:to, ])
    number <- 1:nrow(data)
    data <- cbind(number, data)
    for(i in 2:ncol(data)){
      colnames(data)[i] <- paste0('weight', i-1)
    }
    weightsTable$setData(data)
  }

  message <- paste0('Weights are calculated using the ', options[['weights']], ' weighting scheme.')
  weightsTable$addFootnote(message, symbol="<i>Note.</i>")
    }
  }
}

.predictionsTableClassification <- function(options, res, jaspResults, ready){

  if(!is.null(jaspResults[["predictionsTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if (options[['tablePredictions']]){
      if(is.null(jaspResults[["predictionsTable"]])){

          predictionsTable                       <- createJaspTable("Predictions Table")
          jaspResults[["predictionsTable"]]      <- predictionsTable
          predictionsTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                            "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                            "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo",
                                            "scaleEqualSD", "predictors", "target", "tablePredictions"))
          predictionsTable$position <- 4

          from <- options[["predictionsFrom"]]
          to <- options[['predictionsTo']]
          predictors <- options[["predictors"]]
          target <- options[["target"]]

          predictionsTable$addColumnInfo(name="number", title="Obs. number", type="integer")
          predictionsTable$addColumnInfo(name="real", title="Observed", type="string")
          predictionsTable$addColumnInfo(name='predicted',title = 'Predicted', type = 'string')
          
          if(!ready)
            return()

            for(i in from:to){
                row <- list(number = as.numeric(res[['predictions']][i,1]),
                            real = as.character(res[['predictions']][i,2]),
                           predicted = as.character(res[['predictions']][i,3]))
                predictionsTable$addRows(row)
          }
      }
    }
}
