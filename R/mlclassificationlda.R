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
  
  dataset <- .classLdaReadData(dataset, options)
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  #if (ready) {
    
    # Read dataset
   # dataset <- .classLdaReadData(dataset, options)
    
    # Error checking
    #errors <- .classLdaErrorHandling(dataset, options)
 # }
  #modelContainer <- createJaspContainer(title="titel")
  #jaspResults[["modelContainer"]] <- modelContainer
  # Compute (a list of) results from which tables and plots can be created
  if (ready) error <- .classLdaErrorHandling(dataset, options)
  if (ready) classLdaResults <- .classLdaComputeResults(jaspResults, dataset, options)
  
  # Tables 
  .ldaTableMean(jaspResults, options, classLdaResults, dataset, ready)
  .ldaTableMain(jaspResults, options, classLdaResults, ready)
  .ldaConfTable(jaspResults, options, classLdaResults, dataset, ready) 
  .ldaCoefTable(jaspResults, options, classLdaResults, dataset, ready)
  .ldaPriorTable(jaspResults, options, classLdaResults, dataset, ready)
  
  # Plots 
  .ldaMatrixPlot(jaspResults, options, classLdaResults, dataset, ready)
  #browser()
  .ldaMatricesPlot(jaspResults, options, classLdaResults, dataset, ready)

  return()    
}

# Read dataset
.classLdaReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.factor = c(options$target, options$indicator), columns = options$predictors)
  
  return(data)
}

# Error handling 
.classLdaErrorHandling <- function(dataset, options){
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: The target variable should have at least 2 classes
  if (nlevels(dataset[, .v(options$target)]) < 2){
    JASP:::.quitAnalysis("The target variable should have at least 2 classes.")
  }
  
}

# Compute results 
.classLdaComputeResults <- function(jaspResults, dataset, options){
  
  if (!is.null(jaspResults[["stateClassLdaResults"]])) return (jaspResults[["stateClassLdaResults"]]$object)
  
  # Create results object and add options
  results <- list()
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  results[["predictors"]] <- preds 
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
  results[["trainData"]] <- trainData

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
  results[["postprob"]] <- colMeans(prob$posterior)
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

# .modelContainer <- function(jaspResults, options, classLdaResults, dataset, ready){
#   modelContainer <- createJaspContainer(title = "Training Information")  
# 
#   jaspResults[["modelContainer"]] <- modelContainer 
#   modelContainer$dependOn(options = c("classLdaMeanTable"))
# }
  
# Compute tables 
  .ldaTableMean <- function(jaspResults, options, classLdaResults, dataset, ready){
    if (!is.null(jaspResults[["ldaTableMean"]]) || !options$classLdaMeanTable) return()

    # Create table 
    ldaTableMean <- createJaspTable(title= "Group Means")
    ldaTableMean$dependOn(options = "classLdaMeanTable", optionsFromObject = jaspResults[["stateClassLdaResults"]])
    
    # Add column info 
    ldaTableMean$addColumnInfo(name = "target_level", title = "", type = "string")
    
    jaspResults[["ldaTableMean"]] <- ldaTableMean
    jaspResults[["ldaTableMean"]]$position <- 5
    
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
  #ldaTableMain$addColumnInfo(name = "lvls", title = "", type = "string")
  #ldaTableMain$addColumnInfo(name = "prior",  title = "Prior", type = "integer")
  ldaTableMain$addColumnInfo(name = "testerror", title = "Test Set Error", type = "integer")
  ldaTableMain$addColumnInfo(name = "ntrain", title = "n (Train)", type = "integer")
  ldaTableMain$addColumnInfo(name = "ntest", title = "n (Test)", type = "integer")
  
  # Add data per column 
  #ldaTableMain[["prior"]] <- if (ready) classLdaResults$res$prior  else "."
  #ldaTableMain[["lvls"]] <- if (ready) classLdaResults$res$lev  else "."
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

#jaspResults[["stateClassLdaResults"]]$object$prob$posterior
#probs <- jaspResults[["stateClassLdaResults"]]$object

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
    levelPost <- classLdaResults[["postprob"]]

    ldaPriorTable$addColumnInfo(name = "typeprob", title = "", type = "string")
    ldaPriorTable$addColumnInfo(name = "prior", title = "Prior", type = "number")
    ldaPriorTable$addColumnInfo(name = "posterior", title = "Posterior", type = "number")
    
    ldaPriorTable[["typeprob"]] <- names(levelPriors) 
    ldaPriorTable[["prior"]] <- levelPriors 
    ldaPriorTable[["posterior"]] <- levelPost
    
  }

}

.ldaMatrixPlot <- function(jaspResults, options, classLdaResults, dataset, ready){
  if (!is.null(jaspResults[["ldaMatrixPlot"]]) || !options$matrixplot) return()

  ldaMatrixPlot <- createJaspPlot(title = "Matrix Plot",  width = 400, height = 320)
  ldaMatrixPlot$dependOn(options = "matrixplot", optionsFromObject = jaspResults[["stateClassLdaResults"]])
  jaspResults[["ldaMatrixPlot"]] <- ldaMatrixPlot 

  .ldaMatrixFillPlotDescriptives(ldaMatrixPlot, classLdaResults, dataset, options)

  
  return()
  
}

.ldaMatrixFillPlotDescriptives <- function(ldaMatrixPlot, classLdaResults, dataset, options){
 #browser()
  #if (options$target == 2){}
  # one curve 
  
  #else ()
  # more curves 
  
  lda.fit.scaled <- cbind.data.frame(scale(as.matrix(classLdaResults[["trainData"]][,.v(options$predictors)]), 
                                              scale = FALSE) %*% classLdaResults[["res"]]$scaling, V2 = classLdaResults[["trainData"]][,.v(options$target)])
  
  # Change x for every discriminant 
  
  plot <- JASPgraphs::themeJasp(ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = LD1, group = as.factor(V2), color = as.factor(V2), show.legend = TRUE)) +
    ggplot2::geom_line(stat = "density"), legend.position = "right", legend.title = "Predictor")
  
  
  ldaMatrixPlot$plotObject <- plot
  
  return()
  
}

.ldaMatricesPlot <- function(jaspResults, options, classLdaResults, dataset, ready){
  if (!is.null(jaspResults[["ldaMatricesPlot"]]) || !options$matrixplot) return()
  
  ldaMatricesPlot <- createJaspPlot(title = "Matrices Plot")
  ldaMatricesPlot$dependOn(options = "matrixplot", optionsFromObject = jaspResults[["stateClassLdaResults"]])
  jaspResults[["ldaMatricesPlot"]] <- ldaMatricesPlot 
  ldaMatricesPlot$plotObject <- .ldaMatricesFillPlotDescriptives(classLdaResults, dataset, options)
  
  
  return()
}


# .ldaMatricesFillPlotDescriptives <- function(ldaMatricesPlot, classLdaResults, dataset, options){
#   
#   #.plotFunc <- function() {
#   # if more than 1 LD 
#     
#     l <- length(colnames(classLdaResults[["res"]][["scaling"]])) 
#       
#     plotMat <- matrix(list(), l, l)
#     
#     # minor adjustments to plot margin to avoid cutting off the x-axis labels
#     adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm"))
#     
#     
#     oldFontSize <- JASPgraphs::getGraphOption("fontsize")
#     JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)
#     
#       
#     for (row in seq_len(l)){
#       for (col in seq_len(l)){
#         if(row == col){
#           
#           plotMat[[row, col]] <- .ldaDensityplot(classLdaResults, options, col = col)
#           
#         } 
#         if (col > row){
#           
#           plotMat[[row, col]] <- .ldaScatterPlot(classLdaResults, options, col = col)
#           
#         }
#         if (col < row){
#           
#           p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
#           p <- p + ggplot2::xlab("")
#           p <- p + ggplot2::ylab("")
#           p <- JASPgraphs::themeJasp(p)
#           
#           plotMat[[row, col]] <- NULL
#           
#         }
#       }
#     }
# 
#     JASPgraphs::setGraphOption("fontsize", oldFontSize)
#     
#     # slightly adjust the positions of the labels left and above the plots.
#     labelPos <- matrix(.5, 4, 2)
#     labelPos[1, 1] <- .55
#     labelPos[4, 2] <- .65
#     
#     p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = colnames(classLdaResults[["res"]][["scaling"]]), topLabels = colnames(classLdaResults[["res"]][["scaling"]]),
#                                   scaleXYlabels = NULL, labelPos = labelPos)
#     
#     return(p)
#   #}
#    
#   # obj <- .plotFunc()
#   # 
#   # content <- .writeImage(plot = obj, obj = TRUE)
#   # 
#   # plot <- correlation.plot
#   # plot[["convertible"]] <- TRUE
#   # plot[["obj"]] <- content[["obj"]]
#   # plot[["data"]] <- content[["png"]]
#   # 
#   # correlation.plot <- plot
#   #return(p)
# }

.ldaMatricesFillPlotDescriptives <- function(classLdaResults, dataset, options){

  variables <- colnames(classLdaResults[["res"]][["scaling"]])
  correlation.plot <- NULL
    
    variables <- .v(variables)
    l <- length(variables)
    
    if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
      width <- 580
      height <- 580
    } else if (l <= 2) {
      width <- 400
      height <- 400
    } else {
      width <- 250 * l
      height <- 250 * l
    }
    
    plot <- list()
    
    plot[["title"]] <- "Matrix Plot"
    plot[["width"]]  <- width
    plot[["height"]] <- height
    
    correlation.plot <- plot
    cexText <- 1.6
    
    # .plotFunc <- function() {
      
      plotMat <- matrix(list(), l, l)
      
      # minor adjustments to plot margin to avoid cutting off the x-axis labels
      adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm"))
      
      oldFontSize <- JASPgraphs::getGraphOption("fontsize")
      JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)
      
      for (row in seq_len(l)) {
        for (col in seq_len(l)) {
          if (row == col) {
            if (options$plotDensities) {
                plotMat[[row, col]] <- .ldaDensityplot(classLdaResults, options, col) + adjMargin # plot marginal (histogram with density estimator)
            } else {
              
              p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
              p <- p + ggplot2::xlab("")
              p <- p + ggplot2::ylab("")
              p <- JASPgraphs::themeJasp(p)
              
              plotMat[[row, col]] <- p
            }
          }
          
          if (col > row) {
            if (options$plotStatistics) {
                plotMat[[row, col]] <- .ldaScatterPlot(classLdaResults, options, col) + adjMargin # plot scatterplot
              
            } else {
              
              p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
              p <- p + ggplot2::xlab("")
              p <- p + ggplot2::ylab("")
              p <- JASPgraphs::themeJasp(p)
              
              plotMat[[row, col]] <- p
            }
          }
          
          if (col < row) {
            if (l < 7) {
              if (options$plotStatistics) {
                p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
                p <- p + ggplot2::xlab("")
                p <- p + ggplot2::ylab("")
                p <- JASPgraphs::themeJasp(p)
                
                plotMat[[row, col]] <- p
              
              } else {
                
                p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
                p <- p + ggplot2::xlab("")
                p <- p + ggplot2::ylab("")
                p <- JASPgraphs::themeJasp(p)
                
                plotMat[[row, col]] <- p
              }
            }
            
            if (col == 1 && row == 2){
              plotMat[[2, 1]] <- .ldaLegend(classLdaResults, options, col) + adjMargin # plot marginal (histogram with density estimator)
  
            }
            
            #if (l >= 7) {
            #   if (options$plotStatistics) {
            #     if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
            #       plotMat[[row, col]] <- .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], cexCI= 1.2, hypothesis= options$hypothesis,
            #                                            pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval) + adjMargin
            #       # if(col == 1){
            #       #     plotList[[length(plotList)]] <- plotList[[length(plotList)]] + ggplot2::annotate("text", x = 0, y = 1.5, label = .unv(variables)[row], angle = 90, size = 6, fontface = 2)
            #       # }
            #     } else {
            #       errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
            #       errorMessagePlot <- paste0("Undefined correlation:", "\n", errorMessages[1])
            #       plotMat[[row, col]] <- .displayError(errorMessagePlot, cexText=cexText) + adjMargin
            #     }
            #   } else {
            #     p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
            #     p <- p + ggplot2::xlab("")
            #     p <- p + ggplot2::ylab("")
            #     p <- JASPgraphs::themeJasp(p)
            #     
            #     plotMat[[row, col]] <- p
            #   }
            # }
          }
        }
      }
      
      JASPgraphs::setGraphOption("fontsize", oldFontSize)
      
      # slightly adjust the positions of the labels left and above the plots.
      labelPos <- matrix(.5, 4, 2)
      labelPos[1, 1] <- .55
      labelPos[4, 2] <- .65
      
      p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = .unv(variables), topLabels = .unv(variables),
                                    scaleXYlabels = NULL, labelPos = labelPos)
      
      return(p)
    # }
    
    # obj <- .plotFunc()
    
    #content <- .writeImage(width = width, height = height, plot = obj, obj = TRUE)
    
    # plot <- correlation.plot
    # plot[["convertible"]] <- TRUE
    #plot[["obj"]] <- content[["obj"]]
    #plot[["data"]] <- content[["png"]]
    
    # correlation.plot <- plot
    #return(correlation.plot)
    # return(obj)
  }
  
  
  
.ldaDensityplot <- function(classLdaResults, options, col){
    
  if (length(colnames(classLdaResults[["res"]][["scaling"]])) == 1)  {
  lda.fit.scaled <- cbind.data.frame(scale(as.matrix(classLdaResults[["trainData"]][,.v(options$predictors)]), 
                                             scale = FALSE) %*% classLdaResults[["res"]]$scaling, V2 = classLdaResults[["trainData"]][,.v(options$target)])
    
    # Change x for every discriminant 
    
    p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = lda.fit.scaled[,paste("LD", col, sep = "")], group = as.factor(V2), color = as.factor(V2), show.legend = TRUE)) +
                                    ggplot2::geom_line(stat = "density")
    p <- p + ggplot2::ylab("Density") + ggplot2::xlab(paste("LD", col, sep = ""))
  
  } else {
    lda.fit.scaled <- cbind.data.frame(scale(as.matrix(classLdaResults[["trainData"]][,.v(options$predictors)]), 
                                             scale = FALSE) %*% classLdaResults[["res"]]$scaling, V2 = classLdaResults[["trainData"]][,.v(options$target)])
    
    # Change x for every discriminant 
    
    p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = lda.fit.scaled[,paste("LD", col, sep = "")], group = as.factor(V2), color = as.factor(V2), show.legend = FALSE)) +
      ggplot2::geom_line(stat = "density")
    p <- p + ggplot2::ylab("Density") + ggplot2::xlab(paste("LD", col, sep = ""))
    
  }
  return(JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE, legend.position = c(1,1), legend.justification = c(1,1), legend.title = "Predictor"))
}

.ldaScatterPlot <- function(classLdaResults, options, col){
  data <- classLdaResults[["data"]]$trainData 
  model <- classLdaResults[["res"]]
  lda.data <- cbind(data, stats::predict(model)$x[,c(col - 1, col)])
  
  p <- ggplot2::ggplot(lda.data, ggplot2::aes(x = lda.data[,paste("LD", col - 1, sep = "")], y = lda.data[,paste("LD", col, sep = "")])) +
    ggplot2::geom_point(ggplot2::aes(color = lda.data[,.v(options$target)])) + 
    ggplot2::xlab(paste("LD", col - 1, sep = "")) + ggplot2::ylab(paste("LD", col, sep = ""))  
  p <- p + ggplot2::labs(color=options$target)
  
  return(JASPgraphs::themeJasp(p, legend.position = "right"))
}

.ldaLegend <- function(classLdaResults, options, col){
  lda.fit.scaled <- cbind.data.frame(scale(as.matrix(classLdaResults[["trainData"]][,.v(options$predictors)]), 
                                           scale = FALSE) %*% classLdaResults[["res"]]$scaling, V2 = classLdaResults[["trainData"]][,.v(options$target)])
  
  # Change x for every discriminant 
  
  p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = lda.fit.scaled[,paste("LD", col, sep = "")], group = as.factor(V2), color = as.factor(V2), show.legend = TRUE)) +
    ggplot2::geom_line(stat = "density")
  p <- p + ggplot2::ylab("Density") + ggplot2::xlab(paste("LD", col, sep = ""))
  
  g_legend <- function(a.gplot){ 
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
  }
  
  legend <- g_legend(p)
  
 return(legend)
  
}