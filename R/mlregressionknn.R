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

MLRegressionKNN <- function(jaspResults, dataset, options, state=NULL) {

		# read variables ##
	  dataset              	<- .knnClassificationReadData(dataset, options)
	  
		# error handling ##
	  .knnClassificationErrorHandling(dataset, options)
		ready <- length(options[["predictors"]][options[["predictors"]] != ""] > 0) && options[["target"]] != ""
		
		# Run the analysis
		res <- .knnRegression(dataset, options, jaspResults, ready)
		
		# create the evaluation table
		.knnRegressionSummaryTable(dataset, options, res, jaspResults, ready)
		
		# Create the Error vs K plot ##
		.knnRegressionTestSetErrorPlot(dataset, options, res, jaspResults, ready)
}

.knnRegression <- function(dataset, options, jaspResults, ready){
	
	if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)
	
	# set the seed so that every time the same set is chosen (to prevent random results) ##
	if(options[["seedBox"]])
		set.seed(options[["seed"]])

	formula            <- .makeformulaClassification(options, ready)
	
	if(ready){
		if(options[["modelOpt"]] == "validationManual"){
			res <- .knnRegressionManual(dataset, options, formula, jaspResults)
		}  else if(options[["modelOpt"]] == "validationLeaveOneOut"){
			res <- .knnRegressionLoocv(dataset, options, formula, jaspResults)
		} else if(options[["modelOpt"]] == "validationKFold"){
			res <- .knnRegressionKfold(dataset, options, formula, jaspResults)
		}
	} else {
		res                   <- list()
	}

	res[["formula"]] <- formula

	jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "modelOpt",
                                    "scaleEqualSD", "noOfFolds", "predictors", "target", "maxK", "seed", "seedBox"))

	return(jaspResults[["res"]]$object)
}

.knnRegressionManual <- function(dataset, options, formula, jaspResults){
	
	dataset                 <- na.omit(dataset)
	train.index             <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(options[['trainingDataManual']],1-options[['trainingDataManual']]))
	train                   <- dataset[train.index, ]
	test                    <- dataset[!train.index, ]

	knn.fit <- kknn::kknn(formula = formula, train = train, test = test, k = options[['noOfNearestNeighbours']], distance = options[['distanceParameterManual']],
                        kernel = options[['weights']], scale = options[['scaleEqualSD']])
												
	res <- list()
	y <- dataset[!train.index, .v(options[["target"]])]
	# res[['predictions']] <- data.frame(
	# 	'Observation' = 1:nrow(test),
	# 	'Real' = y,
	# 	'Prediction'= knn.fit$fitted.values)
	res[['optimal.error']] 		<- mean((knn.fit$fitted.values - y)^2)
	res[['optimal.k']] 				<- options[['noOfNearestNeighbours']]
	res[["optimal.distance"]] <- options[["distanceParameterManual"]]
	res[['optimal.weights']] 	<- options[["weights"]]
	res[['Weights']] 					<- as.matrix(knn.fit$W)
	res[['Distances']] 				<- as.matrix(knn.fit$D)
	res[["ntrain"]] 					<- nrow(train)
	res[["ntest"]] 						<- nrow(test)
	res[["rsquare"]] 					<- round(cor(knn.fit$fitted.values, y)^2, 2)
	
	knn.train <- kknn::kknn(formula = formula, train = train, test = train, k = options[['noOfNearestNeighbours']], distance = options[['distanceParameterManual']],
												kernel = options[['weights']], scale = options[['scaleEqualSD']])
	res[["trainingError"]]		<- mean((knn.train$fitted.values - y)^2)
	
	return(res)
}

.knnRegressionLoocv <- function(dataset, options, formula, jaspResults){

	knn.fit <- kknn::train.kknn(formula = formula,
															data = dataset,
															ks = 1:options[['maxK']],
			                        scale = options[['scaleEqualSD']])
	res <- list()
	res[['MSE']] 									<- as.numeric(knn.fit$MEAN.SQU)
	res[['optimal.k']] 						<- knn.fit$best.parameters$k
	res[['optimal.weights']] 			<- knn.fit$best.parameters$kernel
	res[["optimal.distance"]] 		<- knn.fit$distance
	res[['optimal.error']] 				<- res[['MSE']][res[['optimal.k']]]
	# if(is.numeric(knn.fit$fitted.values[[1]])){
	# 		result[['predictions']] <- data.frame(
	# 				'Observation' = 1:nrow(dataset),
	# 				'Real' = as.character(dataset[,.v(options[["target"]])]),
	# 				'Prediction' = round(knn.fit$fitted.values[[1]],0))
	# } else {
	# 		result[['predictions']] <- data.frame(
	# 				'Observation' = 1:nrow(dataset),
	# 				'Real' = as.character(dataset[,.v(options[["target"]])]),
	# 				'Prediction' = as.character(knn.fit$fitted.values[[1]]))
	# }
	res[["ntrain"]] 							<- nrow(dataset)
	res[["ntest"]] 								<- nrow(dataset)
	y 														<- dataset[, .v(options[["target"]])]
	res[["trainingError"]]				<- mean((predict(knn.fit, newdata = dataset) - y)^2)
	res[["rsquare"]] 							<- cor(predict(knn.fit, newdata = dataset), dataset[,.v(options[["target"]])])^2

	return(res)
}

.knnRegressionKfold <- function(dataset, options, formula, jaspResults){
	
	knn.fit <- kknn::cv.kknn(formula = formula, data = dataset, kcv = options[['noOfFolds']], distance = options[['distanceParameterManual']],
							 kernel = options[['weights']], scale = options[['scaleEqualSD']], k = options[["noOfNearestNeighbours"]])
							 
	res <- list()
	# result[['Predictions']] <- data.frame(
	# 	'Observation' = 1:nrow(dataset),
	# 	'True' = as.numeric(knn.fit[[1]][,1]),
	# 	'Prediction' = as.numeric(knn.fit[[1]][,2]))
	res[['optimal.error']] 							<- mean((knn.fit[[1]][,1] - knn.fit[[1]][,2])^2)
	res[['optimal.k']] 									<- options[["noOfNearestNeighbours"]]
	res[["optimal.distance"]] 					<- options[["distanceParameterManual"]]
	res[['optimal.weights']] 						<- options[["weights"]]
	res[["ntrain"]] 										<- nrow(dataset)
	res[["ntest"]] 											<- nrow(dataset)
	res[["rsquare"]] 										<- cor(knn.fit[[1]][,2], dataset[,.v(options[["target"]])])^2
	
	knn.train <- kknn::cv.kknn(formula = formula, data = dataset, kcv = options[['noOfFolds']], distance = options[['distanceParameterManual']],
							 kernel = options[['weights']], scale = options[['scaleEqualSD']], k = options[["noOfNearestNeighbours"]])
	res[["trainingError"]] <- mean((knn.train[[1]][,1] - knn.train[[1]][,2])^2)
	
	return(res)
}

.knnRegressionSummaryTable <- function(dataset, options, res, jaspResults, ready){

  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  evaluationTable                       <- createJaspTable("Evaluation Table")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  evaluationTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "modelOpt",
                                    "scaleEqualSD", "noOfFolds", "predictors", "target", "maxK", "seed", "seedBox"))
  evaluationTable$position <- 1

  evaluationTable$addColumnInfo(name = 'nn', title = 'No. nearest neighbors', type = 'integer')
  evaluationTable$addColumnInfo(name = 'testError', title = 'Test error', type = 'number', format = 'dp:3')
	if(options[["trainingAccuracy"]])
		evaluationTable$addColumnInfo(name = 'trainingError', title = 'Training error', type = 'number', format = 'dp:3')
	evaluationTable$addColumnInfo(name = 'rsquare', title = 'R\u00B2', type = 'number', format = 'dp:2')
	evaluationTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
	evaluationTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')
	evaluationTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'number')
	evaluationTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'number')

	if(!ready)
		return()
		
	if(res[["optimal.k"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of nearest neighbors is the maximum number. You might want to adjust the range op optimization."
    evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
		
	distance  <- ifelse(res[["optimal.distance"]] == 1, yes = "Euclidian", no = "Manhattan")
  weights   <- res[["optimal.weights"]]
  nn        <- res[['optimal.k']]
  error     <- res[['optimal.error']]
  ntrain    <- res[["ntrain"]]
  ntest     <- res[["ntest"]]
	rsquare 	<- res[["rsquare"]]
    
  row <- data.frame(nn = nn, testError = error, rsquare = rsquare, distance = distance, weights = weights, ntrain = ntrain, ntest = ntest)
	if(options[["trainingAccuracy"]])
		row <- cbind(row, trainingError = res[["trainingError"]])  
  evaluationTable$addRows(row)
}

.knnRegressionTestSetErrorPlot <- function(dataset, options, res, jaspResults, ready){
  if(!ready && options[['plotErrorVsK']] && options[["modelOpt"]] != "validationManual"){
    p <- createJaspPlot(plot = NULL, title = "Test Set Error Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(options[['plotErrorVsK']] && options[["modelOpt"]] != "validationManual"){
     if(is.null(jaspResults[["plotErrorVsK"]])){
       
      error 										<- 1:options[["maxK"]]
      dataset                 	<- na.omit(dataset)
      formula                 	<- .makeformulaClassification(options, ready)
       
			 for(i in 1:options[["maxK"]]){
				 
				 train.index             	<- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(options[['trainingDataManual']],1-options[['trainingDataManual']]))
				 train                   	<- dataset[train.index, ]
				 test                    	<- dataset[!train.index, ]
				 
				 knn.fit <- kknn::kknn(formula = formula, train = train, test = test, k = i, distance = res[["optimal.distance"]],
			                         kernel = res[['optimal.weights']], scale = options[['scaleEqualSD']])
				 
				 y <- dataset[!train.index, .v(options[["target"]])]
         error[i] <- mean((knn.fit$fitted.values - y)^2)
        }
        
        d <- data.frame(x = 1:options[["maxK"]], y = error)
        
        xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
        yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
    
        p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
          JASPgraphs::geom_point()
    
      	p <- p + ggplot2::scale_x_continuous(name = "Nearest neighbors", breaks = xBreaks, limits = range(xBreaks))
      	p <- p + ggplot2::scale_y_continuous(name = "Mean Squared Error", breaks = yBreaks, limits = range(yBreaks))
    
       p <- JASPgraphs::themeJasp(p)
       
       jaspResults[["plotErrorVsK"]] 		<- createJaspPlot(plot = p, title = "Test Set Error Plot", width = 400, height = 300)
       jaspResults[["plotErrorVsK"]]		$dependOn(options =c("plotErrorVsK","noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "maxK"))
      jaspResults[["plotErrorVsK"]] 		$position <- 10
     }
  }
}
