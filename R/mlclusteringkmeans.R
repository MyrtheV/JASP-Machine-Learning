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

MLClusteringKMeans <- function(jaspResults, dataset, options, ...) {

    jaspResults$title   <- "K-Means Clustering"
    # read variables ##
    dataset             <- .kMeansClusteringReadData(dataset, options)
    # error handling & code variable names in base64
    .kMeansClusteringErrorHandling(dataset, options)
    ready               <- length(options[["predictors"]][options[["predictors"]] != ""] > 0)
    # Run the analysis and save the results
    res                 <- .kMeansClustering(dataset, options, jaspResults, ready)
    # create the evaluation table
    .kMeansClusteringSummaryTable(res, options, jaspResults, ready)
    # create the cluster information table
    .kMeansClusteringInformationTable(options, res, jaspResults, ready)
     # Create the cluster plot
    .kMeansClusterPlot(dataset, options, res, jaspResults, ready)
    # Create the within ss vs cluster plot
    .kMeansWithinSumOfSquaresPlot(options, res, jaspResults, ready)
}

.kMeansClusteringReadData <- function(dataset, options){
  predictors <- unlist(options['predictors'])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)) {
          dataset <- .readDataSetToEnd(columns.as.numeric = predictors, exclude.na.listwise = predictors)
  }
  return(dataset)
}

.kMeansClusteringErrorHandling <- function(dataset, options){
  predictors <- unlist(options$predictors)
  if(length(predictors[predictors != '']) > 0){
      for(i in 1:length(predictors)){
          errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                               all.target = predictors[i],
                               observations.amount = "< 2",
                               exitAnalysisIfErrors = TRUE)
      }
  }
}

.kMeansClustering <- function(dataset, options, jaspResults, ready){

  if(!is.null(jaspResults[["res"]]$object)) return(jaspResults[["res"]]$object)

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]])
    set.seed(options[["seed"]])

    if(ready){
        if(options[["modelOpt"]] == "validationManual"){
            res <- .kMeansClusteringManual(dataset, options)
        } else {
            res <- .kMeansClusteringOptimized(dataset, options)
        }
    } else {
        res <- list()
    }
    jaspResults[["res"]] <- createJaspState(res)
    jaspResults[["res"]]$dependOnOptions(c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", 
                                              "maxClusters", "seedBox"))

    return(jaspResults[["res"]]$object)
}

.kMeansClusteringManual <- function(dataset, options){
  
    kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                   centers = options[['noOfClusters']],
                   iter.max = options[['noOfIterations']],
                   nstart = options[['noOfRandomSets']],
                   algorithm = options[['algorithm']])
    res <- list()
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[["pred.values"]] <- kfit$cluster
    res[['clusters']] <- options[['noOfClusters']]
    res[["N"]] <- nrow(dataset)
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    res[['AICweights']] <- 1
    res[['BICweights']] <- 1
    m = ncol(kfit$centers)
    n = length(kfit$cluster)
    k = nrow(kfit$centers)
    D = kfit$tot.withinss
    res[['AIC']] <- D + 2*m*k
    res[['BIC']] <- D + log(n)*m*k
    return(res)
}

.kMeansClusteringOptimized <- function(dataset, options){

    WSS <- numeric(options[["maxClusters"]] - 1) # take as the max half of the N
    clusters <- 1:options[["maxClusters"]]

    res<-list()
    res[['clusterrange']] <- 1:options[["maxClusters"]]

    for(i in seq_along(clusters)){

        clusterIndex <- clusters[i]
        kfit_tmp <- kmeans(dataset[, .v(options[["predictors"]])],
                           centers = clusterIndex,
                           iter.max = options[['noOfIterations']],
                           nstart = options[['noOfRandomSets']],
                           algorithm = options[['algorithm']])

        res[['WithinSumSquares_store']][i] <- kfit_tmp$tot.withinss
        m = ncol(kfit_tmp$centers)
        n = length(kfit_tmp$cluster)
        k = nrow(kfit_tmp$centers)
        D = kfit_tmp$tot.withinss
        res[['AIC_store']][i] <- D + 2*m*k
        res[['BIC_store']][i] <- D + log(n)*m*k
        res[["N_store"]][i] <- nrow(dataset)
        res[['TSS_store']][i] <- kfit_tmp$totss
        res[['BSS_store']][i] <- kfit_tmp$betweenss
        res[["rsquare_store"]][i] <- res[['BSS_store']][i]/res[['TSS_store']][i]
    }
    if(options[["modelOpt"]] == "validationRsquared"){
      res[['clusters']] <- res[["clusterrange"]][[which.max(res[["rsquare_store"]])]]
    } else if(options[["modelOpt"]] == "validationAIC"){
      res[['clusters']] <- res[["clusterrange"]][[which.min(res[["AIC_store"]])]]
    } else if(options[["modelOpt"]] == "validationBIC"){
      res[['clusters']] <- res[["clusterrange"]][[which.min(res[["BIC_store"]])]]
    }
    # predictions for best model.
    kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                       centers = res[['clusters']],
                       iter.max = options[['noOfIterations']],
                       nstart = options[['noOfRandomSets']],
                       algorithm = options[['algorithm']])
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    m = ncol(kfit_tmp$centers)
    n = length(kfit_tmp$cluster)
    k = nrow(kfit_tmp$centers)
    D = kfit_tmp$tot.withinss
    res[['AIC']] <- D + 2*m*k
    res[['BIC']] <- D + log(n)*m*k
    res[["N"]] <- nrow(dataset)
    dAIC <- res[["AIC_store"]][res[['clusters']]] - min(res[['AIC_store']])
    res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
    dBIC <- res[['BIC_store']][res[['clusters']]] - min(res[['BIC_store']])
    res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
    return(res)
}

.kMeansClusteringSummaryTable <- function(res, options, jaspResults, ready){

  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  evaluationTable                       <- createJaspTable("K-Means Clustering Model Summary")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  jaspResults[["evaluationTable"]]$position <- 1
  evaluationTable$dependOnOptions(c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                      "aicweights", "modelOpt", "seed", "maxClusters"))

  evaluationTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1')
  evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1')
  evaluationTable$addColumnInfo(name = 'n', title = 'N', type = 'number', format = 'dp:1')
  if(options[["aicweights"]]){
      evaluationTable$addColumnInfo(name = "aicweights", title = "w(AIC)", type = "number", format = "dp:2")
      evaluationTable$addColumnInfo(name = "bicweights", title = "w(BIC)", type = "number", format = "dp:2")
  }

  evaluationTable$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")
  evaluationTable$addCitation("Wagenmakers, E. J., & Farrell, S. (2004). AIC model selection using Akaike weights. Psychonomic bulletin & review, 11(1), 192-196.")

  if(!ready)
    return()

  row <- data.frame(clusters = res[['clusters']], measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']], bic = res[['BIC']], n = res[["N"]])
  if(options[["aicweights"]])
    row <- cbind(row, aicweights = res[["AICweights"]], bicweights = res[["BICweights"]])
  evaluationTable$addRows(row)
}

.kMeansClusteringInformationTable <- function(options, res, jaspResults, ready){

  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if (options[['tableClusterInformation']]){

    clusterInfoTable                        <- createJaspTable("Cluster Information")
    jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    clusterInfoTable$dependOnOptions(c("tableClusterInformation","predictors", "modelOpt",
                                        "noOfClusters","noOfRandomSets", "tableClusterInfoSize",
                                        "tableClusterInfoSumSquares", "tableClusterInfoCentroids",
                                        "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters"))
    clusterInfoTable$position               <- 2
    clusterInfoTable$transpose              <- TRUE

    clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
    clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
    clusterInfoTable$addColumnInfo(name = 'withinss', title = 'Within Sum of Squares', type = 'number', format = 'dp:2')

    if(!ready)
      return()

    if(options[['tableClusterInfoCentroids']]){
        for( i in 1:length(options[["predictors"]])){
            clusterInfoTable$addColumnInfo(name = paste0('centroid', i), title = paste0('Centroid ', options[["predictors"]][i]), type = 'number', format = 'dp:3')
        }
    }

    cluster <- 1:res[["clusters"]]
    size <- res[["size"]]
    withinss <- res[["WSS"]]

    row <- data.frame(cluster = cluster, size = size, withinss = withinss)

    if(options[['tableClusterInfoCentroids']]){
        for( i in 1:length(options[["predictors"]])){
            row <- cbind(row, "tmp" = res[['centroids']][ ,i])
            colnames(row)[length(colnames(row))] <- paste0("centroid", i)
        }
    }

    if(options[['tableClusterInfoBetweenSumSquares']]){
        message <- paste0('The Between Sum of Squares of the ', res[["clusters"]], ' cluster model is ', round(res[['BSS']],2))
        clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
    }

    if(options[['tableClusterInfoTotalSumSquares']]){
        message <- paste0('The Total Sum of Squares of the ', res[["clusters"]], ' cluster model is ', round(res[['TSS']],2))
        clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
    }

    clusterInfoTable$addRows(row)
  }
}

.kMeansClusterPlot <- function(dataset, options, res, jaspResults, ready){
  if(!ready && options[['plot2dCluster']]){
    p <- createJaspPlot(plot = NULL, title= "Cluster Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['plot2dCluster']]){
    if(is.null(jaspResults[["plot2dCluster"]])){
      
      if(options[["seedBox"]])
        set.seed(options[["seed"]])
      
      unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
      data <- dataset[unique.rows, .v(options[["predictors"]])]
      tsne_out <- Rtsne::Rtsne(as.matrix(data))
      
      kfit <- kmeans(data, centers = res[["clusters"]], iter.max = options[['noOfIterations']], nstart = options[['noOfRandomSets']], algorithm = options[['algorithm']])
      pred.values <- kfit$cluster
      
      tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = pred.values)
      p <- ggplot2::ggplot(tsne_plot) + ggplot2::geom_point(ggplot2::aes(x = x, y = y, fill = factor(col)), size = 4, stroke = 1, shape = 21, color = "black") + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
      p <- JASPgraphs::themeJasp(p)
      jaspResults[["plot2dCluster"]] 		<- createJaspPlot(plot = p, title= "Cluster Plot", width = 400, height = 300)
      jaspResults[["plot2dCluster"]]		$dependOnOptions(c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                          "aicweights", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters"))
      jaspResults[["plot2dCluster"]] 		$position <- 3
    }
  }
}

.kMeansWithinSumOfSquaresPlot <- function(options, res, jaspResults, ready){
  if(!ready && options[['withinssPlot']] && options[["modelOpt"]] != "validationManual"){
    p <- createJaspPlot(plot = NULL, title= "Within Sum of Squares Plot", width = 400, height = 300)
    p$setError("Plotting not possible: No analysis has been run.")
    return()
  } else if(ready && options[['withinssPlot']] && options[["modelOpt"]] != "validationManual"){
     if(is.null(jaspResults[["optimPlot"]])){
       
       values <- res[['WithinSumSquares_store']]
       d <- data.frame(x = 1:options[["maxClusters"]], y = values)
       
       xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
       yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
   
       p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
         JASPgraphs::geom_point()
   
     	p <- p + ggplot2::scale_x_continuous(name = "Cluster", breaks = xBreaks, limits = range(xBreaks))
     	p <- p + ggplot2::scale_y_continuous(name = "Within Sum of Squares", breaks = yBreaks, limits = range(yBreaks))
   
      p <- JASPgraphs::themeJasp(p)
   
     jaspResults[["optimPlot"]] 		 <- createJaspPlot(plot = p, title= "Within Sum of Squares Plot", height = 300, width = 400)
     jaspResults[["optimPlot"]]		   $dependOnOptions(c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                         "aicweights", "modelOpt", "ready", "seed", "withinssPlot"))
     jaspResults[["optimPlot"]] 		 $position <- 4
     }
  }
}
