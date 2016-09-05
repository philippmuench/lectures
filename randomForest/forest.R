createForest <- function(traindata, input){
  require(randomForest)
  # split the data in x and y vector
  x <- subset(traindata, select=input$independent)
  is.complete <- which(complete.cases(x))
  x <- as.data.frame(x[is.complete,], stringsAsFactors = TRUE)

  ind <- sapply(x, is.character)
  x[ind] <- lapply(x[ind], factor)
  
    y <- subset(traindata,select=input$dependent)
  y <- as.factor(as.character(as.matrix(y[is.complete,])))

     # generate forest
  forest <- randomForest(x, y, ntree = 500,
                         do.trace=10, importance=TRUE, proximity=TRUE, keep.inbag=T)
  return(forest)
}

plotForest <- function(forest){
  require(randomForest)
  theplot <- varImpPlot(forest)
  return(theplot)
}
