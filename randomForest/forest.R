createForest <- function(x,y, trees){
  require(randomForest)
     # generate forest
  forest <<- randomForest(x, y, ntree = trees,
                         do.trace=50, importance=TRUE, proximity=TRUE, keep.inbag=T)
  return(forest)
}

plotForest <- function(forest){
  require(randomForest)
  theplot <- varImpPlot(forest)
  return(theplot)
}
