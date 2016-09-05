createForest <- function(x,y, trees){
  require(randomForest)
    # Random Forest CART model
    forest <<- randomForest(x, y, ntree = trees,mtry=2, 
                         do.trace=50, importance=TRUE, proximity=TRUE, keep.inbag=T)
  return(forest)
}

createBagged <- function(x,y, trees){
  require(randomForest)
  # Bagged CART model  
  forest <<- randomForest(x, y, ntree = trees,mtry=3, 
                          do.trace=50, importance=TRUE, proximity=TRUE, keep.inbag=T)
  return(forest)
}

plotForest <- function(forest){
  require(randomForest)
  theplot <- varImpPlot(forest)
  return(theplot)
}
