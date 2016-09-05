

# createTree: 
# this function creates decision tree 
# using "Recursive Partitioning and Regression Trees"
createTree <- function(formular, traindata){
  require(rpart)
  tree <- rpart(formular,data=traindata, method="class",
               parms=list(split="information"))
  return(tree)
}

plotTree <- function(obj){
  require(rpart.plot)
  treeplot <- rpart.plot(obj)
  return(treeplot)
}

plotHeatmap <- function(obj,traindata){
  library(gplots)
  library(RColorBrewer)
  # gernerate propability matrix for predictions
  pred <- predict(obj, type = c("prob"))
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
  theplot <- heatmap.2(pred, density.info="none",
                       trace="none",
                       dendrogram="none", 
                       col=my_palette,
                       margins =c(12,9), notecol="black")
  return(theplot)
}

plotSplit <- function(tree, traindata, input){
  library(plotmo)
  theplot <- plotmo(tree, type="prob", nresponse="yes", 
         type2="image", pt.col=ifelse(traindata$Survived=="no", "red", 
                ifelse(traindata$Survived=="yes",
                       "blue", "green")))
  return(theplot)
}


getFormular <- function(input){
  return(paste(input$dependent, "~", gsub(",", " +",
                                          toString(input$independent))))
}

getData <- function(name){
  dat <- as.data.frame(read.csv(name, stringsAsFactors=FALSE))
  return(dat)
}

init_load <- function(){
  traindata <<- read.csv("titanic.csv", stringsAsFactors=FALSE)
  return(traindata)
}