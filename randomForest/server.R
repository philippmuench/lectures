# server.R

shinyServer(
  function(input, output) {
    init_load()

    traindata <- reactive({
      if(input$dataset == "Titanic")
      getData("titanic.csv")
    })
   
    formula <- reactive({
      getFormular(input)
    })
    
    tree <- reactive({
      if(input$lowcp){
        createTree(formula(),traindata(), lowcp=TRUE)
      } else {
        createTree(formula(),traindata(), lowcp=FALSE)
      }
    })
 
    x <- reactive({
      x <- subset(traindata(), select=input$independent)
      is.complete <- which(complete.cases(x))
      x <- as.data.frame(x[is.complete,], stringsAsFactors = TRUE)
      ind <- sapply(x, is.character)
      x[ind] <- lapply(x[ind], factor)
      x
    })
    
    y <- reactive({
      x <- subset(traindata(), select=input$independent)
      is.complete <- which(complete.cases(x))
      x <- as.data.frame(x[is.complete,], stringsAsFactors = TRUE)
      ind <- sapply(x, is.character)
      x[ind] <- lapply(x[ind], factor)
      y <- subset(traindata(),select=input$dependent)
      as.factor(as.character(as.matrix(y[is.complete,])))
    })
    
    forest <- reactive({
      # create model
      createForest(x(),y(), trees= input$numtrees)
    })
    
    bagged <- reactive({
      createBagged(x(),y(), trees= input$numtrees)
    })
    
    output$plot_tree <- renderPlot({
      p <- plotTree(tree())
      print(p)
    }, height=700)
    
    output$plot_heat <- renderPlot({
      p <- plotHeatmap(tree(),traindata())
      print(p)
    }, height=700)
    
    output$plot_cp <- renderPlot({
      p <-  plotcp(tree())
      print(p)
    }, height=700)
    
    output$plot_prune <- renderPlot({
      tree_pruned <- prune(tree(), cp=input$cp )
      p <- plotTree(tree_pruned)
      print(p)
    }, height=700)
    
    
   output$plot_split <- renderPlot({
     p <- plotSplit(tree(), traindata(), input)
     print(p)
     }, height=500)
    
   output$plot_forest <- renderPlot({
     p <- plotForest(forest())
     print(p)
   }, height=500)
   
   output$compare <- renderPlot({
  
     #Compare the OOB error of Bagging vs. Random forests
     par(lwd=1, font=2)
     plot(bagged()$err.rate[,1],type = "l", col="red",xlim = c(0,100), ylim = c(0.1,0.4), ylab = "OOB Error", xlab = "Number of trees")
     par(new=T)
     plot(forest()$err.rate[,1],,type = "l", col="blue",xlim = c(0,100), ylim = c(0.1,0.4), ylab="", xlab="")
     legend('topright', c("Bagged CART", "Random Forest"),lty=1, col=c('red', 'blue'), bty='n', cex=.75)
     par(new=F)
     
   }, height=500)
   
    output$table <- DT::renderDataTable(
      DT::datatable(traindata(), options = list(pageLength = 25, scrollX = TRUE))
    )
    
    output$forest_accuracy = renderPrint({
       print(forest())
    })

    output$tree_accuracy = renderPrint({
      print(printcp(tree()))
    })
    
    output$min_xerror = renderPrint({
      cat(paste("min xerror:", min(tree()$cptable[,"xerror"]), "\n"))
    })
    
  }
)
