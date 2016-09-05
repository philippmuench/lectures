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
      createTree(formula(),traindata())
    })
 
    forest <- reactive({
      createForest(traindata(), input)
    })
    
    output$plot_tree <- renderPlot({
      p <- plotTree(tree())
      print(p)
    }, height=700)
    
    output$plot_heat <- renderPlot({
      p <- plotHeatmap(tree(),traindata())
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
   
    output$table <- DT::renderDataTable(
      DT::datatable(traindata(), options = list(pageLength = 25, scrollX = TRUE))
    )
    
  }
)
