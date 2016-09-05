library(shiny)
library(ggplot2)
library(rpart)
library(gplots)
library(RColorBrewer)
library(plotmo)
library(randomForest)
library(stats)

set.seed(121)

source("tree.R")
source("forest.R")

traindata <- read.csv("titanic.csv", stringsAsFactors=FALSE)

shinyUI(
  fluidPage(h3("Random forest lecture"),
    fluidRow(column(4,
                    wellPanel(
                    
conditionalPanel(
  condition="input.tsp=='map' | input.tsp=='forest' | input.tsp == 'cp' | input.tsp=='tree'",
  selectInput("dataset", 
              label = "dataset",
              choices = c("Titanic"),
              selected = "Titanic"),   
  
   selectInput("dependent", 
                  label = "dependent variable",
                  choices = colnames(traindata),
                  selected = colnames(traindata)[2]),
      
      selectInput("independent", 
                  label = "indepenend variables",
                  choices = colnames(traindata),
                  selected =colnames(traindata)[5:7] , multiple = T)
    
    ),
conditionalPanel(
  condition="input.tsp=='forest'",
  sliderInput("numtrees", label = ("Number of trees"), min = 1, 
              max = 5000, value = 100),
  helpText(strong("Gini")),
  helpText("Gini is a measure of node impurity. A low Gini (decrease in Gini) means that a particular predictor variable plays a greater role in portioning the data into the defined classes."),
  helpText(strong("Gini importance")),
  helpText("Gini importance measures the average gain of purity by splits of a given variable."),
  helpText(strong("Mean Decrease Accuracy")),
  helpText("The decrease in model accuracy from permuting the values in each feature."),
  helpText(strong("out-of-bag (OOB) error estimate")),
  helpText("because each tree is constructed using a different bootstrap sample from the data. The proportion of times that j is not equal to the true class averaged over all cases is the OOB error estimate.")
  
),
conditionalPanel(
  condition="input.tsp=='cp'",
  textInput("cp", label = ("cp"), value = 0.01), 
  helpText(strong("Pruning")),
  helpText("reduces the size of decision trees by removing sections of the tree that provide little power to classify instances. -> selected nodes are automatically optimized  based on cross-validation to prevent overfitting"),
  helpText(strong("cross validated error summary")),
  helpText("Plotcp() provides a graphical representation to the cross validated error summary. The cp values are plotted against the geometric mean to depict the deviation until the minimum value is reached.")
), 
conditionalPanel(
  condition="input.tsp=='tree' | input.tsp=='cp'",
  checkboxInput("lowcp", label = "lowCP", value = TRUE),
  helpText(strong("Cross-Validation error")),
  helpText("Root node error * xerror * 100"),
  helpText(textOutput("min_xerror"))
)

)),

    column(8,(
      tabsetPanel(
      tabPanel("table", div(DT::dataTableOutput("table"), 
                            style = "font-size:80%",  width = 10), value="map"),
      tabPanel("tree", plotOutput("plot_tree", width="100%", height="auto"),
               verbatimTextOutput("tree_accuracy"),  value="tree"),
      tabPanel("tree - heatmap", plotOutput("plot_heat",
                                            width="100%", height="auto"), value="map"),
      tabPanel("tree - splitpoints", plotOutput("plot_split", width="100%", 
                                                height="auto"), value="map"),
      tabPanel("tree - cp", plotOutput("plot_prune", width="100%", height="auto"),  
               plotOutput("plot_cp", width="100%", height="auto"), value="cp"),
      tabPanel("ensemble", plotOutput("plot_forest", width="100%", height="auto"), verbatimTextOutput("forest_accuracy"), value="forest")
    
      ,id="tsp"
      )
     # plotOutput("plot1", width="100%", height="auto")
    )
  ))
))