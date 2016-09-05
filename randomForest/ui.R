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
  fluidPage(
    fluidRow(column(4,
                    wellPanel(
                    
conditionalPanel(
  condition="input.tsp=='map'",
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
  condition="input.tsp=='dfg'",
  selectInput("response",
              label = "response variable",
              choices = c(0,1), 
              selected = 0)
)

)),

    column(8,(
      tabsetPanel(
      tabPanel("Dataset", div(DT::dataTableOutput("table"), style = "font-size:80%",  width = 10), value="map"),
      tabPanel("single tree", plotOutput("plot_tree"), value="map"),
      tabPanel("heatmap", plotOutput("plot_heat"), value="map"),
      tabPanel("splitpoints", plotOutput("plot_split"), value="map"),
      tabPanel("ensemble", plotOutput("plot_forest"), value="forest")
    
      ,id="tsp"
      )
     # plotOutput("plot1", width="100%", height="auto")
    )
  ))
))