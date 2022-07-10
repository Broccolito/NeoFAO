library(shiny)
library(bslib)
library(reactable)
library(dplyr)
library(pracma)

ui = fluidPage(
  titlePanel("NeoFAO"),
  theme = bs_theme(version = 4, bootswatch = "cosmo"),
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h5("Met1"),
      
      fluidRow(
        column(width = 4,
               numericInput(inputId = "met1x", label = NULL, value = 580)
        ),
        column(width = 4,
               numericInput(inputId = "met1y", label = NULL, value = 370)
        ),
        column(width = 4,
               numericInput(inputId = "met1z", label = NULL, value = 480)
        )
        
      ),
      
      h5("Met5"),
      
      fluidRow(
        column(width = 4,
               numericInput(inputId = "met5x", label = NULL, value = 740)
        ),
        column(width = 4,
               numericInput(inputId = "met5y", label = NULL, value = 460)
        ),
        column(width = 4,
               numericInput(inputId = "met5z", label = NULL, value = 490)
        )
      ),
      
      h5("Calcaneus"),
      
      fluidRow(
        column(width = 4,
               numericInput(inputId = "calcaneusx", label = NULL, value = 590)
        ),
        column(width = 4,
               numericInput(inputId = "calcaneusy", label = NULL, value = 785)
        ),
        column(width = 4,
               numericInput(inputId = "calcaneusz", label = NULL, value = 487)
        )
      ),
      
      h5("Talus"),
      
      fluidRow(
        column(width = 4,
               numericInput(inputId = "talusx", label = NULL, value = 585)
        ),
        column(width = 4,
               numericInput(inputId = "talusy", label = NULL, value = 690)
        ),
        column(width = 4,
               numericInput(inputId = "talusz", label = NULL, value = 300)
        )
      ),
      
      br(),
      selectInput(inputId = "method", label = "Method", choices = c("FAO Talus Real-time" = "NeoFAO", "FAO TALAS" = "FAO"), selected = "FAO Talus Real-time"),
      hr(),
      actionButton(inputId = "calculate_fao", label = "Calculate Offset")
      
    ),
    
    mainPanel = mainPanel(
      
      h5("Measurements"),
      reactableOutput("input_parameters"),
      h5("Method"),
      textOutput("method_used"),
      hr(),
      h5("Offset (%)"),
      textOutput("offset_percentage"),
      hr(),
      plotOutput("offset_plot")
      
    )
  )
  
)

server = function(input, output, session) {
  
  source("get_fao.R")
  source("get_fao_plot.R")
  source("get_fao_input.R")
  
  values = reactiveValues()
  
  observeEvent(input$calculate_fao,{
    
    values[["met1"]] = c(isolate(input$met1x),isolate(input$met1y),isolate(input$met1z))
    values[["met5"]] = c(isolate(input$met5x),isolate(input$met5y),isolate(input$met5z))
    values[["calcaneus"]] = c(isolate(input$calcaneusx),isolate(input$calcaneusy),isolate(input$calcaneusz))
    values[["talus"]] = c(isolate(input$talusx),isolate(input$talusy),isolate(input$talusz))
    values[["parallel_to_foot"]] = ifelse(isolate(input$method) == "NeoFAO", TRUE, FALSE)
    values[["method"]] = ifelse(isolate(input$method) == "NeoFAO", "FAO Talus Real-time", "FAO TALAS")
    
    output$input_parameters = renderReactable({
      fao_input = get_fao_input(met1 = values[["met1"]], 
                                met5 = values[["met5"]], 
                                calcaneus = values[["calcaneus"]], 
                                talus = values[["talus"]], 
                                parallel_to_foot = values[["parallel_to_foot"]])
      reactable(fao_input)
    })
    
    output$method_used = renderText({
      values[["method"]]
    })
    
    output$offset_percentage = renderText({
      fao = get_fao(met1 = values[["met1"]], 
                    met5 = values[["met5"]], 
                    calcaneus = values[["calcaneus"]], 
                    talus = values[["talus"]], 
                    parallel_to_foot = values[["parallel_to_foot"]])
      paste0(as.character(round(fao, 2)), " %")
    })
    
    output$offset_plot = renderPlot({
      plt = get_fao_plot(met1 = values[["met1"]], 
                         met5 = values[["met5"]], 
                         calcaneus = values[["calcaneus"]], 
                         talus = values[["talus"]], 
                         parallel_to_foot = values[["parallel_to_foot"]])
      plt
    })
    
  })
  
}

shinyApp(ui, server)