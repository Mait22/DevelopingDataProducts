library(shiny)


shinyUI(fluidPage(
    titlePanel("Assignment by Mait 22"),
    sidebarLayout(
        sidebarPanel("Select cases for analysis",
                     
                     
                     
                     
                     
                     
                     uiOutput("Area"),
                     checkboxInput("C1", label="Select all areas to analysis", value = TRUE),
                     sliderInput("Educational_level", "Select cases by educational level to be included in analysis:",
                                 min = 1, max = 53, value = 53),
                     
                     actionButton("go","Click to update selection")
        ),
        
        
        
        mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
            
            
            tabsetPanel(
                
                
                
                tabPanel("Filtered data",
                         textOutput("vasteid_andmebaasis"),
                         DT::dataTableOutput('tbl')),
                
                tabPanel(title = "Linear model to explain variable InfantMortality",
                         verbatimTextOutput("modelSummary")),
                
                tabPanel(title = "Documentation of assignment", 
                         fluidRow(includeMarkdown("documentation.md")))
                
                
                
                
                    
                    
                )))))
                
                
                
                
