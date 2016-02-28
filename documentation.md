---
title: "Documentation"
author: "Mait22"
date: "28 February 2016"
---

## General introduction to Shiny app  
My assignment uses dataset called „swiss“ from R package datasets.  Dataset “swiss” dataset describes Standardized fertility measure and socio-economic indicators for each of 47 French-speaking provinces of Switzerland at about 1888.


## The dataset consists of following 7 variables:  
1.  Region (added to dataset by myself from rownames), 
2.  Fertility, 
3.  Agricultural employment ratio (% of males involved in agriculture as occupation), 
4.  Examination (% ratio of military draftees receiving highest marks in tests), 
5.  Education (% ratio of draftees having educational level beyond primary educational level), 
6.  Catholic (% ratio of Catholic religion confession vs protestant religious confession) 
7.  Infant Mortality.


## Functionality of my app:  
My Shiny app lets users to select cases from total dataset of 47 regions by selecting cases to be included in analysis by region and educational level. This makes for example possible to:  
1.  check if model parameters are sensitive to certain single observations;  
2.  fit the model only on certain subset of observations (for example only on regions of Southern Switzerland). 


## My app fits and reports the following linear regression model:  
Dependent variable: Infant Mortality  
Explanatory (i.e. independent) variables: Fertility, Examination, Education, Catholic


##  Full code of UI part of my app  


```
library(shiny)


shinyUI(fluidPage(
    titlePanel("Assignment by Mait 22"),
    sidebarLayout(
        sidebarPanel("Select cases for analysis",
                     
                     
                     
                     
                     
                     
                     uiOutput("Area"),
                     checkboxInput("C1", label="Select all areas to analysis", value = TRUE),
                     sliderInput("Educational_level", 
                     "Select cases by educational level to be included in analysis:",
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
```

##  Full code of SERVER part of my app  


```
library(shiny)
library(DT)
library(dplyr)
library(markdown)

#Loading in the data
library(datasets)
data(swiss)
data <- swiss
data 
data$region <- row.names(data)


#Get unique values across one or many columns
#Two column slection capabilities of this function are not used in current assignment
unique_a_colm <- function(df, indexes){
       base_u <- unique(df[,indexes[1]])
       for (i in indexes){
              temp <- unique(df[,i])
              if(length(temp) >= 1){
                     for (n in temp)
                            if(n %in% base_u == FALSE){
                                   base_u[(length(base_u)+1)] <- n
                            }
              }
              
       }
       
       return(base_u)
}



#Shiny server function
shinyServer(function(input,output,session){


       output$Area <- renderUI({selectInput("Area_i", "Select area:", 
       as.list(unique_a_colm(data,c(7))),multiple = TRUE, 
       selected = as.list(unique_a_colm(data,c(7))))})

       
       
       observe({
              c1 <- input$C1
              
              
              if(c1==TRUE){
                     updateSelectInput(session, "Area_i", choices = as.list(unique_a_colm(data,c(7))),selected = as.list(unique_a_colm(data,c(7))))
              }
              if(c1==FALSE){
                  updateSelectInput(session, "Area_i", choices = as.list(unique_a_colm(data,c(7))),selected = NULL)
              }

       })
       
       subseted_data <- eventReactive(input$go,{
        
        subseted_data <- data[data$Education <= input$Educational_level & (data$region) %in% unlist(input$Area_i),]       
       

       return(subseted_data)
       
       })
       
       
       output$vasteid_andmebaasis<- renderText({paste("Observations based on Your selection: ",
       as.character(dim(subseted_data())[1]))})
       
       
       output$tbl = DT::renderDataTable(subseted_data(),
                                        server = TRUE, 
                                        options = list(dom = 'C<"clear">lfrtip'))
     
       
       
       
       linear_model <- eventReactive(input$go,{
           
            iput_data <- subseted_data()
            model <- lm(Infant.Mortality ~ Fertility + Agriculture + Examination + Catholic + Education, data = iput_data)
           return(model)
           
       })
       
       
       
       output$modelSummary <- renderPrint({
           summary(linear_model())
           
           })
       
       
})

```






