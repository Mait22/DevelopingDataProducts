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


       output$Area <- renderUI({selectInput("Area_i", "Select area:", as.list(unique_a_colm(data,c(7))),multiple = TRUE, selected = as.list(unique_a_colm(data,c(7))))})

       
       
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
       
       
       output$vasteid_andmebaasis<- renderText({paste("Observations based on Your selection: ",as.character(dim(subseted_data())[1]))})
       
       
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




       
       
       

       

       

       
       
       
       
       
       
       
       
       
        
