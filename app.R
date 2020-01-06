##################################STEP OR #############################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("shinyWidgets")
library(stringr)
options(shiny.maxRequestSize=1000*1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Final sort"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      actionButton(inputId = "sauve", label = "sauvegarde")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      # tableOutput("contents"),
      fluidRow(
        radioButtons("radio", label = h3("Choix du tri"),
                     choices = list("Predefini (OR, RR...)" = 1, "Personalise" = 2, "les deux" = 3), 
                     selected = 1, inline = T)),
      fluidRow(
        conditionalPanel(
          condition = "input.radio !=1",     
          sliderInput("slider1", label = h3("Nombre de termes a garder"), min = 0, 
                      max = 6, value = 0),
          column(4,
                 conditionalPanel(
                   condition = "input.slider1 > 0",
                   textInput("text1", label = h3("mot a garder 1"), value = "NULL")),
                 conditionalPanel(
                   condition = "input.slider1 > 3",
                   textInput("text4", label = h3("mot a garder 4"), value = "NULL"))),
          
          column(4,
                 conditionalPanel(
                   condition = "input.slider1 > 1",
                   textInput("text2", label = h3("mot a garder 2"), value = "NULL")),
                 conditionalPanel(
                   condition = "input.slider1 > 4",
                   textInput("text5", label = h3("mot a garder 5"), value = "NULL"))),
          column(4,
                 conditionalPanel(
                   condition = "input.slider1 > 2",
                   textInput("text3", label = h3("mot a garder 3"), value = "NULL")),
                 conditionalPanel(
                   condition = "input.slider1 > 5",
                   textInput("text6", label = h3("mot a garder 6"), value = "NULL"))))),
      fluidRow(
        actionButton(inputId = "sort", label = "tri"),
        h3("nombre d\'articles avant tri"),
        verbatimTextOutput("avant_tri"),
        h3("nombre d\'articles apres tri"),
        verbatimTextOutput("tri"),
        downloadButton("downloadData", "Download") 
        
        
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  data = reactiveValues()
  
  observeEvent(input$sauve, {
    data$table = read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Le fichier a bien ete enregistre !",
      type = "success"
    )  
    
  })
  output$test <- renderPrint({"attente tri"})
  data2 <- reactiveValues()
  observeEvent(input$sort, {
    if (input$radio ==1 ||input$radio == 3 ) { # Predefini OR, RR
      
      memory <- c(" or ", " rr ", "relative risk"," odd ","odds")
      for (i in 1:length(memory)) {
        data$table[,i+2] <- str_detect(data$table[,2] ,memory[i])
      }
      data$table[,length(memory)+3] <- F
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "tri database", value = 0)
      for (i in 1:length(memory)) {
        for (j in 1:nrow(data$table)) {
          if (data$table[j,(i+2)]){data$table[j,(length(memory)+3)] <- T}
          progress$inc(1/(length(memory)*nrow(data$table)), detail = paste("Doing step", i,"part",j))
        }
      }
    } 
    if (input$radio == 2) {
      if (input$slider1>0) {
        memory <- c(input$text1, input$text2, input$text3, input$text4, input$text5,input$text6)
        for (i in 1:input$slider1) {
          data$table[,i+2] <- str_detect(data$table[,2] ,memory[i])
        }
        data$table[,input$slider1+3] <- F
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "tri database", value = 0)
        for (i in 1:input$slider1) {
          for (j in 1:nrow(data$table)) {
            if (data$table[j,(i+2)]){data$table[j,(input$slider1+3)] <- T}
            progress$inc(1/(input$slider1*nrow(data$table)), detail = paste("Doing step", i,"part",j))
          }
        }
        
      }
    }
    if (input$radio == 3) {
      if (input$slider1>0) {
        memory <- c(input$text1, input$text2, input$text3, input$text4, input$text5,input$text6)
        for (i in 1:input$slider1) {
          data$table[,i+8] <- str_detect(data$table[,2] ,memory[i])
        }
        data$table[,input$slider1+9] <- F
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "tri database", value = 0)
        for (i in 1:input$slider1) {
          for (j in 1:nrow(data$table)) {
            if (data$table[j,(i+8)]){data$table[j,(input$slider1+9)] <- T}
            progress$inc(1/(input$slider1*nrow(data$table)), detail = paste("Doing step", i,"part",j))
            data$table[,(input$slider1+10)]<- (data$table[,(input$slider1+9)] | data$table[,8])
          }
        }
        
      }
    }
    
    if (input$radio == 1) {
    data2$tri <- subset(data$table, data$table[,8] == T)
    }
    if (input$radio == 2) {
      data2$tri <- subset(data$table, data$table[,(input$slider1+3)] == T)
    }
    if (input$radio == 3) {
      data2$tri <- subset(data$table, data$table[,(input$slider1+10)] == T)
    }
    #           names(data2$tri<- c("uid","abstract",memory,"to remove"))
    data2$tri2 <- data2$tri[,1:2]
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "La base a bien ete triee !",
      type = "success"
    )
    
    
  })
  date_jour <- str_sub(date(),start = 9,end = 10)
  date_mois <- str_sub(date(),start = 5,end = 7)
  date_annee <- str_sub(date(),start = 21,end = 24)
  date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
  
  name_id <- str_c("shiny.stepOR_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".csv")
  value <- reactiveValues(download = 0)
  observeEvent(input$sort, {
    output$test <-renderPrint({
      head(data2$tri2)})
    output$avant_tri <-renderPrint({nrow(data$table)})
    output$tri <-renderPrint({nrow(data2$tri)})
    value$download <- 1
    
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      name_id
    },
    content = function(file) {
      write.csv(data2$tri2, file, row.names = FALSE)
    }
  )
  
}
# Create Shiny app ----
shinyApp(ui, server)