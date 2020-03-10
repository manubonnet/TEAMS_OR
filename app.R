##################################STEP OR #############################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#library(backports)
if (!require("devtools"))
  install.packages("devtools")
if (!require("processx"))
  install.packages("processx")
#install.packages("nlme")
library(nlme)
if (!require("scales"))
  install.packages("scales")
library("devtools")
library(rentrez)
library("XML")
#install.packages("reutils",check_built=T)
# library(reutils)

library(lubridate)
#devtools::install_github("gschofl/reutils")
set_entrez_key("2f426efbccf334610530e682833b93e33508")
Sys.getenv("ENTREZ_KEY")
options("scipen"=100)
options(reutils.api.key = "34ad5abbcddf5a94d9dbfb34ad005be64d0a")
options(reutils.email = "emmanuelbeaunez@gmail.com")
#rsconnect::appDependencies()

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
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      fluidRow(
        column(6,
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ",")     
        ),
        column(6,
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"')
        )
      ),
      
      
      # Input: Select number of rows to display ----
      actionButton(inputId = "sauve", label = "sauvegarde")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      conditionalPanel("input.sauve",  
                       # Output: Data file ----
                       # tableOutput("contents"),
                       fluidRow(
                         radioButtons("radio", label = h3("Sorting choice:"),
                                      choices = list("Predefined (OR, RR, HR)" = 1, "Custom" = 2, "Both" = 3), 
                                      selected = 1, inline = T)),
                       fluidRow(
                         conditionalPanel(
                           condition = "input.radio !=1",     
                           sliderInput("slider1", label = h3("Nombre de termes a garder"), min = 1, 
                                       max = 6, value = 1),
                           column(4,
                                  conditionalPanel(
                                    condition = "input.slider1 > 0",
                                    textInput("text1", label = h3("mot a garder 1"), value = "Enter a word")),
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
                         conditionalPanel("input.sort",
                                          h3(textOutput("tri")),
                                          downloadButton("downloadData", "Download"),
                                          h4(uiOutput("tab"))
                         )
                       )
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  name_RF= reactiveValues()
  name_RF <- "RF"
  data = reactiveValues()
  
  observeEvent(input$sauve, {
    sendSweetAlert(
      session = session,
      btn_labels = NA,
      title = "Saving database",
      text = "Please wait until \"Done !\" appears on your screen.",
      closeOnClickOutside = F,
      type = "warning"
    )
    data$table = read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
    name_RF <- str_split(input$file1$name, "_", simplify = TRUE)[2]
    output$name_RF <- renderText(name_RF)
    outputOptions(output, "name_RF", suspendWhenHidden = FALSE)
    extract<- data$table
    a <- entrez_search(db = "pubmed", term = paste(extract[,1],collapse = " ") ,use_history = T) 
    
    a$QueryTranslation
    article <- entrez_fetch(db="pubmed",web_history =a$web_history ,rettype ="xml",parsed = T)
    
    b <- getNodeSet(article,"//MedlineCitation")
    mem <- NULL
    newid <- NULL
    for (i in 1:length(b)) {
      bbb <- xmlSerializeHook(b[[i]])
      bbb <- xmlDeserializeHook(bbb)
      ttt <- XML::xpathSApply(bbb, "//Abstract", XML::xmlValue)
      aaa <- XML::xpathSApply(bbb, "///MedlineCitation/PMID[@Version='1']", XML::xmlValue)
      if(length(ttt)==0) ttt <- "NA"
      if(length(aaa)==0) aaa <- "NA"
      ttt <- paste((XML::xpathSApply(bbb, "//ArticleTitle", XML::xmlValue)),ttt)
      mem<-c(mem,ttt)
      newid <-c(newid,aaa)
    }
    extract$newid <- newid
    extract$newabs <- mem
    data$table <- cbind(extract$newid,extract$newabs)
    
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Database saved !",
      type = "success"
    )  
    
  })
  output$test <- renderPrint({"attente tri"})
  data2 <- reactiveValues()
  observeEvent(input$sort, {
    data$table <- data$table[,1:2]
    lin_av <- "????"
    if (input$radio ==1 ||input$radio == 3 ) { # Predefini OR, RR
      
      memory <- c("OR", "RR", "relative risk","odd","odds","Odd","Odds","Relative risk", "Relative Risk","HR","Hazard Ratio","hazard ratio")
      for (i in 1:length(memory)) {
        data$table <- cbind(data$table,str_detect(data$table[,2] ,memory[i]))
      }
      data$table <- cbind(data$table,F)
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
        data$table <- as.data.frame(data$table)
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
        data$table <- as.data.frame(data$table)
        memory2 <- c(input$text1, input$text2, input$text3, input$text4, input$text5,input$text6)
        for (i in 1:input$slider1) {
          data$table[,i+length(memory)+3] <- str_detect(data$table[,2] ,memory2[i])
        }
        data$table[,input$slider1+1+length(memory)+3] <- F
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "tri database", value = 0)
        for (i in 1:input$slider1) {
          for (j in 1:nrow(data$table)) {
            if (data$table[j,(i+length(memory)+3)]){data$table[j,(input$slider1+1+length(memory)+3)] <- T}
            progress$inc(1/(input$slider1*nrow(data$table)), detail = paste("Doing step", i,"part",j))
           
          }
        }
        data$table[,length(memory)+3] <- as.logical(data$table[,length(memory)+3])
        data$table[,(input$slider1+1+length(memory)+4)]<- (data$table[,(input$slider1+1+length(memory)+3)] | data$table[,length(memory)+3])
      }
    }
    
    if (input$radio == 1) {
      data2$tri <- subset(data$table, data$table[,(length(memory)+3)] == T)
    }
    if (input$radio == 2) {
      data2$tri <- subset(data$table, data$table[,(input$slider1+3)] == T)
    }
    if (input$radio == 3) {
      data2$tri <- subset(data$table, data$table[,(input$slider1+length(memory)+5)] == T)
    }
    #           names(data2$tri<- c("uid","abstract",memory,"to remove"))
    data2$tri2 <- data2$tri[,1:2]

    names(data2$tri2)<- c("Pubmed uID","Title and Abstract")
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "La base a bien ete triee !",
      type = "success"
    )
    date_jour <- str_sub(date(),start = 9,end = 10)
    date_mois <- str_sub(date(),start = 5,end = 7)
    date_annee <- str_sub(date(),start = 21,end = 24)
    date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
    name_RF2 <- str_split(input$file1$name, "_", simplify = TRUE)[2]
    name_id <- str_c("shiny.stepOR_",name_RF2,"_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".csv")
    # col.names(data2$tri2)<- c("uid","abstract")
    # print(head(data2$tri2))
    #name_id <- isolate(name_id)
    output$downloadData <- downloadHandler(
      filename = function() {
        name_id
      },
      content = function(file) {
        write.table(data2$tri2, file, row.names = FALSE, sep = ";")
      }
    )
    
    ids <- paste0(data2$tri2[,1],collapse = "+")
    url_pub <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=",ids)
    
    url <- a("PubMed link", href=url_pub)
    output$tab <- renderUI({
      tagList("Direct", url,"to see these",nrow(data2$tri),"articles on PubMed website.")
    })
    
   # https://www.ncbi.nlm.nih.gov/pubmed/?term=
    
  })

  value <- reactiveValues(download = 0)
  observeEvent(input$sort, {
    output$test <-renderPrint({
      head(data2$tri2)})
    output$avant_tri <-renderText({paste(nrow(data$table),"articles before sorting")})
    output$tri <-renderText({paste(nrow(data$table),"articles before sorting, and",nrow(data2$tri),"articles after sorting.")})
    value$download <- 1
    
  })

  
}
# Create Shiny app ----
shinyApp(ui, server)