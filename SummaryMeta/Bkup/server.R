
library(shiny)
library(DT)   
library(ggplot2)
source("./Factor.R")
################################################
#Load Data.  This is run only once when the server.R is called

load("./Data/Meta.Rdata")
        
#Create table of results 
res <- Meta[,c(1,36:41)]
index <- grep("FDR", names(Meta))
Meta[,index]<- signif(Meta[index],2)
index <- grep("effectSize$|FC", names(Meta))
Meta[,index]<- round(Meta[index],2)

##End Load Data  
##################################################

shinyServer <- function(input, output, session) {  
  
  #I dont think this variable is currently used
  Title <- reactive({
    s = as.numeric(rownames(res[RowValue(),Names()])[input$x1_rows_selected])
    if (is.null(s)) s=2  
    if (length(s)==0) s=2 
    res[s,2]
  })
  
  #Select default variables to shown in the results table
  observe({
    choices2 <-  names(res)
    updateCheckboxGroupInput(session, "show_vars1",
                             label = "Select Columns to Include",
                             choices = choices2,
                             selected = c("FC", "Pval","gene", "TF", "maxExpr", "Limmaanova"                             
                             ) , 
                             inline = TRUE)
  })
  observe({
    choices2 <-  c("gene", "UC", "CD", "RA", "Pso", "NASH", "NAFLD", "AD", "SLE", "FC", "Pval", "TF")
    updateCheckboxGroupInput(session, "show_vars2",
                             label = "Select Columns to Include",
                             choices = choices2,
                             selected = c("gene", "FC", "Pval","gene", "TF", "maxExpr", "Limmaanova"                             
                             ) , 
                             inline = TRUE)
  })
  #Used to remove columns rows from data table() based on categories selected 
  #from show_vars1 checkbox on client
  Names <- reactive({
    NameLabels <- (input$'show_vars1')
    NameLabels
  }) 
  
  Names2 <- reactive({
    NameLabels <- (input$'show_vars2')
      Match <- grep(paste(NameLabels, collapse="|"),colnames(Meta), value=TRUE) 
      Match
  }) 
  
  ##Used to select rows based on slider inputs
  ## Currently the only option is All
  ## This is useful to have a small number of default genes in the results table
  RowValue <- reactive({
    Value <- as.numeric(row.names(unique(res[,Names()])))
    if(input$action_selectiontype == "TWO") Value <- c(792, 796, 797, 798)#c(20990, 3385,7582, 10233,8432)
    Value
  })
  

  
  # turn input selection to reactive
  Proteinnames <- reactive({
    NameLabels <- (input$'protein')
    print(sprintf("Proteinnames %s", NameLabels))
    NameLabels
  })
  
  
  #  if (input$go=="PullDown"){
  #  Make Datatable a reactive input  
  DataForTable <- reactive({
    Meta[,Names()]
  })
  DataForTable2 <- reactive({
    Meta[,Names2()]
  })
  
  #Set DataTable options
  output$x1 = DT::renderDataTable(DataForTable(), server = T, escape=T, selection = 'single', options=list(
    lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All'))))
  
  output$x2 = DT::renderDataTable(DataForTable2(), server = T, escape=T, selection = 'single', options=list(
    lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All'))))
  
  SW <- reactive({ 
    s = NULL
    s = as.numeric(rownames(DataForTable()[RowValue(),]))[input$x1_rows_selected]
    print(sprintf("Names() %s", Names()))
    print(sprintf("input$x1_rows_selected %s", input$x1_rows_selected))
    if (is.null(s)) s=2  
    if (length(s)==0) s=2 
    print(sprintf("row selected %s", s))
    
    s
  })
  SW2 <- reactive({ 
    s = NULL
    s = as.numeric(rownames(DataForTable()[RowValue(),]))[input$x2_rows_selected]
    print(sprintf("Names() %s", Names()))
    print(sprintf("input$x2_rows_selected %s", input$x2_rows_selected))
    if (is.null(s)) s=2  
    if (length(s)==0) s=2 
    print(sprintf("row selected %s", s))
    
    s
  })
  
  updateSelectInput(session, "protein", choices=res$gene_name[1:10], selected="MTOR")
  
  
  output$main_plot <-  renderPlot({
    generow <- as.numeric(SW())
    generow1 <- which(generow==as.numeric(row.names(res)))
    print(sprintf("main plot SW() %s", generow))
    # print(sprintf("mydata1 %s", mydata1))
    print(PlotFunction(num = generow1, meta = Meta))
    print("Complete1")
  })
  
  output$main_plot2 <-  renderPlot({
    generow <- as.numeric(SW2())
    generow1 <- which(generow==as.numeric(row.names(Meta)))
    print(sprintf("main plot SW() %s", generow))
    # print(sprintf("mydata1 %s", mydata1))
    print(PlotFunction(num = generow1, meta = Meta))
    print("Complete1")
  })
  
}