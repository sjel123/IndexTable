
library(shiny)
library(DT)   
library(ggplot2)
library(plotly)
source("./Factor.R")
################################################
#Load Data.  This is run only once when the server.R is called

#load("../Data/Metadev.Rdata")
load("/app/Shiny/IndexTable2/Data/MetaAnalysis2/Meta2_0.rds")
Meta <- Meta1 


#Create table of results 
    res <- Meta#[,c(1,42:47)]
    res[,2:length(res)] <- signif(res[,2:length(res)],2)
    Meta[,2:length(Meta)] <- signif(Meta[,2:length(Meta)],2)  
#Load Data for individual Studies
#load("../Data/IndivAT.RData") #(Loads Data2 dataframe)
      
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
                             selected = c("FC", "Pval","gene", "TF", "maxExpr", "Limmaanova","effectSize", "FDR", "hetPval" ), 
                             inline = TRUE)
  })
  observe({
    choices2 <-  c("gene", "uc_", "cd_", "ra_", "pso", "nash", "nafld", "ad_", "sle","effectSize", "FDR", "hetPval")
    updateCheckboxGroupInput(session, "show_vars2",
                             label = "Select Columns to Include",
                             choices = choices2,
                             selected = c("ra_","gene"                            
                             ) , 
                             inline = TRUE)
  })
  
  observe({
    choices2 <-  names(Meta)[grep("FDR", names(Meta))]
      choices2 <- gsub("_effectSizeFDR", '', choices2)
      updateSelectInput(session, "show_vars3",
                             label = "Select Disease Comparison",
                             choices = choices2,
                             selected = c("ra_healthy_synovial"                             
                             )) 
                        })
  
  #Used to remove columns rows from data table() based on categories selected 
  #from show_vars1 checkbox on client
  Names <- reactive({
    NameLabels <- (input$'show_vars1')
    NameLabels
  }) 
  
  Names2 <- reactive({
    NameLabels <- (input$'show_vars2')
      matches <-  grep(paste(NameLabels,collapse="|"), 
                            colnames(Meta), value=TRUE)
    matches
  }) 
  Disease <- reactive({
    Dise<- input$'show_vars3'
    Dise
  }) 
  
  ##Used to select rows based on slider inputs
  ## Currently the only option is All
  ## This is useful to have a small number of default genes in the results table
  RowValue <- reactive({
    Value <- as.numeric(row.names(res[,Names()]))
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
  DataForTable2 <- reactive({
    Meta[,Names2()]
  })
  #For Individual Study Tab
  DataForTable <- reactive({
    index <- grep("gene|ADy", names(Meta))
    Meta[,index]
  })
  
  #Set DataTable options
  #For Individual Study Tab
  output$x1 = DT::renderDataTable(DataForTable(), server = T, escape=T, selection = 'single', options=list(
    lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All'))))
  #For Graph Tab
  output$x2 = DT::renderDataTable(DataForTable2(), server = T, escape=T, selection = 'single', options=list(
    lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')))) 
 
   #For Individual Study Tab
  SW <- reactive({ 
    s = NULL
    s = as.numeric(rownames(DataForTable()[,]))[input$x1_rows_selected]
    print(sprintf("Names() %s", Names()))
    print(sprintf("input$x1_rows_selected %s", input$x1_rows_selected))
    if (is.null(s)) s=2  
    if (length(s)==0) s=2 
    print(sprintf("row selected %s", s))
    
    s
  })
  
  #For graph Tab
  SW2 <- reactive({ 
    s = NULL
    s = input$x2_rows_selected
    print(sprintf("Names() %s", Names()))
    print(sprintf("input$x2_rows_selected %s", input$x2_rows_selected))
    if (is.null(s)) s=24769  
    if (length(s)==0) s=24769 
    print(sprintf("row selected %s", s))
    
    s=input$x2_rows_selected
  })
  
  updateSelectInput(session, "protein", choices=res$gene_name[1:10], selected="MTOR")
  
  
  output$main_plot <-  renderPlot({
    generow <- as.numeric(SW2())
    generow1 <- which(generow==as.numeric(row.names(Meta)))
    print(sprintf("main plot SW() %s", generow))
    # print(sprintf("mydata1 %s", mydata1))
    print(PlotFunction2(num = generow1, meta = Meta)[[1]])
    print("Complete1")
  })
  output$main_plot1 <-  renderPlot({
    generow <- as.numeric(SW2())
    generow1 <- which(generow==as.numeric(row.names(Meta)))
    print(sprintf("main plot SW() %s", generow))
    # print(sprintf("mydata1 %s", mydata1))
    print(PlotFunction2(num = generow1, meta = Meta)[[2]])
    print("Complete1")
  })
    
  output$main_plot2 <-  renderPlotly({
      index2 <- grep(Disease(), names(Meta))
        Data1 <- Meta[,c(1, index2)] 
          highlight.gene <- Meta[SW2(),1]
        Color <-(ifelse(Data1$gene == Meta[SW2(),1], "red", "grey50"))
        Size <-(ifelse(Data1$gene == Meta[SW2(),1], 3, 1))
    #print(VolcanoPlot(Data=Data1, Disease=Disease()))
    require(plotly)
    colnames(Data1) <- c("Gene", "FC", "PVal")
    p <- ggplot(Data1, aes(x=as.numeric(FC), y=-log10(PVal), label=Gene)) + geom_point(aes(color=Color, size=Size)) +
      ggtitle(Disease())+
      theme(legend.position="none")
    print("Complete2")
    ggplotly(p)
    p
    
  })
  
  #For Individual Study Tab

}