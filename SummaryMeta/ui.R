library(shiny)
library(DT) 

                                                                                                              
##################################################
header <- headerPanel(img(src="IndexBanner.jpg", height=120))

header[[2]]$attribs$id ="header"

ui <- fluidPage( 
   # tags$head(
   # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   # ),
# header,
sidebarLayout(
  sidebarPanel(width=1,
    
    h3("Download Index Table"),
    tags$a(href='INDEXYTable0.1.xlsx', target='blank', 'INDEXTable Download', download = 'InDExTable.xlsx')
    
  ),#end SideBarPanel
    mainPanel(  
      fluidRow(
        img(src = "CSI_3.jpg", align="right", height = 72, width = 336)),
      fluidRow(
       img(src="IndexBanner.jpg", align="left", height=72, width =1000)
       )
      ),#'End Main Panel'

 ),#End Sidebarlayout

  
  tabsetPanel(
    tabPanel("Data",
             titlePanel("INflammation Disease EXpression Table"),
             
             fluidRow(checkboxGroupInput('show_vars2', 'Columns in Table to show:', choices=NULL, inline=TRUE)),
             fluidRow(column (6, DT::dataTableOutput('x2'),  hr(),fluid=FALSE)),
             fluidRow(
               column(6, h3("Expression"),
                      plotOutput("main_plot2",  height = "600px"))
             )#End Fluid Row
    ),#End TabPanel

    tabPanel("Graphs",
             titlePanel("INflammation Disease EXpression Table"),

             # tags$a(href='INDEXYTable0.1.xlsx', target='blank', 'INDEXTable Download', download = 'InDExTable.xlsx'),
             selectInput("protein", "Choose protein", NULL),
             textOutput("protein"),
             radioButtons('go', label= h3("Select Input or Datatable search"),
                          choices = list("PullDown"=1, "DataTable"=2),
                          selected=2),

             fluidRow(checkboxGroupInput('show_vars1', 'Columns in Table to show:', choices=NULL, inline=TRUE)),
             fluidRow(radioButtons("action_selectiontype", "Selection type",
                                   choices = c("All"),
                                   selected = "All", inline = TRUE)),
             h3("Adjusted P Values"),

             fluidRow(column (6, DT::dataTableOutput('x1'),  hr(),fluid=FALSE)
             ),#End Fluid Row

             fluidRow(
               column(6, h3("Expression"),
                      plotOutput("main_plot",  height = "600px"))
             )#End Fluid Row
        ) # end tabPanel
  )#End Tabset Panel

  )#End UI
