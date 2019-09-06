library(shiny)
library(DT) 
library(plotly)

                                                                                                              
##################################################
header <- headerPanel(img(src="IndexBanner.jpg", height=120))

header[[2]]$attribs$id ="header"

ui <- fluidPage( 
   # tags$head(
   # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   # ),
# header,
  titlePanel("INflammation Disease EXpression Table Development Version"),
sidebarLayout(
  sidebarPanel(width=1,
    
    h3("Download Index Table"),
    tags$a(href='MetadevAnalysis.txt', target='blank', 'INDEXTable Download', download = 'InDExTableDev.txt')
    
  ),#end SideBarPanel
    mainPanel( 
      HTML('<center><img src="CSI_3.jpg" height = "72" width = "336"></center>'),
      fluidRow(
       img(src="IndexBanner.jpg", align="left", height=72, width =1000)
       )
      )#'End Main Panel'

 ),#End Sidebarlayout

  
  tabsetPanel(
    tabPanel("Background",
             titlePanel("INflammation Disease EXpression Development Table"),
             column(6, h3("Background"),
                    h4("The INDEX table attempts to summarize gene expression data from Immune related diseases.
                       In general most of the comparisons are from disease vs normal/healthy disease.  The data is the results
                      of a Meta-analysis of public RNA expression data."),
                    h4(""),
                    h4("The data is derived from DiseaseExpress and supplement with 
                       some additional studies."),
                    
                    h4("The data is presented as fold change and pvalue calculations for each disease tissue pair")),
             column(6, h3 ("Background2"),
                    h4("The entire table can be download using the link in the upper right corner.
                       Data can search by clicking on the Data tab"),
                    h4("Conact anyone in Computational Systems Immunology for more assistance or Scott Jelinsky"),
                    a("Contact Scott Jelinsky", 
                      href="mailto:scott.jelinsky@pfizer.com")
             ),
             column(12, 
                    h3("This is a beta version.  Expect major updates to the data and the site shortly"))
    ),#End TabPanel
    tabPanel("Graphs",
             titlePanel("INflammation Disease EXpression Table"),
             helpText("Note: This is a placeholder for some help text"),
             
             fluidRow(checkboxGroupInput('show_vars2', 'Columns in Table to show:', choices=NULL, inline=TRUE)),
             fluidRow(column (6, DT::dataTableOutput('x2'),  hr(),fluid=FALSE)),
             fluidRow(
               column(6, h3("Expression"),
                      plotOutput("main_plot",  height = "600px")),
              column(6, h3("Disease"),
                     selectInput('show_vars3', 'Disease Comparison:', choices=NULL),
                      plotlyOutput("main_plot2",  height = "600px"))
             ),#End Fluid Row
             fluidRow(
               column(6, h3("Expression"),
                      plotOutput("main_plot1",  height = "400px"))
             )#End Fluid Row
    ),#End TabPanel

    tabPanel("Individual Study Data AT",
             titlePanel("INflammation Disease EXpression Table"),
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
                      plotlyOutput("main_plot3",  height = "600px"))
             )#End Fluid Row
        ) # end tabPanel
  )#End Tabset Panel

  )#End UI
