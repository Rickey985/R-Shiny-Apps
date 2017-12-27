library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(highcharter)
library(DT)
setwd('C:/Users/Abhijeet/Desktop/Men Hockey')
all_data=read.csv('all_data.csv')
all_data$ID=with(all_data, ifelse(name=='Adam Bignell', 55,
                                  ifelse(name=='Michael Morgan', 47,
                                         ifelse(name=='Phillip Fife', 27,
                                                ifelse(name=='Daniel Perigo', 25,
                          ifelse(name=='Nick Halagian', 23,
                                 ifelse(name=='Tommy Tsicos', 22,
                                        ifelse(name=='Markson Bechtold', 10,
                          ifelse(name=='Conner Cole', 72,
                                 ifelse(name=='Ryan Haynes', 7,
                                        ifelse(name=='Eric Cimino', 6,
                          ifelse(name=='Eric Diodati', 28,
                                 ifelse(name=='Cam Nicoll', 9,
                                        ifelse(name=='Kenny Turner', 12,
                          ifelse(name=='Mitchell Smith', 15,
                                 ifelse(name=='Lee Dower', 18,
                                        ifelse(name=='Cole Murphy', 19, 
                          ifelse(name=='Brandon Grandinetti', 20,
                                 ifelse(name=='Michael Siddall', 21,
                                        ifelse(name=='Mike Moffat', 40,
                          ifelse(name=='Joey Champigny',11, 'ABC')))))))))))))))))))))

###Game.Num=x axis, means the num of games passed


server=function(input,output,session){
  output$players=renderUI(selectizeInput('players',"Choose Player's ID Number:",
                                         choices={paste(unique(all_data[,1]),'-',unique(all_data[,2]))}
                                         ,multiple=F))
  output$graph_type=renderUI(selectizeInput('graph_type','Choose The Data To View',
                                            choices={c('Sensitivity Curve', names(all_data)[6:length(names(all_data))])}))
  
  
  abc=reactiveValues()
  observeEvent(input$players,{
    abc$players=input$players
  })
  observeEvent(input$graph_type,{
    abc$graph=input$graph_type
  })
  
  
  output$DT_Summary=renderDataTable({
    top_left=c(abc$players,'*Blank*')
    top_right=c(abc$graph,'*Blank*')
    
    abc=data.frame(top_left,top_right)
    abc
  },class='cell-border stripe',rownames=F,caption='This is a simple caption for the table',
  options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#7FFF00', 'color': '#000'});",
      "}")))
  
  
#To Implement:  Course C, Sensitivity Curves
  
}


ui=dashboardPage(header = dashboardHeader(title="Simple Shiny App"),
                 sidebar = dashboardSidebar(disable=T),
                 body=dashboardBody(
                   tags$style(type='text/css','#add_basement {}'),
                   
                   useShinyjs(),
                   column(6, align='center',wellPanel(
                     fluidRow(
                       strong(h4("A template for an extremely simple shiny app",align='center'))
                     ),
                     fluidRow('*Can place a small logo here*',align='center'),
                     fluidRow(column(12,div(style='height:20px'))),
                     fluidRow(column(4, uiOutput('players')),
                              column(4, uiOutput('graph_type'))
                              
                              ),
                     fluidRow(column(12,div(style='height:75px'))),
                     fluidRow(column(4,'Quick Player Summary'),column(8,dataTableOutput('DT_Summary')))
                     
                     
                   )), column(6)
                   
                   
                 )
)

shinyApp(ui=ui,server=server)