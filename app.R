#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets4)


familiarity<-function(x){
  x<-x[!is.na(x)]
  y<-ifelse(x=="I have heard of it and could describe it to someone, but I haven't seen it -OR- I have seen it but can't remember much about it",5,
     ifelse(x=="I have seen it and remember it well",10,0))

  return(mean(y,na.rm=TRUE))
}


scene_score<-function(r,s){
  ss<-10-max((10-(10/(((s-5)/15)+1))),0)-((10/(1+((10/r)-1)^-((abs(5-s)/10)+1)))/((abs(5-s)/15)+1))
  return(ss)
}


sn<-sheet_names("https://docs.google.com/spreadsheets/d/1JlaUvRKJx0Vi7E70xzq1J-LV4JJsVXngzDE-dHFjpek/edit#gid=1635074780")
sn_movies<-sn[!sn %in% c("Mailing List", "Data Summary")]
#sn_movies<-c("",sn[!sn %in% c("Mailing List", "Data Summary")])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Scene Score"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            selectInput("movie",
                        label = "what movie did we watch son??",
                        choices = sn_movies,
                        selected = NULL,
                        width = "100%"
                        ),

            sliderInput("colin",
                        label = "Colin",
                        min=0,
                        max=10,
                        value=0,
                        step=.1
                        ),

            sliderInput("mark",
                        label = "Mark",
                        min=0,
                        max=10,
                        value=0,
                        step=.1
                        ),

            sliderInput("jackson",
                        label = "Jackson",
                        min=0,
                        max=10,
                        value=0,
                        step=.1
                        ),

        ),

        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput("ourscores_label"),
          verbatimTextOutput("ourscores"),
          verbatimTextOutput("familiar_label"),
          verbatimTextOutput("familiar"),
          verbatimTextOutput("scenescore_label"),
          verbatimTextOutput("scenescore")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  myData <- reactive({
    read_sheet("https://docs.google.com/spreadsheets/d/1JlaUvRKJx0Vi7E70xzq1J-LV4JJsVXngzDE-dHFjpek/edit#gid=1635074780",sheet=input$movie)
  })

  ourscores <- reactive({
    round(mean(c(input$colin,input$mark,input$jackson),na.rm=TRUE),2)
  })

  familiar <- reactive({
    sn<-read_sheet("https://docs.google.com/spreadsheets/d/1JlaUvRKJx0Vi7E70xzq1J-LV4JJsVXngzDE-dHFjpek/edit#gid=1635074780",sheet=input$movie)
    round(familiarity(sn[,2]),2)
  })

  scenescore <- reactive({
    sn<-read_sheet("https://docs.google.com/spreadsheets/d/1JlaUvRKJx0Vi7E70xzq1J-LV4JJsVXngzDE-dHFjpek/edit#gid=1635074780",sheet=input$movie)
    fam<-round(familiarity(sn[,2]),2)
    os<-round(mean(c(input$colin,input$mark,input$jackson),na.rm=TRUE),2)

    scene_score(os,fam)

    #scene_score(output$ourscores,output$familiar)
  })

  output$ourscores_label <- renderText({"Our Average Score [0-10]:"})
  output$familiar_label <- renderText({"Average Familiarity [0-10]:"})
  output$scenescore_label <- renderText({"Scene Score [0-10]:"})

  output$ourscores <- renderText({ourscores()})
  output$familiar <- renderText({familiar()})
  output$scenescore <- renderText({scenescore()})

}

# Run the application
shinyApp(ui = ui, server = server)
