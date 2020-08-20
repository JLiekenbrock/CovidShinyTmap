library(tmap)
library(shiny)


source("coronaData.R")
#library(rsconnect)



ui <- fluidPage(
  titlePanel("Interactive map of Covid-19 outbreak"),
  tmapOutput("map"),
  
  fluidRow(
  ),
  fluidRow(
    column(width=4,
      selectInput("var", "Variable", names(total2[c(19:32)])[1:14]),
    ),
    column(width=4,
       sliderInput(inputId = "slider", 
                   label = "Date",
                   min = min(total2$RDate),
                   max = max(total2$RDate),
                   value =  min(total2$RDate),
                   step = 1,
                   animate = TRUE),
    ),
  ),
)

server <- function(input, output, session) {
  lastindex <- reactiveVal(0)
  animationmode <- reactiveVal(0)
  lastvar <- reactiveVal(0)
  
  ## create static element
  output$map <- renderTmap({
    tm_shape(total2[total2$RDate==min(total2$RDate),]) + 
      tm_polygons(col="total_deaths",
                  zindex = 401,
                  id="location",)
  })
  
  observe({
    if(input$var=="total_deaths"){
      vbreaks=c(0,0.1,200,2000,30000,100000,10000000)
      vlabels=c("0","1 to 199","200 to 1,999","2,000 to 29,999","30,000 to 99,999", "more than 100,000")
    } else if (input$var=="total_cases"){
      vbreaks =c(0,1000,20000,100000,500000,1000000,3000000)
      vlabels = c("0 to 999","1,000 to 19,999","20,000 to 99,999", "100,000 to 499,999", "500,000 to 999,999", "more than 1,000,000")
    } else if (input$var=="total_cases_per_million"){
      vbreaks = c(0,50,200,1000,5000,10000,50000)
      vlabels = c("0 to 50", "50 to 200", "200 to 1,000", "1,000 to 5,000", "5,000 to 10,000","more than 10,000")
    } else if (input$var=="total_deaths_per_million"){
      vbreaks = c(0,1,10,100,500,15000)
      vlabels = c("0 to 1", "1 to 10","10 to 100", "100 to 500","more than 500")
    } else if (input$var=="new_cases"){
      vbreaks = c(0,0.1,50,500,3000,15000,1000000)
      vlabels = c("0","1 to 49","50 to 499","500 to 2,999","3,000 to 14,999","more than 15,000")
    } else if (input$var=="new_deaths"){
      vbreaks = c(0,0.1,10,50,100,500,100000)
      vlabels = c("0","1 to 9","10 to 49","50 to 99", "100 to 499", "more than 500")
    } else if (input$var=="new_cases_per_million"){
      vbreaks = c(0,1,10,50,100,500)
      vlabels = c("0 to 1","1 to 10","10 to 49","50 to 99", "more than 100")
    } else if (input$var=="new_deaths_per_million"){
      vbreaks = c(0,1,5,10,50)
      vlabels = c("0 to 1","1 to 5","5 to 10","more than 10")
    } else if (input$var=="new_tests_per_thousand" | input$var=="new_tests_smoothed_per_thousand"){
      vbreaks = c(0,1,2,5,10)
      vlabels = c("0 to 1","1 to 2","2 to 5","more than 5")
    } 
    else {
      vbreaks = round(seq(0,max(total2[[input$var]],na.rm=TRUE),length.out=6),digits=0)
      vlabels = paste(vbreaks[1:length(vbreaks)-1])
    }
    
    dat = total2[total2$RDate== input$slider,]
    dat[[input$var]][dat[[input$var]]<0]=NA 
    
    if(input$slider==lastindex()+1 ){
      
      tmapProxy("map", session, {
        #tm_remove_layer((401)) +
        tm_shape( dat) + 
          tm_polygons(col=input$var,
                      breaks = vbreaks,
                      labels = vlabels,                      
                      zindex = 401,
                      id="location") 
      })
      animationmode(TRUE)
      
    }
    else if(animationmode()==TRUE &input$slider!=lastindex()+1 &input$slider!=lastindex() | animationmode()==TRUE & lastvar()!=input$var  ){
      #Reset the map after animation
      output$map = renderTmap({
        tm_shape( dat ) + 
          tm_polygons(col=input$var,
                      breaks = vbreaks,
                      labels = vlabels,                      
                      zindex = 401,
                      id="location") 
      })
      animationmode(FALSE)
      
    }
    else if(animationmode()!=TRUE){
      # use regular mode
      tmapProxy("map", session, {
        tm_remove_layer((401)) +
          tm_shape( dat) + 
          tm_polygons(col=input$var,
                      breaks = vbreaks,
                      labels = vlabels,                      
                      zindex = 401,
                      id="location") 
      })
    }
    
    lastindex(input$slider)
    lastvar(input$var)
  })
  
}	

shinyApp(ui, server)
