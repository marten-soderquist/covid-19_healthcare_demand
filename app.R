library(shiny)
shinyApp(
  
  selectableRegions = letters[1:10]
  
  ui <- fluidPage(
    
    titlePanel("Test"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("region","Region",
                  c("Sverige","Region stockholm","region Västerbotten")
                  ),
        selectInput("scenario","Scenario",
                    c("1","2","3","4","5")
                    ),
        checkboxGroupInput("variable", "Variables to show:",
                           c("Sjuka","IVA","Döda")
                  ),
        actionLink("selectall", "Select All")
        ),
      
      mainPanel(
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot")
      h3("Graf över resultat")
      
      )
    )
  ),
  
  
  server <- function(input, output,session){
    observe(
      {
        if(input$selectall == 0){
          
        }
        else{
          
        }
      }
    )
  }
)
  
shinyApp(ui,server)
