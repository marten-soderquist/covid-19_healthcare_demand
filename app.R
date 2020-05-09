library(shiny)
library(vroom)
library(tidyverse)

#regions = read.csv("data/region_data.csv", encoding = "UTF-8")
regions = vroom("data/region_data.csv")
selectableRegions = unique(regions$selectedRegion)

### Vroom can read several files and append these into a single table. So,
### create one file per municipality to reduce memory use or per parameter
### combination?
data = read.csv("data/test/model_output.csv")

shinyApp(
  ui <- fluidPage(
    
    titlePanel("Test"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("regionSelect","Område",
                    character(0)
        ),
        inputPanel(
          textOutput(outputId = "contactPanelId"),
          selectInput("contactAG1Id","0-59",choices = NULL),
          selectInput("contactAG2Id","60-79",choices = NULL),
          selectInput("contactAG3Id","80+",choices = NULL),
          selectInput("infectiousPeriodId","Isoleringsfaktor",choices = NULL )
        ),
        
        # checkboxGroupInput("variable", "Variables to show:",
        #                    c("Sjuka","IVA","Döda")
        # ),
      ),
      
      mainPanel(
        
        # Output: Histogram ----
        #
        textOutput(outputId = "t1"),
        tableOutput("tableOutputID"),
        plotOutput(outputId = "distPlot")
      )
    )
  ),
  
  
  server <- function(input, output,session){
    output$contactPanelId <- renderPrint("Kontaktgrad")
    updateSelectInput(session,inputId = "regionSelect", choices = selectableRegions, selected = NULL)
    
    
    # ageGroup1Choices <- reactive({
    #   ag1c <- vroom("./data/model_output.csv")
    #   ag1c %>% pull(ContactAG1) %>% distinct()
    #   updateSelectInput(session, ContactAG1Id, ag1c)
    # })
    
    
    
    
    filteredData <- reactive({
      region <- input$regionSelect
      ageGroup1Contact <- input$contactAG1Id
      ageGroup2Contact <- input$contactAG2Id
      ageGroup3Contact <- input$contactAG3Id
      infectiousPeriod <- input$infectiousPeriodId
      
      municipalityCode <- regions %>% filter(selectedRegion == region) %>% pull(MunicipalityCode)
      
      
    })
    
    output$tableOutputID <- renderTable({filteredData()})
    
    output$t1 <- renderPrint({
      date()[1]})
  }
)

shinyApp(ui,server)
