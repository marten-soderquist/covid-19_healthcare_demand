library(shiny)
library(vroom)
library(tidyverse)

#regions = read.csv("data/region_data.csv", encoding = "UTF-8")
regions = vroom(
  "data/metadata/region_data.csv",
  delim = ',',
  col_types = list(selectedRegion = "c", MunicipalityCode = "c")
)
selectableRegions = unique(regions$selectedRegion)
ages = vroom::vroom(
  "data/metadata/agegroups.csv",
  delim = ',',
  col_types =  list(AgeGroup = "c", Age = "c")
)
municipalityMetadata <-
  vroom::vroom(
    "data/metadata/municipalities.csv",
    delim = ',',
    col_types = list(
      Index = "_",
      MunicipalityCode = "c",
      Name = "c",
      Lat = "_",
      Long = "_",
      GroupCode = "_",
      CountyCode = "_"
    )
  )

### Vroom can read several files and append these into a single table. So,
### create one file per municipality to reduce memory use or per parameter
### combination?

textSize = 18
lineWidth = 1

columnTypes = list(
  MunicipalityCode = "c",
  AgeGroup = "c",
  Date = "D",
  I = "d",
  IV = "d",
  J = "d",
  M = "d",
  RI = "d",
  V = "d",
  VR = "d",
  I_J = "d",
  V_VR = "d",
  I_J_RI = "d"
)
dataSets = list(
  vroom::vroom("data/R_1.csv", col_types = columnTypes),
  vroom::vroom("data/R_2.csv", col_types = columnTypes),
  vroom::vroom("data/R_3.csv", col_types = columnTypes),
  vroom::vroom("data/R_4.csv", col_types = columnTypes),
  vroom::vroom("data/R_5.csv", col_types = columnTypes)
)


shinyApp(ui <- fluidPage(title = "COVID-19 i Sverige",
  titlePanel("COVID-19, vårdbehov och dödlighet i Sverige"),
  p("Här presenteras data från modellen som beskrivs i ", 
    tags$b("COVID-19 healthcare demand and mortality in Sweden in response to non-pharmaceutical (NPIs) mitigation and suppression scenarios (Rocklöv, et al.), preprint"),
    tags$a(href="https://www.medrxiv.org/content/10.1101/2020.03.20.20039594v3", "[Länk].")),
  tags$hr(),
  fluidRow(
    column(
      2,
      selectInput("regionSelect", "", character(0)),
      p("Resultat kan visas för riket, regioner/län eller enskilda kommuner."),p(tags$em("Rutan är sökbar genom att markera och skriva.")),
      
      selectInput(
        "scenarioSelect",
        "",
        choices = c(
          "Scenario A" = 1,
          "Scenario B" = 2,
          "Scenario C" = 3,
          "Scenario D" = 4,
          "Scenario E" = 5
        )
      ),
      p("A) inga folkhälsointerventioner (kontrafaktiskt scenario)"),
      p("B) lägre fysisk distansering i åldrar 0-59 år, måttligt i åldrar 60+ år"),
      p("C) måttlig fysisk distansering i åldrar 0-59 år, måttligt hög i åldrar 60+ år"),
      p("D) måttlig fysisk distansering i åldrar 0-59 år, hög i åldrar 60+ år, samt förbättrad isolering av sjuka"),
      p("E) samma som D), men med ännu mer förbättrad isolering av sjuka."),
      p(span("En komplett beskrivning av scenario, modell och antaganden återfinns "),
        a(href="https://www.medrxiv.org/content/10.1101/2020.03.20.20039594v3", "här."))
    ),
    column(10,
           fluidRow(
             column(6, plotOutput("plotIVAId")),
             column(6, plotOutput("plotCareloadId"))
           ),
           fluidRow(
             column(6, plotOutput("plotDeathsId")),
             column(6, plotOutput("plotContagiousTotalId"))
           ))
  )
  ),
  server <- function(input, output, session) {
    updateSelectInput(session,
                      inputId = "regionSelect",
                      choices = selectableRegions,
                      selected = NULL)
    
    plotData <- reactive({
      regionSelection <-
        regions %>% dplyr::filter(selectedRegion == input$regionSelect)
      selectedDataset <-
        dplyr::inner_join(regionSelection, dataSets[[as.integer(input$scenarioSelect)]]) %>% dplyr::inner_join(ages)
      
      
      selectedDataset %>% dplyr::group_by(selectedRegion, Age, Date) %>%
        dplyr::summarize(
          deaths = sum(M),
          intensiveCare = sum(IV),
          contagiousInSociety = sum(I_J),
          careload = sum(V_VR),
          contagiousTotal = sum(I_J_RI)
        )
    })
    
    commonTheme = theme(text = element_text(size = textSize),
                        legend.position = 'bottom')
    
    # output$tableOutputId <- renderTable({plotData()})
    output$plotDeathsId <- renderPlot({
      ggplot(plotData(), aes(x = Date, y = deaths)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(
        fun = sum,
        geom =
          'line',
        size = lineWidth,
        aes(color = "Total")
      ) +
        labs(
          title = paste("Dödsfall,", input$regionSelect),
          x = element_blank(),
          y = element_blank(),
          color = "Åldersgrupp"
        )  + commonTheme
    })
    
    output$plotIVAId <-
      renderPlot({
        ggplot(plotData(), aes(x = Date, y = intensiveCare)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(
          fun = sum,
          geom =
            'line',
          size = lineWidth,
          aes(color = "Total")
        ) +
          labs(
            title = paste("Intensivvårdspatiener,", input$regionSelect),
            x = element_blank(),
            y = element_blank(),
            color = "Åldersgrupp"
          ) + commonTheme
      })
    output$plotCareloadId <-
      renderPlot({
        ggplot(plotData(), aes(x = Date, y = careload)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(
          fun = sum,
          geom =
            'line',
          size = lineWidth,
          aes(color = "Total")
        ) +
          labs(
            title = paste("Inlagda patienter (ej IVA),", input$regionSelect),
            x = element_blank(),
            y = element_blank(),
            color = "Åldersgrupp"
          ) + commonTheme
      })
    output$plotContagiousTotalId <-
      renderPlot({
        ggplot(plotData(), aes(x = Date, y = contagiousTotal)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(
          fun = sum,
          geom =
            'line',
          size = lineWidth,
          aes(color = "Total")
        ) +
          labs(
            title = paste("Infekterade,", input$regionSelect),
            x = element_blank(),
            y = element_blank(),
            color = "Åldersgrupp"
          ) + commonTheme
      })
    
    # output$t1 <- renderPrint({
    #   date()[1]})
  }
)

  shinyApp(ui, server)
  