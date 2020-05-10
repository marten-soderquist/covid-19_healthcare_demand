library(shiny)
library(vroom)
library(tidyverse)

#regions = read.csv("data/region_data.csv", encoding = "UTF-8")
regions = vroom(
  "data/metadata/region_data.csv",
  col_types = list(selectedRegion = "c", MunicipalityCode = "c")
)
selectableRegions = unique(regions$selectedRegion)
ages = vroom::vroom("data/metadata/agegroups.csv",
                    col_types =  list(AgeGroup = "c", Age = "c"))

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


shinyApp(ui <- fluidPage(#titlePanel("Title"),
                         fluidRow(
                           column(2, selectInput("regionSelect", "",
                                                 character(0))),
                           column(10, selectInput(
                             "scenarioSelect",
                             "",
                             choices = c(
                               "Scenario 1" = 1,
                               "Scenario 2" = 2,
                               "Scenario 3" = 3,
                               "Scenario 4" = 4,
                               "Scenario 5" = 5
                             )
                           )
                           )
                         ),
                         
                         fluidRow(
                            
                           column(6, plotOutput("plotIVAId")
                           ),
                           column(6, plotOutput("plotCareloadId")
                           )
                         ),
                         fluidRow(
                           column(4, plotOutput("plotDeathsId")
                           ),
                           column(4, plotOutput("plotContagiusId")),
                           column(4, plotOutput("plotContagiousTotalId"))
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
           
           commonTheme = theme( text = element_text(size=textSize),
                                legend.position = 'bottom')
           
           # output$tableOutputId <- renderTable({plotData()})
           output$plotDeathsId <- renderPlot({
             ggplot(plotData(), aes(x = Date, y = deaths)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(fun = sum, geom =
                                                                                                          'line', size = lineWidth, aes(color="Total")) +
               labs(
                 title = paste("Cumulative deaths in",input$regionSelect),
                 x = element_blank(),
                 y = element_blank(),
                 color = "Agegroups"
               ) + commonTheme
           })

           output$plotIVAId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = intensiveCare)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(fun = sum, geom =
                                                                                                            'line', size = lineWidth, aes(color="Total")) +
                 labs(
                   title = paste("Patients in intensive care in", input$regionSelect),
                   x = element_blank(),
                   y = element_blank(),
                   color = "Agegroups"
                 ) + commonTheme
             })
           output$plotContagiusId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = contagiousInSociety)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(fun = sum, geom =
                                                                                                            'line', size = lineWidth, aes(color="Total")) +
                 labs(
                   title = paste("Contagious persons not isolated in",input$regionSelect),
                   x = element_blank(),
                   y = element_blank(),
                   color = "Agegroups"
                 ) + commonTheme
             })
           output$plotCareloadId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = careload)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(fun = sum, geom =
                                                                                                            'line', size = lineWidth, aes(color="Total")) +
                 labs(
                   title = paste("Hospitalized patients in ",input$regionSelect,", excluding intensive care", sep=""),
                   x = element_blank(),
                   y = element_blank(),
                   color = "Agegroups"
                 ) + commonTheme
             })
           output$plotContagiousTotalId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = contagiousTotal)) + geom_line(aes(color = Age), size = lineWidth) + stat_summary(fun = sum, geom =
                                                                                                            'line', size = lineWidth, aes(color="Total")) +
                 labs(
                   title = paste("Total number of contagious in", input$regionSelect),
                   x = element_blank(),
                   y = element_blank(),
                   color = "Agegroups"
                 ) + commonTheme
             })
           
           # output$t1 <- renderPrint({
           #   date()[1]})
         })

shinyApp(ui, server)
