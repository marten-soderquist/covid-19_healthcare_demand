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


shinyApp(ui <- fluidPage(titlePanel("Title"),
                         
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("regionSelect", "Område",
                                         character(0)),
                             inputPanel(
                               textOutput(outputId = "contactPanelId"),
                               selectInput(
                                 "scenarioSelect",
                                 "Scenario",
                                 choices = c(
                                   "Scenario 1" = 1,
                                   "Scenario 2" = 2,
                                   "Scenario 3" = 3,
                                   "Scenario 4" = 4,
                                   "Scenario 5" = 5
                                 )
                               )
                             ),
                           ),
                           
                           mainPanel(
                             textOutput(outputId = "t1"),
                             # tableOutput("tableOutputId"),
                             plotOutput("plotDeathsId"),
                             plotOutput("plotIVAId"),
                             plotOutput("plotContagiusId"),
                             plotOutput("plotCareloadId"),
                             plotOutput("plotContagiousTotalId")
                           )
                         )),
         
         
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
           
           # output$tableOutputId <- renderTable({plotData()})
           output$plotDeathsId <- renderPlot({
             ggplot(plotData(), aes(x = Date, y = deaths)) + geom_line(aes(color = Age)) + stat_summary(fun = sum, geom =
                                                                                                          'line', aes(color="Total")) +
               labs(
                 title = paste("Cumulative deaths in",input$regionSelect),
                 x = "Date",
                 y = "Number",
                 color = "Agegroups"
               )
           })

           output$plotIVAId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = intensiveCare)) + geom_line(aes(color = Age)) + stat_summary(fun = sum, geom =
                                                                                                            'line', aes(color="Total")) +
                 labs(
                   title = paste("Persons under intensive care in", input$regionSelect),
                   x = "Date",
                   y = "Number",
                   color = "Agegroups"
                 )
             })
           output$plotContagiusId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = contagiousInSociety)) + geom_line(aes(color = Age)) + stat_summary(fun = sum, geom =
                                                                                                            'line', aes(color="Total")) +
                 labs(
                   title = paste("Contagious persons not isolated in",input$regionSelect),
                   x = "Date",
                   y = "Number",
                   color = "Agegroups"
                 )
             })
           output$plotCareloadId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = careload)) + geom_line(aes(color = Age)) + stat_summary(fun = sum, geom =
                                                                                                            'line', aes(color="Total")) +
                 labs(
                   title = paste("Hospitalized persons in",input$regionSelect,"excluding intensive care"),
                   x = "Date",
                   y = "Number",
                   color = "Agegroups"
                 )
             })
           output$plotContagiousTotalId <-
             renderPlot({
               ggplot(plotData(), aes(x = Date, y = contagiousTotal)) + geom_line(aes(color = Age)) + stat_summary(fun = sum, geom =
                                                                                                            'line', aes(color="Total")) +
                 labs(
                   title = paste("Total number of Contagious in", input$regionSelect),
                   x = "Date",
                   y = "Number",
                   color = "Agegroups"
                 )
             })
           
           # output$t1 <- renderPrint({
           #   date()[1]})
         })

shinyApp(ui, server)
