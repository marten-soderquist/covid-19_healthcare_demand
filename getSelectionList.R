library(shiny)
library(tidyverse)
library(vroom)

getSelectionList <- function() {
  regions = vroom::vroom(
    "data/metadata/region_data.csv",
    delim = ',',
    col_types = list(selectedRegion = "c", MunicipalityCode = "_", Section = "i")
  )
  selectionList <- list(`Sverige` = regions  %>% dplyr::filter(Section == 1) %>% dplyr::select(selectedRegion) %>% dplyr::distinct() %>% pull(),
                        `Regioner` = regions %>% dplyr::filter(Section == 2) %>% dplyr::select(selectedRegion) %>% dplyr::arrange(selectedRegion) %>% dplyr::distinct() %>% pull(),
                        `Kommuner` = regions %>% dplyr::filter(Section == 3) %>% dplyr::select(selectedRegion) %>% dplyr::arrange(selectedRegion) %>% dplyr::distinct() %>% pull()
                           )
  print(selectionList)
}