library(purrr)
rawChoices <- dir(pattern="*.csv") %>% map(tools::file_path_sans_ext) %>% map(~ strsplit(.x, '_')) %>% flatten()
variableChoice <- list("ageGroup1" = rawChoices %>% map(1) %>% map_dbl(~ as.double(.x)) %>% unique() %>% sort() %>% as.list(),
                       "ageGroup2" = rawChoices %>% map(2) %>% map_dbl(~ as.double(.x)) %>% unique() %>% sort() %>% as.list(),
                       "ageGroup3" = rawChoices %>% map(3) %>% map_dbl(~ as.double(.x)) %>% unique() %>% sort() %>% as.list(),
                       "infectiousPeroid" = rawChoices %>% map(4) %>% map_dbl(~ as.double(.x)) %>% unique() %>% sort() %>% as.list()
                       )
                       