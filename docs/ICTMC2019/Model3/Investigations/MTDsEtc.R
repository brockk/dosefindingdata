

source('Load.R')


studies %>% nrow 
# 139 studies

studies %>% 
  filter(is.na(MTDorRP2D)) %>% nrow
# 33 missing MTDorRP2D

studies %>% 
  filter(!is.na(MTDorRP2D)) %>% nrow
# 106 have MTDorRP2D

studies %>% 
  filter(!is.na(MTDorRP2D)) %>%
  inner_join(dlt, by = c('Study' = 'Study', 
                         'MTDorRP2D' = 'Dose')) %>% 
  nrow
# 74 mapped on Study and MTDorRP2D. 32 cannot be mapped

studies %>% 
  filter(!is.na(MTDorRP2D)) %>%
  left_join(dlt, by = c('Study' = 'Study', 
                         'MTDorRP2D' = 'Dose')) %>% 
  filter(is.na(n)) %>%
  select(Study, MTDorRP2D, Note) %>% 
  print(n = 100)
# TODO - audit these

# TODO - examine MTD/RP2D in light of observed/modelled data

