

# This script in the root of the project loads the datasets
source('Load.R')

# Load objective response model and samples
dlt_mod <- readRDS('docs/ICTMC2019/Model3/dlt_mod.rds')
dlt_fitted_draws <- readRDS('docs/ICTMC2019/Model3/dlt_fitted_draws.rds')
obj_resp_mod <- readRDS('docs/ICTMC2019/Model3/obj_resp_mod.rds')
obj_resp_fitted_draws <- readRDS('docs/ICTMC2019/Model3/obj_resp_fitted_draws.rds')




# This script loads objects specific to this modelling task.
# It relies on dlt_fitted_draws being loaded.
source('docs/ICTMC2019/Model3/ModellingObjects.R')

library(patchwork)


studies %>% filter(Study == 'Saji2008_1')
dlt %>% filter(Study == 'Saji2008_1')

studies %>% filter(Study == 'Kim2009_1')
dlt %>% filter(Study == 'Kim2009_1')

studies %>% filter(Study == 'Larocca2013_1')
dlt %>% filter(Study == 'Larocca2013_1')


study <- 'Yu2010_1'
# 'Boulin2014_1'  # Just chose the highest response rate
# 'Sharma2013_3'  # Weird choice
# 'Roberts2012_1'  # Just chose the highest response rate
# 'Roberts2012_2'  # Just chose the highest response rate
# TODO - check out Gandhi2014_1 what is RP2D?
# TODO - check out Jerusalem2011_1 what is RP2D?
# TODO - check out Srivastava2011_1 what is RP2D?
# TODO - check out Younes2010_1 what is RP2D?
# TODO - check out Saraiya2008_1 what is RP2D?
# TODO - check out Yu2010_1 what is RP2D?

## Plots side by side
{ 
  dlt_analysis <- analyse_dlt(
    dlt %>% filter(Study == study) %>% distinct(AnalysisSeriesId) %>% .[[1]], 
    alpha = 0.05, add_title = TRUE, legend1 = FALSE)
  efficacy_analysis <- analyse_obj_resp(
    obj_resp %>% filter(Study == study) %>% distinct(AnalysisSeriesId) %>% .[[1]], 
    alpha = 0.05, add_title = FALSE, legend1 = TRUE, 
    line_col = 'darkgreen', ribbon_fill = 'chartreuse')
  
  rp2d <- studies %>% filter(Study == study) %>% select(MTDorRP2D) %>% .[[1]]
  rp2d_dlt_rows <- dlt %>% filter(Study == study & Dose == rp2d)
  rp2d_obj_resp_rows <- obj_resp %>% filter(Study == study & Dose == rp2d)
  if(rp2d_dlt_rows %>% nrow == 1) {
    p1 <- dlt_analysis$p1 + 
      labs(subtitle = 'Toxicity') + 
      geom_point(data = rp2d_dlt_rows, aes(x = DoseLevel, y = ProbEvent), 
                 pch = 21, fill = NA, size = 15) + 
      theme(text = element_text(size=20))
  } else {
    p1 <- dlt_analysis$p1 + 
      labs(subtitle = 'Toxicity') + 
      theme(text = element_text(size=20))
  }
  
  if(rp2d_obj_resp_rows %>% nrow == 1) {
    p2 <- efficacy_analysis$p1 + 
      labs(subtitle = 'Response') + 
      geom_point(data = rp2d_obj_resp_rows, aes(x = DoseLevel, y = ProbEvent), 
                 pch = 21, fill = NA, size = 15) + 
      theme(text = element_text(size=20))
  } else {
    p2 <- efficacy_analysis$p1 + 
      labs(subtitle = 'Response') + 
      theme(text = element_text(size=20))
  }
  
  p1 + p2
}
