# troubleshoots
#from draw_heatmap_all_copy
pot_sub_Data <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    potsinclude==T
  ) %>% 
  select(AU:`Data categorisationsystem conditions`, category:technology, technology_detail:label)



cost_sub_Data <- all_data %>% 
  filter(
    !is.na(value) , 
    is.finite(value),
    costsinclude==T) %>% 
  select(AU:`Data categorisationsystem conditions`, category:technology, technology_detail:label)
