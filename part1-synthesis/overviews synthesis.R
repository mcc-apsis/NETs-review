library(tidyverse)
library(xlsx)
library(wesanderson)
library(RColorBrewer)
library(ggthemes)
library(plotmath)

#####
#All data for ranges
#####
load("data/all_data.RData") #BOTTOM UP 
net_names <- data.frame(
  longname  = c(sort(unique(all_data$technology)), "Enhanced weathering (terrestrial and ocean) and Ocean alkalinisation"),
  shortname = c("AR", "BECCS", "BC", "Bioenergy", "DAC", "EW", "OA", "OF", "SCS", "Storage", "EW&OA")
)


#############
## Costs
costs <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="cost",
    costsinclude==T
  ) %>%
  group_by(technology) %>% 
  summarise(
    cost_min = min(value, na.rm = T),
    cost_max = max(value, na.rm = T)
  )

## Pots

pots <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="totalPotential",
    potsinclude==T
  ) %>%
  group_by(technology) %>% 
  summarise(
    pot_min = min(value, na.rm = T),
    pot_max = max(value, na.rm = T)
  )



tech_ranges <- left_join(costs, pots)

tech_names <- c(sort(unique(tech_ranges$technology)))
net_names <- filter(net_names, longname %in% tech_names)
tech_ranges <- tech_ranges %>%mutate(
  net = paste(net_names$shortname[which(net_names$longname == tech_ranges$technology)]),
  source = "This study",
  year = "2050"
)

#####
#Expert Judgments from draw_heatmap_all
#####
data_ej <- list(
  data.frame(
    net      = "AR",
    pot_min  = 0.5,
    pot_max  = 3.6,
    cost_min = 5,
    cost_max = 50
  ),
  data.frame(
    net      = "BECCS",
    pot_min  = 0.5,
    pot_max  = 5,
    cost_min = 100,
    cost_max = 200
  ),
  data.frame(
    net      = "BC",
    pot_min  = 0.3,
    pot_max  = 2.0,
    cost_min = 0,
    cost_max = 120
  ),
  data.frame(
    net      = "DAC",
    pot_min  = 0.5,
    pot_max  = 5,
    cost_min = 150,
    cost_max = 300
  ),
  data.frame(
    net      = "EW",
    pot_min  = 2,
    pot_max  = 4,
    cost_min = 50,
    cost_max = 200
  ),
  data.frame(
    net      = "OA",
    pot_min  = 10,
    pot_max  = 25,
    cost_min = 90,
    cost_max = 110
  ),
  data.frame(
    net      = "OF",
    pot_min  = NA,
    pot_max  = NA,
    cost_min = NA,
    cost_max = NA
  ),
  data.frame(
    net      = "SCS",
    pot_min  = 3,
    pot_max  = 6,
    cost_min = 0,
    cost_max = 100
  )
) %>% 
  do.call("rbind", .)

data_ej <- data_ej %>% mutate(
  source = "This study",
  year = "EJ"
)

#####
#Overview Papers 
##### 
ovp <- read.xlsx("../synthesis_figure_part1.xlsx",1, header = TRUE) %>% 
  mutate(year = as.factor(year))

text_ovp<- read.xlsx("../synthesis_figure_part1.xlsx",2, header = TRUE) %>% 
  mutate(year = as.factor(year)) %>% gather("key", "value", pot_min:cost_max) %>% 
  separate(key, into = c("variable", "measure"), sep = "_") %>% 
  spread(measure, value, fill = NA) %>% filter(!is.na(min))

##### 
#Join and process
#####

x_cost <- 500
x_pot <- 35 

df <- full_join(ovp, tech_ranges, 
                by = c("net" , "year", "pot_max", "pot_min","cost_min", "cost_max", "source")) %>% 
  # filter(year !=2100) %>%
  full_join(data_ej,by = c("net" , "year", "pot_max", "pot_min","cost_min", "cost_max", "source")) %>%
  mutate(
    year = factor(year, levels(factor(year))[c(3,1,2)]),
    bar_type = ifelse(source != "This study", "Review", ifelse(year == "2050", "Literature Range", "Expert Judgment")),
    source = factor(source)
    ) 
  
df <- df %>% mutate(
  source = factor(source, levels(factor(source))[c(1:9,11, 10)])
)


#####
# orderedVec <- df %>% group_by(net) %>% 
#   arrange(pot_max) %>% 
#   mutate(
#     source_ordered = 1:length(source)
#   ) %>% 
#   ungroup()

#factor(source, levels = 1:length(source), labels = unique(.$source))
# %>% 
#   group_by(net) %>% 
#   arrange(desc(pot_max)) %>% 
#   mutate(
#     source_ordered = as.factor(1:length(source))
#   ) %>% ungroup()

#####
df_long_full <- df %>% gather("key", "value", pot_min:cost_max) %>% 
  separate(key, into = c("variable", "measure"), sep = "_") %>% 
  spread(measure, value, fill = NA) %>% 
  mutate(
    trimmed_labels = ifelse((variable == "pot" & max > x_pot), paste("//",as.integer(max)),
                            ifelse((variable == "cost" & max >= x_cost), paste("//", as.integer(max)), NA)),
    facet_labs = factor(variable, levels = c("cost", "pot"), labels = c("Estimated Potentials by 2050 [Gt CO2/yr]", "Estimated Costs [$/t CO2]"))
  )


# shortening extreme estimates
df_long <- df_long_full

df_long$max[(df_long$variable == "pot"  & df_long$max >= x_pot)] <- x_pot
df_long$max[(df_long$variable == "cost" & df_long$max >= x_cost)] <- x_cost



#2050 filter for potentials but NOT costs
df_long <- df_long %>% 
  filter((variable == "cost" | 
            (variable == "pot" & year != "2100")) & 
           (!is.na(min) & (!is.na(max)))) 
  

  
# geom_point adds minimum point estimates but not max
cols <- c("Review" = "#44a7d1", "Literature Range" = "#f2ae5c" , "Expert Judgment" = "#90e599")
facet_labels <- c("pot" = "Estimated Potentials by 2050 [Gt CO2/yr]", "cost" = "Estimated Costs [$/t CO2]")

levels(df_long$facet_labs) <- c("Estimated Costs `$/t` CO[2]", "Estimated Potentials by 2050 Gt CO[2]`/`yr")
# alternatively: list(cost = "Estimated Costs `$/t` CO[2]", pot = "Estimated Potentials by 2050 Gt CO[2]`/`yr")

#Darker blue "#2b6a84"
ggplot(df_long, aes(x = source))+
  geom_linerange(aes(ymin = min, ymax = max, color = bar_type, size = 0.25, alpha = 0.7),
                 position = position_dodge(width = 0.0))+
  geom_point(data = filter(df_long,is.na(max) & !is.na(min)),aes(y = min, x = source, color = bar_type))+
  coord_flip()+
  facet_grid(net~variable, scales = "free", space = "free_y", labeller = labeller(variable = facet_labels))+ #labeller=label_parsed
  theme_bw() +
  geom_text(data = df_long, aes(x = source, y = max, label = trimmed_labels, hjust = 1))+
  geom_text(data = text_ovp, aes(x = source, y = 1, label = min, hjust=0))+
  scale_color_manual(values = cols, name = "")+
  labs(x= "", y = "")+
  guides(size = "none", alpha = "none")+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "grey84"),
        panel.grid.minor.x = element_line(color = "grey95"),
        legend.background = element_rect(fill = "white"),
        legend.position = "bottom")


# ggplot(df_long, aes(x = source))+
#   geom_linerange(aes(ymin = min, ymax = max, color = bar_type, size = 0.25, alpha = 0.7),
#                  position = position_dodge(width = 0.0))+
#   geom_point(data = filter(df_long,is.na(max) & !is.na(min)),aes(y = min, x = source, color = bar_type))+
#   coord_flip()+
#   facet_grid(net~variable, scales = "free", space = "free_y")+# , labeller = labeller(variable = facet_labels))+ #labeller=label_parsed
#   theme_solarized() +
#   geom_text(data = df_long, aes(x = source, y = max, label = trimmed_labels, hjust = 1))+
#   geom_text(data = text_ovp, aes(x = source, y = 1, label = min, hjust=0))+
#   scale_color_manual(values = cols, name = "")+
#   labs(x= "", y = "")+
#   guides(size = "none", alpha = "none")+
#   theme(plot.background = element_rect(fill = "white"), 
#         legend.background = element_rect(fill = "white"),
#         legend.position = "bottom")


  
#ggsave("part1-synthesis/plots/review_ranges2050.png")

#####
# ggplot(filter(df, !is.na(pot_min)), aes(source))+
#   geom_linerange(aes(ymin = pot_min, ymax = pot_max, color = year, size = 1),
#                  position = position_dodge(width = 0.1))+
#   geom_point(data = filter(df,is.na(pot_max) & !is.na(pot_min)),aes(y = pot_min, x = source, color = year))+
#   coord_flip()+
#   facet_grid(net~., scales = "free")
# #remove filter or enter zeros for the ones where we want text "multiple Gt CO2"
# 
# ggplot(df_long, aes(source))+
#   geom_linerange(aes(ymin = min, ymax = max, color = year, size = 0.5),
#                  position = position_dodge(width = 0.1))+
#   geom_point(data = filter(df_long,is.na(max) & !is.na(min)),aes(y = min, x = source, color = year, alpha = 0.5))+
#   coord_flip()+
#   facet_grid(net~variable, scales = "free")
