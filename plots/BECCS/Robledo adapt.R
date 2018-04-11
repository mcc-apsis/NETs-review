setwd("C:/Users/maria_000/Documents/MPP/MCC/Systematic Review/NETs-review/BioE-sideEffects")

library(tidyverse)
library(xlsx)

# SE <- read.xlsx("Bioenergy/Robledo_impacts.xlsx", 
#                 sheetName = "Table SI6 +, -, n impacts", 
#                 startRow = 2, header = T)
# 
# write.xlsx(SE, file ="../../Robledo_impacts2.xlsx")

SE <- read.xlsx("../../Bioenergy/Robledo_impacts2.xlsx", 
                sheetIndex = 1, 
                startRow = 1, header = T)

SE <- SE %>%  mutate(ReNegative = negative *-1, 
             fctr_impact = factor(1:length(Impact), labels = Impact),
             Studies = negative + positive) %>%
  filter(inc == "x")
  # filter(No..Studies.1 >= 30)
#alternatively filter by inc = having put an x on excel file of those we want to include

rects <- data.frame(xstart = c(0.5, 2.5,8.5, 11.5), 
                    xend = c(2.5, 8.5, 11.5, 12.5), 
                    Impact.Category = factor(1:4, 
                                             labels = c("Social and Health", "Environmental","Economic","Technological")))

 ggplot(data = SE, aes())+
  geom_bar(aes(y = positive, x = fctr_impact), stat = "identity", fill = "#879b4a", width = 0.6)+
  geom_bar(aes(y = ReNegative, x = fctr_impact), stat = "identity", fill = "#8e2316", width = 0.6)+
  geom_text(aes(label = Studies, y =-60, x = fctr_impact))+
  geom_rect(data=rects, aes(ymin=min(SE$ReNegative) -2, ymax=max(SE$positive) +2, xmin=xstart,
                            xmax=xend, fill=Impact.Category), alpha =0.3)+
  coord_flip()+
  labs(x = "Impact",
       y = "No. of Studies")+
  theme_bw()+
   guides(fill = guide_legend(reverse = T))+
   theme(axis.text = element_text(size = 12))  #+
  #theme(axis.text.y = element_text(color = SE$Impact.category))

pdf(file = "Side Effects adapted.pdf", width = 13, height = 6)


