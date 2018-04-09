library(igraph)

G1 <- read.graph('bib_data/1990-2005/pre_2006_3.graphml',format='graphml')

G2 <- read.graph('bib_data/1990-2017/all_years_3.graphml',format='graphml')

c1 <- centr_degree(G1)

c1_b <- centr_betw(G1)

c2 <- centr_degree(G2)

c2_b <- centr_betw(G2)
