rm(list=ls())
library(RPostgreSQL)
library(tm)
library(wordcloud)
source("/home/galm/pg_keys.R")
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(scimetrix)

library(topicmodels)
library(tm)
library(SnowballC)


drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)

q <- paste0('SELECT "scoping_doc"."UT", "scoping_doc"."PY", "scoping_doc"."content", "scoping_doc"."title"
  FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
  WHERE "scoping_doc_query"."query_id" = ',1498)



papers <- data.frame(dbGetQuery(con, q))  %>%
  filter(content != "")

corpus <- corporate(papers, "content")

ignoreWords <- c("the", "however", "this", "and")
corpus <- tm::Corpus(tm::VectorSource(papers$content)) %>%
  tm::tm_map(tm::removePunctuation) %>% tm::tm_map(tm::removeWords, 
                                                   tm::stopwords()) %>% tm::tm_map(tm::removeWords, ignoreWords) %>% 
  tm::tm_map(tm::stemDocument)

dtm <- makeDTM(corpus,0.99,papers$UT,0.02,0)


rem <- filter(papers,UT %in% dtm$removed)
papers_used <- subset(papers, !(UT %in% dtm$removed))
corpus_used <- refresh_corp(dtm$dtm)


#optimal_k(dtm$dtm, 30)


library("slam")
set.seed(123)

i <- c(
  1,1,
  2,2,
  3,
  4,4,
  5,5,
  6,6,
  7,7,
  8,8,8,
  9,9
  )
  
j <- c(
  "ocean","fertilisation",
  "air","captur",
  "biochar",
  "ethic","just",
  "afforest","reforest",
  "ocean","alkalinization",
  "soil","sequest",
  "bioenergy","capture","storage",
  "enhanced","weathering"
)

jj <- j[1]

j_stem <- c()

for (jj in j) {
  jj <- tm::stemDocument(jj)
  print(jj)
  
  jj <- which(jj==dtm$dtm$dimnames$Terms)
  print(jj)
  j_stem <- c(j_stem,jj)
}



SeedWeight <- 500 - 0.1
deltaS <- simple_triplet_matrix(i, j_stem, v = rep(SeedWeight, length(j)),
                                nrow = 9, ncol = dtm$dtm$ncol)
set.seed(1000)
ldaS <- LDA(dtm$dtm, k = 9, method = "Gibbs", seedwords = deltaS, 
            control = list(alpha = 0.1, best = TRUE,
                           verbose = 500, burnin = 500, iter = 100, thin = 100))

apply(deltaS, 1, function(x) which(x == SeedWeight))
apply(posterior(ldaS)$terms, 1, function(x) order(x, decreasing = TRUE)[1:5])

scimetrix::visualise(ldaS,corpus_used,dtm$dtm)




fitted <- ldaS
phi <- posterior(fitted)$terms %>% as.matrix
theta <- posterior(fitted)$topics %>% as.matrix
vocab <- colnames(phi)

doc_length <- vector()
for (i in 1:length(corpus_used)) {
  temp <- paste(corpus_used[[i]]$content, collapse = " ")
  doc_length <- c(doc_length, stri_count(temp, regex = "\\\\S+"))
}

temp_frequency <- as.matrix(dtm$dtm)

#temp_frequency <- inspect(dtm$dtm)


freq_matrix <- data.frame(ST = colnames(temp_frequency), 
                          Freq = colSums(temp_frequency))

json_lda <- LDAvis::createJSON(
  phi = phi, theta = theta, 
  vocab = vocab, doc.length = doc_length, 
  term.frequency = freq_matrix$Freq
  )

json_lda <- abcde(
  phi = phi, theta = theta, 
  vocab = vocab, doc.length = doc_length, 
  term.frequency = freq_matrix$Freq
)
