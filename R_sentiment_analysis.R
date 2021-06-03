

# Housekeeping ------------------------------------------------------------

rm(list = ls())
library(tidytext) # for unnesting tokens
library(tidyverse) # tidyverse approach to data cleaning + pipes
library(ggplot2) # make beautiful plots
library(wordcloud) # word clouds --> dope
library(scales) # adjust scales in ggplot axes like percentage or dollars
library(stringr) # regex
library(GGally) # for the infamous ggcorr
library(textdata) 
library(topicmodels) # topic modelling
library(reshape2)
library(widyr) # untidying already tidy data for transformations & viz
library(igraph)
library(ggraph)
library(forcats)
library(lattice)
theme_set(theme_bw() + 
            theme(panel.grid.minor.x = element_blank(),
                  plot.title = element_text(size = 12, 
                                            face = "bold", 
                                            hjust = 0.5 )))
stop_words_de <- data.frame(word = stopwords::stopwords("de"), 
                            stringsAsFactors = F) # german stopwords dataset
stop_words_en <- stop_words[1] # english stopwords dataset
color <- c(brewer.pal( 3, "Set2" )[1], 
           brewer.pal( 3, "Set2" )[2], 
           brewer.pal( 3, "Set2" )[3], 
           brewer.pal( 3, "Set2" )[5])
# Load in the data --------------------------------------------------------

incubators_content <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/scraping-austrian-incubators/main/data/incubators_raw_content_languages_sentiment.csv", 
           fileEncoding = "utf-8")

# Token Analysis ----------------------------------------------------------



base_data <- incubators_content %>%
  filter(LanguageCode == 'en') %>% 
  filter(nchar(content) > 300)

base_data <- base_data %>% select(!c(LanguageCode, Score, img_link))

knitr::kable(table(base_data$creator), 
             caption = "Looking at how many articles we have per incubator")