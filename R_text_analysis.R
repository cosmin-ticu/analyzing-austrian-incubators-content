

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
library(kableExtra) # check tables in viewer
theme_set(theme_bw() + 
            theme(panel.grid.minor.x = element_blank(),
                  plot.title = element_text(size = 12, 
                                            face = "bold", 
                                            hjust = 0.5 )))

stop_words_de <- read.csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt",
                            stringsAsFactors = F,
                            fileEncoding = "utf-8") %>% 
  rename("word" = a)
  
stop_words_en <- stop_words[1] # english stopwords dataset
color <- c(brewer.pal( 12, "Paired" ),
           brewer.pal( 3, "Set2" ))
# Load in the data --------------------------------------------------------

incubators_content <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/scraping-austrian-incubators/main/data/incubators_raw_content_languages_sentiment.csv", 
           fileEncoding = "utf-8")

# Create base df of both languages; keep only proper articles w/ at least ~ 3 sentences
base_data <- incubators_content %>% 
  filter(nchar(content) > 300)

# Base df for english
base_data_en <- base_data %>%
  filter(LanguageCode == 'en')

# Base df for german
base_data_de <- base_data %>%
  filter(LanguageCode == 'de')

base_data <- base_data %>% select(!c(Score, img_link))
base_data_en <- base_data_en %>% select(!c(LanguageCode, Score, img_link))
base_data_de <- base_data_de %>% select(!c(LanguageCode, Score, img_link))


# Token Analysis ----------------------------------------------------------

knitr::kable(as.data.frame(table(base_data$creator)) %>% 
               rename("Incubator" = Var1,
                      "Count of Articles" = Freq),
             caption = "Looking at how many articles we have per incubator") %>% 
  kable_styling()

stop_words_en <- stop_words_en %>% 
  add_row(word = "it’s") %>%  # specify custom stopwords
  add_row(word = "don’t") %>% 
  add_row(word = as.character(1:2100))

stop_words_de <- stop_words_de %>% 
  add_row(word = as.character(1:2100))

unnested_data <- base_data %>% 
  unnest_tokens(word, content) %>%
  anti_join(stop_words_en) %>% 
  anti_join(stop_words_de)

unnested_data_en <- base_data_en %>% 
  unnest_tokens(word, content) %>%
  anti_join(stop_words_en)

unnested_data_de <- base_data_de %>% 
  unnest_tokens(word, content) %>%
  anti_join(stop_words_de)

knitr::kable(unnested_data %>%
               count(word, sort = TRUE) %>% 
               head(20)%>% 
               rename("Word" = word,
                      "Frequency" = n), 
             caption = "Top words all around") %>% 
  kable_styling()

knitr::kable(unnested_data_de %>%
               count(word, sort = TRUE) %>% 
               head(20)%>% 
               rename("Word" = word,
                      "Frequency" = n), 
             caption = "Top german words all around") %>% 
  kable_styling()

# wordcloud with most common words
unnested_data %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# wordcloud with most common words
unnested_data_de %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Token frequency analysis ------------------------------------------------

## GERMAN Relative frequency of words among incubators' content
unnested_data_de %>% 
  filter(creator != "TechHouse") %>% 
  count(creator, word) %>%
  group_by(creator) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(creator, proportion) %>% 
  pivot_longer(names_to = "creator", values_to = "proportion", 
               cols = setdiff(unique(unnested_data_de$creator), 
                              c("A1Startup","TechHouse"))) %>% 
  ggplot(aes(x = proportion, y = A1Startup, 
                      color = abs(A1Startup - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~creator) +
  theme(legend.position="none") +
  labs(y = "A1Startup", x = NULL, 
       title = "German Relative Word Frequencies Among Incubators' Content Comparing to A1Startup")
ggsave(path = "artefacts/", 
       filename = "R_relative_token_frequency_DE.png")

## English relative frequency of words among incubators' content

# Relative word frequencies among incubators with few articles
incubators_interest <- c('INiTS', 'Tech2Impact',
                         'Factory1','I2C',
                         'ImpactHub')

unnested_data_temp <- unnested_data_en %>%
  filter(creator %in% incubators_interest)

unnested_data_temp %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(creator, word) %>%
  group_by(creator) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(creator, proportion) %>% 
  pivot_longer(names_to = "creator", values_to = "proportion", 
               cols = setdiff(unique(unnested_data_temp$creator), 
                              c("ImpactHub"))) %>% 
  ggplot(aes(x = proportion, y = ImpactHub, 
                      color = abs(ImpactHub - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~creator, ncol = 2, nrow = 2) +
  theme(legend.position="none") +
  labs(y = "Impact Hub", x = NULL, 
       title = "English Relative Word Frequencies Among Incubators with Few Articles Comparing to Impact Hub")
ggsave(path = "artefacts/", 
       filename = "R_relative_token_frequency_EN_few.png")

# Relative word frequencies among incubators with many articles
incubators_interest <- c('TheVentury', 'MatchMakerVentures',
                         'AustrianStartups','AgroInnovationLab',
                         'ImpactHub')

unnested_data_temp <- unnested_data_en %>%
  filter(creator %in% incubators_interest)

unnested_data_temp %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(creator, word) %>%
  group_by(creator) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(creator, proportion) %>% 
  pivot_longer(names_to = "creator", values_to = "proportion", 
               cols = setdiff(unique(unnested_data_temp$creator), 
                              c("ImpactHub"))) %>% 
  ggplot(aes(x = proportion, y = ImpactHub, 
                      color = abs(ImpactHub - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~creator, ncol = 2, nrow = 2) +
  theme(legend.position="none") +
  labs(y = "Impact Hub", x = NULL, 
       title = "English Relative Word Frequencies Among Incubators with Many Articles Comparing to Impact Hub")
ggsave(path = "artefacts/", 
       filename = "R_relative_token_frequency_EN_many.png")

## compare most frequent words
# plot top 10 words for TheVentury
unnested_data %>% 
  filter(creator %in% c('TheVentury')) %>% 
  count(word, sort = T) %>% 
  head(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = 'Frequency', title = 'Top 10 Most Frequent Words for TheVentury')

# plot top 10 words for AgroInnovationLab
unnested_data %>% 
  filter(creator %in% c('AgroInnovationLab')) %>% 
  count(word, sort = T) %>% 
  top_n(10, n) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = 'Frequency', title = 'Top 10 Most Frequent Words for AgroInnovationLab')

# Word Correlations -------------------------------------------------------

## English
test_cor <- unnested_data_en %>% 
  count(creator, word) %>%
  group_by(creator) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(creator, proportion) %>% 
  select(-word)

# replace NA with 0 for correct correlation evaluation
test_cor[is.na(test_cor)] <- 0

ggcorr(test_cor, 
       midpoint = 0.5, 
       limits = c(0,1)) +
  ggtitle("Comparing English Word-Use Correlation between Incubators")

## German
test_cor <- unnested_data_de %>% 
  count(creator, word) %>%
  group_by(creator) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(creator, proportion) %>% 
  select(-word)

# replace NA with 0 for correct correlation evaluation
test_cor[is.na(test_cor)] <- 0

ggcorr(test_cor, 
       midpoint = 0.5, 
       limits = c(0,1)) +
  ggtitle("Comparing German Word-Use Correlation between Incubators")
