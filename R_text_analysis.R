
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

# Load in the lexicons ----------------------------------------------------

# get all sentiment lexicons
nrc <- get_sentiments("nrc") # a bus-load of sentiments
bing <- get_sentiments("bing") # negative, neutral or positive
afinn <- get_sentiments("afinn") # from -5 to 5 rating negative to positive
loughran <- get_sentiments("loughran") # sentiment lexicon for business
german_sentiment_positive <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/analyzing-austrian-incubators-content/main/sentiment_lexicons/positive_words_de.txt",
           header = F) %>% 
  rename("word" = V1)
german_sentiment_positive$sentiment <- "positive"
german_sentiment_negative <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/analyzing-austrian-incubators-content/main/sentiment_lexicons/negative_words_de.txt",
           header = F) %>% 
  rename("word" = V1)
german_sentiment_negative$sentiment <- "negative"
german_sentiment <- rbind(german_sentiment_negative, german_sentiment_positive)

# Define stop-words lexicon(s)
stop_words_en <- stop_words_en %>% 
  add_row(word = "it’s") %>%  # specify custom stopwords
  add_row(word = "don’t") %>% 
  add_row(word = as.character(1:2100))
stop_words_de <- stop_words_de %>% 
  add_row(word = as.character(1:2100))

# Prepare datasets --------------------------------------------------------

# Create base df of both languages; keep only proper articles w/ at least ~ 3 sentences
base_data <- incubators_content %>% 
  filter(nchar(content) > 300)

# Base df for english
base_data_en <- base_data %>%
  filter(LanguageCode == 'en')

# Base df for german
base_data_de <- base_data %>%
  filter(LanguageCode == 'de')

# Take out columns with no use
base_data <- base_data %>% select(!c(Score, img_link))
base_data_en <- base_data_en %>% select(!c(LanguageCode, Score, img_link))
base_data_de <- base_data_de %>% select(!c(LanguageCode, Score, img_link))

# Unnest the tokens and remove stopwords --> EN&DE, EN, DE
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

# General Analysis ----------------------------------------------------------

knitr::kable(as.data.frame(table(base_data$creator)) %>% 
               rename("Incubator" = Var1,
                      "Count of Articles" = Freq),
             caption = "Looking at how many articles we have per incubator") %>% 
  kable_styling()

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
       title = "English Relative Word Frequencies Among Incubators with Many Articles Comparing to Impact Hub - Contd.")
ggsave(path = "artefacts/", 
       filename = "R_relative_token_frequency_EN_many.png")

## compare most frequent words between specialized and non-specialized incubator
# plot top 10 words for TheVentury
unnested_data %>% 
  filter(creator %in% c('TheVentury')) %>% 
  count(word, sort = T) %>% 
  head(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE, fill = color[9]) +
  labs(y = NULL, x = 'Frequency', title = 'Top 10 Most Frequent Words for The Ventury')
ggsave(path = "artefacts/", 
       filename = "R_most_frequent_words_Ventury.png")

# plot top 10 words for AgroInnovationLab
unnested_data %>% 
  filter(creator %in% c('AgroInnovationLab')) %>% 
  count(word, sort = T) %>% 
  top_n(10, n) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE, fill = color[3]) +
  labs(y = NULL, x = 'Frequency', title = 'Top 10 Most Frequent Words for Agro Innovation Lab')
ggsave(path = "artefacts/", 
       filename = "R_most_frequent_words_AgroInnovation.png")

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

ggcorr(test_cor[,c(4,1:3,5,7:11,6)], # change ordering so that they fit better
       midpoint = 0.5, 
       limits = c(0,1),
       label_size = 2) +
  ggtitle("Comparing English Word-Use Correlation Between Incubators")
ggsave(path = "artefacts/", 
       filename = "R_incubators_token_correlation_EN.png")

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
  ggtitle("Comparing German Word-Use Correlation Between Incubators")
ggsave(path = "artefacts/", 
       filename = "R_incubators_token_correlation_DE.png")

# TF-IDF ------------------------------------------------------------------

## tf_idf bar chart(s) for content words 2 parts
unnested_data_filtered_incubators <- unnested_data %>% 
  filter(!creator %in% c("TechHouse","InvestmentReadyProgram"))
# Part 1
unnested_data_filtered_incubators %>% 
  filter(creator %in% unique(unnested_data_filtered_incubators$creator)[1:6]) %>% 
  count(creator, word, sort = T) %>% 
  left_join(.,unnested_data_filtered_incubators %>% 
              count(creator, 
                    word) %>% 
              group_by(creator) %>% 
              summarise(total = sum(n))) %>% 
  bind_tf_idf(word, 
              creator, 
              n) %>%
  group_by(creator) %>%
  slice_max(tf_idf, 
            n = 7,
            with_ties = F) %>%
  ungroup() %>% 
  ggcharts::bar_chart(word, 
                      tf_idf, 
                      facet = creator, 
                      fill = creator) +
  scale_fill_manual(values = color[1:6]) +
  theme_bw() +
  labs(y = "Term Frequency by Inverse Document Frequency (TF-IDF)",
       x = NULL, 
       title = "Comparing Each Incubator's Most Characteristic Words") +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "R_tf-idf_incubator_content_part1.png")

# Part 2
unnested_data_filtered_incubators %>% 
  filter(creator %in% unique(unnested_data_filtered_incubators$creator)[7:12]) %>% 
  count(creator, word, sort = T) %>% 
  left_join(.,unnested_data_filtered_incubators %>% 
              count(creator, 
                    word) %>% 
              group_by(creator) %>% 
              summarise(total = sum(n))) %>% 
  bind_tf_idf(word, 
              creator, 
              n) %>%
  group_by(creator) %>%
  slice_max(tf_idf, 
            n = 7,
            with_ties = F) %>%
  ungroup() %>% 
  ggcharts::bar_chart(word, 
                      tf_idf, 
                      facet = creator, 
                      fill = creator) +
  scale_fill_manual(values = c(color[7:10],color[12:13])) +
  theme_bw() +
  labs(y = "Term Frequency by Inverse Document Frequency (TF-IDF)",
       x = NULL, 
       title = "Comparing Each Incubator's Most Characteristic Words - Contd.") +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "R_tf-idf_incubator_content_part2.png")

## Create treemap of all entities with more than 5 occurences
incubators_contentANDimages %>% 
  count(creator, img_object) %>% 
  filter(n > 5) %>% 
  treemap(., index = c("creator", "img_object"), 
          vSize = "n", vColor = "creator") %>% 
  extract2("tm") %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>%
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(img_object), 1.2, .5)) %>% 
  ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  geom_rect(aes(fill = color, size = primary_group),
            show.legend = FALSE,
            color = "black", 
            alpha = .3) +
  scale_fill_identity() +
  # set thicker lines for group borders
  scale_size(range = range(tm_plot_data$primary_group)) +
  # add labels
  ggfittext::geom_fit_text(aes(label = img_object), min.size = 1) +
  # options
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  ggtitle("Count Treemap of Image Entities by (Unlabeled) Incubators") + 
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "AWS_image_entities_treemap.png")

## Treemap for tf_idf
incubators_contentANDimages %>% 
  count(creator, img_object) %>% 
  left_join(.,incubators_contentANDimages %>% 
              count(creator, img_object) %>% 
              group_by(creator) %>% 
              summarise(total = sum(n))) %>% 
  bind_tf_idf(img_object, creator, n) %>%
  group_by(creator) %>%
  slice_max(tf_idf, n = 10,
            with_ties = F) %>%
  ungroup() %>% 
  treemap(., index = c("creator", "img_object"), vSize = "tf_idf", vColor = "creator") %>% 
  extract2("tm") %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>%
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(img_object), 1.2, .5)) %>% 
  ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  geom_rect(aes(fill = color, size = primary_group),
            show.legend = FALSE,
            color = "black", 
            alpha = .3) +
  scale_fill_identity() +
  # set thicker lines for group borders
  scale_size(range = range(c(0.5,1.2))) +
  # add labels
  ggfittext::geom_fit_text(aes(label = img_object), min.size = 1) +
  # options
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  ggtitle("TF-IDF Treemap of Uncommon Image Entities by (Unlabeled) Incubators") + 
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "AWS_image_entities_tf_idf_treemap.png")

# Topic modeling ----------------------------------------------------------

## EN 2-topic modeling and plotting top words
articles_topics_en <- unnested_data_en %>% 
  count(article_URL, word, sort = T) %>% 
  ungroup() %>%
  cast_dtm(article_URL, word, n) %>% 
  LDA(k = 2, control = list(seed = 1234)) %>% 
  tidy(matrix = "beta")

# get top terms for each topic and visualize
articles_topics_en %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = ifelse(topic == 1, "Topic 1 EN", "Topic 2 EN")) %>%
  ggcharts::bar_chart(x = term, 
                      y = beta,
                      fill = factor(topic),
                      facet = factor(topic)) +
  labs(x = NULL, 
       y = "Probability of appearance",
       title = "Comparing English Top Word Appearance Probability Between the Two Computed Topics") +
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(color[7],color[7]))
ggsave(path = "artefacts/", 
       filename = "R_top_words_topic_modeling_EN.png")

## EN 2-topic modeling - plotting significant word disparities - interesting findings
# calculate differences in beta between topics
articles_topics_en %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  top_n(20, abs(log_ratio)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = F, fill = color[7]) +
  coord_flip() +
  labs(y = "Log2 Ration of Beta between identified Topics 1 & 2",
       x = NULL,
       title = "Most Significant English Word Disparities Between Identified Topics")
ggsave(path = "artefacts/", 
       filename = "R_significant_word_disparities_topic_modeling_EN.png")

## DE 2-topic modeling and plotting top words
articles_topics_de <- unnested_data_de %>% 
  count(article_URL, word, sort = T) %>% 
  ungroup() %>%
  cast_dtm(article_URL, word, n) %>% 
  LDA(k = 2, control = list(seed = 1234)) %>% 
  tidy(matrix = "beta")

# get top terms for each topic and visualize
articles_topics_de %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = ifelse(topic == 1, "Topic 1 DE", "Topic 2 DE")) %>%
  ggcharts::bar_chart(x = term, 
                      y = beta, 
                      fill = factor(topic),
                      facet = factor(topic)) +
  labs(x = NULL, 
       y = "Probability of appearance",
       title = "Comparing German Top Word Appearance Probability Between the Two Computed Topics") +
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(color[1],color[1]))
ggsave(path = "artefacts/", 
       filename = "R_top_words_topic_modeling_DE.png")

## DE 2-topic modeling - plotting significant word disparities - uninteresting findings
# calculate differences in beta between topics
articles_topics_de %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  top_n(20, abs(log_ratio)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = F, fill = color[1]) +
  coord_flip() +
  labs(y = "Log2 Ration of Beta between identified Topics 1 & 2",
       x = NULL,
       title = "Most Significant German Word Disparities Between Identified Topics")
ggsave(path = "artefacts/", 
       filename = "R_significant_word_disparities_topic_modeling_DE.png")


# Incubator classification ------------------------------------------------

## Classification attempt EN - only keeping incubators with more than 10 articles
#group articles by creator
creators_gamma_en <- unnested_data_en %>% 
  filter(!creator %in% c('InvestmentReadyProgram','TechHouse')) %>%
  unite(creator_article, creator, article_URL) %>% 
  select(-c(article_title, date)) %>% 
  count(creator_article, word, sort = T) %>% 
  ungroup() %>% 
  #cast document term matrix 
  cast_dtm(creator_article,word,n) %>% 
  #LDA
  LDA(k = 9, 
      control = list(seed = 1234)) %>% 
  tidy(matrix = "gamma") %>%
  #separate creator from URL
  separate(document, c("creator", "article_url"), sep = "_", convert = TRUE)

# plot topic probability across creators
creators_gamma_en %>%
  mutate(creator = reorder(creator, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ creator) +
  labs(x = "Topic", y = expression(gamma),
       title = "English Topic Probability (Boxplot Distribution of Articles) Across Incubators")
ggsave(path = "artefacts/", 
       filename = "R_topic_modeling_incubator_classification_EN.png")

## Classification attempt DE - only keeping incubators with more than 10 articles
#group articles by creator
creators_gamma_de <- unnested_data_de %>% 
  filter(!creator %in% c('TechHouse','AustrianStartups')) %>%
  unite(creator_article, creator, article_URL) %>% 
  select(-c(article_title, date)) %>% 
  count(creator_article, word, sort = T) %>% 
  ungroup() %>% 
  #cast document term matrix 
  cast_dtm(creator_article,word,n) %>% 
  #LDA
  LDA(k = 6, 
      control = list(seed = 1234)) %>% 
  tidy(matrix = "gamma") %>%
  #separate creator from URL
  separate(document, c("creator", "article_url"), sep = "_", convert = TRUE)

# plot topic probability across creators
creators_gamma_de %>%
  mutate(creator = reorder(creator, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ creator) +
  labs(x = "Topic", y = expression(gamma),
       title = "German Topic Probability (Boxplot Distribution of Articles) Across Incubators")
ggsave(path = "artefacts/", 
       filename = "R_topic_modeling_incubator_classification_DE.png")

# Word co-occurrences -----------------------------------------------------

## Titles
# English
set.seed(1234)
base_data_en %>% 
  unnest_tokens(word, article_title) %>% 
  anti_join(stop_words_en) %>% 
  pairwise_count(word, article_URL, sort = TRUE, upper = FALSE) %>%
  filter(n >= 3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme(legend.position = "none") +
  ggtitle("Word Co-Occurrence Map for Incubators' English Article Titles")
ggsave(path = "artefacts/", 
       filename = "R_incubators_title_tokens_co-occurence_EN.png")

# German
set.seed(1234)
base_data_de %>% 
  unnest_tokens(word, article_title) %>% 
  anti_join(stop_words_de) %>% 
  pairwise_count(word, article_URL, sort = TRUE, upper = FALSE) %>%
  filter(n >= 3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme(legend.position = "none") +
  ggtitle("Word Co-Occurrence Map for Incubators' German Article Titles")
ggsave(path = "artefacts/", 
       filename = "R_incubators_title_tokens_co-occurence_DE.png")

## Content body
# English
set.seed(1234)
unnested_data_en %>% 
  pairwise_count(word, article_URL, sort = TRUE, upper = FALSE) %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme(legend.position = "none") +
  ggtitle("Word Co-Occurrence Map for Incubators' English Article Content")
ggsave(path = "artefacts/", 
       filename = "R_incubators_content_tokens_co-occurence_EN.png")

# German
set.seed(1234)
unnested_data_de %>%  
  pairwise_count(word, article_URL, sort = TRUE, upper = FALSE) %>%
  filter(n >= 25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme(legend.position = "none") +
  ggtitle("Word Co-Occurrence Map for Incubators' German Article Content")
ggsave(path = "artefacts/", 
       filename = "R_incubators_content_tokens_co-occurence_DE.png")

# N-gram analysis ---------------------------------------------------------

# make bigrams
bigram_counts <- base_data %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2) %>%
  count(creator, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

stop_words_en_de <- rbind(stop_words_de,
                          stop_words_en)
stop_words_en_de <- stop_words_en_de[!duplicated(stop_words_en_de$word),]

# choice of filtering
# looking at top two-word combinations where neither can be a stopword
bigram_counts_nostopwords <- bigram_counts %>% 
  subset(!word1 %in% stop_words_en_de & !word2 %in% stop_words_en_de)

bigram_counts_nostopwords %>%
  filter(n > 19) %>%
  select(-creator) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('"Pirate" Treasure Map of All Frequently-Used Content Word-Pairs')
ggsave(path = "artefacts/", 
       filename = "R_bi-grams_treasure_map.png")

# Sentiment analysis ------------------------------------------------------

knitr::kable(
  unnested_data_en %>%
    inner_join(nrc) %>% 
    filter(sentiment == "anticipation") %>% 
    count(word, sort = T) %>% 
    head(15), 
  caption = 'Top 15 English Words by Frequency for the "Anticipation" Sentiment') %>% 
  kable_styling()

knitr::kable(
  unnested_data_en %>%
    inner_join(nrc) %>% 
    count(sentiment, sort = T), 
  caption = "NRC sentiments ranked by word count") %>% 
  kable_styling()

## Top positive/negative words - bing
unnested_data_en %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to Sentiment", 
       x = NULL,
       title = "Top Positive & Negative Words by Occurrence - Bing Lexicon")+ 
  coord_flip() +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "R_sentiment_top_words_bing.png")

## Top positive/negative words - nrc
unnested_data_en %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, 
             n, 
             fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, 
             scales = "free_y",
             nrow = 2,
             ncol = 5) + 
  labs(y = "Contribution to Sentiment", 
       x = NULL,
       title = "Each Sentiment's Top Words by Occurrence - NRC Lexicon") + 
  coord_flip() +
  scale_fill_manual(values = color)
ggsave(path = "artefacts/", 
       filename = "R_sentiment_top_words_nrc.png")

## Top positive/negative words - afinn
unnested_data_en %>% 
  inner_join(afinn) %>%
  filter(!value == 0) %>% 
  mutate(sentiment = ifelse(value < 0, "negative","positive")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, 
             n, 
             fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, 
             scales = "free_y",
             nrow = 2,
             ncol = 5) + 
  labs(y = "Contribution to Sentiment", 
       x = NULL,
       title = "Top Positive & Negative Words by Occurrence - Afinn Lexicon") + 
  coord_flip() +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "R_sentiment_top_words_afinn.png")

## Top positive/negative words - german open-source lexicon
unnested_data_de %>%
  inner_join(german_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to Sentiment", 
       x = NULL,
       title = "Top Positive & Negative Words by Occurrence - German Lexicon")+ 
  coord_flip() +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "R_sentiment_top_words_german-sentiment.png")

## Comparing all the sentiment datasets to pick most suitable for analysis
# Getting all sentiment datasets on the same scale
bind_rows(unnested_data_en %>% 
              inner_join(afinn) %>%
              mutate(sentiment = ifelse(value < 0, "negative", 
                                        ifelse(value == 0, "neutral", 
                                               "positive")))%>%
              count(index = ID, sentiment)%>%
              spread(sentiment, n, fill = 0) %>%
              mutate(sentiment = positive - negative) %>% 
              mutate(method = "AFINN"),
          unnested_data_en %>%
              inner_join(bing) %>%
              count(index = ID, sentiment) %>%
              spread(sentiment, n, fill = 0) %>%
              mutate(sentiment = positive - negative) %>% 
              mutate(method = "Bing et al."),
          unnested_data_en %>% 
              inner_join(nrc %>% filter(sentiment %in% c("positive",
                                                         "negative"))) %>%
              count(index = ID, sentiment) %>%
              spread(sentiment, n, fill = 0) %>%
              mutate(sentiment = positive - negative) %>% 
              mutate(method = "NRC")) %>%
  ggplot(aes(as.character(index), sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs( x = NULL, 
        y = "Sentiment Score", 
        title = "Positivity/Negativity Scores Between the Three English Lexicons on Our Entire Sample") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(path = "artefacts/", 
       filename = "R_sentiment_positive-negative_lexicons_EN.png")

## Comparison to AWS sentiment - need to add neutrality score for apples-to-apples comparison
bind_rows(unnested_data_en %>% 
              left_join(afinn) %>%
              mutate(sentiment = ifelse(is.na(value), "neutral", 
                                        ifelse(value < 0, "negative", 
                                               "positive")))%>%
              count(index = ID, sentiment)%>%
              spread(sentiment, n, fill = 0) %>%
              mutate(sentiment = positive - negative) %>% 
              mutate(method = "AFINN"),
          unnested_data_en %>%
              left_join(bing) %>%
              mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>% 
              count(index = ID, sentiment) %>%
              spread(sentiment, n, fill = 0) %>%
              mutate(sentiment = positive - negative) %>% 
              mutate(method = "Bing et al."),
          unnested_data_en %>% 
              left_join(nrc %>% filter(sentiment %in% c("positive",
                                                         "negative"))) %>%
              mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>% 
              count(index = ID, sentiment) %>%
              spread(sentiment, n, fill = 0) %>%
              mutate(sentiment = positive - negative) %>% 
              mutate(method = "NRC")) %>% 
  mutate(pct_diff_pos_neg = sentiment / (positive + negative + neutral)) %>% 
  select(-c(negative, positive, neutral, sentiment)) %>% 
  bind_rows(base_data_en %>%
               select(c(ID, positive, negative)) %>% 
               mutate(pct_diff_pos_neg = positive - negative,
                      method = "AWS",
                      index = ID) %>% 
               select(c(index, pct_diff_pos_neg, method))) %>%
  ggplot(aes(as.character(index), pct_diff_pos_neg, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, 
             ncol = 1, 
             scales = "free_y") +
  labs( x = NULL, 
        y = "Sentiment Score", 
        title = "Positivity-Negativity Percentage Difference Between the English Lexicons & AWS on Our Entire Sample") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(labels = scales::percent)
ggsave(path = "artefacts/", 
       filename = "R_sentiment_positive-negative_percentage_lexicons_AWS_EN.png")

## German-side Comparison to AWS sentiment - need to add neutrality score for apples-to-apples comparison
unnested_data_de %>%
  left_join(german_sentiment) %>%
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>% 
  count(index = ID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(method = "GermanSentiment") %>% 
  mutate(pct_diff_pos_neg = sentiment / (positive + negative + neutral)) %>% 
  select(-c(negative, positive, neutral, sentiment)) %>% 
  bind_rows(base_data_de %>%
              select(c(ID, positive, negative)) %>% 
              mutate(pct_diff_pos_neg = positive - negative,
                     method = "AWS",
                     index = ID) %>% 
              select(c(index, pct_diff_pos_neg, method))) %>%
  ggplot(aes(as.character(index), pct_diff_pos_neg, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, 
             ncol = 1, 
             scales = "free_y") +
  labs( x = NULL, 
        y = "Sentiment Score", 
        title = "Positivity-Negativity Percentage Difference Between the German Lexicon & AWS on Our Entire Sample") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(labels = scales::percent)
ggsave(path = "artefacts/", 
       filename = "R_sentiment_positive-negative_percentage_lexicon_AWS_DE.png")

## Word clouds
# create wordcloud for positive and negative words - EN
unnested_data_en %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# create wordcloud for positive and negative words - DE
unnested_data_de %>%
  inner_join(german_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# Observing most frequent words and their AFINN negativity/positivity score
unnested_data_en %>%
  count(word, article_URL, sort = TRUE) %>%
  inner_join(afinn, by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * value)) %>%
  top_n(15, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = F) +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score",
       title = "Comparing AFINN Positivity Score Between Most Frequent English Words") +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "R_positivity_word_comparison_EN.png")

# Observing most frequent words and their German Lexicon negativity/positivity score
unnested_data_de %>%
  count(word, article_URL, sort = TRUE) %>%
  inner_join(german_sentiment, by = "word") %>%
  mutate(value = ifelse(sentiment == "negative", -1, 1)) %>% 
  group_by(word) %>%
  summarize(contribution = sum(n * value)) %>%
  top_n(15, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = F) +
  coord_flip() +
  labs(y = "Frequency of word * German Lexicon score",
       title = "Comparing German Lexicon Positivity Score Between Most Frequent German Words") +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "R_positivity_word_comparison_DE.png")

# Appendix/Scrap ----------------------------------------------------------

## tf_idf bar chart(s) for content words per language
# English
unnested_data_en %>% 
  filter(!creator %in% c("TechHouse","InvestmentReadyProgram")) %>% 
  count(creator, word, sort = T) %>% 
  left_join(.,unnested_data_en %>% 
              count(creator, 
                    word) %>% 
              group_by(creator) %>% 
              summarise(total = sum(n))) %>% 
  bind_tf_idf(word, 
              creator, 
              n) %>%
  group_by(creator) %>%
  slice_max(tf_idf, 
            n = 5,
            with_ties = F) %>%
  ungroup() %>% 
  ggcharts::bar_chart(word, 
                      tf_idf, 
                      facet = creator, 
                      fill = creator) +
  scale_fill_manual(values = color) +
  theme_bw() +
  labs(y = "Term Frequency by Inverse Document Frequency (TF-IDF)",
       x = NULL, 
       title = "Comparing the Most Characteristic English Words Between the Incubators") +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "R_tf-idf_incubator_content_EN.png")

# German
unnested_data_de %>% 
  # filter(!creator %in% c("TechHouse")) %>% 
  count(creator, word, sort = T) %>% 
  left_join(.,unnested_data_de %>% 
              count(creator, 
                    word) %>% 
              group_by(creator) %>% 
              summarise(total = sum(n))) %>% 
  bind_tf_idf(word, 
              creator, 
              n) %>%
  group_by(creator) %>%
  slice_max(tf_idf, 
            n = 5,
            with_ties = F) %>%
  ungroup() %>% 
  ggcharts::bar_chart(word, 
                      tf_idf, 
                      facet = creator, 
                      fill = creator) +
  scale_fill_manual(values = color) +
  theme_bw() +
  labs(y = "Term Frequency by Inverse Document Frequency (TF-IDF)",
       x = NULL, 
       title = "Comparing the Most Characteristic German Words Between the Incubators") +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "R_tf-idf_incubator_content_DE.png")

## EN & DE Topic Modeling
articles_topics <- unnested_data %>% 
  count(article_URL, word, sort = T) %>% 
  ungroup() %>%
  cast_dtm(article_URL, word, n) %>% 
  LDA(k = 2, control = list(seed = 1234)) %>% 
  tidy(matrix = "beta")

#get top terms for each topic and visualize
articles_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = ifelse(topic == 1, "Topic 1", "Topic 2")) %>%
  ggcharts::bar_chart(x = term, 
                      y = beta, 
                      fill = factor(topic),
                      facet = factor(topic)) +
  labs(x = NULL, 
       y = "Probability of appearance",
       title = "Comparing Top Word Appearance Probability Between the Two Computed Topics") +
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent)
ggsave(path = "artefacts/", 
       filename = "R_top_words_topic_modeling_DEandEN.png")