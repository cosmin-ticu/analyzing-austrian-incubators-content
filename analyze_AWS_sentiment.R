
# Housekeeping ------------------------------------------------------------

rm(list = ls())
library(ggplot2) # make beautiful plots
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(ggrepel)
library(kableExtra)
theme_set(theme_bw() + 
            theme(panel.grid.minor.x = element_blank(),
                  plot.title = element_text(size = 12, 
                                            face = "bold", 
                                            hjust = 0.5 )))
color <- c(brewer.pal( 12, "Paired" ),
           brewer.pal( 3, "Set2" ))
# Load in the data --------------------------------------------------------

incubators_content <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/scraping-austrian-incubators/main/data/incubators_raw_content_languages_sentiment.csv", 
           fileEncoding = "utf-8")

languages <- incubators_content %>% select(c(article_URL, LanguageCode, Score))
languages <- dcast(languages, article_URL ~ LanguageCode, value.var = "Score")
languages <- languages %>% 
  mutate(en = ifelse(is.na(en), 1 - de, en),
         de = ifelse(is.na(de), 1 - en, de))

incubators_content <- incubators_content %>% select(-Score)

incubators_content <- inner_join(incubators_content, languages)

# Initial EDA on character and article distribution -----------------------------

# Bar chart of article counts
incubators_content %>% 
  group_by(creator) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(count, reorder(creator, count))) +
  geom_col(fill = color[2]) +
  geom_label(aes(label = creator), hjust = -0.05) +
  labs(x = "Number of articles scraped",
       y = NULL,
       title = "Count of Articles Available for Each Incubator") +
  scale_x_continuous(limits = c(0, 320), breaks = seq(0, 350, 25)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(path = "artefacts/", 
       filename = "articles_by_incubator.png")

# Histogram of character counts
incubators_content %>% 
  transmute(chars = nchar(content),
            article_URL = article_URL) %>% 
  ggplot(aes(x = chars)) +
  geom_histogram(fill = color[4],
                 binwidth = 500,
                 color = "black") +
  labs(x = "Number of characters",
       y = NULL,
       title = "Distribution of Character Counts in Incubators' Content")
ggsave(path = "artefacts/", 
       filename = "character_counts_distribution.png")

# Distribution of words per incubator
mean_words_intercept <- mean(nchar(incubators_content$content)/5)
incubators_content %>% 
  select(c(creator, content)) %>% 
  mutate(characters = nchar(content),
         words = nchar(content)/5,
         sentences = nchar(content)/100) %>% 
  select(-content) %>% 
  ggplot(aes(creator, words)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL, 
       y = "Number of words",
       title = "Distribution of Content Words Counts by Incubator") +
  geom_hline(yintercept = mean_words_intercept,
             linetype = "dashed",
             size = 1,
             color = color[6])
ggsave(path = "artefacts/", 
       filename = "word_count_boxplots_by_incubator.png")

# Language EDA ------------------------------------------------------------

# Table showing article languages by incubator
kable(incubators_content %>% 
               group_by(LanguageCode,creator) %>% 
               summarise(n = n()) %>%
               pivot_wider(id_cols = creator,
                           names_from = LanguageCode,
                           values_from = n) %>% 
               transmute_all(~replace(., is.na(.), 0)) %>% 
               transmute(Incubator = creator,
                         Deutsch = de,
                         English = en), caption = "Articles for Each Language by Incubator") %>% 
  kable_styling(latex_options = "striped") %>% 
  save_kable(file = "artefacts/articles_language_counts_by_incubator.png")

# Heatmap of language use distribution per article
incubators_content %>% 
  select(creator, article_URL, en, de) %>% 
  mutate(en_de = en - de,
         English = en,
         Deutsch = de) %>% 
  gather(., key = Language, value = language_score, c(English,Deutsch)) %>% 
  ggplot(aes(reorder(article_URL, en_de), language_score, fill = Language)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, 
       y = NULL,
       title = "Language Heatmap between English & German Use") +
  theme_void()  + 
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 )) +
  scale_fill_manual(values = c(color[1],color[7]))
ggsave(path = "artefacts/", 
       filename = "language_heatmap.png")

# Sentiment EDA -----------------------------------------------------------

# Distribution by sentiment
incubators_content %>%
  mutate(Positivity = positive,
         Negativity = negative,
         Neutrality = neutral,
         Mixed = mixed) %>% 
  select(c("Positivity",
           "Negativity",
           "Neutrality",
           "Mixed")) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, 
             scales = "free") +
  geom_histogram(aes(fill= key), bins = 50) +
  labs(x = NULL, 
       y = NULL,
       title = "Histogram Distributions of Article Sentiments") +
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(color[9],color[6],"gray",color[4]))
ggsave(path = "artefacts/", 
       filename = "AWS_sentiment_histograms.png")

# stacked bar chart sentiment (negative-positive) for each incubator
incubators_content %>%
  mutate(Positive = positive,
         Negative = negative,
         Neutral = neutral,
         Mixed = mixed) %>%
  select(c(creator,Positive, Negative)) %>% 
  gather(., key = Sentiment, value = sentiment_score, 
         c(Positive, Negative)) %>% 
  group_by(., creator, Sentiment) %>% 
  summarise(sentiment_score = mean(sentiment_score)) %>% 
  ggplot(aes(reorder(creator,sentiment_score), 
             sentiment_score, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, 
       y = "Proportion of total sentiment score",
       title = "Proportion of Positive & Negative Sentiment by Incubator") +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "AWS_stacked_bar_positive-negative_byIncubator.png")

# stacked bar chart (all) sentiment for each incubator
incubators_content %>%
  mutate(Positive = positive,
         Negative = negative,
         Neutral = neutral,
         Mixed = mixed,
         PandN = positive + negative) %>%
  select(c(creator,Positive, Negative, Neutral, Mixed, PandN)) %>% 
  gather(., key = Sentiment, value = sentiment_score, 
         c(Positive, Negative, Neutral, Mixed)) %>% 
  group_by(., creator, Sentiment) %>% 
  summarise(sentiment_score = mean(sentiment_score),
            PandN = mean(PandN)) %>% 
  ggplot(aes(reorder(creator,PandN), sentiment_score, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, 
       y = "Proportion of total sentiment score",
       title = "Proportion of All Sentiments by Incubator") +
  scale_fill_manual(breaks = c("Positive","Negative","Mixed","Neutral"),
                    values = c(color[4],color[6],color[9],"gray"))
ggsave(path = "artefacts/", 
       filename = "AWS_stacked_bar_sentiment_byIncubator.png")

# Language distribution by sentiment
incubators_content %>% 
  mutate(Language = ifelse(LanguageCode == "en", "English", "Deutsch")) %>%
  group_by(Language) %>% 
  summarise(Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  gather(., key = Sentiment, value = Sentiment_Score, c(Positive, Negative, Mixed, Neutral))%>% 
  ggplot(aes(reorder(Sentiment,Sentiment_Score), Sentiment_Score, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(color[1],color[7])) +
  labs(x = NULL, 
       y = NULL,
       title = "Proportion of Total Sentiment by Language") +
  scale_y_continuous(labels = scales::percent)
ggsave(path = "artefacts/", 
       filename = "AWS_language_sentiment_bars.png")

# Language positivity-negativity scatterplot 
incubators_content %>% 
  mutate(Language = ifelse(LanguageCode == "en", "English", "Deutsch"),
         Positive = positive,
         Negative = negative) %>%
  ggplot(aes(Positive, Negative, color = Language)) +
  geom_point(size = 5) +
  scale_color_manual(values = c(color[1],color[7]))+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Scatterplot of Articles' Positivity-Negativity by Language")
ggsave(path = "artefacts/", 
       filename = "AWS_positive-negative_language_scatterplot.png")

# Sentiment scatterplot by incubator - Positive Negative
incubators_content %>% 
  group_by(creator) %>% 
  summarise(Positive = mean(positive),
            Negative = mean(negative),
            Neutral = mean(neutral)) %>% 
  ggplot(aes(Positive, Negative)) +
  geom_point(size = 4, aes(color = color[1:14])) +
  geom_label_repel(aes(label = creator)) +
  scale_y_continuous(limits = c(-0.005,0.06), labels = scales::percent) +
  scale_x_continuous(limits = c(0,0.4), labels = scales::percent) +
  theme(legend.position = "none") +
  labs(x = "Positivity",
       y = "Negativity",
       title = "Scatterplot: Incubators' Proportions of Positivity to Negativity")
ggsave(path = "artefacts/", 
       filename = "AWS_positive-negative_incubators_scatterplot.png")

# Sentiment scatterplot by incubator - Positive Neutral
incubators_content %>% 
  group_by(creator) %>% 
  summarise(Positive = mean(positive),
            Negative = mean(negative),
            Neutral = mean(neutral)) %>% 
  ggplot(aes(Positive, Neutral)) +
  geom_point(size = 4, aes(color = color[1:14])) +
  geom_label_repel(aes(label = creator)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.position = "none") +
  labs(x = "Positivity",
       y = "Neutrality",
       title = "Scatterplot: Incubators' Proportions of Neutrality to Positivity")
ggsave(path = "artefacts/", 
       filename = "AWS_positive-neutral_incubators_scatterplot.png")

# Taking a look at most sentimentally loaded articles
incubators_content %>% 
  filter(positive > 0.15 & negative > 0.15) %>% 
  ggplot(aes(positive, negative)) +
  geom_point(aes(color = creator),size = 5) +
  scale_color_manual(values = color) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  geom_label_repel(aes(label = creator)) +
  labs(x = "Positivity",
       y = "Negativity",
       title = "Scatterplot of Most Sentimentally-Loaded Articles' Creators") +
  theme(legend.position = "none")
ggsave(path = "artefacts/", 
       filename = "AWS_sentimentally_loaded_articles_byIncubators.png")

# Scrap -------------------------------------------------------------------

# Unsure what to do with this one
ggplot(incubators_content, aes(positive, negative, color = positive-negative)) +
  geom_point(size = 5) +
  scale_color_distiller()

# cool idea but worthless in practice
incubators_content %>% 
  mutate(diff_pos_neg = positive - negative) %>% 
  slice_max(order_by = abs(diff_pos_neg), n = 100)

# Comparing positivity scores between incubators
incubators_content %>%
  group_by(creator) %>% 
  summarise(positive = mean(positive),
            negative = mean(negative),
            mixed = mean(mixed),
            neutral = mean(neutral)) %>% 
  mutate(score = (positive) / (positive + negative + neutral + mixed)) %>%
  mutate(creator = reorder(creator, score)) %>%
  ggplot(aes(creator, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Incubator",
       y = "Positivity-negativity ratio among news articles",
       title = "Comparing positivity scores between incubators") +
  scale_y_continuous(labels = scales::percent)
  