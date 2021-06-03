# Housekeeping ------------------------------------------------------------

rm(list = ls())
library(ggplot2) # make beautiful plots
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(wordcloud)
theme_set(theme_bw() + 
            theme(panel.grid.minor.x = element_blank(),
                  plot.title = element_text(size = 12, 
                                            face = "bold", 
                                            hjust = 0.5 )))
color <- c(brewer.pal( 12, "Paired" ),
           brewer.pal( 3, "Set2" ))
# Load in the data --------------------------------------------------------

incubators_images_content <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/scraping-austrian-incubators/main/data/incubators_images_10_entities.csv", 
           fileEncoding = "utf-8") %>% 
  select(-Creator) %>% 
  transmute(img_object = Name,
            img_object_confidence = Confidence,
            ID = ID)

incubators_content <- 
  read.csv("https://raw.githubusercontent.com/cosmin-ticu/scraping-austrian-incubators/main/data/incubators_raw_content_languages_sentiment.csv", 
           fileEncoding = "utf-8")

incubators_contentANDimages <- merge(incubators_content, 
                                     incubators_images_content,
                                     by = "ID")

# Image label analysis ----------------------------------------------------

# Less frequent entities and their mean percentage positivity-negativity
image_objects_negativity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  filter(count < 20) %>% 
  mutate(contribution = Negative*-1) %>% 
  top_n(15, abs(contribution))
image_objects_positivity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  filter(count < 20) %>% 
  mutate(contribution = Positive) %>% 
  top_n(15, contribution)
image_objects_positivity_negativity <- rbind(image_objects_negativity, 
                                                 image_objects_positivity)
image_objects_positivity_negativity %>%
  mutate(img_object = reorder(img_object, contribution)) %>%
  ggplot(aes(img_object, contribution)) +
  geom_col(show.legend = F, aes(fill = contribution > 0)) +
  coord_flip() +
  labs(y = "Mean Percent Positive-Negative",
       title = "Comparing Mean Percent Positivity-Negativity between less frequent image objects",
       x = NULL) +
  scale_fill_manual(values = c(color[6],color[4]))

# Less frequent entities and their positivity-negativity score (count-weighted contribution)
image_objects_negativity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  filter(count < 20) %>% 
  mutate(contribution = count*Negative*-1) %>% 
  top_n(15, abs(contribution))
image_objects_positivity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  filter(count < 20) %>% 
  mutate(contribution = count*Positive) %>% 
  top_n(15, contribution)
image_objects_positivity_negativity <- rbind(image_objects_negativity, 
                                             image_objects_positivity)
image_objects_positivity_negativity %>%
  mutate(img_object = reorder(img_object, contribution)) %>%
  ggplot(aes(img_object, contribution)) +
  geom_col(show.legend = F, aes(fill = contribution > 0)) +
  coord_flip() +
  labs(y = "Positivity-Negativity Score: Frequency of image object * Mean Percent Positive-Negative",
       title = "Comparing Positivity-Negativity score between less frequent image objects",
       x = NULL) +
  scale_fill_manual(values = c(color[6],color[4]))

