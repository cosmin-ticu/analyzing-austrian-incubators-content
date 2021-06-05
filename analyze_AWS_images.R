# Housekeeping ------------------------------------------------------------

rm(list = ls())
library(ggplot2) # make beautiful plots
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(wordcloud)
library(tidytext)
library(ggcharts)
library(treemap)
library(magrittr)
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

## Most frequent entities and their positivity-negativity score (count-weighted contribution)
# Negativity DF
image_objects_negativity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  #filter(count < 20) %>% 
  mutate(contribution = count*Negative*-1) %>% 
  top_n(20, abs(contribution))

# Positivity DF
image_objects_positivity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  #filter(count < 20) %>% 
  mutate(contribution = count*Positive) %>% 
  top_n(20, contribution)

# Bind the 2 DFs together
image_objects_positivity_negativity <- rbind(image_objects_negativity, 
                                             image_objects_positivity)

# Plot top 15 words per sentiment
image_objects_positivity_negativity %>%
  mutate(img_object = reorder(img_object, contribution)) %>%
  ggplot(aes(img_object, contribution)) +
  geom_col(show.legend = F, aes(fill = contribution > 0)) +
  coord_flip() +
  labs(y = "Positivity-Negativity Score: Frequency of image object * Mean Percent Positive-Negative",
       title = "Comparing Positivity-Negativity score between most frequent image objects",
       x = NULL) +
  scale_fill_manual(values = c(color[6],color[4]))
ggsave(path = "artefacts/", 
       filename = "AWS_image_entities_postivity_score.png")

## Most sentimentally-loaded entities and their mean percentage positivity-negativity 
# Negativity DF
image_objects_negativity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  mutate(contribution = Negative*-1) %>% 
  top_n(15, abs(contribution))

# Positivity DF
image_objects_positivity <- incubators_contentANDimages %>%
  group_by(img_object) %>% 
  summarise(count = n(),
            Positive = mean(positive),
            Negative = mean(negative),
            Mixed = mean(mixed),
            Neutral = mean(neutral)) %>% 
  mutate(contribution = Positive) %>% 
  top_n(15, contribution)

# Bind the 2 DFs together
image_objects_positivity_negativity <- rbind(image_objects_negativity, 
                                                 image_objects_positivity)

# Plot top 15 words per sentiment
image_objects_positivity_negativity %>%
  mutate(img_object = reorder(img_object, contribution)) %>%
  ggplot(aes(img_object, contribution)) +
  geom_col(show.legend = F, aes(fill = contribution > 0)) +
  coord_flip() +
  labs(y = "Mean Percent Positive-Negative",
       title = "Comparing Mean Percent Positivity-Negativity between Most Sentimentally-Loaded Image Objects",
       x = NULL) +
  scale_fill_manual(values = c(color[6],color[4])) +
  scale_y_continuous(labels = scales::percent)
ggsave(path = "artefacts/", 
       filename = "AWS_image_entities_postivity_percentage.png")

# word cloud for most common entities - saved manually
incubators_contentANDimages %>% 
  count(img_object, sort = T) %>% 
  with(wordcloud(img_object, n, max.words = 100))

## tf_idf bar chart(s) for image entities

# Part 1
incubators_contentANDimages %>% 
  filter(creator %in% unique(incubators_contentANDimages$creator)[1:6]) %>% 
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
  ggcharts::bar_chart(img_object, tf_idf, facet = creator, fill = creator) +
  scale_fill_manual(values = color[1:6]) +
  theme_bw() +
  labs(y = "Term Frequency by Inverse Document Frequency (TF-IDF)",
       x = NULL, 
       title = "Comparing the most characteristic image entities between the incubators") +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "AWS_image_entities_tf-idf_incubator_part1.png")

# Part 2
incubators_contentANDimages %>% 
  filter(creator %in% unique(incubators_contentANDimages$creator)[7:12]) %>% 
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
  ggcharts::bar_chart(img_object, tf_idf, facet = creator, fill = creator) +
  scale_fill_manual(values = c(color[7:9],color[12:14])) +
  theme_bw() +
  labs(y = "Term Frequency by Inverse Document Frequency (TF-IDF)",
       x = NULL, 
       title = "Comparing the most characteristic image entities between the incubators - Contd.") +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  hjust = 0.5 ))
ggsave(path = "artefacts/", 
       filename = "AWS_image_entities_tf-idf_incubator_part2.png")

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
  scale_size(range = range(c(0.5,1.2))) +
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

# Appendix/Scrap ------------------------------------------------------------

# tf_idf
incubators_contentANDimages %>% 
  count(creator, img_object, sort = T) %>% 
  left_join(.,incubators_contentANDimages %>% 
              count(creator, img_object, sort = T) %>% 
              group_by(creator) %>% 
              summarise(total = sum(n))) %>% 
  bind_tf_idf(img_object, creator, n) %>%
  group_by(creator) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(img_object, tf_idf, creator), fill = creator)) +
  geom_col(aes(y = img_object),show.legend = FALSE) +
  facet_wrap(~creator, ncol = 3, scales = "free") +
  labs(x = "TF-IDF", 
       y = NULL, 
       title = "Comparing the most characteristic image entities between the incubators")
