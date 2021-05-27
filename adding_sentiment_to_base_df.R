rm(list = ls())
# Read base df of all content
raw_content <- read.csv("https://raw.githubusercontent.com/cosmin-ticu/analyzing-austrian-incubators-content/main/incubators_raw_content_languages.csv",
                        fileEncoding = "utf-8")

library(aws.comprehend)
library(data.table)
library(tidyverse)

# Set up AWS in R
keyTable <- read.csv("D:/OneDrive - Central European University/Courses/Spring_Term/Data Science 3/accessKeys.csv", 
                     header = T) # accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

# Check mean character length
mean(nchar(raw_content$content))

# Check distribution of character length among content of articles
hist(nchar(raw_content$content))

# Create string vectors (each at most 4500 characters) 
# for articles with contents longer than 4500 characters
raw_content <- raw_content %>% 
  mutate(content2 = strsplit(content, "(?<=.{4500})", perl = TRUE))

# Long format
unnested_raw_content <- unnest(raw_content, cols = content2)

# Function to go row-by-row to get the sentiment
get_sentiment <- function(){
  ret_df <- rbindlist(lapply(unnested_raw_content[[9]], 
                             function(content){
                               
                               Sys.sleep(2)
                               
                               # keep only the columns relating to sentiment
                               t_list <- detect_sentiment(content)[,3:6]
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

# Extract sentiment with function
sentiment_scores <- get_sentiment()

# Bind all the sentiment scores to the long content table
unnested_raw_content_sentiment <- cbind(unnested_raw_content, 
                                        sentiment_scores)

nested_raw_content_sentiment <- unnested_raw_content_sentiment %>% 
  group_by_at(setdiff(names(unnested_raw_content_sentiment), 
                      c("content2",
                        "Mixed",
                        "Neutral",
                        "Negative",
                        "Positive"))) %>% 
  summarise(mixed = mean(Mixed),
            neutral = mean(Neutral),
            negative = mean(Negative),
            positive = mean(Positive))

# Write base file containing full content
write.csv(nested_raw_content_sentiment, 
          file = "incubators_raw_content_languages_sentiment.csv",
          fileEncoding = "utf-8",
          row.names = F)