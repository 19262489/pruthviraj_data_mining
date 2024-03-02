source("require_packages.R")
require_packages(c("httr", "xml2", "tidyverse", "tidytext", "syuzhet","lubridate")
                 
                 library(httr)
                 library(xml2)
                 library(tidyverse)
                 library(tidytext)
                 library(syuzhet)
                 library(lubridate)
                 
                 # 1. HTTP GET Request to the CNN RSS feed URL
                 RSS_URL <- "http://rss.cnn.com/rss/cnn_latest.rss."
                 CNN <- GET(RSS_URL)
                 
                 # Parse the XML content
                 xml_content <- read_xml(rawToChar(CNN$content))
                 
                 # 2. Perform sentiment analysis on the titles and descriptions of the news stories
                 titles <- xml_content %>% xml_find_all("//item/title") %>% xml_text()
                 descriptions <- xml_content %>% xml_find_all("//item/description") %>% xml_text()
                 
                 
                 # Combine titles and descriptions
                 text_data <- tibble(type = c(rep("title", length(titles)), rep("description", length(descriptions))),
                                     text = c(titles, descriptions))
                 
                 
                 # Tokenize the text
                 tokenized_data <- text_data %>%
                   unnest_tokens(word, text)
                 
                 # Perform sentiment analysis using the AFINN lexicon from the syuzhet package
                 sentiment_analysis_data <- tokenized_data %>%
                   mutate(score = get_sentiment(word, method = "afinn")) %>%
                   group_by(type) %>%
                   summarise(sentiment = sum(score, na.rm = TRUE))
                 
                 # 3. Summary statistics of the overall sentiment of the news stories
                 summary_stats <- sentiment_analysis_data %>%
                   summarise(mean_sentiment = mean(sentiment),
                             sd_sentiment = sd(sentiment),
                             min_sentiment = min(sentiment),
                             max_sentiment = max(sentiment))
                 
                 
                 # Print summary statistics
                 print(summary_stats)
                 
                 
                 # Print summary statistics and save to a timestamped file
                current_datetime <- format(now(), "%Y-%m-%d_%H-%M-%S")
                Data <- paste0("sentiment_summary_", current_datetime, ".txt")
                sink(file = Data)
                print(summary_stats)
                sink()  # Close the connection to the file
                 
