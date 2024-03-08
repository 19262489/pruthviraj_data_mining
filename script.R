source("require_packages.R")
require_packages(c("httr", "xml2", "dplyr","stringr","readr", "tidyr", "syuzhet","lubridate")

RSS_URL <- "http://rss.cnn.com/rss/cnn_latest.rss."
CNN <- GET(RSS_URL)
                 
                
xml_content <- read_xml(rawToChar(CNN$content))
                 
titles <- xml_content %>% xml_find_all("//item/title") %>% xml_text()
descriptions <- xml_content %>% xml_find_all("//item/description") %>% xml_text()
                 
                 
text_data <- tibble(type = c(rep("title", length(titles)), rep("description", length(descriptions))),
text = c(titles, descriptions))
                 
                 
tokenized_data <- text_data %>%
unnest_tokens(word, text)
                 
sentiment_analysis_data <- tokenized_data %>%
mutate(score = get_sentiment(word, method = "afinn")) %>%
group_by(type) %>%
summarise(sentiment = sum(score, na.rm = TRUE))
                 
summary_stats <- sentiment_analysis_data %>%
summarise(mean_sentiment = mean(sentiment),
sd_sentiment = sd(sentiment),
min_sentiment = min(sentiment),
max_sentiment = max(sentiment))
                 
                 
#print(summary_stats)
                 
                 
current_datetime <- format(now(), "%Y-%m-%d_%H-%M-%S")
Data <- paste0("sentiment_summary_", current_datetime, ".txt")
sink(file = Data)
print(summary_stats)
sink() 
                 
                 
