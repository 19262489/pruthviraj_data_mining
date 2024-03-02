source("require_packages.R")

require_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Load required packages
require_packages(c("httr", "xml2", "tidyverse", "tidytext", "syuzhet", "lubridate"))

# Perform HTTP GET Request to the CNN RSS feed URL
RSS_URL <- "http://rss.cnn.com/rss/cnn_latest.rss"
CNN <- httr::GET(RSS_URL)

# Parse the XML content
xml_content <- xml2::read_xml(httr::content(CNN, as = "text"))

# Extract titles and descriptions
titles <- xml_content %>% xml2::xml_find_all("//item/title") %>% xml2::xml_text()
descriptions <- xml_content %>% xml2::xml_find_all("//item/description") %>% xml2::xml_text()

# Combine titles and descriptions
text_data <- tibble::tibble(type = c(rep("title", length(titles)), rep("description", length(descriptions))),
                            text = c(titles, descriptions))

# Tokenize the text
tokenized_data <- text_data %>%
  tidytext::unnest_tokens(word, text)

# Perform sentiment analysis using the AFINN lexicon from the syuzhet package
sentiment_analysis_data <- tokenized_data %>%
  dplyr::mutate(score = syuzhet::get_sentiment(word, method = "afinn")) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(sentiment = sum(score, na.rm = TRUE))

# Calculate summary statistics of sentiment
summary_stats <- sentiment_analysis_data %>%
  dplyr::summarise(mean_sentiment = mean(sentiment),
                    sd_sentiment = sd(sentiment),
                    min_sentiment = min(sentiment),
                    max_sentiment = max(sentiment))

# Print summary statistics
print(summary_stats)

# Save summary statistics to a timestamped file
current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
Data <- paste0("sentiment_summary_", current_datetime, ".txt")
sink(file = Data)
print(summary_stats)
sink()  # Close the connection to the file
