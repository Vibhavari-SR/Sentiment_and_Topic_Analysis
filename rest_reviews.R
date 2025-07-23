# Required libraries
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# Read Excel file (change the file path as needed)
library(readxl)
zom <- read_excel("D:/zom.xlsx", range = "N1:N50")
View(zom) # replace with actual file name

# Function to extract rating and review from each tuple string
extract_reviews <- function(cell_text) {
  # Extract each tuple from the string
  tuple_strings <- str_extract_all(cell_text, "\\('Rated .*?'\\)")[[1]]
  
  # Extract rating and review text
  map_dfr(tuple_strings, function(tup) {
    rating <- str_extract(tup, "Rated\\s*\\d+(\\.\\d+)?") %>%
      str_extract("\\d+(\\.\\d+)?") %>%
      as.numeric()
    
    review <- str_match(tup, "'RATED\\\\n\\s*(.*?)'")[,2] %>%
      str_replace_all("\\\\n", " ") %>%
      str_squish()
    
    tibble(Rating = rating, Review = review)
  })
}

# Apply to entire column and flatten the results
all_reviews <- map_dfr(zom, extract_reviews)

# View cleaned data
head(all_reviews)

# 1. Required packages

library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tidyverse)

# 2. Remove NA values
clean_reviews <- all_reviews %>%
  filter(!is.na(Review), Review != "") 

# 3. Create a Corpus
corpus <- VCorpus(VectorSource(clean_reviews$Review))

# 4. Clean the text
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# 5. Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus_clean)

# 6. Remove sparse terms
dtm_sparse <- removeSparseTerms(dtm, 0.98)

# 7. Fit LDA model (e.g., 3 topics)
lda_model <- LDA(dtm_sparse, k = 3, control = list(seed = 1234))

# 8. Extract top terms per topic
topics <- tidy(lda_model, matrix = "beta")  # beta = probability of term in topic

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 9. Plot top terms per topic
library(ggplot2)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(
    title = "Top 10 Terms per Topic in Restaurant Reviews",
    x = "Beta (Term Probability in Topic)",
    y = "Term" 
  )

