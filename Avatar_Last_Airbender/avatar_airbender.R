library(tidyverse)
library(janitor)
library(tidytext)
library(textdata)
library(topicmodels)
library(tvthemes)
library(patchwork)



# Import data from the Tidy Tuesday Github Repository

tbl_avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv') %>%
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")
tbl_scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv') %>%
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")

  
# Un-nest Tokens from the full text variable 

tbl_avatar_words <- tbl_avatar %>% unnest_tokens(output = word, input = full_text, to_lower = TRUE) %>% 
  anti_join(stop_words, by = "word") %>%
  select(id, book, book_num, chapter, word)


# Cast the data set to Document Term Matrix 
# Before Applying the LDA Topic Modelling 

avatar_dtm <- tbl_avatar_words %>% count(book, word, sort = TRUE) %>%
  cast_dtm(document = book, term = word, value = n)


# Apply the LDA Topic Modelling Function
avatar_lda <- LDA(x = avatar_dtm, k = 2, control = list(seed = 1234))


# Cast the data set back to Tibble 
tbl_avatar_lda_topics <- as_tibble(tidy(x = avatar_lda, matrix = "beta"))


# Filter and arrange according to 
# high beta-Probabilities per Topic 
tbl_avatar_topic_beta <- tbl_avatar_lda_topics %>% group_by(topic) %>%
  top_n(n = 10, wt = beta) %>%
  ungroup() %>% 
  arrange(topic, -beta)

image_var <- "D:\\R for Data Science\\Scripts\\avatar_image.png"

# Plotting - Terms/Words Versus the Beta Probability 
p_topic_beta <- tbl_avatar_topic_beta %>% 
  ggplot(mapping = aes(x = reorder_within(term, beta, topic), 
                       y = beta, 
                       fill = as.factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") +
  scale_x_reordered() + 
  coord_flip() + 
  theme_avatar() + 
  labs(x = "Terms/Words", 
       y = "Beta - Probability", 
       caption = "Avatar :: The Last Airbender | Tidy Tuesday (Week 33)") + 
  ggtitle(label = "Avatar::The Last Airbender", subtitle = "LDA Topic Modelling with k = 2 Topics")

# Using background image of avatar
# does not look impressive at all :(
# p_topic_beta_background <- ggbackground(p_topic_beta, background = image_var, alpha = 0.02)

p_topic_beta


ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Avatar\\topic_model.png", 
       plot = p_topic_beta)


# Get the bing sentiments


tbl_bing_sentiments <- get_sentiments("bing")

tbl_avatar_sentiments <- inner_join(tbl_avatar_words, tbl_bing_sentiments, by = "word")


# Positive Sentiments Analysis --------------------------------------------

tbl_positive_sentiments <- tbl_avatar_sentiments %>% group_by(book, word, sentiment) %>% 
  summarise(cnt = n()) %>%
  ungroup() %>% 
  filter(sentiment == "positive") %>%
  group_by(book) %>%
  top_n(n = 8, wt = cnt)


p_positive_words <- ggplot(data = tbl_positive_sentiments) + 
  geom_col(mapping = aes(x = reorder_within(word, cnt, book), y = cnt, fill = book), show.legend = FALSE) +
  facet_wrap(~book, scales = "free") + 
  scale_x_reordered() + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_avatar() + 
  labs(x = "Positive Sentiments", 
       y = "Count")




# Negative Sentiment Analysis ----------------------------------


tbl_negative_sentiments <- tbl_avatar_sentiments %>% group_by(book, word, sentiment) %>% 
  summarise(cnt = n()) %>%
  ungroup() %>% 
  filter(sentiment == "negative") %>%
  group_by(book) %>%
  top_n(n = 8, wt = cnt)


p_negative_words <- ggplot(data = tbl_negative_sentiments) + 
  geom_col(mapping = aes(x = reorder_within(word, cnt, book), y = cnt, fill = book), show.legend = FALSE) +
  facet_wrap(~book, scales = "free") + 
  scale_x_reordered() + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_avatar() + 
  labs(x = "Negative Sentiments", 
       y = "Count")

# Combining Plots with Patchwork ----------------------------------

p_patchwork_plot <- (p_positive_words / p_negative_words) + 
  plot_annotation(title = "Avatar::The Last Airbender - Top 10 Positive/Negative Sentiment Words", 
                  caption = "Avatar :: The Last Airbender | Tidy Tuesday (Week 33)", 
                  theme = theme_avatar(title.size = 12))


p_patchwork_plot

# Save the Plot 

ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Avatar\\positive_sentiments.png", 
       plot = p_patchwork_plot)


