library(tidyverse)
library(janitor)
library(tidytext)
library(ggthemes)
library(textdata)
library(topicmodels)
library(tvthemes)


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


p_topic_beta

ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Avatar\\topic_model.png", plot = p_topic_beta)




  