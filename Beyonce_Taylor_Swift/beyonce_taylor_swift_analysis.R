library(tidyverse)
library(janitor)
library(ggthemes)
library(tidytext)
library(textdata)
library(ggforce)
library(ggimage)
library(topicmodels)
library(patchwork)






# Read in the data ---------------------------------------------


beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv') %>% 
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv') %>% 
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv') %>% 
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv') %>% 
  janitor::clean_names() %>% janitor::remove_empty(which = "rows")

# Read in the image file -----------------------

beyonce_image <- "D:\\R for Data Science\\Scripts\\image\\beyonce.jpg"
taylor_swift_image <- "D:\\R for Data Science\\Scripts\\image\\taylor_swift.png"


# Get the nrc sentiments ---------------------------

nrc_sentiments <- get_sentiments("nrc")


# Analyze beyonce sentiments -------------------------------

beyonce_lyrics_unnested <- beyonce_lyrics %>% select(song_id, song_name, line) %>% 
  unnest_tokens(output = word, input = line, to_lower = TRUE) %>% 
  anti_join(y = stop_words, by = "word")



beyonce_sentiments <- beyonce_lyrics_unnested %>% count(word, sort = TRUE) %>%
  inner_join(nrc_sentiments, by = "word") %>% 
  select(word, sentiment, cnt = n) %>% 
  group_by(sentiment) %>% mutate(rown = row_number(-cnt)) %>% 
  ungroup() %>% 
  filter(sentiment %in% c("anger", "disgust", "joy", "surprise")) %>% 
  filter(rown <= 3) %>% 
  arrange(sentiment, rown)


# Plot Beyonce Sentiments -------------------------------

p_beyonce <- ggplot(data = beyonce_sentiments) + 
  ggforce::geom_arc_bar(mapping = aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1.0, amount = cnt, fill = word), stat ="pie") + 
  geom_image(mapping = aes(x = 0, y = 0, image = beyonce_image), size = 0.35) +
  facet_wrap(facets = ~sentiment, nrow = 2) +
  scale_fill_brewer(palette = "Paired") + 
  theme_solarized_2(light = FALSE, base_size = 10) + 
  theme(plot.title = element_text(color = "white"),
        strip.text = element_text(color = "white"),
        legend.text = element_text(color = "white"), 
        legend.title = element_text(color = "white"), 
        legend.position = "bottom", 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) + 
  guides(fill = guide_legend(nrow = 2, title = "Lyrics")) + 
  labs(title = "Beyonce - Sentiments")
  
 
p_beyonce
 

# Save Beyonce Plot -------------------------------
ggsave(filename = "D:\\R for Data Science\\R plots\\Beyonce_Taylor_Songs\\p_beyonce.png", plot = p_beyonce)


# Analyze Taylor Swift Sentiments -------------------------------

taylor_swift_lyrics_unnested <- taylor_swift_lyrics %>% select(album, title, lyrics) %>% 
  unnest_tokens(output = word, input = lyrics, to_lower = TRUE) %>% 
  anti_join(y = stop_words, by = "word")


taylor_swift_sentiments <- taylor_swift_lyrics_unnested %>% count(word, sort = TRUE) %>%
  inner_join(nrc_sentiments, by = "word") %>% 
  select(word, sentiment, cnt = n) %>% 
  group_by(sentiment) %>% mutate(rown = row_number(-cnt)) %>% 
  ungroup() %>% 
  filter(sentiment %in% c("anger", "disgust", "joy", "surprise")) %>% 
  filter(rown <= 3) %>% 
  arrange(sentiment, rown)




# Plot Taylor Swift Sentiments -------------------------------

p_taylor_swift <- ggplot(data = taylor_swift_sentiments) + 
  ggforce::geom_arc_bar(mapping = aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1.0, amount = cnt, fill = word), stat ="pie") + 
  geom_image(mapping = aes(x = 0, y = 0, image = taylor_swift_image), size = 0.35) +
  facet_wrap(facets = ~sentiment, nrow = 2) +
  scale_fill_brewer(palette = "Paired") + 
  theme_solarized_2(light = FALSE, base_size = 10) + 
  theme(plot.title = element_text(color = "white"),
        strip.text = element_text(color = "white"),
        legend.text = element_text(color = "white"), 
        legend.title = element_text(color = "white"), 
        legend.position = "bottom", 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) + 
  guides(fill = guide_legend(nrow = 2, title = "Lyrics")) + 
  labs(title = "Taylor Swift - Sentiments")


p_taylor_swift

# Save Taylor Swifts Plot -------------------------------

ggsave(filename = "D:\\R for Data Science\\R plots\\Beyonce_Taylor_Songs\\p_taylor_swift.png", plot = p_taylor_swift)



# TOPIC MODELLING ----------------------------------------------

# Beyonce Lyrics - Topic Modelling with 2 Topics ---------------


# Un-nest and Cast the data as Document Term Matrix ------------------------

beyonce_lyrics_dtm <- beyonce_lyrics_unnested %>% 
  count(song_id, word, sort = TRUE) %>% 
  cast_dtm(document = song_id, term = word, value = n)

# Apply the Latent Dirichlet Modelling with K = 2 Topics ------------------------

beyonce_lyrics_lda <- LDA(x = beyonce_lyrics_dtm, k = 2, control = list(seed = 1234))


# Convert the object back to tibble after applying the tidy function -----

beyonce_lyrics_topics <- as_tibble(tidy(x = beyonce_lyrics_lda, matrix = "beta"))



# Plot the data ---------------------------------

p_beyonce_topics <- beyonce_lyrics_topics %>% group_by(topic) %>% 
  top_n(n = 5, wt = beta) %>%
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(topic = str_c("Beyonce Topic", topic, sep = " ")) %>% 
  ggplot() + 
  geom_segment(mapping = aes(x = 0, y = reorder_within(term, beta, topic), xend = beta, yend = reorder_within(term, beta, topic), colour = term), show.legend = FALSE, size = 1.0, na.rm = TRUE) + 
  geom_point(mapping = aes(x = beta, y = reorder_within(term, beta, topic), colour = term), show.legend = FALSE, size = 3, na.rm = TRUE) + 
  scale_y_reordered() + 
  scale_color_brewer(palette = "Paired") + 
  facet_wrap(facets = ~topic, scales = "free") + 
  theme_solarized(light = FALSE, base_size = 10) + 
  theme(axis.text = element_text(color = "goldenrod1"), 
        axis.title = element_text(color = "goldenrod1", face = "bold.italic"), 
        strip.text = element_text(face = "bold")) + 
  labs(x = "Beta Probability", 
       y = "")



# Taylor Swift Lyrics - Topic Modelling with 2 Topics ---------------


# Un-nest and Cast the data as Document Term Matrix ------------------------

taylor_swift_lyrics_dtm <- taylor_swift_lyrics_unnested %>% 
  count(album, word, sort = TRUE) %>% 
  cast_dtm(document = album, term = word, value = n)


# Apply Latent Dirichlet Modelling with k = 2 Topics ---------------

taylor_swift_lyrics_lda <- LDA(x = taylor_swift_lyrics_dtm, k = 2, control = list(seed = 1234))


taylor_swift_lyrics_topics <- as_tibble(tidy(x = taylor_swift_lyrics_lda, matrix = "beta"))


# Plot the data --------------------------

p_taylor_swift_topics <- taylor_swift_lyrics_topics %>% group_by(topic) %>% 
  top_n(n = 5, wt = beta) %>%
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(topic = str_c("Taylor Swift Topic", topic, sep = " ")) %>% 
  ggplot() + 
  geom_segment(mapping = aes(x = 0, y = reorder_within(term, beta, topic), xend = beta, yend = reorder_within(term, beta, topic), colour = term), show.legend = FALSE, size = 1.0, na.rm = TRUE) + 
  geom_point(mapping = aes(x = beta, y = reorder_within(term, beta, topic), colour = term), show.legend = FALSE, size = 3, na.rm = TRUE) + 
  scale_y_reordered() + 
  scale_color_brewer(palette = "Paired") + 
  facet_wrap(facets = ~topic, scales = "free") + 
  theme_solarized(light = FALSE, base_size = 10) + 
  theme(axis.text = element_text(color = "goldenrod1"), 
        axis.title = element_text(color = "goldenrod1", face = "bold.italic"), 
        strip.text = element_text(face = "bold")) + 
  labs(x = "Beta Probablity", 
       y = "")



# Combine the plos with Patchwork Package -------------------

p_topic_modelling <- (p_beyonce_topics / p_taylor_swift_topics) + plot_layout(guides = 'collect') + 
  plot_annotation(title = "Beyonce & Taylor Swift Lyrics", 
                  subtitle = "Topic Modelling with k = 2 Topics", 
                  theme = theme_solarized(light = FALSE, base_size = 12) + theme(plot.title = element_text(color = "goldenrod1", face = "bold", size = 12), 
                                                                                 plot.subtitle = element_text(color = "goldenrod1", face = "bold.italic", size = 10)))




p_topic_modelling


# Save the Plot ----------------------------------------------


ggsave(filename = "D:\\R for Data Science\\R plots\\Beyonce_Taylor_Songs\\p_topic_modelling.png", plot = p_topic_modelling)















