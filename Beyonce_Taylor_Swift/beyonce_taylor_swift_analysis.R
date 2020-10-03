library(tidyverse)
library(janitor)
library(ggthemes)
library(tidytext)
library(textdata)
library(ggforce)
library(ggimage)





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


# Save Taylor Swifts Plot -------------------------------

ggsave(filename = "D:\\R for Data Science\\R plots\\Beyonce_Taylor_Songs\\p_taylor_swift.png", plot = p_taylor_swift)















