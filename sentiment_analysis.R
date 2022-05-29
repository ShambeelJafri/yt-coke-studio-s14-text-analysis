#I haven't removed non-latin characters in this version
#meaning urdu text, emojis, numbers are still in there but they won't get picked up anyways



ky_tokens <- kana_yaari_raw_comments %>%
  select(textOriginal) %>%
  unnest_tokens(word, textOriginal)

more_stop_words <- data.frame(word = c("tu", "jhoom", "lyrics", "kana", "yaari",
                                       "br", "coke", "studio", "season", "song",
                                       "subtitles"))

ky_tokens <- ky_tokens %>%
  anti_join(stop_words) %>%
  anti_join(more_stop_words)

ky_tokens_counted <- ky_tokens %>% count(word, sort = T)

#bing -------------------

bing <- get_sentiments("bing")

ky_bing_sentiment <- ky_tokens_counted %>%
  inner_join(get_sentiments("bing")) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)


bing_sentiment <- data.frame(positive = sum(ky_bing_sentiment$positive),
                             negative = sum(ky_bing_sentiment$negative))

#nrc -----------------

nrc <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative"))

ky_nrc_sentiment <- ky_tokens_counted %>%
  inner_join(get_sentiments("nrc")) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

nrc_sentiment <- data.frame(positive = sum(ky_nrc_sentiment$positive),
                            negative = sum(ky_nrc_sentiment$negative))

#comparison of different lexicons
comparison <- data.frame(bing = bing_sentiment$positive - bing_sentiment$negative,
                         nrc = nrc_sentiment$positive - nrc_sentiment$negative)


#words contributing most to negative Bing score
ky_bing_sentiment_neg <- ky_bing_sentiment %>%
  filter(negative > 0) %>%
  head(10)

ky_bing_sentiment_pos <- ky_bing_sentiment %>%
  filter(positive > 0) %>%
  head(10)


ky_bing_sentiment_neg %>%
  arrange(negative) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(word=factor(word, levels=word)) %>%
  ggplot(mapping = aes(x = negative, y = word, fill = word)) +
  geom_col(fill = "#cf6d71") +
  labs(x = "Contribution to Negative Sentiment",
       y = NULL) +
  theme_minimal()

ky_bing_sentiment_pos %>%
  arrange(positive) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(word=factor(word, levels=word)) %>%
  ggplot(mapping = aes(x = positive, y = word, fill = word)) +
  geom_col(fill = "#4ac5e0") +
  labs(x = "Contribution to Positive Sentiment",
       y = NULL) +
  theme_minimal()



