#tokenize comments

tokenized_kana_yaari <- kana_yaari_raw_comments %>%
  select(textOriginal) %>%
  unnest_tokens(word, textOriginal)

#count for word cloud

tokenized_kana_yaari <- kana_yaari_raw_comments %>%
  select(textOriginal) %>%
  unnest_tokens(word, textOriginal) %>%
  count(word, sort = T)

#remove stop words
data("stop_words")
more_stop_words <- data.frame(word = c("coke", "song", "studio","br", "href",
                                       "https", "quot", "cs", "amp", "kana", "yaari", "eva",
                                       "subtitles", "music", "khalil", "lyrics", "kaifi", "captions"))

tokenized_kana_yaari <- tokenized_kana_yaari %>%
  anti_join(stop_words) %>%
  anti_join(more_stop_words)

pal <- brewer.pal(8,"Dark2")

tokenized_kana_yaari %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 75, colors=pal))
