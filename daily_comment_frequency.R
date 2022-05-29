standardize_days_and_count <- function(raw_comments) {

  for (i in 1:nrow(raw_comments)){

    date_to_replace_with <- str_sub(raw_comments$publishedAt[i], start = 6, end = 10)
    original_date <- raw_comments$publishedAt[i]

    raw_comments$publishedAt[i] <- str_replace(raw_comments$publishedAt[i],
                                               original_date,
                                               date_to_replace_with)

  }

  date_counts <- raw_comments %>%
    count(publishedAt)

  return(date_counts)


}


ky <- standardize_days_and_count(kana_yaari_raw_comments)
m <- standardize_days_and_count(mehram_raw_comments)
nnv <- standardize_days_and_count(neray_neray_vas_raw_comments)
p <- standardize_days_and_count(pasoori_raw_comments)
sdn <- standardize_days_and_count(sajan_das_na_raw_comments)
tj <- standardize_days_and_count(tu_jhoom_raw_comments)


ky <- ky %>% mutate(days_since_release = 0:(nrow(ky) - 1)) %>% select(n, days_since_release) %>% rename(kana_yaari = n)
m <- m %>% mutate(days_since_release = 0:(nrow(m) - 1)) %>% select(n, days_since_release) %>% rename(mehram = n)
nnv <- nnv %>% mutate(days_since_release = 0:(nrow(nnv) - 1)) %>% select(n, days_since_release) %>% rename(neray_neray_vas = n)
p <-  p %>% mutate(days_since_release = 0:(nrow(p) - 1)) %>% select(n, days_since_release) %>% rename(pasoori = n)
sdn <-  sdn %>% mutate(days_since_release = 0:(nrow(sdn) - 1)) %>% select(n, days_since_release) %>% rename(sajan_das_na = n)
tj <- tj %>% mutate(days_since_release = 0:(nrow(tj) - 1)) %>% select(n, days_since_release) %>% rename(tu_jhoom = n)

combined <- left_join(tj, ky, by = "days_since_release")
combined <- left_join(combined, m, by = "days_since_release")
combined <- left_join(combined, sdn, by = "days_since_release")
combined <- left_join(combined, nnv, by = "days_since_release")
combined <- left_join(combined, p, by = "days_since_release")



ggplot(combined, aes(x = days_since_release)) +
  geom_line(aes(y = tu_jhoom, colour = "Tu Jhoom")) +
  geom_line(aes(y = kana_yaari, colour = "Kana Yaari")) +
  geom_line(aes(y = mehram, colour = "Mehram")) +
  geom_line(aes(y = sajan_das_na, colour = "Sajan Das Na")) +
  geom_line(aes(y = neray_neray_vas, colour = "Neray Neray Vas")) +
  geom_line(aes(y = pasoori, colour = "Pasoori")) +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0, 135, by = 20)) +
  xlab("days since release") +
  ylab("comments") +
  ggtitle("Daily comments frequency") +
  theme(plot.title = element_text(hjust = 0.5))




