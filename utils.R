#general utils files

library(quanteda)
library(tuber)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tictoc)
library(rworldmap)
library(future)
library(rnaturalearth)
library(ggeasy)
library(writexl)
library(readxl)
library(textclean)


videos <- c("7D4vNcK6D38", #tu jhoom
            "zQDAi8tI-cU", #kana yaari
            "HrFlFMK1mKI", #sajan das na
            "BUm_hFMlsgg", #mehram
            "diGJy8eFbWw", #neray neray vas
            "5Eqb_-j3FDA" #pasoori
)

video_names <- c("tu_jhoom",
                 "kana_yaari",
                 "sajan_das_na",
                 "mehram",
                 "neray_neray_vas",
                 "pasoori" )

#fetch stuff

#stats
for (i in 1:length(videos)) {
  #this gets raw data
  name <- paste0(video_names[i],"_stats")

  assign(name, tuber::get_stats(videos[i]))

}

#raw comments
for (i in 1:length(videos)) {
  #this gets raw data
  name <- paste0(video_names[i],"_raw_comments")

  assign(name, tuber::get_all_comments(videos[i]))

}

##########helper functions
likeCount_to_number <- function(dataframe) {
  #convert col to integer

  dataframe$likeCount <- as.numeric(as.character(dataframe$likeCount))

  return(dataframe)

}

get_comments_and_tokenize <- function(video_id) {
  #fetch  raw comments, cleans and tokenizes them for quicker analysis
  tictoc::tic()
  message("fetching comments")
  df <- tuber::get_all_comments(video_id = video_id) %>%
    select(textDisplay) %>%
    unnest_tokens(word, textDisplay)
  return(df)
  tictoc::toc()

}

count_countries <- function(comment_text) {
  #count the number of country names in tokenized comments

  tokenized_comment_text <- comment_text %>% unnest_tokens(word, textOriginal)

  countries <- tokenized_comment_text %>%
    semi_join(country_names)

  return(countries)

}

trigram_it <- function(raw_comments) {
  #make trigrams from raw comments
  comment_text <- raw_comments %>%
    select(textOriginal)

  trigram_tokenized_comment_text <- comment_text %>%
    unnest_tokens(trigram, textOriginal, token = "ngrams", n = 3)

  return(trigram_tokenized_comment_text)
}

get_chr_length <- function(raw_comments) {

  comments_and_characters <- raw_comments %>%
    select(textOriginal) %>%
    mutate(chr_length = str_length(textOriginal)) %>%
    arrange(chr_length)

  print(summary(comments_and_characters))

  return(comments_and_characters)

}

remove_non_latin <- function(tokenized_comments) {
  #this removes all non-latin characters, including urdu text and emojis

  tokenized_comments <- data.frame(sapply(
    tokenized_comments, iconv, from="latin1", to="ASCII",sub=0)
  )

  non_latin <- tokenized_comments %>% filter(str_detect(word, "0"))

  tokenized_comments <- tokenized_comments %>%
    anti_join(non_latin)

  return(tokenized_comments)

}

remove_numbers <- function(tokenized_comments) {

  nums <- tokenized_comments %>%
    filter(str_detect(word, "^[0-9]")) %>%
    select(word) %>% unique()

  tokenized_comments <- tokenized_comments %>%
    anti_join(nums)

  return(tokenized_comments)

}

standardize_days_and_count <- function(raw_comments) {
  #this counts days since release
  for (i in 1:nrow(raw_comments)){

    date_to_replace_with <- str_sub(raw_comments$publishedAt[i], start = 1, end = 10)
    original_date <- raw_comments$publishedAt[i]

    raw_comments$publishedAt[i] <- str_replace(raw_comments$publishedAt[i],
                                               original_date,
                                               date_to_replace_with)

  }

  date_counts <- raw_comments %>%
    count(publishedAt)

  return(date_counts)


}

likes_and_comments_to_views <- function(stats) {

  stats$likeCount <- as.integer(stats$likeCount)
  stats$commentCount <- as.integer(stats$commentCount)
  stats$viewCount <- as.integer(stats$viewCount)

  round(((stats$likeCount + stats$commentCount) / stats$viewCount) * 100, 2)

}

count_words_per_comment <- function(comments) {
  # also removes empty rows

  comments <- data.frame(gsub('\\p{So}|\\p{Cn}', '', comments$textOriginal, perl = TRUE))

  video_name <- colnames(comments)

  comments <- comments %>% rename(comments = video_name)

  comments$total <- sapply(comments$comments, function(x) length(unlist(strsplit(as.character(x), "\\s"))))

  comments <- subset(x = comments, subset = total != 0 )

  return(comments)

}


