
rm(list=ls())

# Option by book text mining with r : https://www.tidytextmining.com/

# Libraries
library(tm)
library(tokenizers)
library(dplyr)
library(naivebayes)
library(data.table)
library(textreg)

# Setting directories

old.dir <- getwd()

# Profanity

profanitylist <- read.csv("Terms-to-Block.csv", header = FALSE)
profanitylist <- profanitylist[,"V1"]


# To corpus function

to_corpus <- function (file, percentaje) {
        con <- file(file, "r")
        rawfile <- readLines(con, skipNul = TRUE, warn=FALSE)
        close(con)
        samplelines <- rawfile[sample(length(rawfile), round(percentaje*length(rawfile),0))]
        source <- VectorSource(samplelines)
        corpus <- VCorpus(source)
        return(corpus)
}


# Twitter database

set.seed(743)

twitter <- to_corpus("en_US.twitter.txt", 0.025)

twitter <- tm_map(twitter, removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(content_transformer(tolower), lazy = TRUE) %>%
        tm_map(removeWords, stopwords("english"), lazy = TRUE) %>%
        tm_map(removeWords, profanitylist, lazy = TRUE) 

twitter_dtm <- DocumentTermMatrix(twitter)

blogs <- to_corpus("en_US.blogs.txt", 0.025)

blogs <- tm_map(blogs, removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(content_transformer(tolower), lazy = TRUE) %>%
        tm_map(removeWords, stopwords("english"), lazy = TRUE) %>%
        tm_map(removeWords, profanitylist, lazy = TRUE)

blogs_dtm <- DocumentTermMatrix(blogs)

news <- to_corpus("en_US.news.txt", 0.025)

news <- tm_map(news, removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(content_transformer(tolower), lazy = TRUE) %>%
        tm_map(removeWords, stopwords("english"), lazy = TRUE) %>%
        tm_map(removeWords, profanitylist, lazy = TRUE) 

news_dtm <- DocumentTermMatrix(news)

muestra_1gram <- c(twitter_dtm, blogs_dtm, news_dtm)

# Next ngrams

BigramTokenizer <-
        function(x)
                unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

TrigramTokenizer <-
        function(x)
                unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

FourgramTokenizer <-
        function(x)
                unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

FivegramTokenizer <-
        function(x)
                unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)


twitter_bigram_dtm <- DocumentTermMatrix(twitter, control = list(tokenize = BigramTokenizer))

twitter_trigram_dtm <- DocumentTermMatrix(twitter, control = list(tokenize = TrigramTokenizer))

twitter_fourgram_dtm <- DocumentTermMatrix(twitter, control = list(tokenize = FourgramTokenizer))

twitter_fivegram_dtm <- DocumentTermMatrix(twitter, control = list(tokenize = FivegramTokenizer))

## Blogs 

blogs_bigram_dtm <- DocumentTermMatrix(blogs, control = list(tokenize = BigramTokenizer))

blogs_trigram_dtm <- DocumentTermMatrix(blogs, control = list(tokenize = TrigramTokenizer))

blogs_fourgram_dtm <- DocumentTermMatrix(blogs, control = list(tokenize = FourgramTokenizer))

blogs_fivegram_dtm <- DocumentTermMatrix(blogs, control = list(tokenize = FivegramTokenizer))

## news 

news_bigram_dtm <- DocumentTermMatrix(news, control = list(tokenize = BigramTokenizer))

news_trigram_dtm <- DocumentTermMatrix(news, control = list(tokenize = TrigramTokenizer))

news_fourgram_dtm <- DocumentTermMatrix(news, control = list(tokenize = FourgramTokenizer))

news_fivegram_dtm <- DocumentTermMatrix(news, control = list(tokenize = FivegramTokenizer))


# Frecuencias 2

muestra_2gram <- c(twitter_bigram_dtm, blogs_bigram_dtm, news_bigram_dtm)


# Frecuencias 3

muestra_3gram <- c(twitter_trigram_dtm, blogs_trigram_dtm, news_trigram_dtm)


# Frecuencias 4

muestra_4gram <- c(twitter_fourgram_dtm, blogs_fourgram_dtm, news_fourgram_dtm)



# Frecuencias 5

muestra_5gram <- c(twitter_fivegram_dtm, blogs_fivegram_dtm, news_fivegram_dtm)

# 1

onegram <- removeSparseTerms(muestra_1gram,sparse = 0.9997)

dim(onegram)

#' Find frequent terms
colS1 <- colSums(as.matrix(onegram))
length(colS1)
freqs1 <- data.table(name = attributes(colS1)$names, count = colS1)

#' Most frequent and least frequent words
freqs1[order(-count)][1:10] #top 10 most frequent words
freqs1[order(count)][1:10] #least 10 frequent words

write.csv(freqs1, "onegram.csv")

# 2

twogram <- removeSparseTerms(muestra_2gram,sparse = 0.9999)

dim(twogram)


#' Find frequent terms
colS2 <- colSums(as.matrix(twogram))
length(colS2)
freqs2 <- data.table(name = attributes(colS2)$names, count = colS2)

#' Most frequent and least frequent words
freqs2[order(-count)][1:10] #top 10 most frequent words
freqs2[order(count)][1:10] #least 10 frequent words

write.csv(freqs2,"twogram.csv")

# 3

threegram <- removeSparseTerms(muestra_3gram,sparse = 0.99997)

dim(threegram)

#' Find frequent terms
colS3 <- colSums(as.matrix(threegram))
length(colS3)
freqs3 <- data.table(name = attributes(colS3)$names, count = colS3)

#' Most frequent and least frequent words
freqs3[order(-count)][1:10] #top 10 most frequent words
freqs3[order(count)][1:10] #least 10 frequent words

write.csv(freqs3, "trigram.csv")

# 4

fourgram <- removeSparseTerms(muestra_4gram,sparse = 0.99999)

dim(fourgram)

#' Find frequent terms
colS4 <- colSums(as.matrix(fourgram))
length(colS4)
freqs4 <- data.table(name = attributes(colS4)$names, count = colS4)

#' Most frequent and least frequent words
freqs4[order(-count)][1:10] #top 10 most frequent words
freqs4[order(count)][1:10] #least 10 frequent words

write.csv(freqs4, "fourgram.csv")


# 5

fivegram <- removeSparseTerms(muestra_5gram,sparse = 0.99999)

dim(fivegram)

#' Find frequent terms
colS5 <- colSums(as.matrix(fivegram))
length(colS5)
freqs5 <- data.table(name = attributes(colS5)$names, count = colS5)

#' Most frequent and least frequent words
freqs5[order(-count)][1:10] #top 10 most frequent words
freqs5[order(count)][1:10] #least 10 frequent words

write.csv(freqs5, "fivegram.csv")



