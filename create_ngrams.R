twitterFile <-
    file('Coursera Capstone Data/en_US/en_US.twitter.txt')
blogsFile <- file('Coursera Capstone Data/en_US/en_US.blogs.txt')
newsFile <-
    file('Coursera Capstone Data/en_US/en_US.news.txt', 'rb')

twitterText <-
    readLines(twitterFile, encoding = "UTF-8", skipNul = T)
blogsText <- readLines(blogsFile, encoding = "UTF-8", skipNul = T)
#newslines<-R.utils::countLines('Data/en_US/en_US.news.txt')
newsText <-
    readLines(newsFile,
              #n = newslines[1],
              encoding = "UTF-8",
              skipNul = T)

close(twitterFile)
close(blogsFile)
close(newsFile)

minOccur <- 10
maxNGram <- 5
sampleSize = 0.5
sampleSize2 = .2

library(tibble)

twitterTextTib <- tibble(text = twitterText)
twitterTextLength <- length(twitterText)
blogsTextTib <- tibble(text = blogsText)
blogsTextLength <- length(blogsText)
newsTextTib <- tibble(text = newsText)
newsTextLength <- length(newsText)

twitterTrainIndex <-
    sample(1:twitterTextLength, sampleSize * twitterTextLength)
blogsTrainIndex <-
    sample(1:blogsTextLength, sampleSize * blogsTextLength)
newsTrainIndex <-
    sample(1:newsTextLength, sampleSize * newsTextLength)

library(dplyr)

twitterTextTrain <- twitterTextTib %>%
    slice(twitterTrainIndex)
blogsTextTrain <- blogsTextTib %>%
    slice(blogsTrainIndex)
newsTextTrain <- newsTextTib %>%
    slice(newsTrainIndex)

# twitterTextTest <- twitterTextTib[-twitterTrainIndex, ]
# blogsTextTest <- blogsTextTib[-blogsTrainIndex, ]
# newsTextTest <- newsTextTib[-newsTrainIndex, ]

#test <- twitterTextTest$text[1]
library(qdapRegex)
library(tidytext)
data(stop_words)
#

twitterTextTrain %>%
    mutate(
        #text = rm_non_words(text),
        line = row_number(),
        .before = text
    ) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)


twitterSentencesTib <- twitterTextTrain %>%
    unnest_tokens(text, text, token = 'sentences')
blogsSentencesTib <- blogsTextTrain %>%
    unnest_tokens(text, text, token = 'sentences')
newsSentencesTib <- newsTextTrain %>%
    unnest_tokens(text, text, token = 'sentences')



sentencesTib <-
    bind_rows(twitterSentencesTib, blogsSentencesTib, newsSentencesTib)


diverseSample <-
    sample(1:nrow(sentencesTib), sampleSize2 * nrow(sentencesTib))
diverseSampleTib <- sentencesTib %>%
    slice(diverseSample)
# %>%
#     mutate(text = str_replace_all(text, c('\\b(\\W+|\\d+)\\W*|\\d*\\s'='','\\s\\d*|\\W*(\\W+|\\d+)\\s'='')))
#%>%
#select(-line) %>%
#mutate(line = seq_along(text)) %>%

tokensTib <- tibble()

start_time <- Sys.time()
for (n in 1:maxNGram) {
    tokensTib <- rbind(diverseSampleTib,
                       tibble(
                           n,
                           unnest_tokens(
                               #diverseSampleTib,
                               sentencesTib,
                               ngram,
                               text,
                               token = 'ngrams',
                               n = n,
                               collapse = F
                           )
                       ))
}
buildNGramsTime <- Sys.time() - start_time



# group_by(line) %>%
# mutate(
#     wordindex = seq_along(ngram)
#     )

library(stringr)
start_time <- Sys.time()
keepTokens <- tokensTib %>%
    group_by(ngram, n) %>%
    summarise(Occurrances = n(), .groups = 'drop') %>%
    filter(!is.na(ngram),
           Occurrances > minOccur,
           !grepl(pattern = '\\d+|(^\\w$)', x = ngram)) %>%
    mutate(Beginning = word(ngram, 1, -2),
           End = word(ngram,-1)) %>%
    select(-ngram) %>%
    arrange(desc(Occurrances))
keepTokensTime <- Sys.time() - start_time

start_time <- Sys.time()
ngrams <- list()
for (i in 1:maxNGram) {
    ngrams[[i]] <- filter(keepTokens, n == i)
    
}
ngramsTime <- Sys.time() - start_time

ngrams[[1]] <- ngrams[[1]] %>%
    slice_max(order_by = Occurrances, n = 1)

saveRDS(ngrams, 'Coursera_Capstone/Data/ngrams')
