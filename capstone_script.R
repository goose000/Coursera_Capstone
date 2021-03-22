


oldwd <- getwd()
setwd("C:/Users/goose/Documents/R")

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
sampleSize = 0.005
sampleSize2 = .03

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
diverseSampleTib <- sentencesTib[diverseSample, ]
# %>%
#     mutate(text = str_replace_all(text, c('\\b(\\W+|\\d+)\\W*|\\d*\\s'='','\\s\\d*|\\W*(\\W+|\\d+)\\s'='')))
#%>%
#select(-line) %>%
#mutate(line = seq_along(text)) %>%

tokensTib <- tibble()

start_time <- Sys.time()
for (n in 2:maxNGram) {
    tokensTib <- rbind(tokensTib,
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
    slice_max(order_by = Occurrances, n = 10)


twitterSentencesTestTib <- twitterTextTest %>%
    unnest_tokens(text, text, token = 'sentences')
blogsSentencesTestTib <- blogsTextTest %>%
    unnest_tokens(text, text, token = 'sentences')
newsSentencesTestTib <- newsTextTest %>%
    unnest_tokens(text, text, token = 'sentences')

sentencesTestTib <-
    bind_rows(twitterSentencesTestTib,
              blogsSentencesTestTib,
              newsSentencesTestTib)

tokensTestTib <- tibble()
for (n in 1:maxNGram) {
    tokensTestTib <- rbind(tokensTestTib,
                           tibble(
                               n,
                               unnest_tokens(
                                   sentencesTestTib,
                                   ngram,
                                   text,
                                   token = 'ngrams',
                                   n = n,
                                   collapse = F
                               )
                           ))
}

cleanTestTokens <- tokensTestTib %>%
    filter(!is.na(ngram),!grepl(pattern = '\\d+|(^\\w$)', x = ngram)) %>%
    mutate(Beginning = word(ngram, 1, -2),
           End = word(ngram,-1)) %>%
    select(-ngram)



# best <- NULL
# for (i in maxNGram:1) {
#    if (ngramStart$n>=i){
#         if(i>1){
#             temp <- ngrams[[i]] %>%
#                 filter(Beginning==ngramStart$Beginning) %>%
#                 slice_max(order_by = Occurrances, n = 10)
#             best <- append(best,temp$End)
#             best <- best[1:min(10,length(best))]
#
#         }else{
#             best <- append(best,ngrams[[1]]$End)
#             best <- best[1:min(10,length(best))]
#         }
#
#     }
# }



testForMatch <- function(ngramTib, ngramDictionary, nresults = 10) {
    best <- NULL
    maxNGram <- length(ngramDictionary)
    for (i in maxNGram:1) {
        if (ngramTib$n >= i) {
            if (i > 1) {
                temp <- ngramDictionary[[i]] %>%
                    filter(Beginning == ngramTib$Beginning) %>%
                    slice_max(order_by = Occurrances, n = nresults)
                best <- append(best, temp$End)
                best <- best[1:min(nresults, length(best))]
                
            } else{
                best <- append(best, ngramDictionary[[1]]$End)
                best <- best[1:min(nresults, length(best))]
            }
            
        }
    }
    if (ngramTib$End %in% best)
        which(best == ngramTib$End)
    else
        'No Match'
}

testResults <-
    vector(mode = 'character', length = nrow(cleanTestTokens))
start_time <- Sys.time()
for (gram in 1:nrow(cleanTestTokens)) {
    print(gram)
    testResults[gram] <-
        testForMatch(cleanTestTokens[gram, ], ngrams)
}
testingTime <- Sys.time() - start_time
score <- sum(testResults == 'No Match') / length(testResults)
score


ngramify <- function(string, maxNGram = 5) {
    ngramSize <- maxNGram - 1
    ngramList <- tibble()
    stringTib <- tibble(text = string)
    tokenLength <- unnest_tokens(stringTib, text, text) %>% nrow
    if (tokenLength < maxNGram)
        maxNGram <- tokenLength
    for (n in ngramSize:1) {
        temp <-
            unnest_tokens(stringTib, text, text, token = 'ngrams', n = n) %>%
            tail(1) %>%
            mutate(n = n + 1)
        ngramList <- bind_rows(ngramList, temp)
    }
    ngramList
}


predictWord <-
    function(ngramTib,
             ngramDictionary = ngrams,
             nresults = 10) {
        best <- NULL
        maxNGram <- length(ngramDictionary)
        j <- 1
        for (i in maxNGram:1) {
            if (ngramTib$n[1] >= i) {
                if (i > 1) {
                    gram <- ngramTib[j, 1]$text
                    temp <- ngramDictionary[[i]] %>%
                        filter(Beginning == gram) %>%
                        slice_max(order_by = Occurrances, n = nresults) %>%
                        select(End)
                    best <- append(best, temp$End)
                    best <-
                        if (length(best) > 0)
                            best[1:min(nresults, length(best))]
                    j <- j + 1
                } else{
                    best <- append(best, ngramDictionary[[1]]$End)
                    best <- best[1:min(nresults, length(best))]
                }
                
            }
        }
        best
    }

1




























# gram <- 3
# testForMatch(cleanTestTokens[gram,], ngrams)
#
# apply(head(cleanTestTokens), 1, testForMatch, ngramDictionary = ngrams)
# tapply(head(cleanTestTokens), 1, testForMatch, ngramDictionary = ngrams)
# mapply(testForMatch, head(cleanTestTokens), ngramDictionary = ngrams)
# cleanTestTokens %>%
#     mutate(testResults = testForMatch)

# twitterSequenceTable <- twitterTokensTib %>%
#     filter(ngram %in% keepTokens$ngram,
#            !is.na(ngram)) %>%
#     separate(ngram, into = c('fourthlast', 'thirdlast', 'secondlast', 'nextlast', 'last'), fill = 'left')





# trainTwitterData %>%
#     group_by()
#
#
#
#     print(n = 30)
#
# 1











setwd(oldwd)
