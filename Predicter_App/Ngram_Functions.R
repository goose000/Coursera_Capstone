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