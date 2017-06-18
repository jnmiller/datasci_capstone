require(quanteda)
require(data.table)
source("util.R")
 
determineVocabulary <- function(all.tokens, bannedWords, max.size=20000) {
    all.words <- sort(colSums(dfm(unlist(sapply(all.tokens, unlist)))), decreasing=TRUE)
    # Exclude bad words
    all.words <- all.words[!(names(all.words) %in% bannedWords)]
    #f <- which.min(cumsum(all.words) <= max.coverage * sum(all.words))
    vocab <- all.words[1:min(max.size, length(all.words))]
    print(paste0("Vocab size is ", length(vocab)))
    return(vocab)
}

buildFreqTable <- function(cleaned.tokens, n) {
    ngrams <- colSums(dfm(tokens_ngrams(cleaned.tokens, n = n)))
    #print(ngrams)
    ngrams.text <- names(ngrams)
    if (n == 1) {
        h <- rep("", length(ngrams))
    } else {
        h <- historyFromNgram(ngrams.text)
    }
    #print(h)
    table.n <- data.table(
        history=h,
        word=lastWordFromNgram(ngrams.text),
        freq=as.vector(ngrams)
    )
    if (n == 1) {
        setkey(table.n, word)
    } else {
        setkey(table.n, history)
    }
    table.n <- table.n[(history != "") || (word != "")]
    table.n <- table.n[!duplicated(table.n[,c("history","word")])]
    return(table.n)
}

buildFreqTables <- function(cleaned.tokens) {
    tables <- list()
    for(n in seq(1,4)) {
        print(paste0("Counting ", n, "-grams..."))
        # named vector of ngram freqs
        table.n <- buildFreqTable(cleaned.tokens, n)
        #print(table.n)
        tables[[n]] <- table.n
    }
    return(tables)
}
