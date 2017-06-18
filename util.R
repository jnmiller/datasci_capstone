historyFromNgram <- function(ngram) {
    splt <- strsplit(ngram, '_')
    n <- length(splt[[1]])
    if (n == 1) {
        return(rep("", length(ngram)))
    } else {
        return(sapply(splt, function(x) paste(x[1:n-1], collapse='_')))
    }
}

lastWordFromNgram <- function(ngram) {
    splt <- strsplit(ngram, '_')
    n <- length(splt[[1]])
    if (n == 1) {
        return(ngram)
    } else {
        return(sapply(splt, function(x) x[n:n]))
    }
}

getTokens <- function(lines) {
    lines <- gsub("[_\\^~`]", "", lines)
    return(tokens_tolower(tokens(lines, what="fasterword", remove_punct=TRUE, remove_hyphen=TRUE, remove_url=TRUE)))
}

cleanTokens <- function(all.tokens, vocab) {
    # unique words
    print("preparing tokens...")
    terms <- names(colSums(dfm(unlist(sapply(all.tokens, unlist)))))
    oov <- terms[which(is.na(vocab[terms]))]
    dict <- dictionary(
        oov=oov,
        you="u"
    )
    print("applying dictionary...")
    cleaned.tokens <- applyDictionary(all.tokens, dictionary=dict, exclusive=FALSE, capkeys=FALSE)
    return(cleaned.tokens)
}

