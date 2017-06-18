require(data.table)
require(plyr)
require(quanteda)
source("util.R")

ngramsCompletedBy <- function(term, freqTable) {
    return(nrow(freqTable[word==term]))
}

knUnigramProbTable <- function(unigramFreqs, bigramFreqs) {
    kn.counts <- sapply(unigramFreqs$word, ngramsCompletedBy, freqTable=bigramFreqs)
    return(
        data.table(
            history=rep("", length(kn.counts)),
            word=names(kn.counts),
            p=kn.counts/sum(kn.counts)
        )
    )
}

getDiscount <- function(freqTable) {
    n1 <- nrow(freqTable[freq==1])
    n2 <- nrow(freqTable[freq==2])
    return(n1/(n1 + 2*n2))
}

truncateNgram <- function(ngram) {
    s <- strsplit(ngram, '_')[[1]]
    if (length(s) == 1) {
        return("")
    } else {
        return(paste(s[2:length(s)], collapse='_'))
    }
}

kneserNeyProbability <- function(w, h, freqTables, unigramProbTable, n=NULL) {
    # What is the freq of the ngram (history, word)?
    
    # if n is unspecified, assume we are starting at the highest n; (e.g. 4 if 
    # the highest-order ngram model is 4-grams).
    if (is.null(n)) { n <- length(strsplit(h, '_')[[1]]) + 1 }
    if (n > length(freqTables)) { stop("Input ngram too large") }
    
    # If we are down to unigrams, return Kneser-Ney unigram prob.
    if (n == 1) {
        if (h != "") { stop("Error! At unigram but history isn't blank!") }
        p <- unigramProbTable[word==w]$p
        if(length(p) == 0) {
            stop(paste0("The word ", w, " is not in vocab"))
        } else {
            return(p)
        }
    }
    
    # Otherwise, we'll be recursively calculating the interpolation of all 
    # orders of ngram models.
    exactNgramFreq <- freqTables[[n]][(word==w) & (history==h)]$freq
    if(length(exactNgramFreq) == 0) {
        exactNgramFreq <- 0
    }
    discount <- getDiscount(freqTables[[n]])
    sumSharedHistoryFreq <- sum(freqTables[[n]][history==h]$freq)
    p1 <- max(exactNgramFreq - discount, 0)/sumSharedHistoryFreq
    lambda <- (discount/sumSharedHistoryFreq) * nrow(freqTables[[n]][history==h])
    smallerHistory <- truncateNgram(h)
    
    print(paste0("Calculating p(",w,"|",h,"); p1=",p1,", l=", lambda))
    return(
        p1 +
        lambda * kneserNeyProbability(w, smallerHistory, freqTables, unigramProbTable, n-1)
    )
}

knProb <- function(freqTable, lowerOrderProbTable) {
    # Given a frequency table of the current ngram order,
    # and a probability table of the lower order model,
    # return the current order probability table.
    freqs <- freqTable$freq
    discount <- getDiscount(freqTable)
    histFreqs <- ddply(freqTable, .(history), summarize, histFreq=sum(freq), histCt=length(freq))
    histStats <- join(freqTable, histFreqs, by="history")
    sameHistoryFreq <- histStats[,histFreq]
    sameHistoryCount <- histStats[,histCt]
    
    probs <- sapply(freqs - discount, function(x) max(x, 0))/sameHistoryFreq
    lambdas <- (discount/sameHistoryFreq) * sameHistoryCount
    tmp <- data.table(
        history = sapply(freqTable$history, truncateNgram, USE.NAMES = FALSE),
        word = freqTable$word
    )
    lowerProbsTbl <- join(tmp, lowerOrderProbTable, by=c("history","word"), type="left")
    errs <- lowerProbsTbl[is.na(lowerProbsTbl$p),]
    if( nrow(errs) > 0 ) {
        print(errs)
        stop("Lower-order probability vector has join mismatch with current order ngram table!")
    }
    
    lowerProbs <- lowerProbsTbl$p
    
    if( !(length(probs) == length(lambdas) && length(lambdas) == length(lowerProbs)) ) {
        print(paste("|probs| =", length(probs)))
        print(paste("|lambdas| =", length(lambdas)))
        print(paste("|lowerProbs| =", length(lowerProbs)))
        print(errs)
        stop("Probability vectors from current and lower-order ngrams are not the same lengths.")
    }
    
    return( data.table(
        history = freqTable$history,
        word = freqTable$word,
        freq = freqs,
        p = probs + (lambdas * lowerProbs)
    ))
}

getKNProbTables <- function(freqTables) {
    # freqTables - list of frequency tables, table N in list is N-grams
    p1 <- knUnigramProbTable(freqTables[[1]], freqTables[[2]])
    p2 <- knProb(freqTables[[2]], lowerOrderProbTable = p1)
    p3 <- knProb(freqTables[[3]], lowerOrderProbTable = p2)
    p4 <- knProb(freqTables[[4]], lowerOrderProbTable = p3)
    setkey(p1, word)
    setkey(p2, history)
    setkey(p3, history)
    setkey(p4, history)
    return(list(p1, p2, p3, p4))
}

predictWordKN <- function(text, vocab, probTables, cleanTokens=TRUE) {
    model.n <- 4
    cleantoks <- getTokens(text)
    if(cleanTokens) {
        cleantoks <- cleanTokens(cleantoks, vocab) 
    }
    toklen <- length(cleantoks[[1]])
    ngrams <- tokens_ngrams(cleantoks, n=min(model.n-1, toklen))[[1]]
    ngram <- ngrams[length(ngrams)]
    h <- ngram
    res <- data.table()
    for(n in seq(min(toklen+1,model.n), 2)) {
        res.n <- probTables[[n]][(history==h) & (word != "oov") & (p >= 0.00001)][order(p, decreasing=TRUE), c("word", "p")]
        res <- rbind(res, res.n)
        h <- truncateNgram(h)
        if(nrow(res) >= 5) {
            break
        }
    }
    if (nrow(res) == 0) {
        res <- rbind(res, probTables[[1]][order(p, decreasing=TRUE),][word != "oov", c("word", "p")][1:5])
    }
    res <- ddply(res, .(word), summarize, p=max(p))
    setorderv(res, cols=c("p"), order=-1)
    return(res[1:min(5, nrow(res)),])
}
