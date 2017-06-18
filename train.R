source("util.R")
source("import_clean.R")
source("kneser_ney.R")

print("Reading text...")

# lines <- c(
#     "Jane went to school and Bob stayed home. Jane went to the store and Bob stayed at work",
#     "Then Jane stayed and Bob went? The store was packed today.",
#     "The president said he would go to Israel. The president said he would appoint a special prosecutor.",
#     "I'm feeling good today",
#     "feeling good",
#     "feeling bad about this situation",
#     "the broncs played well today"
# )
# test.lines <- c(
#     "Jane went to work and Bob stayed home. Jane went to school and Bob went to the store",
#     "The president said the situation was good.",
#     "I'm feeling bad about this",
#     "the broncs played bad today."
# )

lines <- c(
    readLines('data/final/en_US_sample/train_en_US.blogs.txt'),
    readLines('data/final/en_US_sample/train_en_US.news.txt'),
    readLines('data/final/en_US_sample/train_en_US.twitter.txt')
)
test.lines <- c(
    readLines('data/final/en_US_sample/test_en_US.blogs.txt'),
    readLines('data/final/en_US_sample/test_en_US.news.txt'),
    readLines('data/final/en_US_sample/test_en_US.twitter.txt')
)

bannedWords <- unique(tolower(readLines('data/swearWords.txt'))) # wwww.bannedwordlist.com
all.tokens <- getTokens(lines)
vocabulary <- determineVocabulary(all.tokens, bannedWords, max.size = 20000)
cleantoks <- cleanTokens(all.tokens, vocabulary)
freqTables <- buildFreqTables(cleantoks)
print("Building probability tables...")
probTables <- getKNProbTables(freqTables)

print("Writing probability tables to file...")
for (i in seq(1, length(probTables))) {
    fwrite(probTables[[i]], paste0("probtables", i, ".bin"))
}

print("Loading test set for evaluation...")
# build frequency tables from test set
test.cleantoks <- cleanTokens(getTokens(test.lines), vocabulary)
test.4grams <- buildFreqTable(test.cleantoks, n=4)

print("Scoring top-3 accuracy on test set 4-grams...")
score.func <- function(r) {
    preds <- predictWordKN(paste(strsplit(r$history, '_')[[1]], collapse=" "), vocab=vocabulary, probTables=probTables, cleanTokens=FALSE)
    preds <- preds[1:min(3, nrow(preds)),]
    if (nrow(preds[preds$word==r$word,]) > 0) {
        return(r$freq)
    } else {
        return(0)
    }
}
test.sample.size <- 1000
set.seed(42)
test.sample <- test.4grams[runif(nrow(test.4grams)) <= test.sample.size/nrow(test.4grams), ]
print("Getting scores")
scores <- adply(test.sample, score.func, .margins=1, .id=NULL, .expand=FALSE)$V1
accuracy <- sum(scores)/sum(test.sample$freq)
print(paste("Accuracy:", accuracy))

print("Scoring model perplexity on test set...")
# Perplexity = 2^((-1/N)*sum(log(prob(x), 2))) for a test set of ngrams x, length N.
# We modify it slightly to use frequencies instead of just N, to save calculating
# the same probabilities again and again for the same ngram.
getNgramProb <- function(freqtbl.row, ptbls) {
    h <- freqtbl.row$history
    w <- freqtbl.row$word
    f <- freqtbl.row$freq
    n <- length(strsplit(h, '_')[[1]]) + 1
    for (i in seq(n, 1)) {
        if (i == 1) {
            return(ptbls[[i]][word==w]$p * f)
        } else {
            row <- ptbls[[i]][(history==h) & (word==w)]
            if (nrow(row) == 1) {
                return(row$p * f)
            } else {
                h <- truncateNgram(h)
            }
        }
    }
}
model.probs <- adply(test.sample, getNgramProb, ptbls=probTables, .margins=1, .expand=FALSE, .id=NULL)$V1
perplexity <- 2 ^ ( (-1/sum(test.sample$freq)) * sum(log(model.probs, 2)) )
print(paste("PERPLEXITY:", perplexity))
