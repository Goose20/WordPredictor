library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(quanteda)
library(data.table)
library(ggplot2)
library(ngram)


set.seed(0)
# load data

blogs <- readLines("data/en_US.blogs.txt",encoding = "UTF-8", skipNul = TRUE)
news <- readLines("data/en_US.news.txt",encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("data/en_US.twitter.txt",encoding = "UTF-8", skipNul = TRUE)

#get sample
sSize <- 0.01
blogs <- sample(blogs,round(sSize*length(blogs)))
news <- sample(news,round(sSize*length(news)))
twitter <- sample(twitter,round(sSize*length(twitter)))

# make lower case
blogs <- tolower(blogs)
news <- tolower(news)
twitter <- tolower(twitter)

# remove punctuation
blogs <- removePunctuation(blogs)
news <- removePunctuation(news)
twitter <- removePunctuation(twitter)

# remove numbers
blogs <- removeNumbers(blogs)
news <- removeNumbers(news)
twitter <- removeNumbers(twitter)

blogs <- iconv(blogs,"latin1","ASCII",sub = "")
news <- iconv(news,"latin1","ASCII",sub = "")
twitter <- iconv(twitter,"latin1","ASCII",sub = "")

tData <- VCorpus(VectorSource(rbind(blogs, news, twitter)))
rm(news, blogs, twitter)

#unigram
uniGram <- TermDocumentMatrix(tData, control = list(wordLengths = c(1, 25)))
uniGram <- sort(slam::row_sums(uniGram), decreasing = TRUE)
unifreq <- data.table(tok = names(uniGram), freq = uniGram)
saveRDS(unifreq, "data/unigram.RDS")

# biGram
biTokenizer <- function(x) { unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE) }
biGram <- TermDocumentMatrix(tData, control = list(wordLengths = c(2, 50),tokenize = biTokenizer))
biGram <- sort(slam::row_sums(biGram), decreasing = TRUE)
bifreq <- data.table(tok = word(names(biGram),1), pred = word(names(biGram),2), freq = biGram) 
saveRDS(bifreq, "data/bigram.RDS")

# triGram
triTokenizer <- function(x) { unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE) }
triGram <- TermDocumentMatrix(tData, control = list(wordLengths = c(3, 75),tokenize = triTokenizer))
triGram <- sort(slam::row_sums(triGram), decreasing = TRUE)
triFreq <- data.table(tok = word(names(triGram),1,2), pred = word(names(triGram),3), freq = triGram)
saveRDS(triFreq, "data/trigram.RDS")

# quadGram
quadTokenizer <- function(x) { unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE) }
quadGram <- TermDocumentMatrix(tData, control = list(wordLengths = c(4, 100),tokenize = quadTokenizer))
quadGram <- sort(slam::row_sums(quadGram), decreasing = TRUE)
quadFreq <- data.table(tok = word(names(quadGram),1,3), pred = word(names(quadGram),4), freq = quadGram)
saveRDS(quadFreq, "data/quadgram.RDS")



