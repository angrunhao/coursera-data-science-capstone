library(plyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tm)
library(tokenizers)
# DATA INPUT

conB <- file("data\\en_US\\en_US.blogs.txt", "r")
conN <- file("data\\en_US\\en_US.news.txt", "r")
conT <- file("data\\en_US\\en_US.twitter.txt", "r")
dataB <- readLines(conB, skipNul = TRUE)
dataN <- readLines(conN, skipNul = TRUE)
dataT <- readLines(conT, skipNul = TRUE)
Encoding(dataB) <- "UTF-8"
Encoding(dataN) <- "UTF-8"
Encoding(dataT) <- "UTF-8"
dataAll <- list(blog = dataB, news = dataN, twitter = dataT)


close(conB)
close(conN)
close(conT) ## It's important to close the connection when you are done

# DATA SUMMARY 
dataSummary <- data.frame(type = c("blog", "news", "twitter"))
dataSummary$length <- sapply(dataAll, length)

wordCounter <- function(textList) {
  sum(str_count(textList, "[\\w\']+"))
}
dataSummary$wordFreq <- sapply(dataAll, wordCounter)

ggplot(dataSummary, aes(x = type, y = length)) + geom_bar(stat = "identity") + labs(title = "No. of Entries")
ggplot(dataSummary, aes(x = type, y = wordFreq)) + geom_bar(stat = "identity") + labs(title = "No. of Words")

# TASK 0

#sum(str_detect(dataT, "love")) / sum(str_detect(dataT, "hate"))
#subset(dataT, str_detect(dataT, "biostats"))
#sum(str_detect(dataT, "A computer once beat me at chess, but it was no match for me at kickboxing"))

# DATA EXPLORATION

## Data Sampling
dataSample <- list(blog = NA, news = NA, twitter = NA)
dataSampleSummary <- data.frame(type = c("blog", "news", "twitter"))

set.seed(123)
binomial <- function(dataList) {
  rbinom(dataList, 1, .05)
}
dataToSample <- lapply(dataAll, binomial)
for (i in 1:3) {
  dataSample[[i]] <- subset(dataAll[[i]],dataToSample[[i]] == 1)
  dataSample[[i]] <- gsub('[^[:alnum:][:space:]]+','',dataSample[[i]])
}

dataSampleSummary$length <- sapply(dataSample, length)
dataSampleSummary$wordFreq <- sapply(dataSample, wordCounter)

## Corpus creation and content regex
corpus <- VCorpus(VectorSource(dataSample))
rm(dataSample)
rm(dataB)
rm(dataN)
rm(dataT)

## Single Word exploration
singleWordExploration <- function(corpus, name) {
  tdm <- TermDocumentMatrix(corpus[name], control = list(wordLengths = c(3,Inf),
                                                         removePunctuation = TRUE,
                                                         stopwords = TRUE))
  freq <- data.frame(word = tdm$dimnames$Terms, frequency = tdm$v)
  freq <- arrange(freq, -frequency)
  freq$word = with(freq, reorder(word, frequency))
  ggplot(freq[1:20,], aes(x = word, y = frequency)) + geom_bar(stat = "identity") + coord_flip() +
    labs(title = paste("Top 25 non-stopwords in", name, "corpus", sep = " "))
}
singleWordExploration(corpus = corpus, name = "blog")
singleWordExploration(corpus = corpus, name = "news")
singleWordExploration(corpus = corpus, name = "twitter")

# n-grams creation
tokenizing <- function(corpus, name, nOrder) {
  tokenizer <- function(text) {
    unlist(lapply(ngrams(words(text), nOrder), paste, collapse = " "), use.names = FALSE)
  }
  control_list_ngram = list(tokenize = tokenizer)
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  
  ngram <- TermDocumentMatrix(corpus[name], control = control_list_ngram)
  ngram <- data.frame(word = ngram$dimnames$Terms, frequency = ngram$v)
  
  return(ngram)
}

Nbigram <- tokenizing(corpus, "news", 2)
write.csv(Nbigram, "data\\Nbigram.csv")
Ntrigram <- tokenizing(corpus, "news", 3)
write.csv(Ntrigram, "data\\Ntrigram.csv")
Nquadgram <- tokenizing(corpus, "news", 4)
write.csv(Nquadgram, "data\\Nquadgram.csv")

Bbigram <- tokenizing(corpus, "blog", 2)
Btrigram <- tokenizing(corpus, "blog", 3)
Bquadgram <- tokenizing(corpus, "blog", 4)

Tbigram <- tokenizing(corpus, "news", 2)
Ttrigram <- tokenizing(corpus, "news", 3)
Tquadgram <- tokenizing(corpus, "news", 4)

predictor <- function(input, bigram, trigram) {
  returnLastWord <- function(stringText) {
    wordList <- strsplit(as.String(stringText), " ")[[1]]
    wordList[length(wordList)]
  }
  
  searcher <- function(before, ngram) {
    after <- subset(ngram, str_detect(ngram$word, fixed(paste(before, "", sep = " "))))
    after <- arrange(after, -frequency)[1:3,]
    sapply(after$word, returnLastWord)
  }
  input <- tolower(input)
  inputSplit <- strsplit(as.String(input), " ")[[1]]
  
  if (length(inputSplit) == 1) {
    result <- searcher(inputSplit[length(inputSplit)], bigram)
    
  } else {
    result <- searcher(paste(inputSplit[length(inputSplit)-1], 
                             inputSplit[length(inputSplit)], sep = " "), trigram)
    if (result[1] == "NA") {
      result <- searcher(inputSplit[length(inputSplit)], bigram)
    }
    
  }
  result
}

bigram <- read.csv("data\\Nbigram.csv")
trigram <- read.csv("data\\Ntrigram.csv")
# quadgram <- read.csv("data\\Nquadgram.csv")
result <- predictor(input = "has been", bigram = bigram, trigram = trigram)
