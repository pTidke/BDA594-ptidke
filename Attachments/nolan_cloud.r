library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(NLP)

setwd("Documents/coursework/BDA594/exercise2/wordcloud/")
my_corpus = Corpus(DirSource("./nolan/"))

tdm = TermDocumentMatrix(my_corpus,
control = list(removePunctuation = TRUE,
stopwords = c("the", "youre", "dont", "can", "will", "you", "and", "your", "that", "this", "what", "for", "not", "have", "was", "but", stopwords("english")),
removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

wordcloud(dm$word, dm$freq, , min.freq = 1, random.order=FALSE, max.words=250, rot.per=0,
colors=brewer.pal(8, "Dark2"), scale = c(3, 0.5))

png("nolan_scripts.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, , min.freq = 1, random.order=FALSE, max.words=250, rot.per=0,
colors=brewer.pal(8, "Dark2"), scale = c(3, 0.5))