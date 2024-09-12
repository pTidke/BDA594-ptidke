library("tm")
library("wordcloud")
library("RColorBrewer")

text <- readLines("./Documents/coursework/BDA594/exercise2/wordcloud/job_companies/job_companies.txt")
docs <- Corpus(VectorSource(text))
my_data <- data.frame(text = text, freq = 1, stringsAsFactors = FALSE)
View(my_data)
my_agr <- aggregate(freq ~ ., data = my_data, sum)
View(my_agr)
wordcloud(words = my_agr$text, freq = my_agr$freq, min.freq = 1,
max.words=50, random.order=FALSE, rot.per=0,
colors=brewer.pal(8, "Dark2"), scale = c(5, 0.5))