setwd('~/Documents/Travail/Reading/Robidoux/WordCloud/')

if(!require(wordcloud)) install.packages("wordcloud")
if(!require(SnowballC)) install.packages("SnowballC")
if(!require(tm)) install.packages("tm")

text = readLines(file.choose())
docs = Corpus(VectorSource(text))

wc = tm_map(docs, PlainTextDocument)
wc = tm_map(wc, removePunctuation)
wc = tm_map(wc, tolower)
#wc = tm_map(wc, stemDocument)
wc = tm_map(wc, removeNumbers)
wc = tm_map(wc, removeWords, c(stopwords('english'),
                               "table", "this", "use", "may", "also", "two", "doi", "besner", "see",
                               "australia", "australian", "austria", "author", "authorized","authors",
                               "authorship", "\u2b0d", "\u2b0e", "ziegler", "can", "low", "using", "used",
                               "whether", "first","journal", "main", "factors", "thus", "three", 
                               "coltheart", "found", "one", "given", "however", "mse", "new", "present",
                               "presented", "presents","psychology", "reported", "research", "university",
                               "stolz", "figure", "either", "robidoux", "item", "within", "perry",
                               "omalley", "als", "macquarie", "second", "psychological"))

tdm <- TermDocumentMatrix(wc)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d[d$freq>10 & d$word != "\u2b0d",]

wordcloud(d$word, d$freq, scale=c(5,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "YlOrRd"))

write.csv(d, file="wordcloud.csv")
