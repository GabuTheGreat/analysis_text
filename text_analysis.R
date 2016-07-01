#This file comtains codes to mine all the text documents
#load libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)


#Create the file path and see the content of the folder
my_filePath <- file.path("C:/Users/Mutua/Documents/texts") 
my_filePath
dir(my_filePath)

#load the text
docs <- Corpus(DirSource(my_filePath))
#checking the summary
summary(docs)

#preprocessing the data
#Removing Punctuation
docs <- tm_map(docs, removePunctuation)  

#Removing special characters
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}

#Removing numbers
docs <- tm_map(docs, removeNumbers) 

#Converting to lowercase
docs <- tm_map(docs, tolower)

#Removing "stopwords" (common words) that usually have no analytic value
docs <- tm_map(docs, removeWords, stopwords("english"))

#Removing particular words
docs <- tm_map(docs, removeWords, c("http", "tco"))

#Removing common word endings
docs <- tm_map(docs, stemDocument)

#Stripping unnecesary whitespace from your documents
docs <- tm_map(docs, stripWhitespace)

#Saving as a text document
docs <- tm_map(docs, PlainTextDocument)   

#create a document term matrix
dtm <- DocumentTermMatrix(docs)

#Organize terms by their frequency
freq <- colSums(as.matrix(dtm))
#check the length
length(freq)
#order the length
ord <- order(freq)

#Removing sparse terms
dtms <- removeSparseTerms(dtm, 0.1)
#inspect the result
inspect(dtms)

#check the most frequent words
#getting funny characters
freq[head(ord)]

#check the least frequent words
freq[tail(ord)]

#check frequencies of frequency - top 20
head(table(freq), 20)


#check frequencies of least frequent words - bottom 20
tail(table(freq), 20)

#all terms that appear frequently
x = findFreqTerms(dtm, lowfreq=900)

#another way
wf <- data.frame(word=names(freq), freq=freq)

#plot all the words that appear more than 2700
#plot word frequencies

library(ggplot2) 
p <- ggplot(subset(wf, freq>2700), aes(word, freq))
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


#Relationships Between Terms
findAssocs(dtm, c("eastleigh" , "somalis"), corlimit=0.98)

#Plotting wordcloud - Hundred reapeated words
set.seed(142) 
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

#clustering per term similarity
dtmss <- removeSparseTerms(dtm, 0.01)

#calculate distance between words & then cluster them according to similarity
library(cluster)  
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")
fit

#plotting the results
plot(fit, hang=-1)
