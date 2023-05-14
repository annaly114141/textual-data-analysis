###################### EDA ##################################

library(tm) 
library(SnowballC)
library(dplyr)

#Read file
corpus <- VCorpus(DirSource('./docs'))

#Build Corpus
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1]) #inspect first 1 col

#Create term document matrix
tdm <- TermDocumentMatrix(corpus,
                          control = list(minWordLength=c(1,Inf))) #terms: 1143 in 52 documents 
t <- removeSparseTerms(tdm, sparse = 0.98)
m <- as.matrix(t)

#Check details of documents
print(corpus)
class(corpus)
#Examine contents
corpus[1]
class(corpus[1])

class(corpus[[1]])
corpus[[1]]$meta
corpus[[1]]$content

# Clean text
corpus <- tm_map(corpus, tolower)
corpus[[1]]$content

corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

corpus <- tm_map(corpus, removeNumbers)
corpus[[1]]$content

corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus[[1]]$content

corpus <- tm_map(corpus, removeWords, c('and', 'the', 'just', 'thats'))
#corpus <- tm_map(cleanset, gsub, pattern = 'thats', replacement = 'that')

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1])

tdm <- TermDocumentMatrix(corpus) #documents: 3, sparsity 67% 
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:1] #first 10 words, 1 column


#Bar plot
w <- rowSums(tdm)
w<- subset(w,w>= 10) #words frquency > 10
barplot(sort(w, decreasing = TRUE),
        las = 2,
        col = rainbow(50))


# Word Cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) #bigger words are more frequent
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 100,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3))

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
head(w)
wordcloud2(w,
           size = 0.5,
           shape = 'circle',
           rotateRatio = 0.5,
           minSize=1)

#letterCloud(w, word = "A", size = 2)


########## Sentiment Analysis ###########################################################################################
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#Read file
corpus <- VCorpus(DirSource('./docs'))
corpus <- tm_map(corpus, removeNumbers)
corpus <- iconv(corpus, to = 'utf-8-mac') #convert to character vector


#Obtain Sentiment scores
#the entry shows positive sentiment, a range of emotions can be found throughout the journal
s <- get_nrc_sentiment(corpus) #one column
get_nrc_sentiment('tough') #score of 1 for sadness and 1 negative. The score comes from sadness and negative
get_nrc_sentiment('sick') #disgust, sadness and negative in the entry

#Barplot
barplot(sort(colSums(s), decreasing = TRUE),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Diary Entry')


###### Unigrams, Bigrams and n-grams ############################################
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(forcats)

#Word Tokenisation
corpus1 <- data.frame(txt = corpus, stringAsFactors = FALSE)

corpus1 %>%
  unnest_tokens(output = word, input = txt) %>%
  head()

corpus1 %>% 
  unnest_tokens(output = word, input = txt) %>% 
  count(word, sort = TRUE) 

#stopword removal
corpus1 %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

#unigram visualisation
corpus1 %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  mutate(word = fct_reorder(word, -n)) %>%
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  labs(title = "Top unigrams of Diary")

#Bigrams and N-grams
corpus1 %>%
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  mutate(word = fct_reorder(word, n)) %>%
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams of Diary")

#Using bigrams to provide context in sentimental analysis
bigrams_separated <- corpus1 %>%
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ")

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
 
#Visualising a Network of Bigrams with ggraph
library(igraph)

bigram_counts <- corpus1 %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) 

#filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

bigram_graph
 
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



### Text Clustering in R ##########################################################################
library(tm)
data <- readLines('/Users/anna/Desktop/AT3\ MLA/AT3\ MLA\ Project/docs/Diary\ of\ a\ trauma\ surgeon.txt')

#Build Corpus
corpus <- Corpus(VectorSource(data))

# Clean text
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, removeWords, c('and', 'the'))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c('and', 'the', 'just', 'thats'))

#Create term document matrix
tdm <- TermDocumentMatrix(corpus,
                          control = list(minWordLength=c(1,Inf))) 
t <- removeSparseTerms(tdm, sparse = 0.98)
m <- as.matrix(t)

#Plot frequent terms
freq <- rowSums(m)
freq <- subset(freq, freq >= 10)
barplot(freq, las=2, col = rainbow(50))

#Hierarchical words clustering using dendrogram
#higher distance means the two words should not be in the same cluster, if it is low then it should be in the same cluster
distance <- dist(scale(m))
print(distance, digits = 2) 
hc <- hclust(distance, method = "ward.D")
plot(hc, hang = -1)
rect.hclust(hc, k=10) #group outcome into 10 clusters

#Non-Hierarchical k-means clustering of words
#sum of squares:we want cluster values/variability to be low, and the total_SS to be high: meaning elements are closer to each-other, and cluster distance is hgiher
# k = 10: cluster-to-cluster distance/variability (total_SS): 32.3%
#k = 3, 19.8%
#the percentage has not go down
m1 <- t(m) #create transpose of m matrix, rows become col & col become rows
set.seed(222)
k <- 10 #10 clusters
kc <- kmeans(m1, k)
kc  #K-means clustering with 10 clusters

#Determine the number of clusters
#Elbow method for given number of clusters, we can calculate variance in the data explained by the clustering. 
#this is typicaaly increase with more clusters
k <- 10
varper <- NULL
for (i in 1:k) {
  kfit <- kmeans(m , i)
  varper <- c(varper, kfit$betweenss/kfit$totss)
}
varper
plot(1:k, varper, xlab = "# of clusters", ylab = "explained variance")


######## Kmeans Method 2  ###################################################################################################################
#compute distance between document vectors
distance <- dist(scale(m))

#kmeans - run with nstart=100 and k= 5, 10, 15 to compare results with hclust
kfit <- kmeans(distance, 10, nstart=100)

#Load library
library(cluster)
clusplot(as.matrix(distance), kfit$cluster, color=T, shade=T, labels=2, lines=0)
 
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster

#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss

#Elbow  approach to find optimum number of clusters
wss <- 2:(length(corpus)-1)
for (i in 2:(length(corpus)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(corpus)-1), wss[2:(length(corpus)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#rerun using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

kfit <- kmeans(cd, 2, nstart=100)

