# -------------------------------------------------------------------------------------
# Adcase 2018

# Team members:
  # Ana Popova,           FN: 11142
  # Hristo Gospodinov,    FN: 11143
  # Nikolay Bojurin,      FN: 79141
  # Kamelia Lokmadzhieva, FN: 79138
  # Kamelia Kosekova,     FN: 79145
  # Izabella Taskova,     FN: 79139
  # Ivana Mihaylova,      FN: 79150

# -------------------------------------------------------------------------------------


rm(list=ls())
library(quanteda)
library(textmineR)
library(SnowballC)
library(tm)
library(wordnet)
library(tm)
library(wordcloud)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(igraph)
library(tidyr)
library(ggraph)
library(plyr)


# -------------------------------------------------------------------------------------
# Load the data
df <- read.csv("Data set task.csv", sep=";", dec=",")
df$Last.seen.date <- as.Date(df$Last.seen.date, format='%d.%m.%y')


# -------------------------------------------------------------------------------------
# Data exploration

# Explore the data and make initial transformations

# Look at some descriptive statistics
tmp1 <- do.call(data.frame, 
                list(mean = sapply(df, mean, na.rm = TRUE),
                     std = unlist(sapply(df, function(x) {if (is.numeric(x) == TRUE) sd(x, na.rm = TRUE) else NA})),
                     median = unlist(sapply(df, function(x) {if (is.numeric(x) == TRUE) median(x, na.rm = TRUE) else NA})),
                     min = unlist(sapply(df, function(x) {if (is.numeric(x) == TRUE) min(x, na.rm = TRUE) else NA})),
                     max = unlist(sapply(df, function(x) {if (is.numeric(x) == TRUE) max(x, na.rm = TRUE) else NA})),
                     class = sapply(df, class),
                     NAs = sapply(df, function(x) sum(length(which(is.na(x))))),
                     count = sapply(df, length)
                     
                ))
tmp1$percentNA <- with(tmp1, NAs / count)
tmp1$Varname <- rownames(tmp1)

# how many of the last global searches are 0
check <- df[df$Avg..Monthly.Searches.Global == 0,]

# taking only the searchterms plus the date and the domain
df_searchterms <- df[,c(1,2,5)]

# converting all searchterms to lowercase

df_searchterms$Search.Term <- as.character(df_searchterms$Search.Term)
df_searchterms$Search.Term <- tolower(str_trim(df_searchterms$Search.Term))


# -------------------------------------------------------------------------------------
# Initial look at transformed data
myStemMat <- dfm(df_searchterms$Search.Term, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)

quartz()
textplot_wordcloud(myStemMat, min_count = 25, random_order = FALSE,
                   rotation = .25,  min_size=1.2, max_size=4, 
                   color= RColorBrewer::brewer.pal(8,"Set3"))


topfeatures(myStemMat, 20)  # 20 top words
myStemMat[1:3,1:10]
test_df <- data.frame(df$Search.Term)
test_df$df.Search.Term[1]
test_df$df.Search.Term <- as.character(test_df$df.Search.Term)


# -------------------------------------------------------------------------------------
# Dictionary

dict <- read.csv("dictionary.csv")[2]
dict <- as.vector(dict$x)
dict1 <- read.csv("dictionary.csv")[2]

#### checking whether words are in a dictionary

#appending words to the dictionary that the dictionary lacks, based on freq > 10
ap_words <- c("ecommerce", "infusionsoft", "personalization", "personalized", "e-commerce", "optimization", "e-mail", "analytics", "omnichannel", "customize", "autoresponder", "localized", "lifecycle", "personalize", "behavioral", "responder")
dict <- c(dict, as.character(ap_words))

which(dict == "for") #22864
which(dict == "e") #9581 71385
which(dict == "vs") #293
which(dict == "and") #21456
which(dict == "in") #574 586 23205
dict[22864] <- "remove"
dict[9581] <- "remove"
dict[71385] <- "remove"
dict[293] <- "remove"
dict[21456] <- "remove"
dict[574] <- "remove"
dict[586] <- "remove"
dict[23205] <- "remove"

# Separate data into 2 columns: words in dictionary and words not in dictionary
words_in_dict <- data.frame(matrix(ncol = 1, nrow=nrow(test_df)))
words_not_in_dict <- data.frame(matrix(ncol = 1, nrow=nrow(test_df)))
test_df$df.Search.Term <- sapply(test_df$df.Search.Term, tolower)

empty_df <- matrix(, nrow = 5671, ncol = 1)

# Dictionary acrobatics:
  # check if every word is within the dict - if yes - save in 1 coumn, if not - save in another
  # goal: maintain original data structure
  # Execution with loops

# transformations
for (i in 1:nrow(test_df)) { 
  empty_df[i,1] <- paste(names(data.frame(dfm(test_df$df.Search.Term[i], remove = stopwords("english"),  remove_punct = TRUE, remove_numbers = TRUE)))[-1], collapse=" ")
}

# save in appropriate column
for (i in 1:nrow(test_df)) { 
  t <- unlist(strsplit(test_df[i,1], " "))
  yes <- list()
  no <- list()
  for (k in 1:length(t)) {
    
    if (t[k] %in% dict) {
      yes[k] <- t[k]
    } else {
      no[k] <- t[k]
    }
    if (length(yes) == 0) {
      words_in_dict[i,1] <- 0
    } else {
      words_in_dict[i,1] <- paste(sort(unlist(yes)), collapse= " ")
    }
    if (length(no) == 0) {
      words_not_in_dict[i,1] <- 0 
    } else {
      words_not_in_dict[i,1] <- paste(sort(unlist(no)), collapse= " ")
    }
    
    
  }
}


# -------------------------------------------------------------------------------------
# Additional transformations

# stem all terms

colnames(words_in_dict) <- "Search.Term"
empty_df = data.frame(matrix(ncol=1, nrow=nrow(words_in_dict)))

for (i in 1:nrow(words_in_dict)) { 
  empty_df[i,1] <- paste(names(data.frame(dfm(words_in_dict$Search.Term[i], stem = TRUE)))[-1], collapse=" ")
}

df_searchterms$search_edit <- empty_df$matrix.ncol...1..nrow...nrow.test_df..

colnames(empty_df) <- c("stemmed")


# -------------------------------------------------------------------------------------
# Clustering

# document term matrix, distance matrix - base clusters on this 

dtm <- CreateDtm(doc_vec = empty_df$stemmed,doc_names = empty_df$stemmed,ngram_window = c(1, 2), 
                 remove_punctuation = FALSE, remove_numbers=FALSE, stopword_vec = c())
tf_mat <- TermDocFreq(dtm)
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)
cdist <- as.dist(1 - csim)

# -----------------------------------------
# Hierarchical clustering - 5 clusters

hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 5)

quartz()
plot(hc, ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 5, border = "red")


p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary
# write.csv(cluster_summary, "cl_5_hcluct.csv")

hfit_cl <- as.data.frame(clustering)
# write.csv(hclust, "fit_cl_5_hclust.csv")


quartz()
wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "dark blue"),
                     main = "Top words in cluster 100")

# -----------------------------------------
# K-means algorithm, 5 clusters

# the code is commented out because it takes about an hour to run
# set.seed(123)
# wss <- 2:10
# for (i in 2:10) wss[i] <- sum(kmeans(cdist,centers=i,nstart=10)$withinss)
# 
# quartz()
# plot(2:10, wss[2:10], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#     # optimum number of clusters is 5;


kfit <- kmeans(cdist, 5, nstart=10)

kfit

kfit_cl <- as.data.frame(kfit$cluster)
# write.csv(kfit_cl, "kfit_cl_5_kmeans.csv")

kfit$size  
kfit$tot.withinss 
kfit$betweenss 
kfit$centers

cluster_words2 <- lapply(unique(kfit$cluster), function(x){
  rows <- dtm[ kfit$cluster == x , ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

cluster_summary2 <- data.frame(cluster2 = unique(kfit$cluster),
                              size = as.numeric(table(kfit$cluster)),
                              top_words = sapply(cluster_words2, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary2
# write.csv(cluster_summary2, "cl_5_kmeans.csv")


quartz()
wordcloud::wordcloud(words = names(cluster_words2[[ 5 ]]), 
                     freq = cluster_words2[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "dark blue"),
                     main = "Kmeans: Cluster 1")


colnames(hfit_cl)[1] <- "hfit_cl"
colnames(kfit_cl)[1] <- "kfit_cl"


# -------------------------------------------------------------------------------------
# Stick it all together

# Expand the original set
df <- cbind(df, kfit_cl)
df <- cbind(df, hfit_cl)
df <- cbind(df, empty_df)
df <- cbind(df, words_not_in_dict)

cols <- c("stemmed_terms", "not_in_dict")
colnames(df)[18:19] <- cols

# get the frequency of occurance of stemmed_terms
zzz <- as.data.frame(table(df$stemmed_terms))
colnames(zzz)[1] <- c("stemmed_terms")

# record the frequency in df
df <- left_join(df, zzz)


# =====================================================================================


# Bigrams

ngr <- tokens(empty_df$stemmed)
ngr1 <- tokens_ngrams(ngr,n=2, concatenator = " ")

df_bi = data.frame()

for (i in 1:length(ngr1)) {
  
  #print(paste("i is: ", i, collapse=" "))
  for (k in 1:length(ngr1[[i]])) {
    if(length(ngr1[[i]]) == 0) next
    #print(paste("k is",k,collapse=" "))
    #print(i)
    df_bi[i,k] = ngr1[[i]][k]
  }
}
#we have 5669 instead of 5671 because the last 2 rows dont have bigrams


df_bi$Costs = df$Estimated.Cost[1:5669]
df_bi$Clicks = df$Estimated.Clicks[1:5669]
df_bi$Searches = df$Avg..Monthly.Searches.Local[1:5669]
df_bi$Domain = df$Domain[1:5669]
df_bi$Clusters = kfit_cl[1:5669,1]
df_bi$HClusters = clustering[1:5669]

df_bi$stemmed_terms = empty_df$stemmed[1:5669]
df_bi <- left_join(df_bi, zzz)

#filtering

df_filtered = df_bi[df_bi$Searches > 0,]
df_filtered = df_filtered[df_filtered$Clicks > 0,]
df_filtered = df_filtered[df_filtered$Freq > 1,]

df_filtered_1 = df_filtered[, colSums(is.na(df_filtered)) != nrow(df_filtered)]

df_large = data.frame(df_filtered_1$V1, df_filtered_1$Costs, df_filtered_1$Clicks, df_filtered_1$Searches, df_filtered_1$Domain, df_filtered_1$Clusters, df_filtered_1$HClusters, df_filtered_1$stemmed_terms)
df_large = rbind(df_large, data.frame(df_filtered_1.V1 = df_filtered_1$V2, df_filtered_1$Clicks,
                                      df_filtered_1$Costs, df_filtered_1$Searches, 
                                      df_filtered_1$Domain, df_filtered_1$Clusters, 
                                      df_filtered_1$HClusters, df_filtered_1$stemmed_terms))
df_large = rbind(df_large, data.frame(df_filtered_1.V1 = df_filtered_1$V3, df_filtered_1$Costs, df_filtered_1$Clicks,df_filtered_1$Searches, df_filtered_1$Domain, df_filtered_1$Clusters, df_filtered_1$HClusters, df_filtered_1$stemmed_terms))

df_large = na.omit(df_large)

df_large_summary = ddply(df_large,~df_filtered_1.V1, summarise, ACG_Cost=mean(df_filtered_1.Costs), AVG_Clicks=mean(df_filtered_1.Clicks), AVG_Search = mean(df_filtered_1.Searches))
#df_large_summary = ddply(df_large,~df_filtered_1.V1, summarise, ACG_Cost=sum(df_filtered_1.Costs), AVG_Clicks=sum(df_filtered_1.Clicks), AVG_Search = sum(df_filtered_1.Searches))
df_large_summary <- left_join(df_large_summary, clusters_per_one)
freq_DF = data.frame(table(df_large$df_filtered_1.V1))
colnames(freq_DF) <- c("df_filtered_1.V1", "freq")
df_large_summary <- left_join(df_large_summary, freq_DF)
#df_large_summary <- left_join(df_large_summary, df_large[,c(1,7)])
df_large_summary$ACPC <- df_large_summary$ACG_Cost / df_large_summary$AVG_Clicks


df_large_summary <- df_large_summary[df_large_summary$freq > 3, ]
#plot(df_large_summary$ACPC, df_large_summary$AVG_Search)
#with(LifeCycleSavings[1:9,], text(sr~dpi, labels = row.names(LifeCycleSavings[1:9,]), pos = 4))

ggplot(df_large_summary, aes(x=ACG_Cost,y=AVG_Clicks))  + geom_text(label=df_large_summary$df_filtered_1.V1, color=df_large_summary$Cluster, size=5) + scale_fill_brewer()#+ scale_fill_manual(values=c("red", "blue", "green", "black", "yellow"))

#write.csv(df_bi, file='df_bi5.csv')



# -------------------------------------------------------------------------------------
# More bigrams - another approach

# get all bigrams
# note the change in row number
df1 <- df %>%
  unnest_tokens(bigram, stemmed_terms, token="ngrams", n=2, drop=FALSE)

# work with stemmed_terms and bigrams only for simplicity
df2 <- df1[,c(20, 21)]

# optional ?? 
# separate the bigrams into underlying words
bigrams_separated <- df2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# get rid of stop words (again :D)
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)      # from 11949 to 10804 rows

# here you may need to run: rm(count) 

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts

# reunite the separated bigrams 
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# get tf-idf for bigrams and present it in a sorted way
bigram_tf_idf <- bigrams_united %>%
  count(stemmed_terms, bigram) %>%
  bind_tf_idf(bigram, stemmed_terms, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

# graph - here we filter for only relatively common combinations ">20"
bigram_counts  # original count
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

# pretty graph below
quartz()
set.seed(2018)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, color="white"), show.legend = FALSE,
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dark red", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color="white") +
  theme_void() #


write.csv(df, "df.csv")

