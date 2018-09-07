rm(list=ls())
library(quanteda)

setwd("/Users/hristogospodinov/Downloads/Sofia University/Analytical /adcase")
df = read.csv("dataset.csv", sep=';', dec=',')
df$Last.seen.date <- as.Date(df$Last.seen.date, format='%d.%m.%y')


#how many of the last global searches are 0
check <- df[df$Avg..Monthly.Searches.Global == 0,]

#taking only the searchterms plus the date and the domain
df_searchterms <- df[,c(1,2,5)]

#converting all searchterms to lowercase

df_searchterms$Search.Term <- tolower(df_searchterms$Search.Term)

myStemMat <- dfm(df_searchterms$Search.Term, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

textplot_wordcloud(myStemMat, min_count = 30, random_order = FALSE,
                   rotation = .25, 
                   color= RColorBrewer::brewer.pal(8,"Dark2"))

topfeatures(myStemMat, 20)  # 20 top words
myStemMat[1:3,1:10]
test_df <- data.frame(df$Search.Term)
test_df$df.Search.Term[1]
test_df$df.Search.Term <- as.character(test_df$df.Search.Term)

test <- apply(test_df, 1, function(x) names(data.frame(dfm())))
dict <- read.csv("dictionary.csv")[2]
dict <- as.vector(dict$x)
empty_df = data.frame(matrix(ncol=1, nrow=nrow(test_df)))


for (i in 1:nrow(test_df)) { 
  empty_df[i,1] <- paste(names(data.frame(dfm(test_df$df.Search.Term[i], remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)))[-1], collapse=" ")
}




df_searchterms$search_edit <- empty_df$matrix.ncol...1..nrow...nrow.test_df..


### dictionaries

# load dictionary
load(system.file("words", "english.RData", package = "SnowballC")) # voc

# load another dictionary
dict_dir <- "/Users/hristogospodinov/Downloads/Sofia University/Analytical /adcase/dict/en_US.dic"
dict_url <- 'https://sourceforge.net/projects/wordlist/files/speller/2018.04.16/hunspell-en_US-2018.04.16.zip/download'
dict_local_zip <- file.path(dict_dir, basename(dict_url))
if (! file.exists(dict_local_zip)) {
  download.file(dict_url, dict_local_zip)
  unzip(dict_local_zip, exdir=dict_dir)
}

dict_files <- list.files(file.path(dict_dir), full.names=TRUE)
dict_files_match <- as.numeric(tools::file_ext(dict_files)) <= 60 & grepl("english-", dict_files, fixed = TRUE)
dict_files <- dict_files[dict_files_match ]
