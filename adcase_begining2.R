#install.packages("quanteda")
#library(quanteda)
## load data
setwd("F:\\Kamelia\\Master\\II semester\\Analytics\\Lecture 3\\Case study Google\\dig_marketing")
data <- read.csv("Data set task.csv")
## examine data
names(data)
# see classes
cl <- sapply(data,class)
# see data structure
str(data)
unique(data$Domain) # 13 levels - 13 different advertisers
# see summary statistics
library(fBasics)
var.summary <- sapply(data[,-c(1,2,5)], basicStats)
stats <- data.frame(matrix(,nrow=16,ncol=12))
for (i in 1:12) {
  stats[,i] <- var.summary[[i]]
}
colnames(stats) <- colnames(data)[-c(1,2,5)]
x <- basicStats(data$Position)
rownames(stats) <- rownames(x)
rm(x)
hist(data$Position)

############ Task 2 ##############
x <- data$Search.Term
# transform to character
x <- as.character(x)
# clean phrases using function from sentimentr
library(sentimentr)
x=gsub("[[:punct:]]","",x) # cleans punctuation
x=gsub("[[:digit:]]", "", x) # cleans numbers
x=gsub("http\\w+", "", x) # cleans web references
x=gsub("[ \t]{2,}", "", x) # cleans 2 or more tabs
x=gsub("^\\s+|\\s+$", "", x) # cleans 1 or more
x=tolower(x) # convert all upper cases to lower cases

# use function from quanteda
my_dfm <- dfm(x, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
# see top 20 features
topfeatures(my_dfm, 20)
### use the matrix for calculating frequencies (task 3)

# split words
xs=strsplit(x,split="[ ]")
xs=unlist(xs)

# load dictionary
load(system.file("words", "english.RData", package = "SnowballC")) # voc

# load another dictionary
dict_dir <- tempdir()
dict_url <- 'https://sourceforge.net/projects/wordlist/files/speller/2018.04.16/hunspell-en_US-2018.04.16.zip/download'
dict_local_zip <- file.path(dict_dir, basename(dict_url))
if (! file.exists(dict_local_zip)) {
  download.file(dict_url, dict_local_zip)
  unzip(dict_local_zip, exdir=dict_dir)
}

dict_files <- list.files(file.path(dict_dir, 'final'), full.names=TRUE)
dict_files_match <- as.numeric(tools::file_ext(dict_files)) <= 60 & grepl("english-", dict_files, fixed = TRUE)
dict_files <- dict_files[ dict_files_match ]

#words <- unlist(sapply(dict_files, readLines, USE.NAMES=FALSE))
words2 <- unlist(sapply(dict_files, readLines, USE.NAMES=FALSE))
length(words2)

# check words
words <- tolower(words)
ind <- rep(0,length(xs))
for (i in 1:length(xs)) {
  ind[i] <- ifelse(xs[i] %in% words, 1, 0)
}

test <- xs[which(ind==0)]
unt <- unique(test)
unall <- unique(xs)

ind2 <- rep(0,length(unt))
for (i in 1:length(unt)) {
  ind2[i] <- ifelse(unt[i] %in% words2, 1, 0)
}
