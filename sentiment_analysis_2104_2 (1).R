# Import data----
rm(list=ls()) #clean the environment 
library(sentimentr)
library(stringr)

#vinagi si mahame prazni/na tn poleta:
dd=read.csv("ted_main.csv", na.strings = c(""," ", "NA", "#NA"), stringsAsFactors = FALSE)
dd2=read.csv("transcripts.csv", na.strings = c(""," ", "NA", "#NA"), stringsAsFactors = FALSE)

#suzdavame nova tablica s imeto na variables & classes
ddclass=data.frame(names(dd),sapply(dd,class))

#krushtavame si kolonite s lesno razbiraemi imena
colnames(ddclass)=c("Varname","Varclass")

#2ri data set
dd2class=data.frame(names(dd2),sapply(dd2,class))
colnames(dd2class)=c("Varname","Varclass")

#gledame kolko reda & coloni imame
dim(dd) #[1] 2550   17
dim(dd2) # [1] 2467    2

#proveriavame dali imame unikalni/dublirani i tn:
# unique(), duplicated(), intersect(), setdiff()

#koi sa unique i si gi slagame v nova tablica?
a=unique(dd2$url)

#vijdame kolko sa ni unique redovete
length(a) #[1] 2464

#vijdame che imame 3 dublirashti se redove
duplicated(dd2$url) #tova ne se chete

b=duplicated(dd2$url) #krushtavame si ia kat b
table(b) # summary tablica

#vijdame koi sa duplicate redove
which(b==T)
# mahame duplicates redove i suzdaveme nova
dd2new=dd2[-c(1115,1116,1117),]


dim(dd2new) #2464    2
dim(dd)
#pravim sushtoto za purvia data set
a2=unique(dd$url)
length(a2) #[1] 2464

#

ted=dd$url
tr=dd2new$url

c=intersect(dd2new$url,dd$url) 

#Merging the two datasets
last=merge(dd,dd2new,by='url')
names(last)

Brown=str_detect(last$main_speaker,"Brown")
lname=last[which(Brown==T),]
Bren=str_detect(lname$main_speaker,"Bren")
fname=lname[which(Bren==T),]
fnadim(fname)
fname = fname[which(fname$views == max(fname$views)),]


sent_terms = extract_sentiment_terms(fname$transcript)
test <- data.frame(apply(sent_terms[,c(3,4,5)], c(1,2), function(k) length(unlist(k))))

ss=sentiment(get_sentences(fname$transcript))
test$PosIndex = test$positive / (test$positive + test$negative + test$neutral)
test$NegIndex = test$negative / (test$positive + test$negative + test$neutral)

plot(test$PosIndex, type='l')
lines(test$NegIndex, type='l', col='red')

library("SnowballC")
negative = unlist(sent_terms$negative)
positive = unlist(sent_terms$positive)
ws_neg= data.frame(wordStem(negative))
ws_pos = data.frame(wordStem(positive))
positive_count = data.frame(table(ws_pos))
negative_count = data.frame(table(ws_neg))
rating <- fname$ratings
#taking the rating and parsing the content


rating1 <- unlist(strsplit(rating, "id"))
rating1 <- rating1[-1]
rating1 <- rating1[-length(rating1)]

b = rating1[seq(5, length(rating1), 8)]
c = rating1[seq(8, length(rating1), 8)]
c = gsub("}", "", c)
c = gsub(":", "", c)
c = gsub("\\{", "", c)
c = gsub(", ", "", c)
c = gsub("]", "", c)
Rating_Final = data.frame(Word=b,count=c)

#### the above code apparently works when we have only one rating but would 
#### not be applicable for the entire set, the below one is an attempt to do it for all
test <- last$ratings #taking all the ratings
#empty_df = data.frame() #creating 2 supplementary empty dataframe which we will be feeding with the parsed data
#empty_df2 = data.frame()
empty_list = list() #and one supplementary list as well :)
for (i in 1:length(test)) {
  empty_list[i] <- str_extract_all(test[i], "\\{[^{}]+\\}")
} #this loop should parse all the data into lists within the empty_list

df = data.frame(matrix(ncol=14, nrow=1)) #dataframe which will fill the counters of the words 
#adding the column names to the df so that we can use them as to put the coun tin the appropriate column
colnames(df) <- c("Inspiring", "Fascinating", "Jaw-dropping", "Confusing", "Beautiful", "Informative", "Persuasive", "OK","Courageous","Funny","Ingenious","Longwinded","Unconvincing", "Obnoxious")

for (i in 1:length(empty_list)) {
  #the first layer of the loop goes through each one of the large lists (2464 elements)
  for (k in 1:length(empty_list[[i]])) {
    #this layer goes through each row of the inner lists (14 elements)
    pos_begining <- regexpr('e\':', empty_list[[i]][k])  #gets the position of the begining of the word
    pos_beg1 <- regexpr('unt\'', empty_list[[i]][k]) #position of the begining of the count
    pos_end <- regexpr(', \'c', empty_list[[i]][k]) #end of word
    pos_end1 <- nchar(empty_list[[i]][k]) #end of count
    #empty_df[i,k] <- substr(empty_list[[i]][k], pos_begining+5, pos_end-2) #puts the words in the first df
    #empty_df2[i,k] <- substr(empty_list[[i]][k], pos_beg1+6, pos_end1-1) #puts the counts in the seocnd df
    df[i, substr(empty_list[[i]][k], pos_begining+5, pos_end-2)] <- substr(empty_list[[i]][k], pos_beg1+6, pos_end1-1)
  }
} 

#mergin the 2 dfs so we have every transcript with all the counters

df_merged <- data.frame(last, df)


#########
tx=fname$transcript
tx=gsub("[[:punct:]]","",tx) # cleans punctuation
tx=gsub("[[:digit:]]", "", tx) # cleans numbers
tx=gsub("http\\w+", "", tx) # cleans web references
tx=gsub("[ \t]{2,}", "", tx) # cleans 2 or more tabs
tx=gsub("^\\s+|\\s+$", "", tx) # cleans 1 or more
tx=tolower(tx) # convert all upper cases to lower cases
fname[,19]=tx

#str_detect - find the Brene Brown, first find Brown speakers, then find Brene


#Disregard---------------------------------------------
dim(dd2new)


names(dd) # "sentiment" "text" 
names(dd2) # "sentiment" "text" 


rapply(dd,class) # "numeric" "character" 
rapply(dd2,class) # "numeric" "character" 
dd$sentiment=as.factor(dd$sentiment)
levels(dd$sentiment) # "-1" "1" 

# Correct for punctuation, capital letters, etc. 
tx=dd$text
tx=gsub("[[:punct:]]","",tx) # cleans punctuation
tx=gsub("[[:digit:]]", "", tx) # cleans numbers
tx=gsub("http\\w+", "", tx) # cleans web references
tx=gsub("[ \t]{2,}", "", tx) # cleans 2 or more tabs
tx=gsub("^\\s+|\\s+$", "", tx) # cleans 1 or more
tx=tolower(tx) # convert all upper cases to lower cases
dd$tx=tx

# Subsample and string split ----
# Subsample 1 & Subsample m1
ss1=dd[dd$sentiment==1,3]
ssm1=dd[dd$sentiment==-1,3]
ss1s=strsplit(ss1,split="[ ]")
ss1s=unlist(ss1s)
ssm1s=strsplit(ssm1,split="[ ]")
ssm1s=unlist(ssm1s)

# Find most frequently used words ----
ss1dd=data.frame(table(ss1s))
ss1dd=ss1dd[order(ss1dd$Freq, decreasing = T),]
ssm1dd=data.frame(table(ssm1s))
ssm1dd=ssm1dd[order(ssm1dd$Freq, decreasing = T),]

# Extract sentiment words ----
ss1sent=data.frame(extract_sentiment_terms(get_sentences(ss1)))
ssm1sent=data.frame(extract_sentiment_terms(get_sentences(ssm1)))






