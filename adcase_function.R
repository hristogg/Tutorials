words_in_dict <- data.frame(matrix(ncol = 1, nrow=nrow(test_df)))
words_not_in_dict <- data.frame(matrix(ncol = 1, nrow=nrow(test_df)))
test_df$df.Search.Term <- sapply(test_df$df.Search.Term, tolower)
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
      words_in_dict[i,1] <- paste(unlist(yes), collapse= " ")
    }
    if (length(no) == 0) {
      words_not_in_dict[i,1] <- 0 
    } else {
      words_not_in_dict[i,1] <- paste(unlist(no), collapse= " ")
    }
   
      
    }
}
