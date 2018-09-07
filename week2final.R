
zipfiels <- list.files("./specdata", full.names = TRUE)


df <- data.frame()

for (i in zipfiels) { 
       
        df <- rbind(df, read.csv(i, colClasses = c(Date="Date")))
}

fixnumber <- function(k)
        {
        ls <- character()
        for (i in seq(length(k))) 
        {
                
                if (k[i] < 10) {
                        ls[i] <- paste("00", k[i], sep="")
                }
                else if(k[i] < 100) {
                        ls[i] <- paste("0", k[i], sep="")
                }
                else 
                        ls[i] <- paste(k[i])
        }
        ls <- na.omit(ls)
        
}


pollutantmean <- function(directory, polutant, id){
        #declaring empty data frame which we would fill based on the id
        df <- data.frame()
        #getting all files in the directory
        id <- fixnumber(id)
        for (i in id) { 
                #adding all the csvs to a dataframe object
                df <- rbind(df, read.csv(paste(getwd(), "/", directory,"/", i, ".csv", sep=""), colClasses = c(Date="Date")))
        }
        polid <- numeric()
        if (polutant == "sulfate") {
                polid <- 2
        } else if (polutant == "nitrate"){
                polid <- 3
        }
        else
                print("Please select for a polutant either Sulfate or Nitrate")
        
        mean(df[,polid], na.rm=TRUE)
}

complete <- function(directory, id = 1:332)
{
        fin_datafr <- data.frame(filename=numeric(0), obs=numeric(0))
        id <- fixnumber(id)
      
        for (i in id) {
                temp_df <- read.csv(paste(getwd(), "/", directory,"/", i, ".csv", sep=""), colClasses = c(Date="Date"))
                temp_val <- temp_df[!is.na(temp_df$sulfate) & !is.na(temp_df$nitrate), ]
                temp_val <- nrow(temp_val)
        
                fin_datafr <- rbind(fin_datafr, list(filename=as(i,"numeric"), obs=temp_val))
        }
        fin_datafr
        
}
        

makeVector <- function(x, ...)
{
        m <- NULL
        set <- function(k) 
        {
                x <<- k
                m <<- NULL
        }
        get <- function() x
        
        setmean <- function(me) m <<- me
        getmean <- function() m
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}

test <- function(x)
{
        if (x == "heart attack") {
                return(11)
        }
        else if (x == "sok") {
                result <- 12
        }
        else {
                result <- 13
        }
        return(result)
}


pm <- proc.time()
for(n in 1:1000) {
sapply(split(DT$pwgtp15,DT$SEX),mean)
}
proc.time() - pm

