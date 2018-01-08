



complaint2 <- read_csv("consumer_complaints.csv")

#Formating Data Values to month,date and year format
complaint2$date_received <- mdy(complaint2$date_received)
complaint2$date_sent_to_company <- mdy(complaint2$date_sent_to_company)
#Formating required variables to factors
complaint2$product<-as.factor(complaint2$product)
complaint2$company<-as.factor(complaint2$company)
complaint2$sub_product<-as.factor(complaint2$sub_product)
complaint2$issue <- as.factor(complaint2$issue)

#####Function to clean the text

# For Sentiment analysis the below function is used to clean up the corpus
tm_clean <- function(corpus){
  tm_clean <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,removeWords,c(stopwords("en"),"xxxx","xx"))
  return(corpus)
}


#Extracting customer complaints for particular company

data3 <- data%>%
  na.omit() %>%
  select(company,consumer_complaint_narrative) %>%
  filter(company =='Equifax')

#Cleaning up the text based on text2vec package
text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  x  = gsub("'","",x)
  return(x)
}



data3$id <- seq.int(nrow(data3))

stp <- tm::stopwords('english')
stp1 <- c("xxxx","xxx","xxxxx","xx","x","company","companies","said","told",
          "however","since","asked","stated","equifax","well","item","items","done",
          "going","n_t")
comn  = unique(c(stp, stp1) )                 # Union of two list
stopwords = unique(gsub("'"," ",comn) ) 


x= text.clean(data3$consumer_complaint_narrative)
x  =  removeWords(x,stopwords)            # removing stopwords created above
x  =  stripWhitespace(x )                  # removing white spac



