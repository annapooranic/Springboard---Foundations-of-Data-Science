install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tm")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("qdap")
install.packages("widyr")
install.packages("quanteda")
install.packages("maps")
install.packages("plotly")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("textir")
install.packages("caret")
install.packages("ggrepel")
install.packages("text2vec")
install.packages("NLP")
install.packages("RWeka")











library(plyr)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(tidytext)
library(tidyverse)
library(qdap)
library(widyr)
library(reshape2)
library(wordcloud)
library(maps)
library(choroplethr)
library(choroplethrMaps)
library(textir)
library(caret)
library(ggrepel)
library(text2vec)

library(slam)
library(NLP)
library(tokenizers)
library(RWeka)



complaint2 <- read_csv("consumer_complaints.csv")


complaint2$date_received <- mdy(complaint2$date_received)
complaint2$date_sent_to_company <- mdy(complaint2$date_sent_to_company)





complaint2$product<-as.factor(complaint2$product)
complaint2$company<-as.factor(complaint2$company)
complaint2$sub_product<-as.factor(complaint2$sub_product)
complaint2$issue <- as.factor(complaint2$issue)




##Exploratory Data Analysis


complaint2 %>%
  count(company) %>%
  arrange(desc(n))%>%
  top_n(25) %>%
  ggplot(aes(company,n,fill=company))+
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90),legend.position = "none")



#### Products with highest number of complaints

complaint2 %>%
  count(product) %>%
  arrange(desc(n))%>%
  ggplot(aes(product,n,fill=product))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90),legend.position = "none")



sub_issue_data <- complaint2 %>%
  select(complaint_id,sub_issue)%>%
  na.omit()


#### Complaint numbers based on the Issue categories


sub_issue_data %>%
  count(sub_issue) %>%
  arrange(desc(n))%>%
  top_n(25) %>%
  ggplot(aes(sub_issue,n,fill=sub_issue))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90),legend.position = "none")


product_issue <- complaint2 %>%
  select(product,issue)%>%
  na.omit %>%
  group_by(product,issue)%>%
  count()





#### To identify top issues resported by customers under each product other than Credit card

product_issue %>%
  filter(n>250)%>%
  filter(product !="Credit card") %>%
  ggplot(aes(issue,n,fill=issue))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  facet_wrap(~product,scale= "free",nrow = 6)+
  coord_flip()



#### Complaint category under Credit card

product_issue %>%
  filter(n>250)%>%
  filter(product =="Credit card") %>%
  ggplot(aes(issue,n,fill=issue))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  facet_wrap(~product,scale= "free",nrow = 6)+
  coord_flip()

####Complaint category for products other than credit card

product_issue %>%
  filter(n>250)%>%
  filter(product !="Credit card") %>%
  ggplot(aes(issue,n,fill=issue))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  facet_wrap(~product,scale= "free",nrow = 6)+
  coord_flip()


#### From which mode more complaints are received

complaint2 %>%
  select(company,product,issue,submitted_via)%>%
  na.omit()%>%
  count(submitted_via) %>%
  arrange(desc(n))%>%
  ggplot(aes(submitted_via,n,fill=submitted_via))+
  scale_y_continuous(labels = scales :: comma)+
  geom_bar(stat="identity")+
  theme(legend.position = "none")



top_companies <- complaint2 %>%
  count(company) %>%
  arrange(desc(n))%>%
  top_n(10)


####Mode by which more complaints are received based on companies

complaint2 %>%
  select(company,product,issue,submitted_via)%>%
  filter(company %in% top_companies$company)%>%
  group_by(company)%>%
  na.omit()%>%
  count(submitted_via) %>%
  ggplot(aes(company,n,fill=submitted_via))+
  geom_bar(stat="identity",position = position_dodge())



#### Distribution of complaints over United States


all_states <- map_data("state")

complaint2 <- complaint2 %>%
  mutate(region = state.name[match(state,state.abb)] )

complaint2$region[is.na(complaint2$region)] <- "district of columbia"
complaint2$region <- tolower(complaint2$region)


map_complaint <- complaint2 %>%
  select(company,product,region) %>%
  group_by(region)%>%
  count(region)

map_state <- merge(all_states,map_complaint,by="region")
map_state<- map_state[map_state$region!="district of columbia",]


map_state %>%
  ggplot(aes(x=long,lat,group=group,fill= n))+
  geom_polygon(color="white")+
  scale_fill_continuous(low = "thistle2",high="red",guide="colorbar")+
  theme_bw()+labs(fill = "Complaints",title="Number of Complaints by State",x="",y="")+
  scale_y_continuous(breaks=c())+scale_x_continuous(breaks=c())+theme(panel.border=element_blank())


#### Companies with highest number of complaints distribution based on state

comp_company_state <- complaint2 %>%
  select(company,region)%>%
  group_by(region)%>%
  count(company)%>%
  top_n(1,n)%>%
  arrange(desc(n))

map_company <-merge(all_states,comp_company_state,by="region")
map_company<- map_company[map_company$region!="district of columbia",]

map_company %>%
  ggplot(aes(x=long,lat,group=group,fill= company))+
  geom_polygon(color="white")+
  theme_bw()+labs(fill = "Complaints",title="Number of Complaints by State",x="",y="")+
  scale_y_continuous(breaks=c())+scale_x_continuous(breaks=c())+theme(panel.border=element_blank())



#### Products with highest number of complaints distribution based on state


comp_product_state <- complaint2 %>%
  select(product,region)%>%
  group_by(region)%>%
  count(product)%>%
  top_n(1,n)%>%
  arrange(desc(n))


map_product <-merge(all_states,comp_product_state,by="region")
map_product<- map_product[map_product$region!="district of columbia",]


map_product %>%
  ggplot(aes(x=long,lat,group=group,fill= product))+
  geom_polygon(color="white")+
  theme_bw()+labs(fill = "Complaints",title="Number of Complaints by State",x="",y="")+
  scale_y_continuous(breaks=c())+scale_x_continuous(breaks=c())+theme(panel.border=element_blank())



##Data Analysis



#####Function to clean the text


tm_clean <- function(corpus){
  tm_clean <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,removeWords,c(stopwords("en"),"xxxx","xx"))
  return(corpus)
}



data <- complaint2 %>%
  select(company,product,issue,state,zipcode,submitted_via,company_response_to_consumer,timely_response,`consumer_disputed?`,consumer_complaint_narrative) %>%
  na.omit 


#### Function to calculate sentiment

GetSentiment <- function(i){
  sentiment1 <- data %>%
    filter(company == i ) %>%
    select(consumer_complaint_narrative) %>%
    VectorSource() %>%
    VCorpus() %>%
    tm_clean() %>%
    DocumentTermMatrix()%>%
    tidy()%>%
    inner_join(get_sentiments("bing"),c(term = "word")) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
    mutate(company = i)
  return(sentiment1)
}



company_consumer_comp <- complaint2 %>%
  select(company,consumer_complaint_narrative)%>%
  na.omit() %>%
  count(company) %>%
  arrange(desc(n))%>%
  filter(n>100)


####Calculating overall sentiments for companies

comp <- company_consumer_comp$company

listcomp <- as.list(comp)

sentiments1 <- data_frame()

for(i in listcomp )
{
  sentiments1 <- rbind(sentiments1,GetSentiment(i))
}



#### Complaint percentage calculation function

GetPercentage <- function(i){
  d <- data %>%
    filter(company == i) %>%
    count(company) %>%
    mutate(per = (n/66617)*100) 
  return(d)
}



#### Companies complaint percentage.


complaint_percent <- data_frame()
for(i in listcomp )
{
  complaint_percent <- rbind(complaint_percent,GetPercentage(i))
}


####Dispute rate Calculation function



disp_rate <- function(i){
  d1 <- data %>%
    filter(company == i) %>%
    count(company,`consumer_disputed?`) %>%
    spread(`consumer_disputed?`,n,fill=0,drop = TRUE) %>%
    mutate(total = Yes + No) %>%
    mutate(YP = (Yes/total)*100) %>%
    mutate(NP = (No/total)*100)
  return(d1)
  
}

#### Companies Dispute rate


dispute_rate <- data_frame()

for(i in listcomp)
{
  dispute_rate<- rbind(dispute_rate,disp_rate(i))
}


####Companies response calculation


company_response <- function(i){
  d4 <- data %>%
    filter(company == i) %>%
    count(company,timely_response) 
  
  return(d4)
}


####Companies timely response

tim_resp <- data_frame()

for(i in listcomp )
{
  tim_resp <- rbind(tim_resp,company_response(i)) 
  
}

#### Calculating yes and No percentage


resp_percent <- tim_resp %>%
  spread(timely_response,n,fill=0,drop = TRUE) %>%
  mutate(total = Yes + No) %>%
  mutate(YP = (Yes/total)*100) %>%
  mutate(NP = (No/total)*100)%>%
  arrange(desc(total))


####Building a DataFrame with all the parameters calculated


result1 <- full_join(complaint_percent,dispute_rate,by = "company")
result2 <- full_join(result1,sentiments1,by ="company")
result3 <- full_join(result2,resp_percent,by="company")



final_result <- result3%>%
  select(company,n,per,No.x,Yes.x,YP.x,NP.x,negative,positive,sentiment,
         No.y,Yes.y,YP.y,NP.y)%>%
  setNames(c("Company","Total Complaints","Complaint Percent","No Disputes",
             "Disputes","Dispute Percent","No Dispute Percent","Negative Sentiment",
             "Positive Sentiment","Sentiment","No Timely Response","Timely Response",
             "Timely Response Percent","No Timely Response Percent"))

#######################################################

data3 <- data%>%
  na.omit() %>%
  select(company,consumer_complaint_narrative) %>%
  filter(company =='Equifax')

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




tok_fun = word_tokenizer  # using word & not space tokenizers

it_0 = itoken( x,
               #preprocessor = text.clean,
               tokenizer = tok_fun,
               ids = data3$id,
               progressbar = F)

vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                          ngram = c(2L, 2L) #,
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                term_count_min = 10)

length(pruned_vocab);  str(pruned_vocab)


vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus

dtm_0  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix

# Sort bi-gram with decreasing order of freq
tsum = as.matrix(t(rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq
head(tsum)











text2 = x
text2 = paste("",text2,"")

pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; i = 0

for (term in names(tsum)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
  # setTxtProgressBar(pb, i)
}


it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
              # preprocessor = text.clean,
              tokenizer = tok_fun,
              ids = data$id,
              progressbar = F)

vocab = create_vocabulary(it_m     # vocab func collects unique terms and corresponding statistics
                          # ngram = c(2L, 2L),
                          #stopwords = stopwords
)


pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 1)
vectorizer = vocab_vectorizer(pruned_vocab)

dtm_m  = create_dtm(it_m, vectorizer)
dim(dtm_m)


dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
dtm = dtm[a0,]                  # drop empty docs

dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums
inspect(dtm[1:5, 1:5])     # inspect() func used to view parts of a DTM object      




#--------------------------------------------------------#
## Step 2a:     # Build word cloud                       #
#--------------------------------------------------------#

#   1- Using Term frequency(tf)             

tst = round(ncol(dtm_0)/100)  # divide DTM's cols into 100 manageble parts
a = rep(tst,99)
b = cumsum(a);rm(a)
b = c(0,b,ncol(dtm_0))

ss.col = c(NULL)
for (i in 1:(length(b)-1)) {
  tempdtm = dtm_0[,(b[i]+1):(b[i+1])]
  s = colSums(as.matrix(tempdtm))
  ss.col = c(ss.col,s)
}

tsum = ss.col
tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
head(tsum)



windows()  # New plot window
wordcloud(names(tsum), tsum,     # words, their freqs 
          scale = c(4, 0.5),     # range of word sizes
          1,                     # min.freq of words to consider
          max.words = 100,       # max #words
          colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
title(sub = "Term Frequency - Wordcloud")     # title for the wordcloud display



# plot barchart for top tokens
test = as.data.frame(round(tsum[1:15],0))

windows()  # New plot window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "Blue") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



dtm.tfidf = tfidf(dtm, normalize= FALSE)

tst = round(ncol(dtm.tfidf)/100)
a = rep(tst, 99)
b = cumsum(a);rm(a)
b = c(0,b,ncol(dtm.tfidf))

ss.col = c(NULL)
for (i in 1:(length(b)-1)) {
  tempdtm = dtm.tfidf[,(b[i]+1):(b[i+1])]
  s = colSums(as.matrix(tempdtm))
  ss.col = c(ss.col,s)
  
}

tsum = ss.col

tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
head(tsum)


windows()  
wordcloud(names(tsum), tsum, scale=c(4,0.5),1, max.words=100,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency Inverse Document Frequency - Wordcloud")


as.matrix(tsum[1:20])     #  to see the top few tokens & their IDF scores


# plot barchart for top tokens
test = as.data.frame(round(tsum[1:15],0))
windows()  # New plot window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#------------------------------------------------------#
# step 2c - Term Co-occurance Matrix (TCM)             #
#------------------------------------------------------#

vectorizer = vocab_vectorizer(pruned_vocab, 
                              grow_dtm = FALSE, 
                              skip_grams_window = 5L)

tcm = create_tcm(it_m, vectorizer) # func to build a TCM

tcm.mat = as.matrix(tcm)         # use tcm.mat[1:5, 1:5] to view
adj.mat = tcm.mat + t(tcm.mat)   # since adjacency matrices are symmetric

z = order(colSums(adj.mat), decreasing = T)
adj.mat = adj.mat[z,z]

# Plot Simple Term Co-occurance graph
adj = adj.mat[1:30,1:30]

library(igraph)




distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0  
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) 
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # func ends

windows()
distill.cog(tcm.mat, 'Distilled COG',  10,  5)



## adj.mat and distilled cog for tfidf DTMs ##

adj.mat = t(dtm.tfidf) %*% dtm.tfidf
diag(adj.mat) = 0
a1 = order(apply(adj.mat, 2, sum), decreasing = T)
adj.mat = as.matrix(adj.mat[a1[1:50], a1[1:50]])

windows()
distill.cog(adj.mat, 'Distilled COG',  10,  10)


#--------------------------------------------------------#
#             Sentiment Analysis                         #
#--------------------------------------------------------#

library(qdap)

x1 = x[a0]    # remove empty docs from corpus

t1 = Sys.time()   # set timer

pol = polarity(x1)         # Calculate the polarity from qdap dictionary
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]                  # Negative Words info  

dim(pol)

head(pol$group)

positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

#--------------------------------------------------------#
#   Create Postive Words wordcloud                       #
#--------------------------------------------------------#

pos.tdm = dtm[,which(colnames(dtm) %in% positive_words)]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows() # opens new image window
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")


#--------------------------------------------------------#
#  Create Negative Words wordcloud                       #
#--------------------------------------------------------#

neg.tdm = dtm[,which(colnames(dtm) %in% negative_words) ]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")


# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows()
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#--------------------------------------------------------#
#  Positive words vs Negative Words plot                 #
#--------------------------------------------------------#

len = function(x){
  if ( x == "-" && length(x) == 1)  {return (0)} 
  else {return(length(unlist(x)))}
}

pcount = unlist(lapply(p, len))
ncount = unlist(lapply(n, len))
doc_id = seq(1:length(wc))

windows()
plot(doc_id,pcount,type="l",col="green",xlab = "Document ID", ylab= "Word Count")
lines(doc_id,ncount,type= "l", col="red")
title(main = "Positive words vs Negative Words" )
legend("topright", inset=.05, c("Positive Words","Negative Words"), fill=c("green","red"), horiz=TRUE)


# Documet Sentiment Running plot
windows()
plot(pol$all$polarity, type = "l", ylab = "Polarity Score",xlab = "Document Number")
abline(h=0)
title(main = "Polarity Plot" )

### COG for sentiment-laden words ? ###

senti.dtm = cbind(pos.tdm, neg.tdm); dim(senti.dtm)


senti.adj.mat = as.matrix(t(senti.dtm)) %*% as.matrix(senti.dtm)
diag(senti.adj.mat) = 0

windows()
distill.cog(senti.adj.mat,   # ad mat obj 
            'Distilled COG of senti words',       # plot title
            5,       # max #central nodes
            5)        # max #connexns



