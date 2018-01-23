library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(widyr)
library(lubridate)
library(ggplot2)
library(tm)
library(tidytext)
library(tidyverse)
library(qdap)
library(quanteda)
library(maps)
#library(choroplethr)
#library(choroplethrMaps)
library(igraph)
library(wordcloud)
library(text2vec)
library(NLP)
library(textir)
library(tokenizers)
library(RWeka)
library(caret)
library(slam)
library(reshape2)
library(ggrepel)
library(ggraph)
library(igraph)
library(servr)
library(LDAvis)


complaint2 <- read_csv("consumer_complaints.csv")
complaint2$date_received <- mdy(complaint2$date_received)
complaint2$date_sent_to_company <- mdy(complaint2$date_sent_to_company)


complaint2$product<-as.factor(complaint2$product)
complaint2$company<-as.factor(complaint2$company)
complaint2$sub_product<-as.factor(complaint2$sub_product)
complaint2$issue <- as.factor(complaint2$issue)

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

sentiments1



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
complaint_percent

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
dispute_rate



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

tim_resp

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

final_result



text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("n't","not",x)
  x  = gsub("'","",x)
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  #  x  =  gsub("^\\s+|\\s+$", "", x) # remove leading and trailing white space
  
  return(x)
}


negation_word <- data.frame(negation.words)
negation_word <- setNames(negation_word,c("word"))
negation_word$word <- as.character(negation_word$word)
negation_word <- bind_rows(negation_word,data_frame(word=
                                                      c("non","noone","none",as.character(1:3))))

stopword <- stop_words %>%
  anti_join(negation_word)

stopword <- data.frame(stopword$word)
stopword <- setNames(stopword,c("word"))
stopword$word <- as.character(stopword$word)

custom_stopword <- data_frame(word = 
                                c(as.character(1:10),"xxxx","xxx","xxxxx","xx","x","company","companies","said","told",
                                  "however","since","asked","stated","equifax","well","item","items","done",
                                  "going","n_t","s"))


comn_stop_word <- bind_rows(stopword,custom_stopword)

comn_stop_word <- list(comn_stop_word$word)

Mortgage_comments <- data%>%
  filter(product =='Mortgage')%>%
  select(issue,consumer_complaint_narrative)


Mortgage_comments$consumer_complaint_narrative <- text.clean(Mortgage_comments$consumer_complaint_narrative)

Mortgage_comments$consumer_complaint_narrative <- removeWords(Mortgage_comments$consumer_complaint_narrative,unlist(comn_stop_word))

Mortgage_comments_sentiment <- Mortgage_comments %>%
  group_by(issue)%>%
  unnest_tokens(word,consumer_complaint_narrative) %>%
  anti_join(stopword)%>%
  anti_join(custom_stopword)%>%
  count(word,sort = TRUE)%>%
  inner_join(get_sentiments("bing"))%>%
  count(issue,sentiment)%>%
  spread(sentiment, nn, fill = 0) %>%
  mutate(sentiment = positive - negative)

Mortgage_comments_sentiment


Mortgage_unigram_sentiment<- Mortgage_comments %>%
  group_by(issue)%>%
  unnest_tokens(word,consumer_complaint_narrative) %>%
  anti_join(stopword)%>%
  anti_join(custom_stopword)%>%
  inner_join(get_sentiments("bing"))%>%
  count(word,sentiment,sort = TRUE)%>%
  ungroup()%>%
  group_by(issue,sentiment)%>%
  top_n(10)%>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE,width = 1) +
  facet_wrap(~issue, scale = "free") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

Mortgage_unigram_sentiment

Mortgage_bigram <- Mortgage_comments %>%
  # group_by(issue)%>%
  unnest_tokens(bigram,consumer_complaint_narrative,token="ngrams",n=2) %>%
  count(issue,bigram,sort = TRUE)


Mortgage_bigram

Mortgage_bigram_graph <- Mortgage_comments %>%
  # group_by(issue)%>%
  unnest_tokens(bigram,consumer_complaint_narrative,token="ngrams",n=2) %>%
  count(issue,bigram,sort = TRUE)%>%
  ungroup()%>%
  bind_tf_idf(bigram,issue,n)%>%
  arrange(desc(tf_idf))%>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(issue) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = issue)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~issue, ncol = 2, scales = "free") +
  coord_flip()

Mortgage_bigram_graph

bigram_count <- Mortgage_comments %>%
  group_by(issue)%>%
  unnest_tokens(bigram,consumer_complaint_narrative,token="ngrams",n=2) %>%
  count(issue,bigram)

bigram_count %>%
  group_by(issue)%>%
  #filter(n > 300) %>%
  top_n(10,n)%>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = n, width = n)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

Mortgage_trigram <- Mortgage_comments %>%
  group_by(issue)%>%
  unnest_tokens(trigram,consumer_complaint_narrative,token="ngrams",n=3) %>%
  count(issue,trigram)%>%
  bind_tf_idf(trigram,issue,n)%>%
  arrange(desc(tf_idf))%>%
  #mutate(bigram = factor(bigram, levels = rev(unique(word)))) %>% 
  group_by(issue) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(trigram, tf_idf, fill = issue)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~issue, ncol = 2, scales = "free") +
  coord_flip()

Mortgage_trigram

cors <- Mortgage_comments %>%
  group_by(issue)%>%
  unnest_tokens(bigram,consumer_complaint_narrative,token="ngrams",n=2) %>%
  count(issue,bigram)%>%
  pairwise_cor(issue,bigram,n,sort=TRUE)

cors

set.seed(2017)

Mortgage_cor_graph <- cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

Mortgage_cor_graph

trigram_count <- Mortgage_comments %>%
  group_by(issue)%>%
  unnest_tokens(trigram,consumer_complaint_narrative,token="ngrams",n=3) %>%
  count(issue,trigram)%>%
  filter(n >50)


trigram_count$issue <- gsub("Application, originator, mortgage broker","1",trigram_count$issue)


trigram_count$issue <- gsub("Loan modification,collection,foreclosure","2",trigram_count$issue)

trigram_count$issue <- gsub("Loan servicing, payments, escrow account","3",trigram_count$issue)   
trigram_count$issue <- gsub("Settlement process and costs","4",trigram_count$issue) 

trigram_count$issue <- as.numeric(as.character(trigram_count$issue))



tri_cor <- trigram_count %>%
  pairwise_cor(trigram,issue,sort=TRUE)

tri_cor

tri_cor%>%
  filter(item2 == "deed lieu foreclosure")%>%
  filter(correlation > 0.99)%>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha=correlation, width = correlation),colour = "lightgreen") +
  geom_node_point(size = 5, color = "green3") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

product_comments <- data%>%
  select(product,issue,consumer_complaint_narrative)

product_comments$consumer_complaint_narrative <- text.clean(product_comments$consumer_complaint_narrative)

product_comments$consumer_complaint_narrative <- removeWords(product_comments$consumer_complaint_narrative,unlist(comn_stop_word))

bigram_comment <- product_comments %>%
  #group_by(product,issue)%>%
  unnest_tokens(bigram,consumer_complaint_narrative,token="ngrams",n=2) %>%
  count(product,issue,bigram)%>%
  filter(n>75)
# group_by(bigram)



prod_cor <- pairwise_cor(bigram_comment,product,bigram,sort = TRUE)

prod_cor %>%
  filter(correlation > 0) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation),colour="snow3") +
  geom_node_point(size = 6, color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


issue_cor <- pairwise_cor(bigram_comment,issue,bigram,sort = TRUE)

issue_cor %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha=correlation, width = correlation),colour = "lightgreen") +
  geom_node_point(size = 5, color = "green3") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


tok_fun = word_tokenizer  # using word & not space tokenizers

itok = itoken( product_comments$consumer_complaint_narrative,
               tokenizer = tok_fun,
               progressbar = F)

onelex_vocab = create_vocabulary(itok,    #  func collects unique terms & corresponding statistics
                                 ngram = c(1L, 1L))

onelex_pruned_vocab = prune_vocabulary(onelex_vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                       term_count_min = 10)
onelex_model = Collocations$new(vocabulary=onelex_pruned_vocab,collocation_count_min = 50)

onelex_tok <-itoken(product_comments$consumer_complaint_narrative)
onelex_model$fit(onelex_tok, n_iter = 3)

onelex_stat <- onelex_model$collocation_stat

onelex_stat

twolex_vocab = create_vocabulary(itok,    #  func collects unique terms & corresponding statistics
                                 ngram = c(2L, 2L))

twolex_pruned_vocab = prune_vocabulary(twolex_vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                       term_count_min = 10)
twolex_model = Collocations$new(vocabulary=twolex_pruned_vocab,collocation_count_min = 50)

twolex_tok <-itoken(product_comments$consumer_complaint_narrative)
twolex_model$fit(twolex_tok, n_iter = 3)

twolex_stat <- twolex_model$collocation_stat

twolex_stat

itok_mortgage = itoken( Mortgage_comments$consumer_complaint_narrative,
                        tokenizer = tok_fun,
                        progressbar = F)
twolex_vocab_mt = create_vocabulary(itok_mortgage,    #  func collects unique terms & corresponding statistics
                                    ngram = c(2L, 2L))

twolex_pruned_vocab_mt = prune_vocabulary(twolex_vocab_mt,  # filters input vocab & throws out v frequent & v infrequent terms
                                          term_count_min = 10)
twolex_model_mt = Collocations$new(vocabulary=twolex_pruned_vocab_mt,collocation_count_min = 50)

twolex_tok_mt <-itoken(Mortgage_comments$consumer_complaint_narrative)
twolex_model_mt$fit(twolex_tok_mt, n_iter = 3)

twolex_stat_mt <- twolex_model_mt$collocation_stat
twolex_stat_mt


topics = 10
vectorizer = vocab_vectorizer(twolex_pruned_vocab)
dtm = create_dtm(twolex_tok, vectorizer)
lda = LDA$new(topics)
doc_topic = lda$fit_transform(dtm)


lda$get_top_words(n = 10, topic_number = c(1L, 5L, 10L), lambda = 0.2)

lda$plot()

serVis(lda$plot())