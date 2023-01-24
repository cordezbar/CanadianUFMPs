#####TOPIC MODEL ANALYSIS OF CANADIAN UFMPs#####
#####By cordezbar, 2023, University of Toronto
#####PDFs available online, n=74 as of May 2022

#####ENGLISH#####

##1. R packages required
require("topicmodels")
require("tm")
require("tidytext")
require("tidyverse")
require("tidyr")
require("ggplot2")
require("dplyr")
require("plyr")
require("readr")
require("data.table")
require("pdftools")
library("gridExtra")
library("ggpubr")
library("graphics")
library("readxl")
##2. Read Data
#Data as PDFs; files need to be in the same folder
allfiles <- list.files(pattern = "pdf$")
#read only text in PDF files 
all.ufmps <- lapply(allfiles, pdf_text)
#load data as Corpus Vector Source
documents <- Corpus(VectorSource(all.ufmps))
#check if all data are loaded, must be 51 documents
documents

########Pre-Processing############

##3. Basic pre-processing chains to process invalid text
##Ignore "transformation drops documents" results, it doesn't really affect the analysis
#3.1 change all capital letters to lower case
documents <- tm_map(documents, content_transformer(tolower))
#3.2 remove stop words
documents <- tm_map(documents, removeWords, stopwords("english"))
#3.3 remove punctuation but retain hyphenated terms
documents <- tm_map(documents, removePunctuation, preserve_intra_word_dashes = TRUE)
#3.4 remove numbers
documents <- tm_map(documents, removeNumbers)
#3.5 Stemming, or stem words to avoid repeated similar words
#we DO NOT do this because it makes it harder to identify words
#for example, management becomes "manag", and canopies and canopy become "canopi"
#documents <- tm_map(documents, stemDocument, language = "en")
#3.6 remove extra white space from text
documents <- tm_map(documents, stripWhitespace)

##4. Create document-term matrix
DTM <- DocumentTermMatrix(documents)
#4a. We can also create DTM with minimum frequency terms, in this case, 5 mentions
#We do not use this because it changes data significantly 
#minimumFrequency <- 5
#DTM <- DocumentTermMatrix(documents, control = list(bounds = list(global = c(minimumFrequency, Inf))))
#4.1 remove sparse terms, terms that do not occur in many documents, here terms that appear in less than 1% of docs 
#shows same number of words but now only few unique terms
DTM = removeSparseTerms(DTM, .99)
#4.2 Check that all documents still have terms after the preprocessing, remove if not
#this step is KEY! otherwise we can't add the results to the data as the number of rows with data will be different
idx = slam::row_sums(DTM) > 0
DTM = DTM[idx, ]
#look at dimensions of dtm, should show 51 documents 
dim(DTM)
#4.3 Explore the document matrix, get some simple word frequencies 
freq = data.frame(sort(colSums(as.matrix(DTM)), decreasing=TRUE))
#4.3.1 add terms to frequency dataframe from rownames
freq$terms = row.names(freq)
#4.3.2 remove rownames
rownames(freq) = NULL
#4.3.3 change name of frequency column
colnames(freq)[1] = 'frequency'
#4.3.3 check 30 most frequent terms
freq[1:30,]

##5. LDA model BASIC using Gibbs method
#5.1 seet random number generator seed? for Gibbs you have to set it before
set.seed(9161)
#number of topics needs to be specified as K, here we used 8
#the model computes inference via 1000 iterations of Gibbs sampling
#alpha can be changed, it affects the probability distribution of topics in docs
#note that the default alpha is 2.5. A lower alpha will result in higher probabilities. 
#Here we use 0.1, higher probabilities, fits the massive text database
model <- LDA(DTM, k=8, method="Gibbs", control=list(iter = 500), alpha=0.1)
model
#5.2 Examine the top 10 terms in each topics
terms(model, 10)

##Results so far##
#1) the names of cities appear in the topics
#2) symbols like ., -, \u0092s are frequent
#=> these must be removed

##6. Further pre-processing
#6.1 remove common symbols in the files
#toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#Code above doesn't work post-analysis 
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
documents <- tm_map(documents, toSpace, "•")
documents <- tm_map(documents, toSpace, "")
documents <- tm_map(documents, toSpace, "’s")
documents <- tm_map(documents, toSpace, "–")
#6.2 remove names of cities
documents <- tm_map(documents, removeWords, "abbotsford")
documents <- tm_map(documents, removeWords, "ajax")
documents <- tm_map(documents, removeWords, "banff")
documents <- tm_map(documents, removeWords, "barrie")
documents <- tm_map(documents, removeWords, "brantford")
documents <- tm_map(documents, removeWords, "burlington")
documents <- tm_map(documents, removeWords, "calgary")
documents <- tm_map(documents, removeWords, "cambridge")
documents <- tm_map(documents, removeWords, "collingwood")
documents <- tm_map(documents, removeWords, "delta")
documents <- tm_map(documents, removeWords, "edmonton")
documents <- tm_map(documents, removeWords, "fredericton")
documents <- tm_map(documents, removeWords, "guelph")
documents <- tm_map(documents, removeWords, "halifax")
documents <- tm_map(documents, removeWords, "hrm")
documents <- tm_map(documents, removeWords, "hamilton")
documents <- tm_map(documents, removeWords, "kamloops")
documents <- tm_map(documents, removeWords, "kelowna")
documents <- tm_map(documents, removeWords, "kingston")
documents <- tm_map(documents, removeWords, "kitchener")
documents <- tm_map(documents, removeWords, "langley")
documents <- tm_map(documents, removeWords, "lethbridge")
documents <- tm_map(documents, removeWords, "london")
documents <- tm_map(documents, removeWords, "markham")
documents <- tm_map(documents, removeWords, "mississauga")
documents <- tm_map(documents, removeWords, "nanaimo")
documents <- tm_map(documents, removeWords, "newmarket")
documents <- tm_map(documents, removeWords, "new westminster")
documents <- tm_map(documents, removeWords, "oakville")
documents <- tm_map(documents, removeWords, "ottawa")
documents <- tm_map(documents, removeWords, "peel")
documents <- tm_map(documents, removeWords, "peterborough")
documents <- tm_map(documents, removeWords, "prince")
documents <- tm_map(documents, removeWords, "george")
documents <- tm_map(documents, removeWords, "regina")
documents <- tm_map(documents, removeWords, "richmond")
documents <- tm_map(documents, removeWords, "richmond hill")
documents <- tm_map(documents, removeWords, "hill")
documents <- tm_map(documents, removeWords, "saanich")
documents <- tm_map(documents, removeWords, "saskatoon")
documents <- tm_map(documents, removeWords, "sechelt")
documents <- tm_map(documents, removeWords, "grove")
documents <- tm_map(documents, removeWords, "albert")
documents <- tm_map(documents, removeWords, "catharines")
documents <- tm_map(documents, removeWords, "john")
documents <- tm_map(documents, removeWords, "surrey")
documents <- tm_map(documents, removeWords, "thunder")
documents <- tm_map(documents, removeWords, "bay")
documents <- tm_map(documents, removeWords, "toronto")
documents <- tm_map(documents, removeWords, "vancouver")
documents <- tm_map(documents, removeWords, "victoria")
documents <- tm_map(documents, removeWords, "buffalo")
documents <- tm_map(documents, removeWords, "york region")
documents <- tm_map(documents, removeWords, "yorkregion")
documents <- tm_map(documents, removeWords, "york")

##7. Re-create document-term matrix
DTM <- DocumentTermMatrix(documents)
DTM = removeSparseTerms(DTM, .99)
idx = slam::row_sums(DTM) > 0
DTM = DTM[idx, ]
#look at dimensions of dtm, should show 51 documents 
dim(DTM)

##8. Re-run LDA model 
#8.1 random seed
set.seed(9161) # same seed is used to reproduce results
#8.2 model
model <- LDA(DTM, k=8, method="Gibbs", control=list(iter = 500), alpha=0.1)
model
#8.3 rexamine top 10 terms in each topics, preliminary result issues should be corrected
terms(model, 10)

##Results so far##
#1) all topics are built around the terms trees, urban, management, city, strategy
#2) fill terms and, for, will, table, figure, are frequent
#4) fill terms ufb and ufmp inc are frequent; ufb is possibly a text conversion issue
#=> these  must be removed

##9. Further pre-processing
#9.1 remove other common  words
documents <- tm_map(documents, removeWords, "and")
documents <- tm_map(documents, removeWords, "the")
documents <- tm_map(documents, removeWords, "within")
documents <- tm_map(documents, removeWords, "yes")
documents <- tm_map(documents, removeWords, "can")
documents <- tm_map(documents, removeWords, "for")
documents <- tm_map(documents, removeWords, "also")
documents <- tm_map(documents, removeWords, "appendix")
documents <- tm_map(documents, removeWords, "figure")
documents <- tm_map(documents, removeWords, "table")
documents <- tm_map(documents, removeWords, "will")
documents <- tm_map(documents, removeWords, "shall")
documents <- tm_map(documents, removeWords, "ufb")
documents <- tm_map(documents, removeWords, "ufa")
documents <- tm_map(documents, removeWords, "ltd")
documents <- tm_map(documents, removeWords, "groupn")
documents <- tm_map(documents, removeWords, "nfinal")
documents <- tm_map(documents, removeWords, "inc")
documents <- tm_map(documents, removeWords, "nnn")
documents <- tm_map(documents, removeWords, "nhufs") # unclear why, maybe a text conversion issue
#9.2 remove common words that dominate all topics
documents <- tm_map(documents, removeWords, "tree")
documents <- tm_map(documents, removeWords, "trees")
documents <- tm_map(documents, removeWords, "plan")
documents <- tm_map(documents, removeWords, "urban")
documents <- tm_map(documents, removeWords, "ufmp")
documents <- tm_map(documents, removeWords, "city")
documents <- tm_map(documents, removeWords, "town")
documents <- tm_map(documents, removeWords, "ncity")
documents <- tm_map(documents, removeWords, "management")
documents <- tm_map(documents, removeWords, "strategy")
documents <- tm_map(documents, removeWords, "forest")
documents <- tm_map(documents, removeWords, "forests")

##10. Re-create document-term matrix
DTM <- DocumentTermMatrix(documents)
#check dimensions of dtm
dim(DTM)

##11. Re-run LDA model 
#11.1 random seed
set.seed(9161) # same seed is used to reproduce results
#11.2 model
model <- LDA(DTM, k=8, method="Gibbs", control=list(iter = 500), alpha=0.1)
model
#11.3 rexamine top 10 terms in each topics, preliminary result issues should be corrected
terms(model, 10)
#good, we're happy now! 

######POST PROCESSING############

##12. Post processing Results
#12.1 get results from topic models
model.posterior = posterior(model)
#12.2 calculate probability distributions over corpus vocab - topics
beta = model.posterior$terms
#look at dimensions of object just created above
dim(beta)
#should show 8 topics by number of terms of corpus vocab, should show approx 70K terms
#get probability distribution over corpus vocab, for topic 1...only shows first 15 terms in the corpus vocab
beta[1,1:15]
#12.3 calculate probability distributions of topics in docs
gamma = model.posterior$topics
#look at dimensions of object just created above
dim(gamma)
#should show 51 docs by 8 topics
#get topic distributions for the example document, in this case, document 2
gamma[2,]
#12.4 export data on topic distributions per document for later use
getwd()
write.csv(gamma, "gamma.csv")

#13 Get probability of a word being associated to a topic
#13.1 create beta model
beta.topics <- tidy(model, matrix = "beta")
#13.2 show beta topics
beta.topics
#13.3 grouping terms by topic
beta.top.terms <- beta.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#13.4 display the grouped terms 
beta.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#13.5 make the top 5 terms per topic the name of that topic
top3termsPerTopic <- terms(model, 3) #change number if want less or more terms
#create a dataframe called topic names 
topic.names <- apply(top3termsPerTopic, 2, paste, collapse=" ")
topic.names

#####FRENCH#####
## all the steps are the same as above, the only thing that changes is 
## the pre-processing of words, including word removal

##2. Read Data
#Data as PDFs; files need to be in the same folder
allfiles <- list.files(pattern = "pdf$")
#read only text in PDF files 
all.ufmps <- lapply(allfiles, pdf_text)
#load data as Corpus Vector Source
documents <- Corpus(VectorSource(all.ufmps))
#check if all data are loaded, must be 23 documents
documents

########Pre-Processing############

##3. Basic pre-processing chains to process invalid text
##Ignore "transformation drops documents" results, it doesn't really affect results
#3.1 change all capital letters to lower case
documents <- tm_map(documents, content_transformer(tolower))
#3.2 remove stop words
documents <- tm_map(documents, removeWords, stopwords("french"))
#3.3 remove punctuation but retain hyphenated terms
documents <- tm_map(documents, removePunctuation, preserve_intra_word_dashes = TRUE)
#3.4 remove numbers
documents <- tm_map(documents, removeNumbers)
#3.5 Stemming, or stem words to avoid repeated similar words
#we DO NOT do this because it makes it harder to identify words
#for example, management becomes "manag", and canopies and canopy become "canopi"
#documents <- tm_map(documents, stemDocument, language = "en")
#3.6 remove extra white space from text
documents <- tm_map(documents, stripWhitespace)

##4. Create document-term matrix
DTM <- DocumentTermMatrix(documents)
#4a. We can also create DTM with minimum frequency terms, in this case, 5 mentions
#We do not use this because it changes data significantly 
#minimumFrequency <- 5
#DTM <- DocumentTermMatrix(documents, control = list(bounds = list(global = c(minimumFrequency, Inf))))
#4.1 remove sparse terms, terms that do not occur in many documents, here terms that appear in less than 1% of docs 
#shows same number of words but now only few unique terms
DTM = removeSparseTerms(DTM, .99)
#4.2 Check that all documents still have terms after the preprocessing, remove if not
#this step is KEY! otherwise we can't add the results to the data as the number of rows with data will be different
idx = slam::row_sums(DTM) > 0
DTM = DTM[idx, ]
#look at dimensions of dtm, should show 23 documents 
dim(DTM)
#4.3 Explore the document matrix, get some simple word frequencies 
freq = data.frame(sort(colSums(as.matrix(DTM)), decreasing=TRUE))
#4.3.1 add terms to frequency dataframe from rownames
freq$terms = row.names(freq)
#4.3.2 remove rownames
rownames(freq) = NULL
#4.3.3 change name of frequency column
colnames(freq)[1] = 'frequency'
#4.3.3 check 30 most frequent terms
freq[1:30,]

##5. LDA model BASIC using Gibbs method
#5.1 seet random number generator seed? for Gibbs you have to set it before
set.seed(9161)
#number of topics needs to be specified as K, here we used 8
#the model computes inference via 1000 iterations of Gibbs sampling
#alpha can be changed, it affects the probability distribution of topics in docs
#note that the default alpha is 2.5. A lower alpha will result in higher probabilities. 
#Here we use 0.1, higher probabilities, fits the massive text database
model <- LDA(DTM, k=8, method="Gibbs", control=list(iter = 500), alpha=0.1)
model
#5.2 Examine the top 10 terms in each topics
terms(model, 10)

##Results so far##
#1) the names of cities appear in the topics
#2) symbols like ., -, \u0092s are frequent
#=> these must be removed

##6. Further pre-processing
#6.1 remove common symbols in the files
#toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#Code above doesn't work post-analysis 
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
documents <- tm_map(documents, toSpace, "•")
documents <- tm_map(documents, toSpace, "■")
documents <- tm_map(documents, toSpace, "’")
documents <- tm_map(documents, toSpace, "–")
documents <- tm_map(documents, toSpace, "▪")
documents <- tm_map(documents, toSpace, "—")
documents <- tm_map(documents, toSpace, "‐")
documents <- tm_map(documents, toSpace, "")
documents <- tm_map(documents, toSpace, "")
#6.2 remove names of cities
documents <- tm_map(documents, removeWords, "boucherville")
documents <- tm_map(documents, removeWords, "brossard")
documents <- tm_map(documents, removeWords, "drummondville")
documents <- tm_map(documents, removeWords, "gatineau")
documents <- tm_map(documents, removeWords, "granby")
documents <- tm_map(documents, removeWords, "laval")
documents <- tm_map(documents, removeWords, "levis")
documents <- tm_map(documents, removeWords, "longueuil")
documents <- tm_map(documents, removeWords, "montréal")
documents <- tm_map(documents, removeWords, "côte")
documents <- tm_map(documents, removeWords, "grâce")
documents <- tm_map(documents, removeWords, "dame")
documents <- tm_map(documents, removeWords, "neiges")
documents <- tm_map(documents, removeWords, "outremont")
documents <- tm_map(documents, removeWords, "pointe")
documents <- tm_map(documents, removeWords, "claire")
documents <- tm_map(documents, removeWords, "repentigny")
documents <- tm_map(documents, removeWords, "rosemère")
documents <- tm_map(documents, removeWords, "québecnarrondissement")
documents <- tm_map(documents, removeWords, "saint")
documents <- tm_map(documents, removeWords, "bruno")
documents <- tm_map(documents, removeWords, "hyacinthe")
documents <- tm_map(documents, removeWords, "jean-sur-richelieu")
documents <- tm_map(documents, removeWords, "lambert")
documents <- tm_map(documents, removeWords, "laurent")
documents <- tm_map(documents, removeWords, "léonard")
documents <- tm_map(documents, removeWords, "terrebone")
documents <- tm_map(documents, removeWords, "verdun")

##7. Re-create document-term matrix
DTM <- DocumentTermMatrix(documents)
DTM = removeSparseTerms(DTM, .99)
idx = slam::row_sums(DTM) > 0
DTM = DTM[idx, ]
#look at dimensions of dtm, should show 23 documents 
dim(DTM)

##8. Re-run LDA model 
#8.1 random seed
set.seed(9161) # same seed is used to reproduce results
#8.2 model
model <- LDA(DTM, k=8, method="Gibbs", control=list(iter = 500), alpha=0.1)
model
#8.3 rexamine top 10 terms in each topics, preliminary result issues should be corrected
terms(model, 10)

##Results so far##
#1) all topics are built around the terms arbres, arbre
#2) fill terms are frequent
#=> these  must be removed

##9. Further pre-processing
#9.1 remove other common  words
documents <- tm_map(documents, removeWords, "ddmcom")
documents <- tm_map(documents, removeWords, "plus")
documents <- tm_map(documents, removeWords, "figure")
documents <- tm_map(documents, removeWords, "division")
documents <- tm_map(documents, removeWords, "tome")
documents <- tm_map(documents, removeWords, "aussi")
documents <- tm_map(documents, removeWords, "annexe")
documents <- tm_map(documents, removeWords, "comme")
documents <- tm_map(documents, removeWords, "lors")
documents <- tm_map(documents, removeWords, "devrait")
documents <- tm_map(documents, removeWords, "ainsi")
documents <- tm_map(documents, removeWords, "afin")
documents <- tm_map(documents, removeWords, "être")
documents <- tm_map(documents, removeWords, "nplan")
documents <- tm_map(documents, removeWords, "indice")
documents <- tm_map(documents, removeWords, "m²")
documents <- tm_map(documents, removeWords, "où")
documents <- tm_map(documents, removeWords, "nombre")
#9.2 remove common words that dominate all topics
documents <- tm_map(documents, removeWords, "arbre")
documents <- tm_map(documents, removeWords, "arbres")
documents <- tm_map(documents, removeWords, "foresterie")
documents <- tm_map(documents, removeWords, "forêt")
documents <- tm_map(documents, removeWords, "ville")
documents <- tm_map(documents, removeWords, "urbaine")
documents <- tm_map(documents, removeWords, "urbain")
documents <- tm_map(documents, removeWords, "urbains")
documents <- tm_map(documents, removeWords, "directeur")
documents <- tm_map(documents, removeWords, "politique")
documents <- tm_map(documents, removeWords, "gestion")
documents <- tm_map(documents, removeWords, "bois")
documents <- tm_map(documents, removeWords, "boisés")
documents <- tm_map(documents, removeWords, "mai")
documents <- tm_map(documents, removeWords, "plan")
documents <- tm_map(documents, removeWords, "planification")

##10. Re-create document-term matrix
DTM <- DocumentTermMatrix(documents)
#check dimensions of dtm
dim(DTM)

##11. Re-run LDA model 
#11.1 random seed
set.seed(9161) # same seed is used to reproduce results
#11.2 model
model <- LDA(DTM, k=8, method="Gibbs", control=list(iter = 500), alpha=0.1)
model
#11.3 rexamine top 10 terms in each topics, preliminary result issues should be corrected
terms(model, 10)
#good, we're happy now! 

######POST PROCESSING############

##12. Post processing Results
#12.1 get results from topic models
model.posterior = posterior(model)
#12.2 calculate probability distributions over corpus vocab - topics
beta = model.posterior$terms
#look at dimensions of object just created above
dim(beta)
#should show 8 topics by number of terms of corpus vocab, should show approx 23K terms
#get probability distribution over corpus vocab, for topic 1...only shows first 15 terms in the corpus vocab
beta[1,1:15]
#12.3 calculate probability distributions of topics in docs
gamma = model.posterior$topics
#look at dimensions of object just created above
dim(gamma)
#should show 23 docs by 8 topics
#get topic distributions for the example document, in this case, document 2
gamma[2,]
#12.4 export data on topic distributions per document for later use
getwd()
write.csv(gamma, "gamma.csv")

#13 Get probability of a word being associated to a topic
#13.1 create beta model
beta.topics <- tidy(model, matrix = "beta")
#13.2 show beta topics
beta.topics
#13.3 grouping terms by topic
beta.top.terms <- beta.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#13.4 display the grouped terms 
beta.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#13.5 make the top 5 terms per topic the name of that topic
top3termsPerTopic <- terms(model, 3) #change number if want less or more terms
#create a dataframe called topic names 
topic.names <- apply(top3termsPerTopic, 2, paste, collapse=" ")
topic.names
