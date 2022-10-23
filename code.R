library(dplyr)
library(tm)
library(slam)
library(tokenizers)

load("firm_dataset.Rdata")

## function to remove stopwords within the firm descriptions
remove_stopwords <- function(string) {
  x <- unlist(strsplit(string, " "))
  x <- x[!x %in% stopwords(kind = "en")]
  return(paste(x, collapse = " "))
}

## function calculating the cosine similarity between two values
CosineSimilarity <- function(A, B) {  
  sum(A * B) / sqrt(sum(A^2)*sum(B^2)) 
}

## function calculating the log likelihood for a token
calculate.ll <- function(a, b, c, d){
  e1 <- c*(a+b)/(c+d)
  e2 <- d*(a+b)/(c+d)
  ll <- 2*((a*log(a/e1)) + (b*log(b/e2)))
  return(ll)
}
# a = freq. of a word in the oil corpus
# b = freq. of a word in the non-oil corpus
# c = sum of all words in the oil corpus
# d = sum of all words in the non-oil corpus


## function to append a vector as index to a list
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}


## function to filter the firms with the highest cosine similarities
filter.highest.cosine <- function(vector, names) {
  df.res <- vector %>%
    as_tibble() %>%
    mutate(names) %>%
    arrange(-value) %>%
    head(n=5) %>%
    pull(value, names)
}


## function to calculate the annual return of a firm
annual.return <- function(i) {
  return(i[1]*i[2]*i[3]*i[4]*i[5]*i[6]*i[7]*i[8]*i[9]*i[10]*i[11]*i[12])
}



## cik of firms belonging to the oil and non-oil sector
oil.firms.cik <- select(raw.data, cik, industry.fama.french.49) %>%
  filter(industry.fama.french.49 == "30 Oil") %>%
  pull(cik)

no.oil.firms.cik <- select(raw.data, cik, industry.fama.french.49) %>%
  filter(!industry.fama.french.49 == "30 Oil") %>%
  pull(cik)


## cleaning of the firm description texts
business.desc <- section.1.business %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  gsub("\\b\\w{1,2}\\b", "",.) %>%
  gsub("\\b\\w{21,}\\b", "",.) %>%
  gsub("\\b\\s{2,}\\b", " ",.) %>%
  lapply(., remove_stopwords) %>%
  unlist()


## creating bigrams for each token in the corpus
business.desc.bigram <- tokenize_ngrams(business.desc, n = 2, ngram_delim = "_") %>%
  lapply(., paste, collapse = " ") %>%
  unlist()


## creating dtm's fpr oil and non oil firms
corpus <- Corpus(VectorSource(business.desc.bigram))
dtm <- DocumentTermMatrix(corpus, list(global = c(6,99)))


dtm.oil.firms <- dtm[raw.data$cik %in% oil.firms.cik,]
dtm.no.oil.firms <- dtm[!raw.data$cik %in% oil.firms.cik,]


## computing the term frequency matrix for oil and non oil firms
tf.list.oil.firms <- tibble(term = dtm.oil.firms$dimnames$Terms, 
                               freq.oil = col_sums(dtm.oil.firms)) %>%
  arrange(-freq.oil) %>%
  mutate(freq.rank.oil = c(1:length(dtm.oil.firms$dimnames$Terms)))

tf.list.no.oil.firms <- tibble(term = dtm.no.oil.firms$dimnames$Terms, 
                               freq.no.oil = col_sums(dtm.no.oil.firms))


## joining the term frequency matrixes calculating the log likelhood of the respective tokens
tf.list <- tf.list.oil.firms %>%
  left_join(tf.list.no.oil.firms, by = "term") %>%
  mutate(ll = calculate.ll(freq.oil, 
                           freq.no.oil, 
                           sum(freq.oil), 
                           sum(freq.no.oil))) %>%
  arrange(-ll) %>%
  head(n = 500)


## calculating the cosine similarity values and filtering the 5 firms with the 
## highest similarity value for the respective oil firms
cosine.sim.values = list()
for (i in 1:nrow(dtm.oil.firms)) {
  act.oil.firm = list()
  for (j in 1:nrow(dtm.no.oil.firms)) {
    res = CosineSimilarity(A = dtm.oil.firms[i,], B = dtm.no.oil.firms[j,])
    act.oil.firm <- append(act.oil.firm, res)
  }
  act.oil.firm <- unlist(act.oil.firm)
  act.oil.firm <- filter.highest.cosine(act.oil.firm, no.oil.firms.cik)
  cosine.sim.values <- lappend(cosine.sim.values, act.oil.firm)
}

## storing the cik's of the peer group firms
peer.group.cik <- unique(names(unlist(cosine.sim.values)))

# average return oil firms
return.oil.firms <- raw.data %>%
  filter(cik %in% oil.firms.cik) %>%
  select(contains("return.monthly.NY.m")) %>%
  .[] + 1

return.oil.firms$yr.return = apply(X = return.oil.firms, MARGIN = 1, FUN = annual.return)
print(paste0("Avg. Yearly Return - Oil Firms: ",round((mean(return.oil.firms$yr.return)-1)*100, digits = 2), "%"))


# average return peer group
return.peer.group <- raw.data %>%
  filter(cik %in% peer.group.cik) %>%
  select(contains("return.monthly.NY.m")) %>%
  .[] + 1

return.peer.group$yr.return = apply(X = return.peer.group, MARGIN = 1, FUN = annual.return)
print(paste0("Avg. Yearly Return - Peer Group: +",round((mean(return.peer.group$yr.return)-1)*100, digits = 2), "%"))
