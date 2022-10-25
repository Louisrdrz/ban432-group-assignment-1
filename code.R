library(dplyr)
library(tidyr)
library(tm)
library(slam)
library(tokenizers)
library(Metrics)
library(mcreplicate)
library(doParallel)
library(ggplot2)

load("firm_dataset.Rdata")

## function to remove stopwords within the firm descriptions
remove_stopwords <- function(string) {
  x <- unlist(strsplit(string, " "))
  x <- x[!x %in% stopwords(kind = "en")]
  return(paste(x, collapse = " "))
}

## function calculating the cosine similarity between two values
CosineSimilarity <- function(A, B) {
  sum(A * B) / sqrt(sum(A^2) * sum(B^2))
}

## function calculating the log likelihood for a token
calculate.ll <- function(a, b, c, d) {
  e1 <- c * (a + b) / (c + d)
  e2 <- d * (a + b) / (c + d)
  ll <- 2 * ((a * log(a / e1)) + (b * log(b / e2)))
  return(ll)
}
# a = freq. of a word in the oil corpus
# b = freq. of a word in the non-oil corpus
# c = sum of all words in the oil corpus
# d = sum of all words in the non-oil corpus


## function to append a vector as index to a list
lappend <- function(lst, ...) {
  lst <- c(lst, list(...))
  return(lst)
}


## function to filter the firms with the highest cosine similarities
filter.highest.cosine <- function(vector, names) {
  df.res <- vector %>%
    as_tibble() %>%
    mutate(names) %>%
    arrange(-value) %>%
    head(n = 5) %>%
    pull(value, names)
}


## function to calculate the annual return of a firm
annual.return <- function(i) {
  return(i[1] * i[2] * i[3] * i[4] * i[5] * i[6] * i[7] * i[8] * i[9] * i[10] * i[11] * i[12])
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
  gsub("\\b\\w{1,2}\\b", "", .) %>% # Remove terms with less than 3 letters
  gsub("\\b\\w{21,}\\b", "", .) %>% # Remove terms with more than 20 letters
  gsub("\\b\\s{2,}\\b", " ", .) %>% # Remove excess whitespaces
  lapply(., remove_stopwords) %>% # Remove stopwords in every description
  unlist()


## creating bigrams for each token in the corpus
business.desc.bigram <- tokenize_ngrams(business.desc, n = 2, ngram_delim = "_") %>%
  lapply(., paste, collapse = " ") %>%
  unlist()


## creating dtm's fpr oil and non oil firms
corpus <- Corpus(VectorSource(business.desc.bigram))
dtm <- DocumentTermMatrix(corpus, list(global = c(6, 99)))


dtm.oil.firms <- dtm[raw.data$cik %in% oil.firms.cik, ]
dtm.no.oil.firms <- dtm[!raw.data$cik %in% oil.firms.cik, ]


## computing the term frequency matrix for oil and non oil firms
tf.list.oil.firms <- tibble(
  term = dtm.oil.firms$dimnames$Terms,
  freq.oil = col_sums(dtm.oil.firms)
) %>%
  arrange(-freq.oil) %>%
  mutate(freq.rank.oil = c(1:length(dtm.oil.firms$dimnames$Terms)))

tf.list.no.oil.firms <- tibble(
  term = dtm.no.oil.firms$dimnames$Terms,
  freq.no.oil = col_sums(dtm.no.oil.firms)
)


## joining the term frequency matrixes calculating the log likelhood of the respective tokens
tf.list <- tf.list.oil.firms %>%
  left_join(tf.list.no.oil.firms, by = "term") %>%
  mutate(ll = calculate.ll(
    freq.oil,
    freq.no.oil,
    sum(freq.oil),
    sum(freq.no.oil)
  )) %>%
  arrange(-ll) %>%
  head(n = 500)

tf.highest.ll <-
  tf.list %>%
  pull(term)

## recreate document term matrixes with only 500 terms with highest log-likelihood
dtm <- DocumentTermMatrix(corpus, list(dictionary = tf.highest.ll))
dtm.oil.firms <- dtm[raw.data$cik %in% oil.firms.cik, ]
dtm.no.oil.firms <- dtm[!raw.data$cik %in% oil.firms.cik, ]

## calculating the cosine similarity values and filtering the 5 firms with the
## highest similarity value for the respective oil firms
cosine.sim.values <- list()
for (i in 1:nrow(dtm.oil.firms)) {
  act.oil.firm <- list()
  for (j in 1:nrow(dtm.no.oil.firms)) {
    res <- CosineSimilarity(A = dtm.oil.firms[i, ], B = dtm.no.oil.firms[j, ])
    act.oil.firm <- append(act.oil.firm, res)
  }
  act.oil.firm <- unlist(act.oil.firm)
  act.oil.firm <- filter.highest.cosine(act.oil.firm, no.oil.firms.cik)
  cosine.sim.values <- lappend(cosine.sim.values, act.oil.firm)
}

## storing the cik's of the peer group firms
peer.group.cik <- names(unlist(cosine.sim.values))

## average return oil firms
oil.firms.monthly.returns.averages <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

oil.firms.monthly.returns <-
  raw.data %>%
  filter(cik %in% oil.firms.cik) %>%
  select(contains("return.monthly.NY.m"))

for (i in 1:ncol(oil.firms.monthly.returns)) {
  oil.firms.monthly.returns %>%
    pull(i) %>%
    mean() -> oil.firms.monthly.returns.averages[i]
}

## average return of peer firms
peer.firms.monthly.returns.averages <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

peer.firms.monthly.returns <-
  raw.data %>%
  filter(cik %in% peer.group.cik) %>%
  select(contains("return.monthly.NY.m"))

for (i in 1:ncol(peer.firms.monthly.returns)) {
  peer.firms.monthly.returns %>%
    pull(i) %>%
    mean() -> peer.firms.monthly.returns.averages[i]
}

## Combine average returns per month of both portfolios in one df
returns <- tibble(
  month = month.abb,
  oil = oil.firms.monthly.returns.averages,
  peers = peer.firms.monthly.returns.averages
)
returns

## Calculate Root Mean Squared Error
rmse(oil.firms.monthly.returns.averages, peer.firms.monthly.returns.averages)

## Calculate annual returns of both portfolios
annual.return(oil.firms.monthly.returns.averages)
annual.return(peer.firms.monthly.returns.averages)



################### Same for unigrams
business.desc.unigram <- tokenize_ngrams(business.desc, n = 1, ngram_delim = "_") %>%
  lapply(., paste, collapse = " ") %>%
  unlist()

corpus.uni <- Corpus(VectorSource(business.desc.unigram))
dtm.uni <- DocumentTermMatrix(corpus.uni, list(global = c(6, 99)))

dtm.oil.firms.uni <- dtm.uni[raw.data$cik %in% oil.firms.cik, ]
dtm.no.oil.firms.uni <- dtm.uni[!raw.data$cik %in% oil.firms.cik, ]


## computing the term frequency matrix for oil and non oil firms
tf.list.oil.firms.uni <- tibble(
  term = dtm.oil.firms.uni$dimnames$Terms,
  freq.oil = col_sums(dtm.oil.firms.uni)
) %>%
  arrange(-freq.oil) %>%
  mutate(freq.rank.oil = c(1:length(dtm.oil.firms.uni$dimnames$Terms)))

tf.list.no.oil.firms.uni <- tibble(
  term = dtm.no.oil.firms.uni$dimnames$Terms,
  freq.no.oil = col_sums(dtm.no.oil.firms.uni)
)


## joining the term frequency matrixes calculating the log likelhood of the respective tokens
tf.list.uni <- tf.list.oil.firms.uni %>%
  left_join(tf.list.no.oil.firms.uni, by = "term") %>%
  mutate(ll = calculate.ll(
    freq.oil,
    freq.no.oil,
    sum(freq.oil),
    sum(freq.no.oil)
  )) %>%
  arrange(-ll) %>%
  head(n = 500)

tf.highest.ll.uni <-
  tf.list.uni %>%
  pull(term)

## recreate document term matrixes with only 500 terms with highest log-likelihood
dtm.uni <- DocumentTermMatrix(corpus.uni, list(dictionary = tf.highest.ll.uni))
dtm.oil.firms.uni <- dtm.uni[raw.data$cik %in% oil.firms.cik, ]
dtm.no.oil.firms.uni <- dtm.uni[!raw.data$cik %in% oil.firms.cik, ]

## calculating the cosine similarity values and filtering the 5 firms with the
## highest similarity value for the respective oil firms
cosine.sim.values.uni <- list()
for (i in 1:nrow(dtm.oil.firms.uni)) {
  act.oil.firm <- list()
  for (j in 1:nrow(dtm.no.oil.firms.uni)) {
    res <- CosineSimilarity(A = dtm.oil.firms[i, ], B = dtm.no.oil.firms[j, ])
    act.oil.firm <- append(act.oil.firm, res)
  }
  act.oil.firm <- unlist(act.oil.firm)
  act.oil.firm <- filter.highest.cosine(act.oil.firm, no.oil.firms.cik)
  cosine.sim.values.uni <- lappend(cosine.sim.values, act.oil.firm)
}

## storing the cik's of the peer group firms
peer.group.cik.uni <- names(unlist(cosine.sim.values.uni))

## average return oil firms
oil.firms.monthly.returns.averages.uni <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

oil.firms.monthly.returns.uni <-
  raw.data %>%
  filter(cik %in% oil.firms.cik) %>%
  select(contains("return.monthly.NY.m"))

for (i in 1:ncol(oil.firms.monthly.returns.uni)) {
  oil.firms.monthly.returns.uni %>%
    pull(i) %>%
    mean() -> oil.firms.monthly.returns.averages.uni[i]
}

## average return of peer firms
peer.firms.monthly.returns.averages.uni <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

peer.firms.monthly.returns.uni <-
  raw.data %>%
  filter(cik %in% peer.group.cik.uni) %>%
  select(contains("return.monthly.NY.m"))

for (i in 1:ncol(peer.firms.monthly.returns.uni)) {
  peer.firms.monthly.returns.uni %>%
    pull(i) %>%
    mean() -> peer.firms.monthly.returns.averages.uni[i]
}

## Combine average returns per month of both portfolios in one df
returns.uni <- tibble(
  month = month.abb,
  oil = oil.firms.monthly.returns.averages.uni,
  peers = peer.firms.monthly.returns.averages.uni
)
returns.uni

## Calculate Root Mean Squared Error
rmse(oil.firms.monthly.returns.averages, peer.firms.monthly.returns.averages)
rmse(oil.firms.monthly.returns.averages.uni, peer.firms.monthly.returns.averages.uni)

## Calculate annual returns of both portfolios
annual.return(oil.firms.monthly.returns.averages.uni)
annual.return(peer.firms.monthly.returns.averages.uni)



################### Optional Exercise

alternative.firm.portfolios <- mc_replicate(10000,
  sample(
    filter(raw.data, cik %in% no.oil.firms.cik) %>% pull(cik),
    90
  ),
  mc.cores = detectCores() - 1
) %>%
  as_tibble()


portfolio.returns <- data.frame(
  portfolio = integer(),
  rmse = double()
)

for (i in 1:ncol(alternative.firm.portfolios)) {
  act.firms.monthly.returns <-
    filter(raw.data, cik %in% (unique(alternative.firm.portfolios[, i]) %>% pull())) %>%
    select(contains("return.monthly.NY.m"))
  portfolio.returns.act <- tibble(
    month = numeric(),
    month.avg = numeric()
  )
  for (j in 1:ncol(act.firms.monthly.returns)) {
    act.firms.monthly.returns %>%
      pull(j) %>%
      mean() -> act.month.mean
    portfolio.returns.act <- bind_rows(portfolio.returns.act, tibble(month = j, month.avg = act.month.mean))
  }
  portfolio.returns <- bind_rows(
    portfolio.returns,
    tibble(portfolio = i, rmse = rmse(oil.firms.monthly.returns.averages.uni, mean(portfolio.returns.act$month.avg)))
  )
}


portfolio.returns %>%
  ggplot(aes(x = portfolio, y = rmse)) + geom_col(aes(fill=rmse))
