library(tidyverse)
library(tm)
library(lubridate)
library(qdap)

tweets = read_csv('coffee.csv')

coffee_tweets = tweets$text

#Create a Source
coffee_source = VectorSource(coffee_tweets)

#Create a volatile corpus
coffee_corpus = VCorpus(coffee_source)

#Create an example dataframe of text from tweets
example_text = tweets[1:10, c(1, 2, 5)]
example_text$created = mdy_hm(example_text$created)
colnames(example_text) = c('doc_id', 'text', 'date_time')

#Create a dataframe source
df_source = DataframeSource(example_text)

#Create a VCorpus from df_source
df_corpus = VCorpus(df_source)

