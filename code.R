library(tidyverse)
library(tm)

tweets = read_csv('coffee.csv')

coffee_tweets = tweets$text

#Create a Source
coffee_source = VectorSource(coffee_tweets)

#Create a volatile corpus
coffee_corpus = VCorpus(coffee_source)
