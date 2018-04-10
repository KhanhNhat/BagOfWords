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

#Study stemDocument() from tm package
text_data = 'In a complicated haste, Tom rushed to fix a new complication, too complicatedly.'

#Firstly, remove punctuation
(rm_punc = removePunctuation(text_data))

#Secondly, create a vector of word from this:
(char_vector = unlist(str_split(rm_punc, ' ')))

#Then, create a stem words. However, some of them are not real, in this case is 'complic'
(stem_doc = stemDocument(char_vector))

#After that, create a dictionary for stem_doc:
stem_dict = c("In", "a", "complicate", "haste", "Tom", "rush", "to", "fix", "new", "too")

#Finally, apply stemCompletion() to stem_doc to get a meaning words from that.
(stem_complete = stemCompletion(stem_doc, stem_dict))

#Create TDM (Term Document Matrix)
coffee_tdm = TermDocumentMatrix(coffee_corpus)

#Convert TDM to matrix
coffee_m = as.matrix(coffee_tdm)

#Calculate the row sum:
term_frequency = rowSums(coffee_m)

#Sort term_frequency:
term_frequency = sort(term_frequency, decreasing = TRUE)

#Display barplot for the top 15
barplot(term_frequency[1:15], col = 'tan', las = 2)

#Now try to make a word cloud.
library(wordcloud)

term_frequency_df = data.frame(term = names(term_frequency), num = term_frequency)

wordcloud(words = term_frequency_df$term, freq = term_frequency_df$num, max.words = 100, colors = 'blue')

#Create a clean corpus function to make text resource be properly
clean_corpus = function(corpus){
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, c(stopwords('en'), 'coffee'))
  
  return(corpus)
}

#Now, clean up corpus before text analysis
clean_coffee = clean_corpus(coffee_corpus)

clean_coffee_tdm = TermDocumentMatrix(clean_coffee)

clean_coffee_m = as.matrix(clean_coffee_tdm)

clean_coffee_df = as.data.frame(clean_coffee_m)

clean_coffee_df = rownames_to_column(clean_coffee_df, var = 'Term')

clean_coffee_df = clean_coffee_df %>% mutate(numOfTerm = rowSums(clean_coffee_df[, -1]))

clean_coffee_df = arrange(clean_coffee_df, desc(numOfTerm))

clean_coffee_df[1:10, c(1, 1002)]

wordcloud(words = clean_coffee_df$Term, freq = clean_coffee_df$numOfTerm,
          max.words = 50, colors = c('grey50', 'deepskyblue', 'blue'))

#Using color palette from RColorBrewer package:
library(RColorBrewer)
colSelect = brewer.pal(9, 'Blues')
colSelect = colSelect[4:9]
wordcloud(words = clean_coffee_df$Term, freq = clean_coffee_df$numOfTerm,
          max.words = 50, colors = colSelect)
