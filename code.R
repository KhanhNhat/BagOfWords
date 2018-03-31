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

#Create a TDM (Term Document Matrix) from corpus.
coffee_tdm = TermDocumentMatrix(coffee_corpus)

#Then convert it to matrix type
coffee_m = as.matrix(coffee_tdm)

#How about dataframe?
coffee_df = as.data.frame(coffee_m)

#Convert row names to colummn:
coffee_df = rownames_to_column(coffee_df, 'Term')

#Create a new column for dataframe
coffee_df = coffee_df %>% mutate(ObservedCount = rowSums(select(coffee_df, -1)))

#Arrange this dataframe:
coffee_df = coffee_df %>% arrange(desc(ObservedCount))

#Get the top 15 observed terms from this dataframe
top15Term = coffee_df[1:15, c(1, 1002)]

#Create a bar plot for these terms
#reorder() function is used to make geom_col() does not sort the x axis
#In this line, it sorts term base on reducing the value of ObservedCount.
#If want to increase, just delete '-' before ObservedCount.
ggplot(top15Term, aes(x = reorder(Term, -ObservedCount), y = ObservedCount)) + geom_col()
