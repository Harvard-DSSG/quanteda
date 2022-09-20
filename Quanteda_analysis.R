#Quanteda Sentiment Analysis and Text Analysis

#First install and load the packages you should have already from previous sections
#install.packages("quanteda")
#install.packages("tidyverse")
library(quanteda)
library(tidyverse)

#Let's use some new corpora and tools for our analysis section. 
install.packages("quanteda.textmodels") 
library(quanteda.textmodels)

install.packages("quanteda.textstats")
library(quanteda.textstats)

#Sentiment analysis

#Take a quick look at the dictionary we'll be using for sentiment analysis
head(data_dictionary_LSD2015)

#Options are: negative, positive, negating a positive, negating a negative.

#Simple example
txt <- "This aggressive policy will not win friends."
tokens(txt) %>% tokens_lookup(dictionary = data_dictionary_LSD2015)
#To show all words and not just those matching sentiment dictionary, add 
#exclusive = FALSE:
tokens(txt) %>% tokens_lookup(dictionary = data_dictionary_LSD2015, exclusive = FALSE)

#We're going to be using the irish budget talks of 2010 corpus for the next few exercises.
#Here are the names of the current docvars within the corpus,
#which consists of 14 documents. 
docvars(data_corpus_irishbudget2010)

#We're going to add a docvar that says if a document is from a party associated 
#with the 2010 gov, or the opposition. First, what parties are there?
unique(data_corpus_irishbudget2010$party)
#FF and Green made up the current government. All other parties were opposition.
#Ready to create our new docvar (2 methods):
docvars(data_corpus_irishbudget2010, "gov_opp") <-
  ifelse(docvars(data_corpus_irishbudget2010, "party") %in% c("FF", "Green"),
         "Government", "Opposition")
#Or a method more familiar to tidyverse fans:
docvars(data_corpus_irishbudget2010)$gov_opp <- ifelse(docvars(data_corpus_irishbudget2010)$party=="FF"
                                                       |docvars(data_corpus_irishbudget2010)$party=="Green",
                                                       "Government", "Opposition")
docvars(data_corpus_irishbudget2010)

# tokenize and apply dictionary
toks_dict <- data_corpus_irishbudget2010 %>%
  tokens() %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015)
head(toks_dict)
# transform to a dfm
dfmat_dict <- dfm(toks_dict)

#What did we get?
head(dfmat_dict)

#Convert our output to a data frame
dict_output <- convert(dfmat_dict, to = "data.frame")

#Add a column to estimate sentiment:
dict_output$sent_score <- log((dict_output$positive +
                                 dict_output$neg_negative + 0.5) /
                                (dict_output$negative +
                                   dict_output$neg_positive+ 0.5))

View(dict_output)
View(docvars(data_corpus_irishbudget2010))

#Combine the data to get a bigger dataframe with the docvars info included
dict_output <- cbind(dict_output, docvars(data_corpus_irishbudget2010))

View(dict_output)

#We're going to do a scatterplot of negative vs positive from dict_output
#to show you the very basics. First, specify data and axes:
library(ggplot2)
myplot <- ggplot(data = dict_output,
                 aes(x = negative, y = positive))
myplot

#Specify a geometry -- you can put aesthetics in there, too. 
myplot +
  geom_point(aes(colour = party, shape=dict_output$gov_opp)) +
  labs(x = "Negative Statements",
       y = "Positive Statements")



#Text statistics
#Updated with new Quanteda standards, just to see
ire_dfm <- data_corpus_irishbudget2010 %>%
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_group(party) %>% #We'll do something with these groups in a bit
  dfm()
head(ire_dfm)

#Calculating frequency of terms (not broken down by group)
ire_freq <- ire_dfm %>% 
  textstat_frequency(n=15, ties_method = "first")
head(ire_freq) #See how it's showing that it's not breaking down by groups?

#Plot (original)
ire_freqplot <- ire_freq %>%
  ggplot(aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() +
  labs(x = "Frequency", y = NULL)
ire_freqplot

#Plot (updated)
ire_freqplot <- ire_freq %>% ggplot(aes(y=reorder(feature, frequency), x=frequency)) +
  geom_col() +
  labs(x = "Frequency", y = NULL)
ire_freqplot


#Incidentally, we can use a neat little add-on Quanteda package for data viz.
install.packages("quanteda.textplots")
library(quanteda.textplots)
#Word cloud (don't do it!)
set.seed(34)
textplot_wordcloud(ire_dfm,
                   comparison = TRUE,
                   max_words = 40)

#Relative Frequency Analysis
#Relative frequency analysis, also called keyness, 
#compares the frequency of words in one sample compared with another.

#Remember our document variable government vs opposition?
docvars(data_corpus_irishbudget2010, "gov_opp") <-
  ifelse(docvars(data_corpus_irishbudget2010, "party") %in%
           c("FF", "Green"), "Government",
         "Opposition")
#Let's compare government to opposition parties by chi^2. First,
#we need to generate a new DFM with tokens grouped by gov/opp,
#not by party.

key_dfm <- data_corpus_irishbudget2010 %>%
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_group(gov_opp) %>% 
  dfm()
head(key_dfm)

#Now we're ready to calculate keyness
?textstat_keyness
ire_keyness <- textstat_keyness(key_dfm,
                                target = "Opposition")
head(ire_keyness)
#You'll understand better with a plot, so let's take a look:
plot_key <- textplot_keyness(ire_keyness,
                             margin = 0.2,
                             n = 10)
plot_key
#In this case, target could have either been government or opposition
#and the graph would just switch what's on top. In other cases where 
#there are more than two categories, it compares all other categories
#to the target.

#Collocation Analysis
#In corpus linguistics, a collocation is a series of words or terms that 
#co-occur more often than would be expected by chance. Of course, we
#have to run these statistics on tokens, not dfms.

#Let's learn by doing:
ire_col <- data_corpus_irishbudget2010 %>%
  tokens() %>%
  textstat_collocations(min_count = 20, tolower = TRUE)
head(ire_col, 10)

#Activity -- do this again, but remove stopwords and stem words. 
ire_toks_adjusted <- data_corpus_irishbudget2010 %>%
  tokens() %>%
  tokens_remove(pattern = stopwords("en")) %>%
  tokens_wordstem()
col_adjusted <- ire_toks_adjusted %>%
  textstat_collocations(min_count = 5, tolower = FALSE)

#Lexical diversity
#Compute lexical diversity through with the type-token ratio.

ire_dfm <- data_corpus_irishbudget2010 %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>% 
  dfm()

# estimate Type-Token Ratio
ire_lexdiv <- textstat_lexdiv(ire_dfm, measure = "TTR")
View(ire_lexdiv)
#Let's add the document variables so we have more to work with
df_lexdiv <- cbind(ire_lexdiv, docvars(ire_dfm))
head(df_lexdiv)

#Plot the type-token ratio
ggplot(ire_lexdiv, aes(x = TTR,
                      y = reorder(document, TTR))) +
  geom_col() +
  labs(y = NULL)
