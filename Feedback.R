# ----------------------------------------------------------------------------------------------------------------

# Team 4: Mentorship Program
# PMI SFBAC NGL Let's Jam Challenge 2024
# Michelle Kae Celine Jo-anne Bantugon
# Data Analyst
# July 20, 2024

# ----------------------------------------------------------------------------------------------------------------

# Libraries
library(readxl)
library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)
library(readr)
library(ggplot2)
library(openxlsx)

-------------------------------------------------------------------------------
# Feedback TF_IDF

feedback <- read_excel("01 PMI/Feedback.xlsx")
  
# Counting the total missing values per column
colSums(is.na(feedback))
  
# for topic
colnames(feedback)[13] <- "text" #Topic

## Tf -df
bigram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=2) %>%
  separate(quadrogram, c("word1", "word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigram_counts <- bigram %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts


bigram_united <- bigram %>%
  unite(bigram, word1, word2,   sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(Year, bigram) %>%
  bind_tf_idf(bigram, Year, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Heatmap (TOPIC)
library(reshape2)

# Example data frame
bigram_tf_idf <- data.frame(
  Year = c(2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2022, 2022),
  bigram = c("active pmp", "creating presentation", "development track", "leadership development", 
             "mentor assigned", "missed opportunity", "product management", "product manager", 
             "time commitment", "agile pm"),
  n = c(1, 1, 1, 1, 1, 1, 1, 1, 3, 2),
  tf = c(0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.04166667, 0.03191489, 0.02127660),
  idf = c(1.3862944, 1.3862944, 1.3862944, 1.3862944, 1.3862944, 1.3862944, 1.3862944, 1.3862944, 1.3862944, 1.3862944),
  tf_idf = c(0.057762265, 0.057762265, 0.057762265, 0.057762265, 0.057762265, 0.057762265, 0.057762265, 0.057762265, 0.044243437, 0.029495625)
)

# Convert data frame to long format for heatmap
bigram_long <- melt(bigram_tf_idf, id.vars = c("Year", "bigram"), measure.vars = "tf_idf")

# Create heatmap
ggplot(bigram_long, aes(x = bigram, y = as.factor(Year), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "TF-IDF Heatmap of Mentorship Topics", x = "Bigram", y = "Year", fill = "TF-IDF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))  # Remove gridlines

# HEATMAP (MENTOR)
# Combine previous and new data frames
new_data <- data.frame(
  Year = rep(2023, 10),
  bigram = c("consistent linkedin", "cultivate essential", "essential skills", "extremely fortunate", 
             "incredibly encouraging", "linkedin presence", "mentor learned", "mentoring experience", 
             "responsible motivating", "smart goals"),
  n = rep(1, 10),
  tf = rep(0.01694915, 10),
  idf = rep(1.3862944, 10),
  tf_idf = rep(0.023496515, 10)
)

# Convert data frame to long format for heatmap
new_data_long <- melt(new_data, id.vars = c("Year", "bigram"), measure.vars = "tf_idf")

# Create heatmap for 2023 mentorship topics
ggplot(new_data_long, aes(x = bigram, y = as.factor(Year), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(title = "TF-IDF Heatmap of Mentorship Mentors", x = "Bigram", y = "Year", fill = "TF-IDF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))  # Remove gridlines



# TESTIMONIALS
# New data for 2024 and 2020 mentorship topics
new_data <- data.frame(
  Year = c(2024, 2020, 2020, 2024, 2024, 2024, 2024, 2024),
  bigram = c("pmi mentorship", "mary ann", "program management", "1 mentorship", 
             "400 project", "bus 400", "career growth", "career trajectory"),
  n = c(2, 3, 3, 1, 1, 1, 1, 1),
  tf = c(0.032786885, 0.01744186, 0.01744186, 0.016393443, 
         0.016393443, 0.016393443, 0.016393443, 0.016393443),
  idf = rep(1.3862944, 8),
  tf_idf = c(0.04545227, 0.02417955, 0.02417955, 0.02272614, 
             0.02272614, 0.02272614, 0.02272614, 0.02272614)
)

# Convert data frame to long format for heatmap
new_data_long <- melt(new_data, id.vars = c("Year", "bigram"), measure.vars = "tf_idf")

# Create heatmap for 2023 mentorship topics
ggplot(new_data_long, aes(x = bigram, y = as.factor(Year), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "TF-IDF Heatmap of Mentorship Testimonials", x = "Bigram", y = "Year", fill = "TF-IDF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))  # Remove gridlines



##MENTEES
# Data for 2024 mentorship topics
data_2024 <- data.frame(
  Year = rep(2024, 10),
  bigram = c("project management", "biotech industry", "biotech specifically", 
             "career goal", "career path", "communication branding", 
             "corporate career", "current situation", "effectively mentor", 
             "emotional intelligence"),
  n = c(2, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  tf = c(0.05714286, 0.02857143, 0.02857143, 0.02857143, 0.02857143,
         0.02857143, 0.02857143, 0.02857143, 0.02857143, 0.02857143),
  idf = rep(1.3862944, 10),
  tf_idf = c(0.079216821, 0.039608410, 0.039608410, 0.039608410, 0.039608410,
             0.039608410, 0.039608410, 0.039608410, 0.039608410, 0.039608410)
)

# Convert data frame to long format for heatmap
data_2024_long <- melt(data_2024, id.vars = c("Year", "bigram"), measure.vars = "tf_idf")

# Create heatmap for 2024 mentorship topics
ggplot(data_2024_long, aes(x = bigram, y = as.factor(Year), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "lightgreen") +
  labs(title = "TF-IDF Heatmap of Mentorship Mentees", x = "Bigram", y = "Year", fill = "TF-IDF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))  # Remove gridlines







feedback <- read_csv("01 PMI/Motivation.csv")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[4] <- "text" #Motivation

feedback_bigrams <- feedback %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigram is industry std; ngrams (ntokens)

feedback_bigrams #We want to see the bigrams (words that appear together, "pairs")

feedback_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# separate the pair to word 1 and word 2
#to remove stop words and spaces from the bigram data, we need to use the separate function:

bigrams_separated <- feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": (these are unique which create a challenge)
# looking at the single token we dont see the big picture(more business insights)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3,word4, sort = TRUE)
#want to see the new bigrams
quadrogram_counts


# tf-df(2)

## Tf -df
bigram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=2) %>%
  separate(quadrogram, c("word1", "word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigram_counts <- bigram %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts


bigram_united <- bigram %>%
  unite(bigram, word1, word2,   sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(Year, bigram) %>%
  bind_tf_idf(bigram, Year, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


## Tf -df (3)
quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=3) %>%
  separate(quadrogram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3, sort = TRUE)
#want to see the new bigrams
quadrogram_counts


quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3,  sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(Year, quadrogram) %>%
  bind_tf_idf(quadrogram, Year, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

############################
##########################
# Motivation
################
feedback <- read_csv("01 PMI/Motivation.csv")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[3] <- "text" #Motivation

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

length_token <- feedback %>%
  unnest_tokens(word, text)

Y200 <- length_token %>%
  filter(Year == "2023")

afinn_y200 <- Y200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_y200 <- bind_rows(
  Y200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Y200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_y200, bing_and_nrc_y200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_length <- Y200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_y2022 <- bing_counts_length %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for length", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_y2022










#######################

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[14] <- "text" #Mentor


feedback_bigrams <- feedback %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigram is industry std; ngrams (ntokens)

feedback_bigrams #We want to see the bigrams (words that appear together, "pairs")

feedback_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# separate the pair to word 1 and word 2
#to remove stop words and spaces from the bigram data, we need to use the separate function:

bigrams_separated <- feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": (these are unique which create a challenge)
# looking at the single token we dont see the big picture(more business insights)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3,word4, sort = TRUE)
#want to see the new bigrams
quadrogram_counts

########################################
#Mentee

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[13] <- "text" #Mentee


feedback_bigrams <- feedback %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigram is industry std; ngrams (ntokens)

feedback_bigrams #We want to see the bigrams (words that appear together, "pairs")

feedback_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# separate the pair to word 1 and word 2
#to remove stop words and spaces from the bigram data, we need to use the separate function:

bigrams_separated <- feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": (these are unique which create a challenge)
# looking at the single token we dont see the big picture(more business insights)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3,word4, sort = TRUE)
#want to see the new bigrams
quadrogram_counts

############################
########################################
#Mentee

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[14] <- "text" #Additional


feedback_bigrams <- feedback %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigram is industry std; ngrams (ntokens)

feedback_bigrams #We want to see the bigrams (words that appear together, "pairs")

feedback_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# separate the pair to word 1 and word 2
#to remove stop words and spaces from the bigram data, we need to use the separate function:

bigrams_separated <- feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": (these are unique which create a challenge)
# looking at the single token we dont see the big picture(more business insights)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3,word4, sort = TRUE)
#want to see the new bigrams
quadrogram_counts

##################
# TEstimonial
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[15] <- "text" #Testimonial


feedback_bigrams <- feedback %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigram is industry std; ngrams (ntokens)

feedback_bigrams #We want to see the bigrams (words that appear together, "pairs")

feedback_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# separate the pair to word 1 and word 2
#to remove stop words and spaces from the bigram data, we need to use the separate function:

bigrams_separated <- feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": (these are unique which create a challenge)
# looking at the single token we dont see the big picture(more business insights)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3,word4, sort = TRUE)
#want to see the new bigrams
quadrogram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=3) %>%
  separate(quadrogram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3, sort = TRUE)
#want to see the new bigrams
quadrogram_counts

#############################################

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(Year, quadrogram) %>%
  bind_tf_idf(quadrogram, Year, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

#########################
# Length
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[5] <- "text" #Length


feedback_bigrams <- feedback %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigram is industry std; ngrams (ntokens)

feedback_bigrams #We want to see the bigrams (words that appear together, "pairs")

feedback_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# separate the pair to word 1 and word 2
#to remove stop words and spaces from the bigram data, we need to use the separate function:

bigrams_separated <- feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": (these are unique which create a challenge)
# looking at the single token we dont see the big picture(more business insights)
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

quadrogram <- feedback %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=3) %>%
  separate(quadrogram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
  count(word1, word2,word3, sort = TRUE)
#want to see the new bigrams
quadrogram_counts


quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3,  sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(Year, quadrogram) %>%
  bind_tf_idf(quadrogram, Year, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

##############################
# Length
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[5] <- "text" #Length

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

feedback_senti <- feedback %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Joining nrcsurprise with tidy format for length
nrc_counts_length <- feedback_senti %>%
  inner_join(nrcsurprise) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# for afinn
afinn_length <- feedback_senti %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

# binding rows for bing and nrc
bing_and_nrc_length <- bind_rows(
  nrc_counts_length%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  nrc_counts_length %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative)



# binding rows for afinn, bing, and nrc
bind_rows(afinn_length, bing_and_nrc_length) %>%
  ggplot(aes(method, sentiment, fill = method)) +
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  labs(x = "Comparison of Scores in Apartment (Upper Quantile)")

# Most common positive and negative words for Apartment  
bing_counts_apt_upper <- tidy_apt_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_apt <- bing_counts_apt_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for apartment with high ratings", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())


################
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[5] <- "text" #Length

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

length_token <- feedback %>%
  unnest_tokens(word, text)

Y200 <- length_token %>%
  filter(Year == "2023")

afinn_y200 <- Y200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_y200 <- bind_rows(
  Y200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Y200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_y200, bing_and_nrc_y200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_length <- Y200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_y2022 <- bing_counts_length %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for length", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_y2022

##########################
# mentor
################
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[12] <- "text" #mentor

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

length_token <- feedback %>%
  unnest_tokens(word, text)

Y200 <- length_token %>%
  filter(Year == "2023")

afinn_y200 <- Y200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_y200 <- bind_rows(
  Y200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Y200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_y200, bing_and_nrc_y200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_length <- Y200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_y2022 <- bing_counts_length %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for length", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_y2022

##########################
# Sentimental Analysis
##########################
# Testimonial
################
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[15] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

T200 <- test_token %>%
  filter(Year == "2022")

afinn_T200 <- T200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_T200 <- bind_rows(
  T200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  T200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_T200, bing_and_nrc_T200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_T200 <- T200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_T2022 <- bing_counts_T200 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Testimonial (2022)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_T2022


# 2023
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[15] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

T2023 <- test_token %>%
  filter(Year == "2023")

afinn_T2023 <- T2023 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_T2023 <- bind_rows(
  T2023%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  T2023 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_T2023, bing_and_nrc_T2023) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_T2023 <- T2023 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_T2023 <- bing_counts_T2023 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment for Testimonial (2023)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_T2023

# 2024

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[15] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

T2024 <- test_token %>%
  filter(Year == "2024")

afinn_T2024 <- T2024 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_T2024 <- bind_rows(
  T2024%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  T2024 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_T2024, bing_and_nrc_T2024) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_T2024 <- T2024 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_T2024 <- bing_counts_T2024 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment for Testimonial (2024)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_T2024


## ADDITIONALS
##########################
# Testimonial
################
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[14] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

A200 <- test_token %>%
  filter(Year == "2022")

afinn_A200 <- A200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_A200 <- bind_rows(
  A200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  A200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_A200, bing_and_nrc_A200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_A200 <- A200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_A2022 <- bing_counts_A200 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Additional Feedback (2022)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_A2022


# 2023
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[14] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

A2023 <- test_token %>%
  filter(Year == "2023")

afinn_A2023 <- A2023 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_A2023 <- bind_rows(
  A2023%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  A2023 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_A2023, bing_and_nrc_A2023) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_A2023 <- A2023 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_A2023 <- bing_counts_A2023 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment for Additional Feedback (2023)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_A2023

# 2024

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[14] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

A2024 <- test_token %>%
  filter(Year == "2024")

afinn_A2024 <- A2024 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_A2024 <- bind_rows(
  A2024%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  A2024 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_A2024, bing_and_nrc_A2024) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_A2024 <- A2024 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_A2024 <- bing_counts_A2024 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Additional Feedback (2024)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_A2024 

library(gridExtra)
# Combine plots into a single figure
grid.arrange(bing_A2024, bing_A2023, bing_A2022, ncol = 3)


# Leadership dev
##########################
# Testimonial
################
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[7] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

LD200 <- test_token %>%
  filter(Year == "2022")

afinn_LD200 <- LD200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_LD200 <- bind_rows(
  LD200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  LD200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_LD200, bing_and_nrc_LD200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_LD200 <- LD200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_LD2022 <- bing_counts_LD200 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Leadership Development (2022)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_LD2022


# 2023
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[7] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

LD2023 <- test_token %>%
  filter(Year == "2023")

afinn_LD2023 <- LD2023 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_LD2023 <- bind_rows(
  LD2023%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  LD2023 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_LD2023, bing_and_nrc_LD2023) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_LD2023 <- LD2023 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_LD2023 <- bing_counts_LD2023 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment for Leadership Development (2023)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_LD2023

# 2024

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[7] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

LD2024 <- test_token %>%
  filter(Year == "2024")

afinn_LD2024 <- LD2024 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_LD2024 <- bind_rows(
  LD2024%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  LD2024 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_LD2024, bing_and_nrc_LD2024) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_LD2024 <- LD2024 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_LD2024 <- bing_counts_LD2024 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Leadership Development (2024)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_LD2024 

# Length
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[5] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

L200 <- test_token %>%
  filter(Year == "2022")

afinn_L200 <- L200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_L200 <- bind_rows(
  L200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  L200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_L200, bing_and_nrc_L200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_L200 <- L200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_L2022 <- bing_counts_L200 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Length (2022)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_L2022


# 2023
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[5] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

L2023 <- test_token %>%
  filter(Year == "2023")

afinn_L2023 <- L2023 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_L2023 <- bind_rows(
  L2023%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  L2023 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_L2023, bing_and_nrc_L2023) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_L2023 <- L2023 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_L2023 <- bing_counts_L2023 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment for Length (2023)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_L2023

# 2024

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[5] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

L2024 <- test_token %>%
  filter(Year == "2024")

afinn_L2024 <- L2024 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_L2024 <- bind_rows(
  L2024%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  L2024 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_L2024, bing_and_nrc_L2024) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_L2024 <- L2024 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_L2024 <- bing_counts_L2024 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Length (2024)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_L2024 


# Mentor
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[12] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

M200 <- test_token %>%
  filter(Year == "2022")

afinn_M200 <- M200 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_M200 <- bind_rows(
  M200%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  M200 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_M200, bing_and_nrc_M200) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_M200 <- M200 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_M2022 <- bing_counts_M200 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Mentor (2022)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_M2022


# 2023
feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[12] <- "text" #Additional

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

M2023 <- test_token %>%
  filter(Year == "2023")

afinn_M2023 <- M2023 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_M2023 <- bind_rows(
  M2023%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  M2023 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_M2023, bing_and_nrc_M2023) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_M2023 <- M2023 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_M2023 <- bing_counts_M2023 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment for Mentor (2023)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_M2023

# 2024

feedback <- read_excel("01 PMI/Feedback.xlsx")

# Counting the total missing values per column
colSums(is.na(feedback))

# for mentor
colnames(feedback)[12] <- "text" #Testimonial

# Comparing different sentiment libraries
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

test_token <- feedback %>%
  unnest_tokens(word, text)

M2024 <- test_token %>%
  filter(Year == "2024")

afinn_M2024 <- M2024 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_M2024 <- bind_rows(
  M2024%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  M2024 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_M2024, bing_and_nrc_M2024) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for Apartment  
bing_counts_M2024 <- M2024 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating visualizations for Apartment
bing_M2024 <- bing_counts_M2024 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Mentor (2024)", x=NULL)+
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  
  theme(panel.grid = element_blank())
bing_M2024 
