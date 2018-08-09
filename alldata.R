#How words have changed through years

M_author_words.df <- data.frame(M$DE) 
M_author_words.df$M.ID = data.frame(M$ID)
M_author_words.df$M.PY = data.frame(as.numeric(M$PY))
corpusAll <- Corpus(VectorSource(M_author_words.df))

clean_corpusAll <- tm_map(corpusAll, tolower)
clean_corpusAll <- tm_map(clean_corpusAll, removeNumbers)
clean_corpusAll <- tm_map(clean_corpusAll, removeWords, c(stopwords("english"), "ieee","doi","anonymous","acm","automat","comput","software","conference","lect","notes","DOI","int","proceedings", "web","adhoc","comp","engineering","computer","sys","workshops","softw","visual","conf"))
clean_corpusAll <- tm_map(clean_corpusAll, stripWhitespace)
inspect(clean_corpus35[1:3])

dtmAll <- DocumentTermMatrix(clean_corpusAll)
dtmAll.new <- unique(dtmAll$i)
dtmAll.new <- dtm35[dtmAll.new,]

result <- FindTopicsNumber(
  dtmAll.new,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

dtm35.new <- unique(dtm35$i)
dtm35.new <- dtm35[dtm35.new,]
View(dtm35)
dtm35


ldaAll <- LDA(dtmAll.new, k=5, control = list(seed=1234))

library(tidytext)
ldaAll.tidy <-tidy(ldaAll)
topwords <- ldaAll.tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
topwords

help("stop_words")
library(ggplot2)
topwords %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()
