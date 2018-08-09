corpus35 <- Corpus(VectorSource(references))

#seminal analysis
CR <- citations(M35, field = "author", sep = ".  ")
M35[grep("CHEONG E", M35$CR),2]
cbind(CR$Cited[1:11])


clean_corpus35 <- tm_map(corpus35, tolower)
clean_corpus35 <- tm_map(corpus35, removeNumbers)
clean_corpus35 <- tm_map(clean_corpus35, removeWords, c(stopwords("english"), "ieee","doi","anonymous","acm","automat","comput","software","conference","lect","notes","DOI","int","proceedings", "web","adhoc","comp","engineering","computer","sys","workshops","softw","visual","conf"))
clean_corpus35 <- tm_map(clean_corpus35, stripWhitespace)
inspect(clean_corpus35[1:3])

dtm35 <- DocumentTermMatrix(clean_corpus35)
dtm35.new <- unique(dtm35$i)
dtm35.new <- dtm35[dtm35.new,]
View(dtm35)
dtm35


lda35 <- LDA(dtm35.new, k=10, control = list(seed=1234))

library(tidytext)
lda35.tidy <-tidy(lda35)
top.citations <- lda35.tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top.citations

help("stop_words")
library(ggplot2)
top.citations %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip()


ggplot(DFk,aes(Year,value, group=variable, color=variable))+geom_point()+ 
   labs(color="Author Keywords")+
   scale_colour_brewer(palette = "Set1")+
   scale_x_continuous(breaks = seq(min(DF$Year), max(DF$Year), by = 2))

