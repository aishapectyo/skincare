setwd("~/Documents/ds/skincare")
library(ggplot2)
library(dplyr)
library(ggthemes)

#Functions from: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#Data
data <- read.csv("dataR.csv")
data <- sample_n(data, 200)
aspect_ratio <- 2.5

#Plots
fill <- "#E5665A"
line <- "#E5665A"
ggplot(data, aes(word_count)) +
  geom_density(fill = fill, colour = line) +
  theme_fivethirtyeight() +
  labs(title='Word Counts',
       subtitle='Average Distribution of Words of r/SkincareAddiction Comments') 
ggsave("wordCount.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)

fill <- "#FB9F8B"
line <- "#FB9F8B"
ggplot(data, aes(avg_word)) +
  geom_density(fill = fill, colour = line) +
  theme_fivethirtyeight() +
  xlim(0, 30) +
  labs(title='Word Length',
       subtitle='Average Word Length of r/SkincareAddiction Comments') 
ggsave("wordLen.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)

freqT <- data.frame(stringsAsFactors=FALSE,
                 word = c("skin", "face", "products", "acne", "good", "think", "routine", "acid", 
                          "moisturizer", "cleanser", 'oil', 'try', 'sunscreen',"cream"),
                 freq = c(48112, 15898, 12967, 12795, 11675, 10403, 10395, 10349, 10219, 10177,
                          10103, 9777, 9566, 8045))
 
                 
ggplot(freqT, aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "#EE4470") +
  theme_fivethirtyeight() +
  labs(title='Word Frequencies',
       subtitle='Most Common Words in r/SkincareAddiction Comments') +
  coord_flip()

ggsave("wordFreq.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)


brand <- data.frame(stringsAsFactors=FALSE,
                    word = c("The Ordinary", "CeraVe", "Neutrogena", "Differin", "Cetaphil", "Paula's Choice",
                             "Hada Labo", "Vanicream", "La Roche-Posay", "Vaseline", "CosRx", "Clinique",
                             "Drunk Elephant", "Accutane", "Glow Recipe"),
                    freq = c(7000, 4000, 1476, 1279, 882, 876, 758, 753, 728, 684, 602, 568, 564, 461, 439))


ggplot(brand, aes(reorder(word, freq), freq)) +
  geom_bar(stat = "identity", fill = "#9799BA") +
  theme_fivethirtyeight() +
  labs(title='Common Brand Names',
       subtitle='Most Common Brand Names in r/SkincareAddiction Comments') +
  coord_flip()

ggsave("commonProds.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)


pos <- data.frame(stringsAsFactors=FALSE,
                    word = c('Good', 'Dry', 'Great', 'Sensitive', 'Bad', 'Different', 'Gentle', 'Clear', 'Glycolic', 'Salicylic',
                             'Get', 'Try', 'Go', 'Keep', 'Take', 'Help', 'See', 'Love', 'Feel', 'Recommend',
                             'Skin', 'Face', 'Oil', 'Moisturizer', 'Cleanser', 'Time', 'Cream', 'Product', 'Acid', 'Acne',
                             'Also', 'Really', 'Well', 'Even', 'Definitely', 'Always', 'Oily', 'Probably', 'Usually', 'Sometimes'),
                    freq = c(11166, 6751, 5210, 3975, 3614, 3122, 2665, 2644, 2183, 2098,
                             7545, 4513, 3200, 2450, 2065, 1907, 1856, 1746, 1687, 1614,
                             26568, 16094, 13945, 10103, 8652, 8321, 7936, 7862, 7719, 7587,
                             15236, 13449, 6895, 6619, 5557, 4953, 4844, 4281, 4220, 4043),
                    type = c("Adjective","Adjective","Adjective","Adjective","Adjective","Adjective","Adjective","Adjective","Adjective","Adjective",
                             "Verb","Verb","Verb","Verb","Verb","Verb","Verb","Verb","Verb","Verb",
                             "Noun", "Noun","Noun","Noun","Noun","Noun","Noun","Noun","Noun","Noun",
                             "Adverb","Adverb","Adverb","Adverb","Adverb","Adverb","Adverb","Adverb","Adverb","Adverb"))
  
pos %>%
    arrange(type, -freq) %>%
    #dplyr::mutate(term = reorder_within(word, freq, type)) %>%
    ggplot(aes(reorder(word, freq), freq, fill = factor(type))) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values=c("#FB9DA7", "#FCCCD4", "#B86B77", "#EABFB9"))+
    facet_wrap(~ type, scales = "free") +
    theme_fivethirtyeight() +
    labs(title='Parts of Speech',
       subtitle='Most Common Parts of Speech in r/SkincareAddiction Comments') +
    coord_flip() +
    scale_x_reordered()


ggsave("POS.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)


dryCombo <- data.frame(stringsAsFactors=FALSE,
                  word = c('Cream', 'Eczema', 'Oil', 'Winter', 'Moisturizer', 'Irritation', 'Acne', 'Gel', 'Fragance', 'Problem',
                           'Bad', 'Good', 'Hormonal', 'Little', 'Sensitive', 'Cystic', 'Extra', 'Consistent', 'Nose', 'Normal'),
                  freq = c(37, 32, 30, 28, 22, 15, 14, 11, 9, 9,
                           11, 9, 6, 6, 6, 6, 6, 6, 5, 5),
                  type = c("Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", 
                           "Adjective","Adjective","Adjective","Adjective","Adjective","Adjective","Adjective",
                           "Adjective","Adjective","Adjective"))
dryCombo %>%
  arrange(type, -freq) %>%
  #dplyr::mutate(term = reorder_within(word, freq, type)) %>%
  ggplot(aes(reorder(word, freq), freq, fill = factor(type))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("peachpuff4", "peachpuff3"))+
  facet_wrap(~ type, scales = "free") +
  theme_fivethirtyeight() +
  labs(title='Word Combinations for Dry Skin',
       subtitle='Dry Skin Noun and Adjective Pairs in r/SkincareAddiction Comments') +
  coord_flip() +
  scale_x_reordered()


ggsave("drycombo.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)


oilCombo <- data.frame(stringsAsFactors=FALSE,
                       word = c('Summer', 'Prone', 'Skin', 'Acne', 'Redness', 'CosRx', 'Routine', 'Control', 'Oil', 'Combo',
                                'Oily', 'Good', 'Genetic', 'Big', 'Enlarged', 'Senstive', 'Glycolic', 'Gel', 'Overdry', 'Cystic'),
                       freq = c(37, 32, 30, 28, 22, 15, 14, 11, 9, 9,
                                11, 9, 6, 6, 6, 6, 6, 6, 5, 5),
                       type = c("Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", 
                                "Adjective","Adjective","Adjective","Adjective","Adjective","Adjective","Adjective",
                                "Adjective","Adjective","Adjective"))
oilCombo %>%
  arrange(type, -freq) %>%
  #dplyr::mutate(term = reorder_within(word, freq, type)) %>%
  ggplot(aes(reorder(word, freq), freq, fill = factor(type))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("#E44790", "#FF3320")) +
  facet_wrap(~ type, scales = "free") +
  theme_fivethirtyeight() +
  labs(title='Word Combinations for Oily Skin',
       subtitle='Oily Skin Noun and Adjective Pairs in r/SkincareAddiction Comments') +
  coord_flip() +
  scale_x_reordered()


ggsave("oilycombo.png", plot = last_plot(), device = NULL, path = NULL,
       width = 6 * aspect_ratio, height = 6, units = c("in"),
       dpi = 400)

