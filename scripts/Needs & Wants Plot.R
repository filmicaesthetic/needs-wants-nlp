## ---------------------------
## Title: NLP Needs & Wants Visualisation
## ---------------------------
## Author: Dan Fellowes
## Date Created: 2022-05-27
## ---------------------------

# load required packages

library(tidytext)
library(openNLP)
library(textstem)
library(stopwords)
library(tidyverse)
library(NLP)
library(sparkline)
library(ggalluvial)
library(ggforce)
library(readxl)
library(reactable)
library(reactablefmtr)
library(showtext)

##========
##
## Data prep
##
##========

# load data
com_df <- read.csv("data/disneyland_sample_reviews.csv")

source("scripts/Needs & Wants Functions.R")

# run analysis and viz prep function
want_need_plot <- nlp_need_want(com_df, # data frame
                                Review_Text, # column with text
                                Branch, # factor/group column
                                top_words = 20, # number of words to plot
                                include_other = FALSE) # include "Other Words" block

##========
##
## Plot prep
##
##========

# import google font for plot
font_add_google("Roboto", "Roboto")
showtext_auto()

gfont <- "Roboto"

# create facet labels
facet_labs <- list("need" = "NEEDS",
                   "want" = "WANTS",
                   "NEG_need" = "NEG_NEEDS",
                   "NEG_want" = "NEG_WANTS")

##========
##
## Create plot
##
##========

want_need_plot %>%
  ggplot(aes(x = filter, y = comm_perc, stratum = word, alluvium = word)) +
  geom_alluvium(aes(fill = word, colour = word),
                alpha = .75, decreasing = FALSE) +
  geom_text(aes(label = label),
            stat = "alluvium", size = 8, color = "#1d1d1d", decreasing = FALSE, angle = 0) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Responses including text classified as need or want (%)") +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = "none",
        text = element_text(family = gfont, size = 30),
        strip.text = element_text(size = 35, color = "#b0b0b0", face = "bold", hjust = 0),
        strip.background = element_blank(),
        strip.placement = "inside",
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(lineheight=.3)) +
  facet_wrap(~type) +
  labs(caption = "Comments including specified words that infer needing or wanting something are selected, and the sentence including the identified word is extracted for further analysis.\n\n'NEEDS' are identified by searching for 'need', 'must' and 'require'\n'WANTS' are identified by searching for 'want' and 'wish'") +
  ggtitle("Wants & Needs by Group")

# save plot
ggsave("outputs/needs_wants_plot_without_other.png", height = 8, width = 10, dpi = 300)
