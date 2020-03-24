# Script to visualize survey results for the QCBS support letter

# load packages
require(tidyverse)
require(waffle)
require(extrafont)
require(ggsci)

# note: percentages?

# import survey responses
responses <- read_csv("Documents/GitHub/qcbs_surveyresults/data/responses.csv")
# extract questions
questions <- colnames(responses)

# set plotting parameters
wafflerows = 12
wafflesize = .3

## come back to this to make it people icons!

# # get fonts for pictochart
# # downloaded free FontAwesome fonts (for web) and installed .ttf files
# font_import()
# # check that Font Awesome is imported
# fonts()[grep("Awesome", fonts())]
# # this should be fine for Mac OSX
# loadfonts()

## signatures ----

# summarise results
table(responses[,questions[20]]) %>%
  waffle(rows = wafflerows, 
         title = "Student members who have signed", 
         colors = c("#15105c"), 
         size = wafflesize, legend_pos = "none")

## level of study ----

table(responses[,questions[4]]) %>% 
  waffle(rows = wafflerows, 
         title = "Student members who have signed", 
         colors = c("#15105c","#c61558","#ffa600"), 
         size = wafflesize)

## time as member ----

# wrangle data
time = table(responses[,questions[6]]) %>% as.data.frame() 
colnames(time) = c("length", "qty")  
# set answer order for axis
time$length = factor(time$length, levels = c("< 6 mois/months","6 mois/months - 1 an/year",
                                             "1 - 2 ans/years","> 2 ans/years"))  
# plot!
ggplot(data = time) + 
  geom_bar(aes(x = length, y = qty, fill = length), stat = "identity") +
  labs(y = "Time as member", x = "Student members") +
  coord_flip() +
  scale_fill_manual(values = c("#428dc9","#3369a5", "#264780", "#19275c")) +
  theme_classic()
  
# qcbs impact on career ----

# save subset of columns about this question
q8_12 = responses[,questions[8:12]]

# make function to set levels order
impact_levels = function(x) {factor(x, levels = c("Fortement négatif / Strongly negative",
                                 "Neutre / Neutral",
                                 "Positif / Positive",
                                 "Fortement positif / Strongly positive"))}
q8_12 <- apply(q8_12, MARGIN = 2, FUN = impact_levels)

# extract main question
q8_12_question = "Quel impact a eu le CSBQ sur votre parcours académique et/ou votre carrière jusqu'à maintenant? / What impact did the QCBS have on your academic and/or professional career so far?"
# take it out of each individual question
colnames(q8_12) = gsub("Quel impact a eu le CSBQ sur votre parcours académique et/ou votre carrière jusqu'à maintenant? / What impact did the QCBS have on your academic and/or professional career so far?", "", colnames(q8_12))
colnames(q8_12) = gsub("\\[", "", colnames(q8_12))
colnames(q8_12) = gsub("]", "", colnames(q8_12))

# summarise each one
rworkshops = as.data.frame(table(q8_12[,1]))
trainings = as.data.frame(table(q8_12[,2]))
symposium = as.data.frame(table(q8_12[,3]))
network = as.data.frame(table(q8_12[,4]))
leadership = as.data.frame(table(q8_12[,5]))
# plotly donut chart??
rworkshops.p = ggplot(rworkshops)

# questions to deal with:
questions[7]


