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
div_palette = c("#31608f","#8a9ab4","#dadada","#d78b79","#bd3520")
div_palette = c("#2f96c2","#98bdd4","#e5e5e5","#db8d8c","#bd283c")
nice_palette = c("#20115c","#7f0066","#c81b58","#f45e3a","#ffa600")
  
## come back to this to make it people icons!

# # get fonts for pictochart
# # downloaded free FontAwesome fonts (for web) and installed .ttf files
# font_import()
# # check that Font Awesome is imported
# fonts()[grep("Awesome", fonts())]
# # this should be fine for Mac OSX
# loadfonts()
font_import()
loadfonts()       #Register fonts for Windows bitmap output
fonts() 

## signatures ----

# summarise results
signatures = unname(table(responses[,questions[20]]))
waffle(signatures, rows = wafflerows, 
         title = paste(signatures, "student members have signed"), 
         colors = c("#15105c"), 
         size = wafflesize, legend_pos = "none")

## level of study ----
studylevels = table(responses[,questions[4]])
(levelofstudy.p = studylevels %>% 
                  waffle(rows = wafflerows, 
                         title = paste("Study level of signatures"), 
                         colors = c("#15105c","#c61558","#ffa600"), 
                         size = wafflesize))

## time as member ----

# wrangle data
time = table(responses[,questions[5]]) %>% as.data.frame() 
colnames(time) = c("length", "qty")  
# set answer order for axis
time$length = factor(time$length, levels = c("< 1 an/year",
                                             "1 - 2 ans/years","> 2 ans/years"))  
# plot!
(timeasmember.p = ggplot(data = time) + 
                    geom_bar(aes(x = length, y = qty, fill = length), stat = "identity") +
                    labs(x = "Time as member", y = "Student members") +
                    coord_flip() +
                    scale_fill_manual(values = nice_palette) +
                    theme_classic())

## time since member ----

# wrangle data
time = table(responses[,questions[6]]) %>% as.data.frame() 
colnames(time) = c("length", "qty")  
# set answer order for axis
time$length = factor(time$length, levels = c("< 6 mois/months","6 mois/months - 1 an/year",
                                             "1 - 2 ans/years","> 2 ans/years"))  
# plot!
(timeasmember.p = ggplot(data = time) + 
    geom_bar(aes(x = length, y = qty, fill = length), stat = "identity") +
    labs(x = "Time as member", y = "Student members") +
    coord_flip() +
    scale_fill_manual(values = c("#428dc9","#3369a5", "#264780", "#19275c")) +
    theme_classic())

  
# qcbs impact on academic career ----

# save subset of columns about this question
q8_12 = responses[,questions[8:12]]

# make function to set levels order
impact_levels = function(x) {factor(x, levels = c("Fortement négatif / Strongly negative",
                                 "Neutre / Neutral",
                                 "Positif / Positive",
                                 "Fortement positif / Strongly positive"))}
q8_12 <- apply(q8_12, MARGIN = 2, FUN = impact_levels)

# extract main question
q8_12_question = "Quel impact a eu le CSBQ sur votre parcours académique et/ou votre carrière jusqu'à maintenant? \nWhat impact did the QCBS have on your academic and/or professional career so far?"
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
# put together in a data frame
impacts_df = as.data.frame(rbind(rworkshops, trainings, symposium, network, leadership))
impacts_df$category = c(rep("Ateliers R / R workshops", each = 4),
                        rep("Cours et formations / Courses and trainings",each = 4),
                        rep("Symposium annuel / Annual symposium",each=4),
                        rep("Réseau de chercheurs / Network of researchers",each=4),
                        rep("Opportunités de leadership / Leadership opportunities",each=4))
impacts_df$Var1 = impact_levels(impacts_df$Var1)
# do stacked percent barchart
ggplot(impacts_df, aes(fill=Var1, y=Freq, x=category)) + 
  geom_bar(position="fill", stat="identity") +
  labs(y = "Proportion of student members (%)", 
       x = "QCBS activities / Activités du CSBQ",
       title = q8_12_question) +
  scale_fill_manual(values = rev(nice_palette[c(1,2,4,5)])) + 
  theme_classic() +
  theme(legend.title = element_blank()) +
  coord_flip()

# impact on development as researcher ----

resdev = as.data.frame(table(responses[,questions[14]]))
missinglevels = data.frame(Var1 = factor(c(1,2)), Freq = c(0,0))
resdev = rbind(missinglevels, resdev)
ggplot(impacts_df, aes(fill=Var1, y=Freq, x=category)) + 
  geom_bar(position="fill", stat="identity") 
(resdev.p = ggplot(resdev, aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  labs(x = "", y = "", title = questions[14])+
  coord_flip() +
    scale_fill_manual(values = rev(nice_palette)) +
    theme_classic() + theme(legend.position = "none"))

# awards making opportunities accessible ----

awards = as.data.frame(table(responses[,questions[17]]))
missinglevels = data.frame(Var1 = factor(2), Freq = 0)
awards = rbind(missinglevels, awards)
awards$Var1 = factor(awards$Var1, levels = c("1", "2", "3", "4", "5"))

(awards.p = ggplot(awards, aes(x = Var1, y = Freq)) + 
    geom_bar(aes(x = Var1, y = Freq, fill = Var1), stat = "identity") +
    labs(x = "", y = "", title = questions[17]) +
    coord_flip() +
    scale_fill_manual(values = rev(nice_palette)) +
    theme_classic() + theme(legend.position = "none"))

# questions to deal with:
na.omit(responses[,questions[7]])
na.omit(responses[,questions[13]])
na.omit(responses[,questions[16]])
na.omit(responses[,questions[18]])
na.omit(responses[,questions[19]])

