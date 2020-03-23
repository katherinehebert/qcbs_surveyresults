# Script to visualize survey results for the QCBS support letter

# load packages
require(tidyverse)
require(waffle)
require(extrafont)

# import survey responses
responses <- read_csv("Documents/GitHub/qcbs_surveyresults/data/responses.csv")
# extract questions
questions <- colnames(responses)

## level of study
levelstudy = table(responses[,questions[4]])
as.vector(levelstudy)

nrow(responses)
waffle(levelstudy, rows = 12, title = "Level of study", use_glyph = "user",
       colors = c("#15105c","#c61558","#ffa600"), size = 0.3)

