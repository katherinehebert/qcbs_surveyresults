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

# summarise results
levelstudy = table(responses[,questions[4]])

## come back to this to make it people icons!

  # # get fonts for pictochart
  # # downloaded free FontAwesome fonts (for web) and installed .ttf files
  # font_import()
  # # check that Font Awesome is imported
  # fonts()[grep("Awesome", fonts())]
  # # this should be fine for Mac OSX
  # loadfonts()

# waffle plot!
waffle(levelstudy, rows = 12, title = "Level of study", use_glyph = "user",
       colors = c("#15105c","#c61558","#ffa600"), size = 0.3)

