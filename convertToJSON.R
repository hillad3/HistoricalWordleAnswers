rm(list = ls())
gc()

library(data.table)
library(jsonlite)

words <- fread("data/wordle_answers.csv")

jwords <- toJSON(words, pretty=TRUE)

write(jwords, "data/wordle_answers.json")
