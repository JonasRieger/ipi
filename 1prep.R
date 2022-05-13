starttime = Sys.time()

library(tosca)
library(tm)

message("Korpus einlesen")
obj = readRDS("obj_updated.rds")
obj = filterDate(obj, e.date = "2021-12-31")

# Korpus eingrenzen - 1x inflation* ------------------------------------

message("Korpus eingrenzen ...")
a = Sys.time()
message("... Suchwort: 1x inflation* ODER embeddings")

obj = filterWord(obj, search = list(
  data.frame(pattern = "inflation", word = FALSE, count = 1),
  data.frame(pattern = "teuerung", word = TRUE, count = 1),
  data.frame(pattern = "geldentwertung", word = FALSE, count = 1),
  data.frame(pattern = "preissteigerung", word = FALSE, count = 1)), ignore.case=TRUE)

saveRDS(obj, file = "obj_inflation.rds")

# CLEAN ---------------------------------------------------------------
message("Korpus clean ...")
a = Sys.time()
clean = cleanTexts(obj, sw = "de")
message(difftime(Sys.time(), a, units = "hours"), " hours")

saveRDS(clean, file = "clean_inflation.rds")

#prepare LDA ----------------------------------------------------------
message("LDA vorbereiten ...")
a = Sys.time()
wordtable = makeWordlist(clean$text)$wordtable
vocab = sort(names(wordtable)[wordtable>5])
docs = LDAprep(clean$text, vocab)
message(difftime(Sys.time(), a, units = "hours"), " hours")

saveRDS(docs, "docs_inflation.rds")
saveRDS(vocab, "vocab_inflation.rds")

message(difftime(Sys.time(), a, units = "hours"), " hours")
message("Overall time: ", difftime(Sys.time(), starttime, units = "hours"), " hours")
