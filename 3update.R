starttime = Sys.time()

library(tosca)
library(tm)
library(ldaPrototype)
library(rollingldaR3.6)
library(data.table)

#####
# Only the following two lines have to be modified manually for updates.
start_update = "2022-09-01"
end_update = "2022-09-30"
#####
chunks = seq.Date(from = as.Date(start_update), to = as.Date(end_update), by = "month")

rootpath = "//media/TextMining/DoCMA"
setwd(file.path(rootpath, "data", "Working_Paper_Uncertainty"))

message("load...")
a = Sys.time()
obj = readRDS("obj_updated.rds")
message(difftime(Sys.time(), a, units = "mins"), " mins")

message("filter date...")
a = Sys.time()
obj = filterDate(obj, s.date = start_update, e.date = end_update)
obj = textmeta(meta = obj$meta, text = obj$text)

message(sum(duplicated(names(obj$text))), " duplicates deleted")
obj$text = obj$text[!duplicated(names(obj$text))]

message(difftime(Sys.time(), a, units = "mins"), " mins")

setwd(file.path(rootpath, "projects", "2022_Inflation"))

# FILTER -------------------------------------------------------------
message("filter word...")
a = Sys.time()

obj = filterWord(obj, search = list(
  data.frame(pattern = "inflation", word = FALSE, count = 1),
  data.frame(pattern = "teuerung", word = TRUE, count = 1),
  data.frame(pattern = "geldentwertung", word = FALSE, count = 1),
  data.frame(pattern = "preissteigerung", word = FALSE, count = 1)), ignore.case=TRUE)

message(difftime(Sys.time(), a, units = "mins"), " mins")
message("save...")
saveRDS(obj, file.path("obj", paste0("obj_inflation_", start_update, "_", end_update, ".rds")))

# CLEAN ---------------------------------------------------------------
message("clean...")
a = Sys.time()

clean = cleanTexts(obj, sw = "de")

message(difftime(Sys.time(), a, units = "mins"), " mins")
message("save...")
saveRDS(clean, file.path("clean", paste0("clean_inflation_", start_update, "_", end_update, ".rds")))

# MODELING -------------------------------------------------------------
message("modeling...")
a = Sys.time()

texts = clean$text
dates = clean$meta$date[match(names(texts), clean$meta$id)]

rolling = readRDS("rolling_updated.rds")
rolling = updateRollingLDA(rolling, texts, dates, chunks, "3 month")

message(difftime(Sys.time(), a, units = "mins"), " mins")
message("save...")
saveRDS(rolling, "rolling_updated.rds")
saveRDS(rolling, file.path("rolling", paste0("rolling_", end_update, ".rds")))

# MERGING --------------------------------------------------------------
message("merging...")
a = Sys.time()

saveRDS(mergeTextmeta(list(readRDS("obj_inflation_updated.rds"), obj)),
        "obj_inflation_updated.rds")
saveRDS(mergeTextmeta(list(readRDS("clean_inflation_updated.rds"), clean)),
        "clean_inflation_updated.rds")

message(difftime(Sys.time(), a, units = "mins"), " mins")
message("Overall time: ", difftime(Sys.time(), starttime, units = "hours"), " hours")
