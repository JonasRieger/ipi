# 2h, 32GB

starttime = Sys.time()

library(tosca)
library(tm)
library(ldaPrototype)
library(rollinglda)
library(data.table)
library(lubridate)
library(ggplot2)
library(GGally)
library(ggthemes)
library(rollinglda)
library(stringr)
library(tidyverse)

memory = "6 month"

start_update = as.Date(str_extract_all(max(list.files("obj")), "\\d{4}-\\d{2}-\\d{2}")[[1]])[2]+1
end_update = ceiling_date(start_update, "month")-1

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
rolling = updateRollingLDA(rolling, texts, dates, chunks, memory)

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

rm(list = ls())
gc()

#### Analysis
labels = c("Topic 1: Central Banks",
           "Topic 2: News",
           "Topic 3: Emerging Markets",
           "Topic 4: Eurozone",
           "Topic 5: Private Investment",
           "Topic 6: Miscellaneous",
           "Topic 7: Financial Markets",
           "Topic 8: Companies",
           "Topic 9: German Politics",
           "Topic 10: Raw Materials")

counts = readRDS("//media/TextMining/DoCMA/data/Working_Paper_Uncertainty/counts.rds")
obj = readRDS("obj_inflation_updated.rds")
clean = readRDS("clean_inflation_updated.rds")

# update counts
tab = cbind(plotScot(clean, type = "docs"), type = "docs")
colnames(tab) = c("date", "n_inflation", "type")
tab2 = cbind(plotScot(clean, type = "words"), type = "words")
colnames(tab2) = c("date", "n_inflation", "type")
tab = rbindlist(list(tab, tab2))
counts = merge(counts, tab)
saveRDS(counts, "counts.rds")
fwrite(counts, "counts.csv")

unlink("analysis", recursive = TRUE)
dir.create("analysis")

counts = counts[type == "words"]

pdf(file.path("analysis", "words.pdf"), width = 10, height = 7)
print(ggplot(counts) + aes(x = date, y = n/1e6) + geom_line() +
        ylim(c(0, max(counts$n)/1e6)) +
        xlab("monthly") + ylab("n (in Million)"))
print(ggplot(counts) + aes(x = date, y = n_inflation/n) + geom_line() +
        ylim(c(0, max(counts$n_inflation/counts$n))) +
        xlab("monthly"))
dev.off()

file = "rolling_updated.rds"
roll = readRDS(file)
K = getK(getLDA(roll))

## topTexts
dir.create(file.path("analysis", "topTexts"))
mat = topTexts(getLDA(roll), getNames(roll), 100)
showTexts(obj, mat, file = "analysis/topTexts/")

## topTexts per Month
dir.create(file.path("analysis", "topTextsPerMonth"))
for(m in as.character(seq.Date(as.Date("2001-01-01"), max(getDates(roll)), "month"))){
  m = as.Date(m)
  doc_sums = list(document_sums = getDocument_sums(getLDA(roll))[,getDates(roll) >= m & getDates(roll) < m + months(1)])
  mat = topTexts(doc_sums, getNames(roll)[getDates(roll) >= m & getDates(roll) < m + months(1)], 10, tnames = gsub(":", "", gsub(" ", "_", labels)))
  dir.create(file.path("analysis", "topTextsPerMonth", as.character(substr(m, 0, 7))))
  showTexts(obj, mat, file = paste0("analysis/topTextsPerMonth/", as.character(substr(m, 0, 7)), "/"))
}

## topwords
topwords = topWords(getTopics(getLDA(roll)), 200)
prop = round(rowSums(getTopics(getLDA(roll))) / sum(getTopics(getLDA(roll))) * 100, 4)
out = rbind(prop, topwords)
colnames(out) = labels
row.names(out) = c("Proportion (%)", 1:200)
write.csv(out,
          file = file.path("analysis", "topwords.csv"),
          fileEncoding = "UTF-8")

## topics
tab = plotTopic(object = obj, ldaresult = getLDA(roll), ldaID = getNames(roll))
tabrel = plotTopic(object = obj, ldaresult = getLDA(roll), ldaID = getNames(roll), rel = TRUE)
colnames(tab) = colnames(tabrel) = c("date", labels)

write.csv(tab, file = file.path("analysis", "topics.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
write.csv(tabrel, file = file.path("analysis", "topics_rel.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

tabrel_all = cbind(date = tabrel$date,
                   tabrel[, -1] *
                     counts[match(tabrel$date, date), n_inflation/n])
write.csv(tabrel_all, file = file.path("analysis", "topics_rel_to_entire.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

# delete topic 6 (Misc.) for ipi calculation
ipi = data.frame(date = tabrel_all$date,
                 ipi = rowSums(tabrel_all[,-c(1,7)]))
write.csv(ipi, file = file.path("analysis", "ipi.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

sub = as.data.table(tabrel_all)
sub = rbind(cbind(sub[, c(1,2)], "Central Banks", "Causal"),
            cbind(sub[, c(1,3)], "News", "Consequences"),
            cbind(sub[, c(1,4)], "Emerging Markets", "Other"),
            cbind(sub[, c(1,5)], "Eurozone", "Causal"),
            cbind(sub[, c(1,6)], "Private Investment", "Consequences"),
            cbind(sub[, c(1,8)], "Financial Markets", "Consequences"),
            cbind(sub[, c(1,9)], "Companies", "Consequences"),
            cbind(sub[, c(1,10)], "German Politics", "Causal"),
            cbind(sub[, c(1,11)], "Raw Materials", "Causal"),
            use.names = FALSE)
colnames(sub) = c("date", "ipi", "Topic", "Type")

pdf(file.path("analysis", "ipi.pdf"), width = 10, height = 7)
print(ggplot(ipi) + aes(x = date, y = ipi) + geom_line() + xlab("") + ylab("IPI (Share)"))
print(ggplot(sub) +
        aes(x = date, y = ipi, group = Topic, color = Topic) +
        geom_line() +
        xlab("") + ylab("Share") + theme(legend.position = "top") +
        facet_wrap(~Type, nrow = 3))
dev.off()

## topics cosine
# quarter
quarters = lubridate::floor_date(getDates(roll), "quarter")
xquarter = sort(unique(quarters))
nquarter = length(xquarter)

topicsq = lapply(xquarter, function(x){
  tmp = table(factor(unlist(getAssignments(getLDA(roll))[quarters == x])+1, levels = 1:getK(getLDA(roll))), 
              factor(unlist(lapply(getDocs(roll)[quarters == x], function(y) y[1,]))+1,
                     levels = seq_len(length(getVocab(roll)))))
  tmp = matrix(as.integer(tmp), nrow = getK(getLDA(roll)))
  colnames(tmp) = getVocab(roll)
  tmp
})

topics = lapply(1:getK(getLDA(roll)), function(k){
  tmp = sapply(topicsq, function(x) x[k,])
  colnames(tmp) = paste0("Q", 1:nquarter)
  tmp
})

sims = lapply(1:getK(getLDA(roll)), function(k){
  cosineTopics(topics[[k]], pm.backend = "socket", ncpus = 4)
})

valq = sapply(sims, function(x) c(NA, x$sims[cbind(2:nquarter,2:nquarter-1)]))
valq_first = sapply(sims, function(x) x$sims[,1])
valq_last = sapply(sims, function(x) x$sims[nquarter,])

dir.create(file.path("analysis", "intra_topic_cosine"))
for(k in 1:K){
  colnames(sims[[k]]$sims) = rownames(sims[[k]]$sims) = as.character(xquarter)
  write.csv(sims[[k]]$sims, file.path("analysis", "intra_topic_cosine",
                                      paste0(gsub(":", "", gsub(" ", "_", labels))[k],
                                             "_quarterly.csv")))
}

min_quarterly = min(sapply(sims, function(x) min(x$sims, na.rm = TRUE)))
pdf(file.path("analysis", "intra_topic_cosine", "intra_topic_cosine_quarterly_common_limit.pdf"),
    width = 16, height = 14)
for(k in 1:K){
  plot(sims[[k]]$sims %>% 
         as.data.frame() %>%
         rownames_to_column("quarter") %>%
         pivot_longer(-c(quarter), names_to = "quarter2", values_to = "Similarity") %>%
         ggplot(aes(x=quarter, y=quarter2, fill=Similarity)) + 
         geom_raster() +
         scale_fill_viridis_c(option = "inferno", limits = c(min_quarterly,1)) +
         xlab("") + ylab("") +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
         ggtitle(labels[k]))
}
dev.off()
pdf(file.path("analysis", "intra_topic_cosine", "intra_topic_cosine_quarterly.pdf"),
    width = 16, height = 14)
for(k in 1:K){
  plot(sims[[k]]$sims %>% 
         as.data.frame() %>%
         rownames_to_column("quarter") %>%
         pivot_longer(-c(quarter), names_to = "quarter2", values_to = "Similarity") %>%
         ggplot(aes(x=quarter, y=quarter2, fill=Similarity)) + 
         geom_raster() +
         scale_fill_viridis_c(option = "inferno") +
         xlab("") + ylab("") +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
         ggtitle(labels[k]))
}
dev.off()
saveRDS(sims, file.path("analysis", "intra_topic_cosine", "sim_quarterly.rds"))

# month
months = lubridate::floor_date(getDates(roll), "month")
xmonth = sort(unique(months))
nmonth = length(xmonth)

topicsm = lapply(xmonth, function(x){
  tmp = table(factor(unlist(getAssignments(getLDA(roll))[months == x])+1, levels = 1:getK(getLDA(roll))), 
              factor(unlist(lapply(getDocs(roll)[months == x], function(y) y[1,]))+1,
                     levels = seq_len(length(getVocab(roll)))))
  tmp = matrix(as.integer(tmp), nrow = getK(getLDA(roll)))
  colnames(tmp) = getVocab(roll)
  tmp
})

## topWordsPerMonth
topwordsm = lapply(topicsm, topWords, numWords = 50)
topwordsm = lapply(1:K, function(k) sapply(seq_along(topwordsm), function(t) topwordsm[[t]][,k]))
dir.create(file.path("analysis", "topWordsPerMonth"))
for(i in 1:K){
  out = rbind(round(tabrel[, i+1] * 100, 4),
              round(tabrel_all[, i+1] * 1000, 4),
              topwordsm[[i]])
  colnames(out) = as.character(tabrel$date)
  row.names(out) = c("Share (in %) on \"wirtschaft\"&\"unsicher\"-corpus",
                     "Share (in Promille) on entire corpus",
                     1:50)
  write.csv(out,
            file = file.path("analysis", "topWordsPerMonth",
                             paste0(gsub(":", "", gsub(" ", "_", colnames(tabrel)[i+1])), ".csv")),
            fileEncoding = "UTF-8")
}

topics = lapply(1:getK(getLDA(roll)), function(k){
  tmp = sapply(topicsm, function(x) x[k,])
  colnames(tmp) = paste0("Q", 1:nmonth)
  tmp
})

sims = lapply(1:getK(getLDA(roll)), function(k){
  cosineTopics(topics[[k]], pm.backend = "socket", ncpus = 4)
})

valm = sapply(sims, function(x) c(NA, x$sims[cbind(2:nmonth,2:nmonth-1)]))
valm_first = sapply(sims, function(x) x$sims[,1])
valm_last = sapply(sims, function(x) x$sims[nmonth,])

for(k in 1:K){
  colnames(sims[[k]]$sims) = rownames(sims[[k]]$sims) = as.character(xmonth)
  write.csv(sims[[k]]$sims, file.path("analysis", "intra_topic_cosine",
                                      paste0(gsub(":", "", gsub(" ", "_", labels))[k],
                                             "_monthly.csv")))
}

min_monthly = min(sapply(sims, function(x) min(x$sims, na.rm = TRUE)))
pdf(file.path("analysis", "intra_topic_cosine", "intra_topic_cosine_monthly_common_limit.pdf"),
    width = 30, height = 28)
for(k in 1:K){
  plot(sims[[k]]$sims %>% 
         as.data.frame() %>%
         rownames_to_column("month") %>%
         pivot_longer(-c(month), names_to = "month2", values_to = "Similarity") %>%
         ggplot(aes(x=month, y=month2, fill=Similarity)) + 
         geom_raster() +
         scale_fill_viridis_c(option = "inferno", limits = c(min_monthly,1)) +
         xlab("") + ylab("") +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
         ggtitle(labels[k]))
}
dev.off()
pdf(file.path("analysis", "intra_topic_cosine", "intra_topic_cosine_monthly.pdf"),
    width = 30, height = 28)
for(k in 1:K){
  plot(sims[[k]]$sims %>% 
         as.data.frame() %>%
         rownames_to_column("month") %>%
         pivot_longer(-c(month), names_to = "month2", values_to = "Similarity") %>%
         ggplot(aes(x=month, y=month2, fill=Similarity)) + 
         geom_raster() +
         scale_fill_viridis_c(option = "inferno") +
         xlab("") + ylab("") +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
         ggtitle(labels[k]))
}
dev.off()
saveRDS(sims, file.path("analysis", "intra_topic_cosine", "sim_monthly.rds"))

xmin = min(xmonth)

cosine_quarterly = ggmatrix(lapply(1:10, function(i){
  ggplot() + geom_line(aes(x = xmonth, y = valm[,i]), col = "darkgrey") + ylim(c(0,1)) +
    geom_line(aes(x = xquarter, y = valq_first[,i], col = "green")) +
    geom_line(aes(x = xquarter, y = valq_last[,i], col = "red")) +
    geom_line(aes(x = xquarter, y = valq[,i])) +
    annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0)
}), nrow = 5, ncol = 2, ylab = "Cosine Similarity")

cosine_monthly = ggmatrix(lapply(1:10, function(i){
  ggplot() + geom_line(aes(x = xquarter, y = valq[,i]), col = "darkgrey") + ylim(c(0,1)) +
    geom_line(aes(x = xmonth, y = valm_first[,i], col = "green")) +
    geom_line(aes(x = xmonth, y = valm_last[,i], col = "red")) +
    geom_line(aes(x = xmonth, y = valm[,i])) +
    annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0)
}), nrow = 5, ncol = 2, ylab = "Cosine Similarity")

pdf(file.path("analysis", "intra_topic_cosine", "topics_cosine_quarter.pdf"), width = 8, height = 10)
print(cosine_quarterly)
dev.off()

tiff(file.path("analysis", "intra_topic_cosine", "topics_cosine_quarter.tiff"),
     width = 1600, height = 2000, pointsize = 200, compression = "lzw", res = 200,
     type = "cairo")
print(cosine_quarterly)
dev.off()

pdf(file.path("analysis", "intra_topic_cosine", "topics_cosine_month.pdf"), width = 8, height = 10)
print(cosine_monthly)
dev.off()

tiff(file.path("analysis", "intra_topic_cosine", "topics_cosine_month.tiff"),
     width = 1600, height = 2000, pointsize = 200, compression = "lzw", res = 200,
     type = "cairo")
print(cosine_monthly)
dev.off()

