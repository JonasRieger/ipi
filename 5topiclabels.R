library(data.table)
library(topiclabels)
token = ""

files = list.files("analysis/topWordsPerMonth/", full.names = TRUE)

a = fread(files[4])[,-1]
dates = as.Date(as.character(a[1,]))
a = unname(as.matrix(a[-c(1:3),]))

b = label_topics(a, token = token)
b = label_topics(a, token = token, context = "Inflation Perception")

d = rbind(as.character(dates), b$labels, a)
write.table(d, "EmergingMarketsLabels.csv", sep = ",",
            row.names = FALSE, col.names = FALSE)
