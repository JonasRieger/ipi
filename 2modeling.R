library(tosca)
library(ldaPrototype)
library(rollingldaR3.6)

# proto
docs = readRDS("docs_inflation.rds")
vocab = readRDS("vocab_inflation.rds")

proto = LDAPrototype(docs, vocab, K = 10)
saveRDS(proto, "proto10_inflation.rds")

# rolling
clean = readRDS("clean_inflation.rds")
texts = clean$text
dates = clean$meta$date[match(names(texts), clean$meta$id)]

# monthly
rollingmonth = RollingLDA(texts, dates, "month", "3 month", init = "2005-12-31", K = 10)
saveRDS(rollingmonth, "rolling10month_inflation.rds")
# quarterly
rollingquarter = RollingLDA(texts, dates, "month", "3 quarter", init = "2005-12-31", K = 10)
saveRDS(rollingquarter, "rolling10quarter_inflation.rds")
