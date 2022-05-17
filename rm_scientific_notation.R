library(data.table)
setwd("analysis")

a = fread("ipi.csv")
a[, ipi := format(ipi, scientific = F)]

fwrite(a, "ipi_dec.csv")


a = fread("topics_rel_to_entire.csv")
a = format(a, scientific = F)

fwrite(a, "topics_rel_to_entire_dec.csv")



