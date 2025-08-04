
getwd()
list.files("data")
saveRDS(data, "data/events_data-2025-01-15-16-07-31.rds")   # data/ klasörüne yazar


# tekrar okumak için sonradan
data <- readRDS("data/events_data-2025-01-15-16-07-31.rds")


load(".Rdata")
file.copy(".RData", "data/old_workspace.RData")  # kopyala
# veya
file.rename(".RData", "data/old_workspace.RData") # taşı
