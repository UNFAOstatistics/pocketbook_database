library(readr)
library(dplyr)
library(tidyr)
library(stringr)


if (!file.exists("./data_raw")) dir.create("./data_raw")
# Bulk download FAOSTAT
download.file("http://faostat3.fao.org/ftp-faostat/Bulk/FAOSTAT.zip", destfile = "data_raw/faostat_bulk.zip")
# Unzip FAOSTAT
zip_file_faostat <- list.files("data_raw/", pattern = "faostat_bulk", full.names = T)
unzip(zip_file_faostat, exdir = "data_raw/faostat")
zip_file_faostat2 <- list.files("data_raw/faostat", pattern = "zip", full.names = T)
for (i in 1:length(zip_file_faostat2)){
  unzip(zip_file_faostat2[i], exdir = "data_raw/faostat")
}
file.remove(c(zip_file_faostat,zip_file_faostat2))

# read FAOSTAT
csv_files <- list.files("data_raw/faostat", pattern = ".csv", full.names = T)
file_names <- str_replace_all(list.files("data_raw/faostat", pattern = ".csv", full.names = F), ".csv", "")
# 
fao_raw <- data.frame()
fao_meta <- data.frame()
if (!file.exists("./data_processed")) dir.create("./data_processed")
for(i in 1:length(csv_files)){
  df <- read_csv(csv_files[i])
  df$file_name <- paste0(file_names[i],".RData")
  df$id <- paste("var",df$ItemCode,df$ElementGroup,df$ElementCode,sep="-")
  df_meta <- df[c("ItemCode","Item","ElementGroup","ElementCode","Element","Unit","id","file_name")]
  df_meta <- df_meta[!duplicated(df_meta[c("id")]),]
  fao_meta <- rbind(fao_meta,df_meta)
  dat <- df[c("CountryCode","Country","Year","Value","id")]
  dat <- spread(dat, id, Value)
  save(dat, file=paste0("data_processed/",file_names[i],".RData"))
}
file.remove(csv_files) # remove massive csv's
save(fao_meta, file="data_processed/fao_meta.RData")


get_fao_bulk <- function(ItemCode = 1931,
                         ElementCode = 5141){
  
  key <- fao_meta[fao_meta$ItemCode %in% ItemCode & fao_meta$ElementCode %in% ElementCode, ]
  print(key)
  
}






#############################################################33
# Bulk download WDI from World Bank
download.file("http://databank.worldbank.org/data/download/WDI_csv.zip", destfile = "data_raw/wdi_bulk.zip")

# Unzip WDI
zip_file_wdi <- list.files("data_raw/", pattern = "wdi_bulk", full.names = T)
unzip(zip_file_wdi, exdir = "data_raw/wdi")

file.remove(zip_file_wdi) # remove zipfile

# read WDI
wdi_raw <- read_csv("data_raw/wdi/WDI_Data.csv")
names(wdi_raw) <- tolower(names(wdi_raw))
names(wdi_raw) <- str_replace_all(names(wdi_raw), " ", ".")
wdi_raw$indicator.name <- NULL
wdi_raw$country.name <- NULL
wdi_raw <- gather(wdi_raw,
                  Year,
                  value,
                  3:ncol(wdi_raw))
wdi_raw <- wdi_raw[!is.na(wdi_raw$value),]
wdi_raw <- spread(wdi_raw, 
                  indicator.code,
                  value)
wdi_raw$Year <- as.numeric(levels(wdi_raw$Year))[wdi_raw$Year]
wdi_data <- wdi_raw
file.remove("data_raw/wdi/WDI_Data.csv")

save(wdi_data, file="data_processed/wdi_data.RData")

# read quality of government
library(rqog)
qog_std <- read_qog(which.data = "standard", data.dir = "data_raw")
file.remove("data_raw/qog_std_ts_jan15.csv")
save(qog_std, file="data_processed/qog_std.RData")
