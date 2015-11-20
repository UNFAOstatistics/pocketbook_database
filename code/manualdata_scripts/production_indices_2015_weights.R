# new weights for 
library(dplyr)
library(stringr)
library(readr)

# downlaoded all items for all years for net production values for 2004:2006 
# from http://faostat3.fao.org/download/Q/QV/E

# production value data d

download.file("http://faostat3.fao.org/faostat-bulkdownloads/Value_of_Production_E_All_Data_(Norm).zip",
              destfile = "input_data/raw/Value_of_Production_E_All_Data_(Norm).zip")
unzip(zipfile = "./input_data/raw/Value_of_Production_E_All_Data_(Norm).zip",
      exdir = "./input_data/raw/")
d <- read_csv("./input_data/raw/Value_of_Production_E_All_Data_(Norm).csv")
names(d) <- str_replace_all(names(dat), " ", "")

# Meat
## Extract subindexes from Amandas production index data
dat <- read_csv("./input_data/raw/production_indices_2015_weights/production_indices_meat_subindicators.csv")
subind <- unique(dat$ItemName)
# subset the production VALUE data
d_meat <- d %>% filter(Item %in% subind, Year %in% 2004:2006, CountryCode < 5000) %>% 
  group_by(CountryCode,Year) %>% dplyr::summarise(prod_val_meat = sum(Value, na.rm = TRUE))


# Fruits and vegetables
## Extract subindexes from Amandas production index data
dat <- read_csv("./input_data/raw/production_indices_2015_weights/production_indices_vegetable_subindicators.csv")
subind <- unique(dat$ItemName)
# subset the production VALUE data
d_vege <- d %>% filter(Item %in% subind, Year %in% 2004:2006, CountryCode < 5000) %>% 
  group_by(CountryCode,Year) %>% dplyr::summarise(prod_val_vege = sum(Value, na.rm = TRUE))

# Milk
## Extract subindexes from Amandas production index data
dat <- read_csv("./input_data/raw/production_indices_2015_weights/production_indices_milk_subindicators.csv")
subind <- unique(dat$ItemName)
# subset the production VALUE data
d_milk <- d %>% filter(Item %in% subind, Year %in% 2004:2006, CountryCode < 5000) %>% 
  group_by(CountryCode,Year) %>% dplyr::summarise(prod_val_milk = sum(Value, na.rm = TRUE))

# Roots and tubers weights
## Extract subindexes from Amandas production index data
dat <- read_csv("./input_data/raw/production_indices_2015_weights/production_indices_roots_subindicators.csv")
subind <- unique(dat$ItemName)
# subset the production VALUE data
d_roots <- d %>% filter(Item %in% subind, Year %in% 2004:2006, CountryCode < 5000) %>% 
  group_by(CountryCode,Year) %>% dplyr::summarise(prod_val_roots = sum(Value, na.rm = TRUE))

# sugar
## Extract subindexes from Amandas production index data
dat <- read_csv("./input_data/raw/production_indices_2015_weights/production_indices_sugar_subindicators.csv")
subind <- unique(dat$ItemName)
# subset the production VALUE data
d_sugar <- d %>% filter(Item %in% subind, Year %in% 2004:2006, CountryCode < 5000) %>% 
  group_by(CountryCode,Year) %>% dplyr::summarise(prod_val_sugar = sum(Value, na.rm = TRUE))

# vegetable oils
## Extract subindexes from Amandas production index data
dat <- read_csv("./input_data/raw/production_indices_2015_weights/production_indices_vegoil_subindicators.csv")
subind <- unique(dat$ItemName)
# subset the production VALUE data
d_vegoil <- d %>% filter(Item %in% subind, Year %in% 2004:2006, CountryCode < 5000) %>% 
  group_by(CountryCode,Year) %>% dplyr::summarise(prod_val_vegoil = sum(Value, na.rm = TRUE))

prod_ind_weights.df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(d_vegoil,d_sugar,d_roots,d_milk,d_vege,d_meat))
names(prod_ind_weights.df)[names(prod_ind_weights.df)=="CountryCode"] <- "FAOST_CODE"
prod_ind_weights.df <- prod_ind_weights.df[!duplicated(prod_ind_weights.df[c("FAOST_CODE","Year")]),]

save(x = prod_ind_weights.df, file = "./input_data/processed/prod_ind_weights.RData")

