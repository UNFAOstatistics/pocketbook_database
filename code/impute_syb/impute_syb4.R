#!/usr/bin/env r
###########################################################################
## Title: This script compute the FAO/RAF economical aggregates
## Created: 12-03-2017
## Modified: 12-03-2017
###########################################################################
# To allow parallel processing
session_path <- readLines(con = "~/faosync/pocketbooks/pocketbook_database/input_data/session_path.txt")
syb_path <- readLines(con = "~/faosync/pocketbooks/pocketbook_database/input_data/syb_path.txt")

# Needed libraries --------------------------------------------------------
pkg_list <- c("plyr","dplyr","reshape2","data.table","readr","RJSONIO","FAOSTAT")
lapply(pkg_list, library, character.only = TRUE)

load("~/faosync/pocketbooks/pocketbook_database/output_data/2017-02-25-14/SYB2017-02-25-14.RData")
syb.dfo <- SYB.df;syb.df <- SYB.df[!SYB.df$FAOST_CODE %in% "",]; rm(SYB.df)
# sum(colSums(is.na(syb.dfo)))
load(syb_path)
syb.df <- SYB.df;syb.df <- SYB.df[!SYB.df$FAOST_CODE %in% "",]; rm(SYB.df)
# sum(colSums(is.na(syb.df)))

for (y in 2004:2020) {
  for (cd in unique(syb.df$FAOST_CODE)){
    
    for (i in unique(names(syb.df))) {
      if (!i %in% unique(names(syb.dfo))) next()
      syb.df[syb.df$Year == y & 
               syb.df$FAOST_CODE == cd, i] <- ifelse(is.na(syb.df[syb.df$Year == y & 
                                                                    syb.df$FAOST_CODE == cd,i]),
                                                     syb.dfo[syb.dfo$Year == y & 
                                                               syb.dfo$FAOST_CODE == cd, i],
                                                     syb.df[syb.df$Year == y & 
                                                              syb.df$FAOST_CODE == cd, i])
    }
  }
}
saveRDS(syb.df, "~/faosync/pocketbooks/pocketbook/input/data/syb.df4.RDS")