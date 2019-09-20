library(httr)
library(jsonlite)
library(plyr)
library(dplyr)
library(lubridate)

setwd("~/coord_link_share_ct/")

dir.create("data", showWarnings = F)

# load sample urls with date_published in unix time
urls <- readRDS("./rawdata/urls.rds")

# Get my token
api_key <- URLencode("") # add api key here

ct_shares.df <- NULL

for (i in 1:nrow(urls)) {
  startDate <- as.POSIXct(urls$date_published[i], origin="1970-01-01", tz = "UTC")
  endDate <- startDate+days(7) # one week after date_published
  link <- urls$url[i]
  
  query <- GET("https://api.crowdtangle.com/links",
               query = list(
                 link = link,
                 platforms = "facebook,instagram",
                 startDate  = as.character(startDate),
                 endDate  = as.character(endDate),
                 includeSummary = "false",
                 sortBy = "date",
                 token = api_key,
                 count = 100))
  
  
  tryCatch(
    {
      json <- httr::content(query, as = "text", encoding = "UTF-8")
      c <- fromJSON(json, flatten = TRUE)
      if (c$status == 200) {
        if (length(c$result$posts) > 0) {
          ct_shares.df <- rbind_pages(list(ct_shares.df, c$result$posts))
        }
      }
      else {
        print(paste(c$status, i))
      }
    },
    error=function(cond) {
      print(paste("Error: ", message(cond), " on URL: ", i))
    },
    finally={
      Sys.sleep(1) # can be lowered depending on the assigned API rate limit
    })
}

ct_shares.df <- unnest(ct_shares.df, ct_shares.df$expandedLinks) # unnest expanded_url list and creates original and expanded (URL) fields
ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),] # remove duplicates created by the unnesting
ct_shares.df$original <- NULL # remove "original" unshortnen URL

saveRDS(ct_shares.df, file = "./data/ct_shares.df.rds")

rm(list = ls())
