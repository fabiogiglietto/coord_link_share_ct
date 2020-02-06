# ------------------------------------------------------------------------------
# A function for cleaning the (expanded) Facebook URLs gathered from CrowdTangle 
# ------------------------------------------------------------------------------

CleanFacebookURLs <- function(dataset, URLcolumn) {
  library(urltools)
  
  # clan URLcolumns
  dataset <- dataset[!grepl("\\.\\.\\.$", dataset[, URLcolumn]),]
  
  dataset[, URLcolumn] <- gsub("\\?utm_.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?ref.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?fbclid.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?rss.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?ico.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?recruiter.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?sr_share_.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?fb_rel.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?social.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?intcmp_.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?xrs.*", "", dataset[, URLcolumn]) 
  dataset[, URLcolumn] <- gsub("\\?CMP.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?tid.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?ncid.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("&utm_.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?ncid.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?rbs&utm_hp_ref.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("/#\\..*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("\\?mobile.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("&fbclid.*", "", dataset[, URLcolumn])
  dataset[, URLcolumn] <- gsub("/$", "", dataset[, URLcolumn])
  
  dataset[, URLcolumn] <- gsub(".*(http)", "\\1", dataset[, URLcolumn]) # delete all before "http"
  dataset[, URLcolumn][grepl("^http://127.0.0.1", dataset[, URLcolumn])] <- dataset$link[grepl("^http://127.0.0.1", dataset[, URLcolumn])]
  dataset <- dataset[grepl("^http:|^https:|^www", dataset[, URLcolumn]),] # remove all the entries with the URLcolumn URLcolumn that does not start with "http" or "www"
  
  dataset[, URLcolumn] <- gsub(".*https://|.*http://|.*www.", "", dataset[, URLcolumn]) # remove www or http to "normalize" the URLcolumns
  dataset[, URLcolumn] <- gsub("[^[:alnum:][:punct:]]", "", dataset[, URLcolumn]) # remove non alphanumerical character or punctuation (some URLcolumns are followed by an emoticon)
  
  dataset[, URLcolumn] <- gsub("/$", "", dataset[, URLcolumn]) # remove ending URLcolumns == "/"
  
  # delete URLcolumns that are equal to the domain
  dataset$domain <- url_parse(dataset[, URLcolumn])$domain
  dataset$domain  <- gsub(".*https://|.*http://|.*www.", "", dataset$domain) # remove www or http to "normalize" the URLcolumns
  dataset$domain[dataset$domain=="digiunopersalvini.it"] <- "digiunopersalvini.it_exception" # mark "digiunopersalvini" as exception
  dataset <- dataset[dataset[, URLcolumn]!=dataset$domain,]
  dataset$domain[dataset$domain=="digiunopersalvini.it_exception"] <- "digiunopersalvini.it" # restore domains
  
  # remove possible rows with empty field URLcolumn 
  dataset <- dataset[!is.na(dataset[, URLcolumn]),]
  
  return(dataset)
  
}
