library(lubridate)
library(data.table)
library(igraph)
library(dplyr)
library(tidyr)
library(stringr)

setwd("~/coord_link_share_ct/")

estimate_coord_interval <- function(ct_shares.dt) {
  setkey(ct_shares.dt, expanded) # key to improve performances
  setkey(ct_shares.dt, platformId) # key to improve performances
  setkey(ct_shares.dt, date) # key to improve performances
  
  ct_shares.dt <- ct_shares.dt[order(date),] # sort by date
  
  # ranks shares x expanded (url) by date
  ranked_shares <- ct_shares.dt[, .(ct_shares_count=.N,
                                    first_share_date = min(date),
                                    rank = frank(date),
                                    date = date),
                                by=expanded]
  
  # print(mean(ranked_shares$rank))
  
  ranked_shares$sec_from_first_share <- difftime(ranked_shares$date,ranked_shares$first_share_date, units='secs') # seconds from first share
  ranked_shares$perc_of_shares <- ranked_shares$rank/ranked_shares$ct_shares_count # percentage of shares over total shares
  ranked_shares <- subset(ranked_shares, ranked_shares$ct_shares_count > 1) # remove URLs wiht less than 2 shares (can't be coordinated)
  
  # find URLs with an unusual fast second share
  rank_2 <- ranked_shares[rank == 2,
                          .(sec_from_first_share = min(sec_from_first_share)),
                          by=expanded]
  
  rank_2 <- subset(rank_2, rank_2$sec_from_first_share <= as.numeric(quantile(rank_2$sec_from_first_share, 0.1))) # keep 10% of quickest URLs
  
  ranked_shares <- subset(ranked_shares, ranked_shares$expanded %in% rank_2$expanded) # keep only shares of quick URLs
  
  # calculate the time it takes for a URL to reach the 50% of its total number of shares
  ranked_shares_50 <- ranked_shares[perc_of_shares > 0.5,
                                    .(sec_from_first_share = min(sec_from_first_share)),
                                    by=expanded]
  
  print(summary(as.numeric(ranked_shares_50$sec_from_first_share)))
  
  return(paste0(quantile(ranked_shares_50$sec_from_first_share, 0.5), " secs"))
}

# load the dataframe created in step 0
ct_shares.dt <- as.data.table(readRDS("./data/ct_shares.df.rds"))

# get a list of all shared URLs
URLs <- as.data.frame(table(ct_shares.dt$expanded))
names(URLs) <- c("URL", "ct_shares")
URLs <- subset(URLs, URLs$ct_shares>1) # remove URLs shared only 1 time
URLs$URL <- as.character(URLs$URL)

coordination.interval <- estimate_coord_interval(ct_shares.dt) # call the function

# initialize an empty object for results
datalist <- list()

# cycle trough all URLs to find entities that shared the same link within the coordination internal
for (i in 1:nrow(URLs)) {
  url <- URLs$URL[i]
  temp <- subset(ct_shares.dt, ct_shares.dt$expanded==url)
  # subset temp (all shares of one URL) by time published by differnet entities in a coordination.interval
  dat.summary <- setDT(temp)[ , list(count=.N, account.platformId=list(account.platformId), share_date=list(date)), by=cut(as.POSIXct(date), coordination.interval)]
  # avoid adding empty dataframes to datalist
  dat.summary <- subset(dat.summary, dat.summary$count>1)
  
  if (nrow(dat.summary) > 0) {
    dat.summary$url <- url
    datalist <- c(list(dat.summary), datalist)
  }
  rm(url, temp, dat.summary)
}

df <- rbindlist(datalist)
rm(datalist)

saveRDS(df, file = "./data/coordinated_shares.rds")

# create an adjacence matrix from the list of pages in each df row
b <- unique(unlist(df$account.platformId))
d <- unlist(sapply(df$account.platformId,combn,2,toString))
e <- data.frame(table(factor(d,c(paste(b,b,sep=','),combn(b,2,toString)))))
f <- read.table(text = do.call(paste,c(sep =',', e)),sep=',',strip.white = T)
g <- xtabs(V3~V1+V2,f)
g[lower.tri(g)] = t(g)[lower.tri(g)]

g <- graph_from_adjacency_matrix(adjmatrix = g,
                                 weighted = TRUE,
                                 mode="undirected")

write.graph(g,"./data/g.graphml",format = "graphml") # save the full graph

# keep only highly coordinated entities
V(g)$degree <- degree(g)
q <- quantile(E(g)$weight, 0.90) # set the thresold number of repetedly coordinated link sharing to keep 
g <- induced_subgraph(graph = g, vids = V(g)[V(g)$degree > 0 ]) # filter for degree
g <- subgraph.edges(g,eids = which(E(g)$weight >=  q),delete.vertices = T) # filter for edge weight
# find and annotate nodes-components
V(g)$component <- components(g)$membership

write.graph(g,"./data/coordinated_g.graphml", format = "graphml") # save graph of coordinated entities
coordinated_entities <- igraph::as_data_frame(g, "vertices")
write.csv(coordinated_entities, file = "./data/coordinated_entities.csv", row.names = F) # save for further analysis

rm(list = ls())
