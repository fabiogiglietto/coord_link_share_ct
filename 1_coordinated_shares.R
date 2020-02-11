library(lubridate)
library(data.table)
library(igraph)
library(stringr)
library(tidyverse)

estimate_coord_interval <- function(ct_shares.dt) {
  setkey(ct_shares.dt, expanded) # key to improve performances
  setkey(ct_shares.dt, account.url) # key to improve performances
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
  dat.summary <- setDT(temp)[ , list(count=.N, account.url=list(account.url), share_date=list(date)), by=cut(as.POSIXct(date), coordination.interval)]
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

el2 <- df[,c(3,5)] # drop unnecesary columns
el <- unnest(el2) # divide platforms.ids over multiple rows
el$account.url <- trimws(el$account.url) # remove white space from platform.id
v1 <- data.frame(node=unique(el$account.url), type=1) # create a dataframe with nodes and type 0=url 1=page
v2 <- data.frame(node=unique(el$url), type=0)
v <- rbind(v1,v2)
rm(v1,v2) #clear

g2.bp <- graph.data.frame(el,directed = T,vertices = v) # makes the biap
g2.bp <- igraph::simplify(g2.bp, remove.multiple = T, remove.loops = T) # simply the bipartite netwrok to avoid problems with resulting edge weight in projected network
g <- bipartite.projection(g2.bp, multiplicity = T)$proj2 # project page-page network

# cleanup
rm(g2.bp, el, el2, v)

# get a list of unique entities (pages/groups/verified profiles/instagram accounts)
account.info <- ct_shares.dt[, .(shares = .N,
                                 avg.account.subscriberCount=mean(account.subscriberCount)),
                             by = list(account.url)]

more.account.info <- ct_shares.dt[, grepl("account.", colnames(ct_shares.dt)), with=FALSE]
more.account.info$account.subscriberCount <- NULL

account.info <- more.account.info[account.info,
                                  mult = "first",
                                  on = "account.url",
                                  nomatch=0L,]

saveRDS(account.info, file = "./data/all_ct_entities.rds") # save output before subsetting

# add vertex attributes
all_entities.dt <- as.data.table(readRDS("./data/ct_shares.df.rds"))

vertex.info <- subset(account.info, as.character(account.info$account.url) %in% V(g)$name)

V(g)$shares <- sapply(V(g)$name, function(x) vertex.info$shares[vertex.info$account.url == x])
V(g)$avg.account.subscriberCount <- sapply(V(g)$name, function(x) vertex.info$avg.account.subscriberCount[vertex.info$account.url == x]) 
V(g)$account.platform <- sapply(V(g)$name, function(x) vertex.info$account.platform[vertex.info$account.url == x]) 
V(g)$account.name <- sapply(V(g)$name, function(x) vertex.info$account.name[vertex.info$account.url == x]) 
V(g)$account.verified <- sapply(V(g)$name, function(x) vertex.info$account.verified[vertex.info$account.url == x]) 
V(g)$account.handle <- sapply(V(g)$name, function(x) vertex.info$account.handle[vertex.info$account.url == x]) 

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