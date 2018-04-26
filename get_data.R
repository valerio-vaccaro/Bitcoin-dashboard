library(grid)
library(stringr)
library(RJSONIO)
library(Rbitcoin)
library(plyr)
library(igraph)
library(jsonlite)
library(httr)

setwd("~/r-studio-workspace/bitcoin/app")

# Get data
temp <- fromJSON("https://apiv2.bitcoinaverage.com/indices/global/history/BTCUSD?period=daily&format=json")
temp <- do.call(rbind,temp)
data.BTCUSD.daily <- as.data.frame(t(temp),stringsAsFactors=FALSE)
data.BTCUSD.daily$time <- as.POSIXct(unlist(data.BTCUSD.daily$time))
data.BTCUSD.daily$average <- unlist(data.BTCUSD.daily$average)
data.BTCUSD.daily$cur <- "USD"

temp <- fromJSON("https://apiv2.bitcoinaverage.com/indices/global/history/BTCEUR?period=daily&format=json")
temp <- do.call(rbind,temp)
data.BTCEUR.daily <- as.data.frame(t(temp),stringsAsFactors=FALSE)
data.BTCEUR.daily$time <- as.POSIXct(unlist(data.BTCEUR.daily$time))
data.BTCEUR.daily$average <- unlist(data.BTCEUR.daily$average)
data.BTCEUR.daily$cur <- "EUR"

temp <- fromJSON("https://apiv2.bitcoinaverage.com/indices/global/history/BTCUSD?period=monthly&format=json")
temp <- do.call(rbind,temp)
data.BTCUSD.monthly <- as.data.frame(t(temp),stringsAsFactors=FALSE)
data.BTCUSD.monthly$time <- as.POSIXct(unlist(data.BTCUSD.monthly$time))
data.BTCUSD.monthly$average <- unlist(data.BTCUSD.monthly$average)
data.BTCUSD.monthly$cur <- "USD"

temp <- fromJSON("https://apiv2.bitcoinaverage.com/indices/global/history/BTCEUR?period=monthly&format=json")
temp <- do.call(rbind,temp)
data.BTCEUR.monthly <- as.data.frame(t(temp),stringsAsFactors=FALSE)
data.BTCEUR.monthly$time <- as.POSIXct(unlist(data.BTCEUR.monthly$time))
data.BTCEUR.monthly$average <- unlist(data.BTCEUR.monthly$average)
data.BTCEUR.monthly$cur <- "EUR"

data.BTCEUR.monthly$average <- as.numeric(data.BTCEUR.monthly$average)
data.BTCUSD.monthly$average <- as.numeric(data.BTCUSD.monthly$average)
data.BTCEUR.daily$average <- as.numeric(data.BTCEUR.daily$average)
data.BTCUSD.daily$average <- as.numeric(data.BTCUSD.daily$average)
data.BTCEUR.monthly$time <- as.POSIXct(data.BTCEUR.monthly$time)
data.BTCUSD.monthly$time <- as.POSIXct(data.BTCUSD.monthly$time)
data.BTCEUR.daily$time <- as.POSIXct(data.BTCEUR.daily$time)
data.BTCUSD.daily$time <- as.POSIXct(data.BTCUSD.daily$time)
data.daily <- rbind(data.BTCUSD.daily, data.BTCEUR.daily)
data.monthly <- rbind(data.BTCUSD.monthly[, c("average","time","cur")], data.BTCEUR.monthly[, c("average","time","cur")])

save(data.daily, file="./data/data.daily.RData")
save(data.monthly, file="./data/data.monthly.RData")

today <- Sys.Date()
dataset <- NULL
actual_size <- as.numeric(fromJSON('https://chain.api.btc.com/v3/block/latest')$data$height)
start_size <- actual_size-(100*144)-1 # 144 blocks around 1 day
stop_size <- actual_size

load(file="./data/block_size_btc.com.RData")
dataset$delta <- NULL
if (max(dataset$height) > start_size) start_size = max(dataset$height)
while(stop_size!= max(dataset$height)){
   for(i in start_size:stop_size) {
      str(i)
      block <- fromJSON(paste0('https://chain.api.btc.com/v3/block/',i))
      row <- NULL
      row$height <- i
      row$hash <- block$data$hash
      row$size <- block$data$size
      row$n_tx <- block$data$tx_count
      row$time <- block$data$timestamp
      row$pool <- block$data$extras[["pool_name"]]
      row$when <- as.POSIXct(row$time, origin="1970-1-1")
      row$size_mb <- row$size/1024/1024
      dataset <- rbind(dataset, as.data.frame(row))
      Sys.sleep(1)
   }
   str("restart")
}
dataset$delta  <- c(0,tail(dataset$when, -1) - head(dataset$when, -1))
save(dataset, file="./data/block_size_btc.com.RData")


mempool <- read.csv("https://api.blockchair.com/bitcoin/mempool/transactions?fields=block_id,id,hash,time,is_coinbase,input_count,output_count,cdd_total,input_total,input_total_usd,output_total,output_total_usd,fee,fee_usd,fee_per_kb,fee_per_kb_usd,fee_per_kwu,fee_per_kwu_usd,has_witness,size,weight,lock_time&export=csv")
save(mempool, file="./data/mempool.RData")
