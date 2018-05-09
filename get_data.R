library(grid)
library(stringr)
library(RJSONIO)
library(Rbitcoin)
library(plyr)
library(igraph)
library(jsonlite)
library(httr)

setwd("~/r-studio-workspace/bitcoin/app")

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

options(scipen = 999)
today <- as.numeric(as.POSIXct(Sys.Date()))*1000
yesterday <- as.numeric(as.POSIXct(Sys.Date()-1))*1000
lastWeek <- as.numeric(as.POSIXct(Sys.Date()-7))*1000
lastMonth <- as.numeric(as.POSIXct(Sys.Date()-31))*1000

test <- fromJSON(paste0("https://graphs2.coinmarketcap.com/currencies/bitcoin/",yesterday,"/",today,"/"))
price_usd_day <- as.data.frame(test$price_usd)
if (nrow(price_usd_day) < 5) price_usd_day <- as.data.frame(matrix(unlist(test$price_usd), ncol = 2, byrow = TRUE))
names(price_usd_day) <- c("timestamp","value")
price_usd_day$timestamp <- anytime(price_usd_day$timestamp/1000)
save(price_usd_day, file="./data/price_usd_day.RData")

test2 <- fromJSON(paste0("https://graphs2.coinmarketcap.com/currencies/bitcoin/",lastWeek,"/",today,"/"))
price_usd_week <- as.data.frame(test2$price_usd)
if (nrow(price_usd_week) < 5) price_usd_week <- as.data.frame(matrix(unlist(test2$price_usd), ncol = 2, byrow = TRUE))
names(price_usd_week) <- c("timestamp","value")
price_usd_week$timestamp <- anytime(price_usd_week$timestamp/1000)
save(price_usd_week, file="./data/price_usd_week.RData")

test3 <- fromJSON(paste0("https://graphs2.coinmarketcap.com/currencies/bitcoin/",lastMonth,"/",today,"/"))
price_usd_month <- as.data.frame(test3$price_usd)
if (nrow(price_usd_month) < 5) price_usd_month <- as.data.frame(matrix(unlist(test3$price_usd), ncol = 2, byrow = TRUE))
names(price_usd_month) <- c("timestamp","value")
price_usd_month$timestamp <- anytime(price_usd_month$timestamp/1000)
save(price_usd_month, file="./data/price_usd_month.RData")