# server.R
library(ggplot2)
library(grid)
library(stringr)
library(RJSONIO)
library(Rbitcoin)
library(plyr)
library(igraph)
library(plyr)

# load data
load("./data/data.daily.RData")
load("./data/data.monthly.RData")

data.BTCEUR.daily <- data.daily[data.daily$cur=="EUR", ]
data.BTCUSD.daily <- data.daily[data.daily$cur=="USD", ]
act_eur <- data.BTCEUR.daily[1, ]$average
act_usd <- data.BTCUSD.daily[1, ]$average
daily_eur <- round(mean(data.BTCEUR.daily$average), digits = 2)
daily_usd <- round(mean(data.BTCUSD.daily$average), digits = 2)
data.BTCEUR.monthly <- data.monthly[data.monthly$cur=="EUR", ]
data.BTCUSD.monthly <- data.monthly[data.monthly$cur=="USD", ]
monthly_eur <- round(mean(data.BTCEUR.monthly$average), digits = 2)
monthly_usd <- round(mean(data.BTCUSD.monthly$average), digits = 2)

load("./data/block_size_btc.com.RData")

actual_size <- max(dataset$height)
filter_100_days <- dataset$height > actual_size-(100*144)-1
filter_10_days <- dataset$height > actual_size-(10*144)-1
filter_1_day <- dataset$height > actual_size-(1*144)-1
size_10_days <- mean(dataset[filter_10_days & dataset$size_mb>1, ]$size_mb)
size_1_days <- mean(dataset[filter_1_day & dataset$size_mb>1, ]$size_mb)

max_size <- mean(dataset[dataset$height > actual_size-(10*144)-1 & dataset$size_mb>1, ]$size_mb)
filter_free_10_days <- dataset$height > actual_size-(10*144)-1 & (max_size-dataset$size_mb)>0.1
filter_free_1_day <- dataset$height > actual_size-(1*144)-1 & (max_size-dataset$size_mb)>0.1

load("./data/mempool.RData")
mempool$delta <- as.numeric(as.POSIXct(mempool$time) - max(as.POSIXct(mempool$time)))
mempool$has_witness <- as.factor(mempool$has_witness)

server <- function(input, output) {
   
   output$dateBox <- renderInfoBox({
      infoBox(
         "Monthly average", paste(monthly_eur,"EUR"),paste(monthly_usd, "USD"), icon = icon("calendar"),
         color = "green"
      )
   })
   
   output$speedBox <- renderInfoBox({
      infoBox(
         "Daily average",  paste(daily_eur,"EUR"),paste(daily_usd, "USD"), icon = icon("road"),
         color = "yellow"
      )
   })
   
   output$pulseBox <- renderInfoBox({
      infoBox(
         "Actual value",  paste(act_eur,"EUR"),paste(act_usd, "USD"), icon = icon("heart"),
         color = "red"
      )
   })

   output$ma <- renderPlot({
      ggplot(data.monthly, aes(x=time, color=cur)) +
         geom_line(aes(y=average)) +
         geom_hline(aes(yintercept=act_eur), color="red", linetype="dotted")+
         geom_hline(aes(yintercept=act_usd), color="red", linetype="dashed")+
         geom_hline(aes(yintercept=daily_eur), color="yellow", linetype="dotted")+
         geom_hline(aes(yintercept=daily_usd), color="yellow", linetype="dashed")+
         geom_hline(aes(yintercept=monthly_eur), color="green", linetype="dotted")+
         geom_hline(aes(yintercept=monthly_usd), color="green", linetype="dashed")
   })
   
   output$mh <- renderPlot({
      ggplot(data.monthly, aes(x=average, fill=cur)) +
         geom_histogram() +
         scale_y_continuous(labels = scales::percent)
      
   })
   
   output$da <- renderPlot({
      ggplot(data.daily, aes(x=time, color=cur)) +
         geom_line(aes(y=average)) +
         geom_hline(aes(yintercept=act_eur), color="red", linetype="dotted")+
         geom_hline(aes(yintercept=act_usd), color="red", linetype="dashed")+
         geom_hline(aes(yintercept=daily_eur), color="yellow", linetype="dotted")+
         geom_hline(aes(yintercept=daily_usd), color="yellow", linetype="dashed")+
         geom_hline(aes(yintercept=monthly_eur), color="green", linetype="dotted")+
         geom_hline(aes(yintercept=monthly_usd), color="green", linetype="dashed")
   })
   
   output$dh <- renderPlot({
      ggplot(data.daily, aes(x=average, fill=cur)) +
         geom_histogram() +
         scale_y_continuous(labels = scales::percent)
   })
   
   output$b10 <- renderPlot({
      ggplot(dataset[filter_10_days, ], aes(x=as.factor(pool), y=size_mb,  alpha=0.01)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1) +
         geom_text(data=data.frame(x=0,y=1),  aes(x, y), label="1MB", vjust=+2, hjust=-1) +
         labs(title="Distribution of blocks per pool - 10 days analyzed.",
              x ="Pool", y = "Block size (MB)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$bh10 <- renderPlot({
      ggplot(dataset[filter_10_days, ], aes(x=pool)) +
         geom_bar(mapping = aes(x = pool, y = ..count.., fill = pool)) +
         labs(title="Distribution of mined blocks per pool - 10 days analyzed.",
              x ="Pool", y = "# blocks") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$b1 <- renderPlot({
      ggplot(dataset[filter_1_day, ], aes(x=as.factor(pool), y=size_mb,  alpha=0.01)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1) +
         geom_text(data=data.frame(x=0,y=1),  aes(x, y), label="1MB", vjust=+2, hjust=-1) +
         labs(title="Distribution of blocks per pool - 10 days analyzed.",
              x ="Pool", y = "Block size (MB)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$bh1 <- renderPlot({
      ggplot(dataset[filter_1_day, ], aes(x=pool)) +
         geom_bar(mapping = aes(x = pool, y = ..count.., fill = pool)) +
         labs(title="Distribution of mined blocks per pool - 10 days analyzed.",
              x ="Pool", y = "# blocks") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$s1 <- renderPlot({
      ggplot(dataset[filter_1_day, ], aes(x=when, y=as.numeric(delta), color=size_mb,  alpha=0.01)) +
         geom_point() +
         geom_hline(yintercept=600, color="red") +
         geom_smooth() +
         labs(title="Block delay in 10 days",
              x ="Timestamp of the block", y = "Delta from previouse block (s)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) 
   })
   
   output$s10 <- renderPlot({
      ggplot(dataset[filter_10_days, ], aes(x=when, y=as.numeric(delta), color=size_mb,  alpha=0.01)) +
         geom_point() +
         geom_hline(yintercept=600, color="red") +
         geom_smooth() +
         labs(title="Block delay in 10 days",
              x ="Timestamp of the block", y = "Delta from previouse block (s)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) 
   })
   
   output$f10 <- renderPlot({
      ggplot(dataset[filter_free_10_days, ], aes(x=height, y=max_size-size_mb)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1, color="red") +
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
         labs(title="Free space in blocks - 10 days analyzed.",
              x ="Height", y = "Free space (MB)") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$f1 <- renderPlot({
      ggplot(dataset[filter_free_1_day, ], aes(x=height, y=max_size-size_mb)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1, color="red") +
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
         labs(title="Free space in blocks - 10 days analyzed.",
              x ="Height", y = "Free space (MB)") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$fee <- renderPlot({
      ggplot(mempool, aes(x=fee_per_kb/1000, fill=as.factor(delta)))+
         geom_histogram() +
         scale_x_continuous(limits = c(0, 10)) +
         labs(title="Fees distribution in mempool",
              x ="Fee [satoshi per byte]", y = "# transactions") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$adis <- renderPlot({        
      ggplot(mempool, aes(x=delta/3600, fill=as.factor(fee_per_kb/1000)))+
         geom_histogram() +
         scale_x_continuous(limits = c(-1, 0)) +
         labs(title="Ages distribution in mempool (last hour)",
              x ="Age [h]", y = "# transactions") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$sdis <- renderPlot({   
      ggplot(mempool, aes(x=size, fill=has_witness))+
         geom_histogram() +
         scale_x_continuous(limits = c(0, 1000)) +
         labs(title="Size distribution in mempool (last hour)",
              x ="Size", y = "# transactions") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   }) 
   
   output$feesw <- renderPlot({
      ggplot(mempool, aes(x=fee_per_kb/1000, fill=has_witness))+
         geom_histogram() +
         scale_x_continuous(limits = c(0, 10)) +
         labs(title="Fees distribution in mempool, comparison with SegWit",
              x ="Fee [satoshi per byte]", y = "# transactions") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
      
   })
   
   output$pl <- renderPlot({
      seed <- input$wallet
      singleaddress <- blockchain.api.query(method = 'Single Address', bitcoin_address = seed, limit=10000)
      txs <- singleaddress$txs
      bc <- data.frame()
      for (t in txs) {
         hash <- t$hash
         for (inputs in t$inputs) {
            from <- inputs$prev_out$addr
            for (out in t$out) {
               to <- out$addr
               va <- out$value
               bc <- rbind(bc, data.frame(from=from,to=to,value=va, stringsAsFactors=F))
            }
         }
      }
      
      btc <- ddply(bc, c("from", "to"), summarize, value=sum(value))
      btc.net <- graph.data.frame(btc, directed=T)
      V(btc.net)$color <- "blue"
      V(btc.net)$color[unlist(V(btc.net)$name) == seed] <- "red"
      nodes <- unlist(V(btc.net)$name)
      E(btc.net)$width <- log(E(btc.net)$value)/10
      plot.igraph(btc.net, vertex.size=5, edge.arrow.size=0.1, vertex.label=NA, main=paste("BTC transaction network for\n", seed))
      
   })
   
   output$wallet <- renderDataTable({
      blockchain.api.process(input$wallet)
   })
   
   output$transactions <- renderDataTable({
      seed <- input$wallet
      singleaddress <- blockchain.api.query(method = 'Single Address', bitcoin_address = seed, limit=10000)
      txs <- singleaddress$txs
      bc <- data.frame()
      for (t in txs) {
         hash <- t$hash
         for (inputs in t$inputs) {
            from <- inputs$prev_out$addr
            for (out in t$out) {
               to <- out$addr
               va <- out$value
               bc <- rbind(bc, data.frame(from=from,to=to,value=va, stringsAsFactors=F))
            }
         }
      }
      bc
   })
   
}