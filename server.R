# server.R
library(ggplot2)
library(ggpmisc)
library(anytime)
library(grid)
library(stringr)
library(RJSONIO)
library(Rbitcoin)
library(plyr)
library(igraph)
library(plyr)
library(lubridate)

# load data
load("./data/block_size_btc.com.RData")

actual_size <- max(dataset$height)
filter_100_days <- dataset$height > actual_size-(100*144)-1
filter_7_days <- dataset$height > actual_size-(7*144)-1
filter_10_days <- dataset$height > actual_size-(10*144)-1
filter_30_days <- dataset$height > actual_size-(30*144)-1
filter_1_day <- dataset$height > actual_size-(1*144)-1

size_7_days <- mean(dataset[filter_7_days & dataset$size_mb>1, ]$size_mb)
size_10_days <- mean(dataset[filter_10_days & dataset$size_mb>1, ]$size_mb)
size_30_days <- mean(dataset[filter_30_days & dataset$size_mb>1, ]$size_mb)
size_1_days <- mean(dataset[filter_1_day & dataset$size_mb>1, ]$size_mb)

max_size_7 <- mean(dataset[dataset$height > actual_size-(7*144)-1 & dataset$size_mb>1, ]$size_mb)
max_size_10 <- mean(dataset[dataset$height > actual_size-(10*144)-1 & dataset$size_mb>1, ]$size_mb)
max_size_30 <- mean(dataset[dataset$height > actual_size-(30*144)-1 & dataset$size_mb>1, ]$size_mb)

filter_free_7_days <- dataset$height > actual_size-(7*144)-1 & (max_size_7-dataset$size_mb)>0.1
filter_free_10_days <- dataset$height > actual_size-(10*144)-1 & (max_size_10-dataset$size_mb)>0.1
filter_free_30_days <- dataset$height > actual_size-(30*144)-1 & (max_size_30-dataset$size_mb)>0.1
filter_free_1_day <- dataset$height > actual_size-(1*144)-1 & (max_size_7-dataset$size_mb)>0.1

load("./data/mempool.RData")
mempool$delta <- as.numeric(as.POSIXct(mempool$time) - max(as.POSIXct(mempool$time)))
mempool$has_witness <- as.factor(mempool$has_witness)

today <- as.numeric(as.POSIXct(Sys.Date()))*1000
load("./data/price_usd_day.RData")
load("./data/price_usd_week.RData")
load("./data/price_usd_month.RData")
daily_usd <- mean(price_usd_day$value)
weekly_usd <- mean(price_usd_week$value)
monthly_usd <- mean(price_usd_month$value)
price_usd_day$hour <- as.factor(hour(price_usd_day$timestamp))
price_usd_week$day <- as.factor(day(price_usd_week$timestamp))
price_usd_month$day <- as.factor(day(price_usd_month$timestamp))

server <- function(input, output) {
   
   output$dateBox <- renderInfoBox({
      infoBox(
         "Monthly average", paste(round(monthly_usd,3), "USD"), icon = icon("calendar"),
         color = "green"
      )
   })
   
   output$speedBox <- renderInfoBox({
      infoBox(
         "Weekly average", paste(round(weekly_usd,3), "USD"), icon = icon("road"),
         color = "yellow"
      )
   })
   
   output$pulseBox <- renderInfoBox({
      infoBox(
         "Daily average", paste(round(daily_usd,3), "USD"), icon = icon("heart"),
         color = "red"
      )
   })

   output$dp <- renderPlot({
      ggplot(price_usd_day, aes(x=timestamp, y=value)) +
         stat_smooth(
            color = "#FC4E07", fill = "#FC4E07",
            method = "loess"
         ) +
         geom_hline(aes(yintercept=daily_usd), color="red", linetype="dashed") +
         geom_hline(aes(yintercept=weekly_usd), color="yellow", linetype="dashed") +
         geom_hline(aes(yintercept=monthly_usd), color="green", linetype="dashed") +
         geom_line() +
         stat_peaks(colour = "red", span = 25) +
         stat_peaks(geom = "text", colour = "red", span = 25, 
                    vjust = -0.5, x.label.fmt = "%H:%M") +
         stat_valleys(colour = "blue", span = 25) +
         stat_valleys(geom = "text", colour = "blue", angle = 45, span = 25,
                      vjust = 1.5, hjust = 1,  x.label.fmt = "%H:%M") +
         annotate("text", x = max(price_usd_day$timestamp), y = Inf, label = paste0(as.Date(anytime(today/1000)), " - valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         ggtitle("BTC fiat price - 1 day") +
         labs(y="Fiat price [USD]", x="Date") +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$dh <- renderPlot({
      ggplot(price_usd_day, aes(x=value, fill=hour)) +
         geom_histogram() +
         annotate("text", x = max(price_usd_day$value), y = Inf, label = paste0(as.Date(anytime(today/1000)), " - valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         ggtitle("BTC fiat price distribution - 1 day") +
         labs(y="", x="Fiat price [USD]") +
         scale_y_continuous(labels = scales::percent) +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$wp <- renderPlot({
      ggplot(price_usd_week, aes(x=timestamp, y=value)) +
         stat_smooth(
            color = "#FC4E07", fill = "#FC4E07",
            method = "loess"
         ) +
         geom_hline(aes(yintercept=daily_usd), color="red", linetype="dashed") +
         geom_hline(aes(yintercept=weekly_usd), color="yellow", linetype="dashed") +
         geom_hline(aes(yintercept=monthly_usd), color="green", linetype="dashed") +
         geom_line() +
         stat_peaks(colour = "red", span = 25) +
         stat_peaks(geom = "text", colour = "red", span = 25, 
                    vjust = -0.5, x.label.fmt = "%H:%M") +
         stat_valleys(colour = "blue", span = 25) +
         stat_valleys(geom = "text", colour = "blue", angle = 45, span = 25,
                      vjust = 1.5, hjust = 1,  x.label.fmt = "%H:%M") +
         annotate("text", x = max(price_usd_day$timestamp), y = Inf, label = paste0(as.Date(anytime(today/1000)), " - valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         ggtitle("BTC fiat price - 1 week") +
         labs(y="Fiat price [USD]", x="Date") +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$wh <- renderPlot({
      ggplot(price_usd_week, aes(x=value, fill=day)) +
         geom_histogram() +
         annotate("text", x = max(price_usd_week$value), y = Inf, label = paste0(as.Date(anytime(today/1000)), " - valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         ggtitle("BTC fiat price distribution - 1 week") +
         labs(y="", x="Fiat price [USD]") +
         scale_y_continuous(labels = scales::percent) +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$mp <- renderPlot({
      ggplot(price_usd_month, aes(x=timestamp, y=value)) +
         stat_smooth(
            color = "#FC4E07", fill = "#FC4E07",
            method = "loess"
         ) +
         geom_hline(aes(yintercept=daily_usd), color="red", linetype="dashed") +
         geom_hline(aes(yintercept=weekly_usd), color="yellow", linetype="dashed") +
         geom_hline(aes(yintercept=monthly_usd), color="green", linetype="dashed") +
         geom_line() +
         stat_peaks(colour = "red", span = 25) +
         stat_peaks(geom = "text", colour = "red", span = 25, 
                    vjust = -0.5, x.label.fmt = "%d %H:%M") +
         stat_valleys(colour = "blue", span = 25) +
         stat_valleys(geom = "text", colour = "blue", angle = 45, span = 25,
                      vjust = 1.5, hjust = 1,  x.label.fmt = "%H:%M") +
         annotate("text", x = max(price_usd_day$timestamp), y = Inf, label = paste0(as.Date(anytime(today/1000)), " - valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         ggtitle("BTC fiat price - 1 month") +
         labs(y="Fiat price [USD]", x="Date") +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$mh <- renderPlot({
      ggplot(price_usd_month, aes(x=value, fill=day)) +
         geom_histogram() +
         annotate("text", x = max(price_usd_month$value), y = Inf, label = paste0(as.Date(anytime(today/1000)), " - valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         ggtitle("BTC fiat price - 1 month") +
         labs(y="", x="Fiat price [USD]") +
         scale_y_continuous(labels = scales::percent) +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   # output$b10 <- renderPlot({
   #    ggplot(dataset[filter_10_days, ], aes(x=as.factor(pool), y=size_mb,  alpha=0.01)) +
   #       geom_point(aes(color=pool)) +
   #       geom_hline(yintercept=1) +
   #       geom_text(data=data.frame(x=0,y=1),  aes(x, y), label="1MB", vjust=+2, hjust=-1) +
   #       labs(title="Distribution of blocks per pool - 10 days analyzed.",
   #            x ="Pool", y = "Block size (MB)") +
   #       theme(axis.text.x = element_text(angle=45, hjust=1)) +
   #       theme(legend.position="none") +
   #       annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
   #                hjust=1.1, vjust=1.1, col="black", cex=4,
   #                fontface = "bold", alpha = 0.3)
   # })
   # 
   # output$bh10 <- renderPlot({
   #    ggplot(dataset[filter_10_days, ], aes(x=pool)) +
   #       geom_bar(mapping = aes(x = pool, y = ..count.., fill = pool)) +
   #       labs(title="Distribution of mined blocks per pool - 10 days analyzed.",
   #            x ="Pool", y = "# blocks") +
   #       theme(axis.text.x = element_text(angle=45, hjust=1)) +
   #       theme(legend.position="none") +
   #       annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
   #                hjust=1.1, vjust=1.1, col="black", cex=4,
   #                fontface = "bold", alpha = 0.3)
   # })
   
   output$b30 <- renderPlot({
      ggplot(dataset[filter_30_days, ], aes(x=as.factor(pool), y=size_mb,  alpha=0.01)) +
         geom_violin(aes(fill=pool)) +
         geom_hline(yintercept=1) +
         geom_text(data=data.frame(x=0,y=1),  aes(x, y), label="1MB", vjust=+2, hjust=-1) +
         labs(title="Distribution of blocks per pool - 30 days analyzed.",
              x ="Pool", y = "Block size (MB)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$bh30 <- renderPlot({
      ggplot(dataset[filter_30_days, ], aes(x=pool)) +
         geom_bar(mapping = aes(x = pool, y = ..count.., fill = pool)) +
         labs(title="Distribution of mined blocks per pool - 30 days analyzed.",
              x ="Pool", y = "# blocks") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$b7 <- renderPlot({
      ggplot(dataset[filter_7_days, ], aes(x=as.factor(pool), y=size_mb,  alpha=0.01)) +
         geom_violin(aes(fill=pool)) +
         geom_hline(yintercept=1) +
         geom_text(data=data.frame(x=0,y=1),  aes(x, y), label="1MB", vjust=+2, hjust=-1) +
         labs(title="Distribution of blocks per pool - 7 days analyzed.",
              x ="Pool", y = "Block size (MB)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$bh7 <- renderPlot({
      ggplot(dataset[filter_7_days, ], aes(x=pool)) +
         geom_bar(mapping = aes(x = pool, y = ..count.., fill = pool)) +
         labs(title="Distribution of mined blocks per pool - 7 days analyzed.",
              x ="Pool", y = "# blocks") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   output$b1 <- renderPlot({
      ggplot(dataset[filter_1_day, ], aes(x=as.factor(pool), y=size_mb,  alpha=0.01)) +
         geom_violin(aes(fill=pool)) +
         geom_hline(yintercept=1) +
         geom_text(data=data.frame(x=0,y=1),  aes(x, y), label="1MB", vjust=+2, hjust=-1) +
         labs(title="Distribution of blocks per pool - 1 day analyzed.",
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
         labs(title="Distribution of mined blocks per pool - 1 day analyzed.",
              x ="Pool", y = "# blocks") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) +
         theme(legend.position="none") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3)
   })
   
   
   # output$s10 <- renderPlot({
   #    ggplot(dataset[filter_10_days, ], aes(x=when, y=as.numeric(delta), color=size_mb,  alpha=0.01)) +
   #       geom_point() +
   #       geom_hline(yintercept=600, color="red") +
   #       geom_smooth() +
   #       labs(title="Block delay in 10 days",
   #            x ="Timestamp of the block", y = "Delta from previouse block (s)") +
   #       theme(axis.text.x = element_text(angle=45, hjust=1)) 
   # })
   # 
   # output$f10 <- renderPlot({
   #    ggplot(dataset[filter_free_10_days, ], aes(x=height, y=max_size-size_mb)) +
   #       geom_point(aes(color=pool)) +
   #       geom_hline(yintercept=1, color="red") +
   #       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
   #       labs(title="Free space in blocks - 10 days analyzed.",
   #            x ="Height", y = "Free space (MB)") +
   #       annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
   #                hjust=1.1, vjust=1.1, col="black", cex=4,
   #                fontface = "bold", alpha = 0.3)
   # })
   
   output$s30 <- renderPlot({
      ggplot(dataset[filter_30_days, ], aes(x=when, y=as.numeric(delta), color=size_mb,  alpha=0.01)) +
         geom_point() +
         geom_hline(yintercept=600, color="red") +
         geom_smooth() +
         labs(title="Block delay in 30 days",
              x ="Timestamp of the block", y = "Delta from previouse block (s)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) 
   })
   
   output$f30 <- renderPlot({
      ggplot(dataset[filter_free_30_days, ], aes(x=height, y=max_size_30-size_mb)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1, color="red") +
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
         labs(title="Free space in blocks - 30 days analyzed.",
              x ="Height", y = "Free space (MB)") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$s7 <- renderPlot({
      ggplot(dataset[filter_7_days, ], aes(x=when, y=as.numeric(delta), color=size_mb,  alpha=0.01)) +
         geom_point() +
         geom_hline(yintercept=600, color="red") +
         geom_smooth() +
         labs(title="Block delay in 7 days",
              x ="Timestamp of the block", y = "Delta from previouse block (s)") +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$f7 <- renderPlot({
      ggplot(dataset[filter_free_7_days, ], aes(x=height, y=max_size_7-size_mb)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1, color="red") +
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
         labs(title="Free space in blocks - 7 days analyzed.",
              x ="Height", y = "Free space (MB)") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         theme(axis.text.x = element_text(angle=45, hjust=1))
   })
   
   output$s1 <- renderPlot({
      ggplot(dataset[filter_1_day, ], aes(x=when, y=as.numeric(delta), color=size_mb,  alpha=0.01)) +
         geom_point() +
         geom_hline(yintercept=600, color="red") +
         geom_smooth() +
         labs(title="Block delay in 1 day",
              x ="Timestamp of the block", y = "Delta from previouse block (s)") +
         theme(axis.text.x = element_text(angle=45, hjust=1)) 
   })
   
   output$f1 <- renderPlot({
      ggplot(dataset[filter_free_1_day, ], aes(x=height, y=max_size_7-size_mb)) +
         geom_point(aes(color=pool)) +
         geom_hline(yintercept=1, color="red") +
         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
         labs(title="Free space in blocks - 1 day analyzed.",
              x ="Height", y = "Free space (MB)") +
         annotate("text", x = Inf, y = Inf, label = paste0("valeriovaccaro.it"),
                  hjust=1.1, vjust=1.1, col="black", cex=4,
                  fontface = "bold", alpha = 0.3) +
         theme(axis.text.x = element_text(angle=45, hjust=1))
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