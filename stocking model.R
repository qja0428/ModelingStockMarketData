

finviz <- read.csv("finviz.csv")


clean_numeric <- function(s){
  s <- gsub("%|\\$|,|\\)|\\(", "", s)
  s <- as.numeric(s)
}

finviz <- cbind(finviz[,1:6],apply(finviz[,7:68], 2, clean_numeric))

sector_avg_prices <-
  aggregate(Price~Sector,data=finviz,FUN="mean")

sector_avg_prices <- ddply(finviz,"Sector", summarise,
                           Sector_Avg_Price = mean(Price))

ggplot(sector_avg_prices, aes(x=Sector, y=Sector_Avg_Price,
                              fill=Sector)) +
  geom_bar(stat="identity") + ggtitle("Sector Avg Prices") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=FALSE)

industry_avg_prices <- ddply(finviz, .(Sector, Industry), 
                             summarise, Industry_Avg_Price = mean(Price))

industry_avg_prices <- arrange(industry_avg_prices, Sector, Industry)

industry_chart <- filter(industry_avg_prices, Sector=="Financial")
  
ggplot(industry_chart, aes(x=Industry, y=Industry_Avg_Price,
                           fill=Industry)) +
  geom_bar(stat="identity") + theme(legend.position="none") +
  ggtitle("Industry Avg Prices") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

company_chart <- filter(finviz, Industry=="Property & Casualty Insurance")

ggplot(company_chart, aes(x=Company, y=Price, fill=Company)) +
  geom_bar(stat="identity") + theme(legend.position="none") +
  ggtitle("Company Avg Prices") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

finviz <- filter(finviz, Ticker!="BRK-A")

sector_avg <- melt(finviz, id="Sector")
sector_avg <- filter(sector_avg,variable %in% c("Price","P.E","PEG","P.S","P.B"))

sector_avg <- (na.omit(sector_avg))
sector_avg$value <- as.numeric(sector_avg$value)

sector_avg <- dcast(sector_avg, Sector~variable, mean)

names(sector_avg)[2:6] <-
  c("SAvgPE","SAvgPEG","SAvgPS","SAvgPB","SAvgPrice")


industry_avg <- melt(finviz, id=c("Sector","Industry"))
industry_avg <- subset(industry_avg,variable %in%
                         c("Price","P.E","PEG","P.S","P.B"))
industry_avg <- (na.omit(industry_avg))
industry_avg$value <- as.numeric(industry_avg$value)
industry_avg <- dcast(industry_avg, Sector+Industry~variable,
                      mean)
industry_avg <- (na.omit(industry_avg))
colnames(industry_avg)[3:7] <-
  c("IAvgPE","IAvgPEG","IAvgPS","IAvgPB","IAvgPrice")


finviz <- merge(finviz, sector_avg, by.x="Sector",
                by.y="Sector")
finviz <- merge(finviz, industry_avg,
                by.x=c("Sector","Industry"), by.y=c("Sector","Industry"))

finviz$RelValIndex <- apply(finviz[79:88],1,sum)
potentially_undervalued <- subset(finviz,RelValIndex>=8)

target_stocks <- filter(finviz, Price>20 & Price<100 &
                          Volume>10000 &
                          Country=="USA" &
                          EPS..ttm.>0 &
                          EPS.growth.next.year>0 &
                          EPS.growth.next.5.years>0 &
                          Total.Debt.Equity<1 & Beta<1.5 &
                          Institutional.Ownership<30 &
                          RelValIndex>8)

counter <- 0
for (symbol in target_stocks$Ticker){

  url <-
  paste0("http://ichart.finance.yahoo.com/table.csv?s=",symbol,"&a=08&b=7&c=1984&d=01&e=23&f=2014&g=d&ignore=.csv")
  stock <- read.csv(url)
  stock <- na.omit(stock)
  colnames(stock)[7] <- "AdjClose"
  stock[,1] <- as.Date(stock[,1])
  stock <- cbind(Symbol=symbol,stock)
  maxrow <- nrow(stock)-49
  ma50 <-
    cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,50,align="right"))
  maxrow <- nrow(stock)-199
  ma200 <-
    cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,200,align="right"))
  stock <-
    merge(stock,ma50,by.x=c("Symbol","Date"),by.y=c("Symbol",
                                                    "Date"),all.x=TRUE)
  colnames(stock)[9] <- "MovAvg50"
  stock <-
    merge(stock,ma200,by.x=c("Symbol","Date"),by.y=c("Symbol",
                                                     "Date"),all.x=TRUE)
  colnames(stock)[10] <- "MovAvg200"
  price_chart <-
    melt(stock[,c(1,2,8,9,10)],id=c("Symbol","Date"))
  qplot(Date, value, data=price_chart, geom="line",
        color=variable,
        main=paste(symbol,"Daily Stock Prices"),ylab="Price")
  ggsave(filename=paste0("stock_price_",counter,".png"))
  
  price_summary <- ddply(stock, "Symbol", summarise,
                         open=Open[nrow(stock)],
                         high=max(High),
                         low=min(Low),close=AdjClose[1])
  if(counter==0){
    stocks <- rbind(stock)
    price_summaries <- rbind(price_summary)
  }else{
    stocks <- rbind(stocks, stock)
    price_summaries <- rbind(price_summaries, price_summary)
  }
  counter <- counter+1
  

}

qplot(Date, AdjClose, data=stocks, geom="line", color=Symbol,
      main="Daily Stock Prices")

summary <- melt(price_summaries,id="Symbol")
ggplot(summary, aes(x=variable, y=value, fill=Symbol)) +
  geom_bar(stat="identity") + facet_wrap(~Symbol)



