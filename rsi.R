#######RSI######
#Takes in a vector of prices
rsi <- function(priceS){
  library("TTR")
  
  #Set the price series to a numeric
  priceS <- as.character(priceS)
  priceS <- as.numeric(priceS)
  if(sum(is.na(priceS)) >= 3){
    return(0)
  }
  
  priceS[is.na(priceS)] = 0
  closes <- rep(NA, (length(priceS)-1))
  
  #Compute the simple returns
  closes <- diff(priceS)
  
  #Compute the up and down change vectors
  upChange <- rep(0, length(closes))
  downChange <- rep(0, length(closes))
  
  upChange[closes>0] = closes[closes>0]
  downChange[closes>0] = 0
  
  upChange[closes<0] = 0
  downChange[closes<0] = abs(closes[closes<0])
  
  #for(j in 1:length(closes)){
  #  if(closes[j]>0){
  #    upChange[j] <- closes[j]
  #    downChange[j] <- 0
  #  }
  #  else if(closes[j]<0){
  #    upChange[j] <- 0
  #    downChange[j] <- abs(closes[j])
  #  }
  #  else{
  #    upChange[j] <- 0
  #    downChange[j] <- 0
  #  }
  }
  
  #Calculate relative strength using exponetial moving average with no correction
  smoothUp <- EMA(upChange, n=length(closes))
  smoothDown <- EMA(downChange, n=length(closes))
  RS <- smoothUp[length(smoothUp)] / smoothDown[length(smoothDown)]

  #Compute the relative strength index
  RSI <- 100 - 100/(1+RS)
  return(RSI)
  
}

#########Top Stocks#######
#Takes in the singledate of stock data, the ticker data csv, year i.e. 1990 and number of stocks as inputs. 
#Returns a list of lists for 12 months with n-stocks each. 
#Read data in like this.
#ticker <- read.csv("ticker.csv",header=TRUE)
#singledate <- read.csv("singledate.csv", header=TRUE)
#Example function call:
#topN_stocks(singledate, ticker, 1991, 3)
#This will return the top 3 stocks for each month in 1991

topN_stocks <- function(frame,ticker,year,N_stocks){
  library("lubridate")  
  
  #Find the universe of stocks for the given year
  yearX <- paste('X',year,sep="")
  stockUni <- ticker[[yearX]]
  stockUni <- gsub(" ",".",stockUni)
  
  
  #Subset frame into only those stocks in the year
  frameStock <- frame[,which(names(frame) %in% stockUni)]

  #Parse into date time object
  dates <- as.POSIXlt(as.character(frame[,1]), format="%m/%d/%y")

  #Subset dataframe by year
  frameStock <- cbind(dates, frameStock)
  frameStock <- subset(frameStock, year(frameStock[,1])==year)
  
  #Store a dataframe with the previous years stock prices
  pvYear <- subset(frameStock, year(frameStock[,1])==(year-1))
  topStocks <- list()
  
  #For each month create a frame with the last 3 months worth of data in order to compute the rsi
  for(i in 1:12){
    
    rs <- rep(NA, ncol(frameStock)-1)
    threeMthFrame <- data.frame()
    
    if(i == 1){
      first <- pvYear[month(pvYear[,1])==11,]
      second <- pvYear[month(pvYear[,1])==12,]
      third <-  frameStock[month(frameStock[,1])==i,]
    }
    else if(i==2){
      first <- pvYear[month(pvYear[,1])==12,]
      second <- frameStock[month(frameStock[,1])==(i-1),]
      third <-  frameStock[month(frameStock[,1])==i,]
    }
    else{
    first <- frameStock[month(frameStock[,1])==(i-2),]
    second <- frameStock[month(frameStock[,1])==(i-1),]
    third <-  frameStock[month(frameStock[,1])==i,]
    }
    
    threeMthFrame <- rbind(third, second, first)

    #For each stock in the frame compute the rsi
    for(j in 2:ncol(frameStock)){
      rs[j-1] <- rsi(threeMthFrame[,j])
    }
    
    #Sort the data and take the top N_stocks and returns them as a list of lists
    totalData <- as.data.frame(cbind(names(frameStock)[2:length(frameStock)], rs))
    names(totalData) <- c("st","rs")
    sorted <- totalData[order(-rs),]
    topStocks[[i]] <- sorted$st[1:N_stocks]   
  }
  return(topStocks)
}

######Stock Price######
#Enter month as number 1-12 returns a csv with last 3 months of return for the top ten stocks
#at the end of month i.
#Read data in like this.
#ticker <- read.csv("ticker.csv",header=TRUE)
#MAstock <- read.csv("MAstock.csv", header=TRUE)
#Function call: stockPrice(MAstock, ticker, 1990, 1)
#optionalData = TRUE/FALSE
#extraStocks accepts c() list of strings where stocks are "NWL.UN.Equity"
#Sample function call: stockPrice(singledate, ticker, 1990, 3, 10, TRUE, c("AAPL.UQ.Equity","OMX.UN.Equity"))

stockPrice <- function(frame,ticker,year,month,N_stocks, optionalData=FALSE, extraStocks=NULL){
  
 #Not necessary
  if(month == 1&year!=2015){
    month <- 12
    year = year - 1
  }
  else{
    month <- month - 1
  }

  #Calcaulate the top ten for the year
  topStocks <- topN_stocks(frame,ticker,year,N_stocks)
  dates <- as.POSIXlt(as.character(frame[,1]), format="%m/%d/%y")
  frameStock <- frame[,-1]
  frameStock <- cbind(dates, frameStock)
  
  framePV <- frameStock[year(frameStock[,1])==(year-1),]
  frameNY <- frameStock[year(frameStock[,1])==(year+1),]
  frameStock <- frameStock[year(frameStock[,1])==year,]
  
  #For the top ten stocks get the price series for the next month i.e Compute the 10 stocks for March 31st
  #these are prices for the stocks in April. This is done for evaluation purposes after the trading is complete
  if(month == 12){
    nextMonth <- frameNY[month(frameNY[,1])==1,]
  }
  else{
  nextMonth <- frameStock[month(frameStock[,1])==(month+1),]
  }
  
  #For years 1991 - 2014
  if(month==1){
    month1 <- frameStock[month(frameStock[,1])==month,]
    month2 <- framePV[month(framePV[,1])==12,]
    month3 <- framePV[month(framePV[,1])==11,]
  }
  else if(month==2){
    month1 <- frameStock[month(frameStock[,1])==month,]
    month2 <- frameStock[month(frameStock[,1])==(month-1),]
    month3 <- framePV[month(framePV[,1])==12,]
  }
  else{
    month1 <- frameStock[month(frameStock[,1])==month,]
    month2 <- frameStock[month(frameStock[,1])==(month-1),]
    month3 <- frameStock[month(frameStock[,1])==(month-2),]
  }
  
  frameStock <- rbind(month1[nrow(month1):1,],month2[nrow(month2):1,],month3[nrow(month3):1,])
  
  dates <- as.data.frame(as.character(frameStock[,1]))
  names(dates) <- "Date"
  dates1 <- as.data.frame(as.character(nextMonth[,1]))
  names(dates1) <- "Date"
  
  #If optional data for previous months portfolio is added go here
  if(optionalData==TRUE){
    checkTop <- as.character(topStocks[[month]])
    
    #When finding the top 2 stocks if one is already in the portfolio choose the 3rd stock and continue until 
    #you get 2 unique stocks.
      if(sum(extraStocks %in% checkTop)>=1){
        secondCheck = c('TRUE')
        
        while(sum(extraStocks %in% checkTop)>=1 & length(secondCheck) != N_stocks){
          
        topStocks <- topN_stocks(frame,ticker,year,N_stocks+sum(extraStocks %in% checkTop))  
        checkTop <- as.character(topStocks[[month]])  
        topStocks[[month]] <- topStocks[[month]][!(topStocks[[month]] %in% extraStocks)]
        secondCheck <- as.character(topStocks[[month]])    
      }
    }

    #Get the prices for those months and build a dataframe with the price data for all the stocks
    otherStocks <- frameStock[,which(names(frameStock) %in% extraStocks),drop=FALSE]
    next_extra <- nextMonth[,which(names(nextMonth) %in% extraStocks),drop=FALSE]
    
    frameStock <- frameStock[,which(names(frameStock) %in% topStocks[[month]])]
    nextMonth <- nextMonth[,which(names(nextMonth) %in% topStocks[[month]])]
    
    
    frameStock <- cbind(frameStock, otherStocks)
    nextMonth <- cbind(nextMonth, next_extra)
    
    frameStock <- apply(frameStock, 2, as.character)
    frameStock <- apply(frameStock, 2, as.numeric)
    
    nextMonth <- apply(nextMonth, 2, as.character)
    nextMonth <- apply(nextMonth, 2, as.numeric)
    
    frameStock <- cbind(dates, frameStock)
    nextMonth <- cbind(dates1, nextMonth)
    
    nextMonth <- nextMonth[nrow(nextMonth):1,]
    names(nextMonth)[1] <- c("Date")
    
  }
  
  #Do the same as above but just get the price data for 10 stocks. Only used in January 1991.
  else if(optionalData==FALSE){ 
    frameStock <- frameStock[,which(names(frameStock) %in% topStocks[[month]])]
    nextMonth <- nextMonth[,which(names(nextMonth) %in% topStocks[[month]])]
    
    frameStock <- apply(frameStock, 2, as.character)
    frameStock <- apply(frameStock, 2, as.numeric)
    
    nextMonth <- apply(nextMonth, 2, as.character)
    nextMonth <- apply(nextMonth, 2, as.numeric)

    frameStock <- cbind(dates, frameStock)
    nextMonth <- cbind(dates1, nextMonth)
      
    nextMonth <- nextMonth[nrow(nextMonth):1,]
    names(nextMonth)[1] <- c("Date")
  }
  
  #Was a check for NA values but they were dealt with manually.
  #for(j in 2:ncol(frameStock)){
  #  for(i in 2:nrow(frameStock)){
  #    
  #    if(is.na(frameStock[i,j])==TRUE&is.na(frameStock[i-1,j])==FALSE){
  #      frameStock[i,j] <- frameStock[i-1,j]
  #    }
  #  }
  #}
  
  
  #Any NAs in the previous 3 months price data is 0 for calculation purposes
  frameStock[is.na(frameStock)] = 0
  
  return(list(frameStock, nextMonth))
}

