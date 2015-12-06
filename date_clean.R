

businessDay <- as.POSIXlt(business_days[,2])
singledate$Date <- as.POSIXlt(singledate$Date, format="%m/%d/%y")
singledate <- subset(singledate, (singledate$Date %in% businessDay))
goodFriday <- holiday(1990:2014, "GoodFriday")
goodFriday <- as.POSIXlt(goodFriday)
newYear <- holiday(1990:2014, "USNewYearsDay")
newYear <- as.POSIXlt(newYear)
mlk <- holiday(1990:2014, "USMLKingsBirthday")
mlk <- as.POSIXlt(mlk)
wash <- holiday(1990:2014, "USWashingtonsBirthday")
wash <- as.POSIXlt(wash)
mem <- holiday(1990:2014, "USMemorialDay")
mem <- as.POSIXlt(mem)


singledate <- subset(singledate, !(singledate$Date %in% goodFriday))
singledate <- subset(singledate, !(singledate$Date %in% newYear))
singledate <- subset(singledate, !(singledate$Date %in% mlk))
singledate <- subset(singledate, !(singledate$Date %in% wash))
singledate <- subset(singledate, !(singledate$Date %in% mem))

write.csv(singledate, "singledate.csv", row.names=FALSE)
