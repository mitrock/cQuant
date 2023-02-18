# Task 1

setwd("~/Jobs/cQuant")
library(readr)
ERCOT_DA_Prices_2016 <- read_csv("historicalPriceData/ERCOT_DA_Prices_2016.csv")
ERCOT_DA_Prices_2017 <- read_csv("historicalPriceData/ERCOT_DA_Prices_2017.csv")
ERCOT_DA_Prices_2018 <- read_csv("historicalPriceData/ERCOT_DA_Prices_2018.csv")
ERCOT_DA_Prices_2019 <- read_csv("historicalPriceData/ERCOT_DA_Prices_2019.csv")
data<-rbind(ERCOT_DA_Prices_2016,ERCOT_DA_Prices_2017,ERCOT_DA_Prices_2018,ERCOT_DA_Prices_2019)

# Task 2

library(tidyverse)
library(data.table)
library(lubridate)
data$Year<-lubridate::year(data$Date)
data$Month<-lubridate::month(data$Date)
meansum<-data %>%
  group_by(Year, Month, SettlementPoint) %>%
  summarise_at(vars(Price), list(Price = mean))

# Task 3

write.csv(meansum, "AveragePriceByMonth.csv", row.names=FALSE)

# Task 4

data2<-data
data2[data2 <= 0] <- NA
data2 <- na.omit(data2)
#summary(data2$Price)
data2$logprice<-log(data2$Price)
volatility<-data2 %>%
  group_by(Year, SettlementPoint) %>%
  summarise_at(vars(logprice), list(HourlyVolatility = sd))
volatility<-volatility[!grepl('LZ_', volatility$SettlementPoint),]

# Task 5

write.csv(volatility, "HourlyVolatilityByYear.csv", row.names=FALSE)

# Task 6

yearlist<-unique(volatility$Year)
yearmax<-c()
spmax<-c()
for (i in yearlist){
  yearmax<-append(yearmax,max((filter(volatility, Year == i)$HourlyVolatility)))
  sp<-which(volatility$HourlyVolatility==max((filter(volatility, Year == i)$HourlyVolatility)))
  spmax<-append(spmax,volatility$SettlementPoint[sp])
}
Maxes<-data.frame(Year = yearlist,
                  SettlementPoint = spmax,
                  HourlyVolatility = yearmax)
write.csv(Maxes, "MaxVolatilityByYear.csv", row.names=FALSE)

# Task 7

library(padr)
colnames<-list('Variable','Date','X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20','X21','X22','X23','X24')
data$Day<-lubridate::day(data$Date)
data$Hour<-lubridate::hour(data$Date)
data$cleandate<-format(as.POSIXct(data$Date,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
sp_list<-unique(data$SettlementPoint)
day_list<-unique(data$cleandate)
for (i in sp_list){
  df_i=data.frame(matrix(ncol = length(colnames), nrow = 0))
  names(df_i)<-colnames
  currentsp<-filter(data,data$SettlementPoint==i)
  for (j in day_list){
    currentday<-filter(currentsp,cleandate==j)
    if (nrow(currentday)<24) {
      currentday<-pad(currentday)
    }
    rowdata<-t(currentday$Price)
    newrow<-c(i,j,rowdata)
    df_i[nrow(df_i) + 1,] = newrow
  }
  write.csv(df_i, paste0("spot_",i,".csv"), row.names=FALSE)
}

#Bonus 1
meansum2<-meansum %>% mutate(date = make_date(Year, Month))
meansumHB<-meansum2[!grepl('LZ_', meansum2$SettlementPoint),]
meansumLZ<-meansum2[!grepl('HB_', meansum2$SettlementPoint),]
ggplot(data = meansumHB, aes(x=meansumHB$date, y=meansumHB$Price)) + geom_line(aes(color=meansumHB$SettlementPoint))
ggsave("SettlementHubAveragePriceByMonth.png",width = 12,height = 7)

ggplot(data = meansumLZ, aes(x=meansumLZ$date, y=meansumLZ$Price)) + geom_line(aes(color=meansumLZ$SettlementPoint))
ggsave("LoadZoneAveragePriceByMonth.png",width = 12,height = 7)