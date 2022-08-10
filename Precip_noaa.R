library(dplyr)
library(ggplot2)
library(lubridate)

#### read in data ----

daily <- read.csv("e:/Google Drive/research/projects/cny_climate/daily_SYR/USW00014771_prcp.csv")
daily$precip.mm <- daily$prcp/10
daily$precip.in <- daily$precip.mm*0.0393701
daily$dateF <- ymd(daily$date)
daily$doy <- yday(daily$dateF)
daily$year <- year(daily$dateF)
daily$month <- month(daily$dateF)

#### organize hourly precip----

dirP <- c("e:/Google Drive/research/projects/cny_climate/LCD_SYR")

filesP <- list.files(dirP)


listP <- list()

for(i in 1:length(filesP)){
  
  listP[[i]] <- read.csv(paste0(dirP,"/",filesP[i]))
}


precDF <- do.call( "rbind", listP)
tail(precDF$REPORT_TYPE)

precDF$dateF <- ymd_hms(precDF$DATE)
precDF$doy <- yday(precDF$dateF)
precDF$year <- year(precDF$dateF)
precDF$hour <- hour(precDF$dateF) + (minute(precDF$dateF)/60)
precDF$DDay <- precDF$doy + (precDF$hour/24)

unique(precDF$REPORT_TYPE[precDF$year <= 1995])
unique(precDF$REPORT_TYPE[precDF$year == 1996])
unique(precDF$REPORT_TYPE[precDF$year > 1996])

unique(precDF$REPORT_TYPE[precDF$year == 1974])
precDF$REPORT_FIX <- gsub(" ", "", precDF$REPORT_TYPE)
unique(precDF$REPORT_FIX[precDF$year == 1974])

hourlyDF <- precDF %>%
  filter(REPORT_TYPE == "FM-15" | REPORT_FIX == "SAO")

hourlyDF <- hourlyDF %>%
  filter(SOURCE == "C" | SOURCE == "7")

hourlyDF[hourlyDF$year == 1974,]
# convert precip to num and remove trace

hourlyDF$pCalc1 <- ifelse(hourlyDF$HourlyPrecipitation == "T",
                          0,
                          hourlyDF$HourlyPrecipitation)
hourlyDF$susFlag <- ifelse(grepl("s",hourlyDF$pCalc1) == TRUE, 1,0)

susCheck <- hourlyDF[hourlyDF$susFlag == 1 , ]

hourlyDF$pCalc2 <- ifelse(hourlyDF$susFlag == 1, NA, hourlyDF$pCalc1) 
hourlyDF$precip <- as.numeric(hourlyDF$pCalc2 )

# check how many observations are present in each year
hourNN <- hourlyDF %>%
  select(dateF, doy, year, hour, DDay, precip)
hourNN$precip.mm <- hourNN$precip * 25.4
  
hourNN <- na.omit(hourNN) 

hourlyDay <- hourNN %>%
  group_by( year, doy) %>%
  summarise(dailyP = sum(precip.mm), n.p = length(precip))
  


hourlyDay[hourlyDay$n.p > 24, ]
test <- hourlyDF[hourlyDF$doy ==1 & hourlyDF$year == 2001, ]


hist(hourNN$precip.mm)
quantile(hourNN$precip.mm[hourNN$precip.mm > 0], prob=seq(0,1,by=0.05))

topHour <- hourNN %>%
  arrange(desc(precip.mm))

#### total annual precipitation ----

annualP <- hourlyDF %>%
  group_by(year) %>%
  summarise(total.in = sum(na.omit(precip)),
            n.obs = length(na.omit(precip)))
annualP$ndays <- ifelse(leap_year(annualP$year),366,365)
annualP$nFull <- 24*annualP$ndays

annualP$percPresent <- (annualP$n.obs/annualP$nFull)*100

annualTot <- annualP %>%
  filter(percPresent > 80)

ggplot(annualTot, aes(x=year,y=total.in))+
  geom_col()

annualDP <- daily %>%
  group_by(year) %>%
  summarise(total.in = sum(na.omit(precip.in)),
            n.obs = length(na.omit(precip.in)))



annualDTot <- annualDP %>%
  filter(n.obs > 360)

baselineA <- annualDTot %>%
  filter(year < 1990) %>%
  summarise(base= mean(total.in, na.rm=TRUE) )

aveA <- annualDTot %>%
  summarise(ave= mean(total.in, na.rm=TRUE) )

annualDTot <- annualDTot %>%
  mutate(anom = (total.in - baselineA$base))

annualDTot <- annualDTot %>%
  mutate(ave = (total.in - aveA$ave))

annualDTot <- annualDTot %>%
  mutate(Period = ifelse(year >= 1960 & year<= 1989,"1960-1989",
                         ifelse( year >=1990, "1990-2019",NA )))

periodStats <- annualDTot[is.na(annualDTot$Period) == FALSE,] %>% 
  group_by(Period) %>%
  summarise(mean=mean(total.in),
            sd=sd(total.in),
            Percentile50 = quantile(total.in, probs=0.5),
            Percentile25 = quantile(total.in, probs=0.25),
            Percentile75 = quantile(total.in, probs=0.55),
            min=min(total.in),
            max=max(total.in))

print(periodStats, digits=1)
