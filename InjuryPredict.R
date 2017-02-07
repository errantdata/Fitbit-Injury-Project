fitbit=read.csv(file="../Data/Fitbit/fitbitScraper_intraday_20140121-20160921.csv", stringsAsFactors = F)

times=seq(from=as.POSIXct("00:00:00",format="%H:%M:%S"),to=as.POSIXct("23:45:00",format="%H:%M:%S"),by=15*60)
times.length=length(times)

#Pulling time stamps from the Fitbit site sometimes gets screwed up, which leaves N/As in the $time column.
#So, I had to work around that in a really stupid way - please forgive me, good coding gods.
dates=seq.Date(from=as.Date(fitbit$time[1],format="%Y-%m-%d"),to=as.Date(fitbit$time[length(fitbit$time)],format="%Y-%m-%d"),by=1)
dates.length=length(dates)

fitbit$time=rep(times,dates.length)
fitbit$Day=as.Date(unlist(lapply(dates,function(x) rep(x,times.length))),format="%Y-%m-%d",origin="1970-01-01")

