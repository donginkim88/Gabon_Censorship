'''
The purpose of this script is to plot Gabon internet traffic data
'''

library(rjson)
json_data <- fromJSON(file="/Users/Zack/Dropbox/Censorship/Data/gabon-outage.json")
data <- json_data$data$series$`Gabon Networks UP`
seq.date <- seq(data$from, data$until, data$step)

seq.date2 <- as.POSIXct(seq.date, origin='1970-01-01', tz='GMT')



data <- data.frame(traffic=data$values, datehour=seq.date2[-1])


xbreak <- seq(1, nrow(data), 1)

breaks <- NULL
for(i in 1:length(xbreak)){
	if(i %% 48 == 0){
		breaks[i] <- i
	}
	else{
		breaks[i] <- ""
	}
}
breaks[1] <- 1
breaks <- as.numeric(breaks)

labels <- as.Date(levels(factor(data$datehour))[as.numeric(breaks)])


pdf('/Users/Zack/Documents/UCLA/Research/Censorship/Figures/Gabon_Traffic.pdf')
par(mai=c(1.5,1,.5,.5))
plot(1:nrow(data), data$traffic, type='l', xlab='', ylab='Traffic', xaxt="n", main='Gabon Internet Traffic')
axis(1, at=breaks, labels=FALSE)
text(x=breaks, y = 0, labels = labels, srt=90, xpd=TRUE)
dev.off()



#### Narrow the range
data2 <- data[data$datehour > '2016-08-25' & data$datehour < '2016-10-02',]

xbreak <- seq(1, nrow(data2), 1)
breaks <- NULL
for(i in 1:nrow(data2)){
	if(i %% 48 == 0){
		breaks[i] <- i
	}
	else{
		breaks[i] <- ""
	}
}
breaks[1] <- 1
breaks <- as.numeric(breaks)

labels <- as.Date(levels(factor(data2$datehour))[as.numeric(breaks)])




pdf('/Users/Zack/Documents/UCLA/Research/Censorship/Figures/Gabon_TrafficShort.pdf')
par(mai=c(1.5,1,.5,.5))
plot(1:nrow(data2), data2$traffic, type='l', xlab='', ylab='Traffic', xaxt="n", main='Gabon Internet Traffic')
axis(1, at=breaks[1:nrow(data2)], labels=FALSE)
#text(x=1:nrow(data2), y = 0, labels = labels2, srt=90, xpd=TRUE)
text(x=breaks[1:nrow(data2)], y = 0, labels = labels[breaks[1:nrow(data2)]], srt=90, xpd=TRUE)
dev.off()


