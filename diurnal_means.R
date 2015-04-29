
plotDiurnalMeans<-function(id,directory){
        
        #Read in data, rename columns and delete empty columns
        d<-read.csv( paste(directory,"/",id,".csv",sep="") )
        colnames(d)<-c("TimeStamp","TP","PM10","PM2.5","PM1","T","H","WS","WD","GPS")
        d<-subset(d,select=-c(T,H,GPS))
        
        #retain only values where all PM data are valid
        dg<-subset(d,PM10 > 0 & PM2.5 > 0 & PM1 > 0)
        
        #create vectors of dates, months and times of day at which measurements are taken
        #Convert TimeStamp to POSIXlt format
        dg$TimeStamp<-as.POSIXlt(strptime(dg$TimeStamp, format= "%d/%m/%Y %T"))
        
        dates<-with(dg,paste(TimeStamp$year+1900,"-",TimeStamp$mon,"-",TimeStamp$mday,sep=""))
        dates<-as.Date(dates)
      
        months<-with(dg,TimeStamp$mon)
        
        times<-with(dg,paste(TimeStamp$hour,":",TimeStamp$min,sep=""))
        
        #add these vectors to the data frame
        dg<-cbind(dg,dates,months,times)
               
        #find the means at each time of day
        s.times<-split(dg,dg$times)
        s2<-sapply(s.times, function(x) {colMeans(x[, c("TP", "PM10", "PM2.5","PM1")])})
        s3<-t(s2)
        s4<-data.frame(s3)
        s5<-data.frame(c(seq(0,23.75,.25)),s4)
        colnames(s5)[1]<-"Time"

        ggplot(melt(s5,id.vars="Time"),aes(x=Time,y=value))+
        scale_x_continuous(limits=c(0,24),breaks=c(0,4,8,12,16,20,24))+geom_line()+
        scale_y_continuous(limits = c(0, NA))+
        facet_wrap(~variable,scales="free_y")+
        ylab( bquote(mu~g~m^{-3}))+
        ggtitle("CAQU data")

        #head(dg)
}