
plotDiurnalMeans<-function(id,directory){
        
        #Read in data, rename columns and delete empty columns
        d<-read.csv( paste(directory,"/",id,".csv",sep="") )
        colnames(d)<-c("TimeStamp","TP","PM10","PM2.5","PM1","T","H","WS","WD","GPS")
        d<-subset(d,select=-c(T,H,GPS))
        
        #retain only values where all PM data are valid
        dg<-subset(d,PM10 > 0 & PM2.5 > 0 & PM1 > 0)
        
        #Convert TimeStamp to POSIXlt format
        dg$TimeStamp<-as.POSIXlt(strptime(dg$TimeStamp, format= "%d/%m/%Y %T"))
        
        dates<-with(dg,paste(TimeStamp$year+1900,"-",TimeStamp$mon,"-",TimeStamp$mday,sep=""))
        dates<-as.Date(dates)
      
        months<-with(dg,TimeStamp$mon)
          
        dg<-cbind(dg,dates,months)
        
        nr<-nrow(dg)
        
        #set up empty data frame for the mean values
        PM.means <- data.frame(Time= numeric(0), PMall=numeric(0), PM10=numeric(0),
                               PM2_5=numeric(0),PM1=numeric(0))
        
        
        #create vector of times of day at which measurements are taken
        times<-with(dg,paste(TimeStamp$hour,":",TimeStamp$min,sep=""))
        dg<-cbind(dg,times)
        daytimes<-unique(times)
        nt=length(daytimes)
        
        #find means for each variable at each time of day
        for (i in 1:nt){
                PM.means[i,1]=i/4 #the time of day, in hours 0-24
                for (j in 2:5){
                        PM.means[i,j]<-mean(subset(dg[,j],dg$times==daytimes[i]) )       
                }
                
        }
        
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
        
        
        #ggplot(melt(PM.means,id.vars="Time"),aes(x=Time,y=value))+
          #scale_x_continuous(limits=c(0,24),breaks=c(0,4,8,12,16,20,24))+geom_line()+
          #scale_y_continuous(limits = c(0, NA))+
          #facet_wrap(~variable,scales="free_y")+
          #ylab( bquote(mu~g~m^{-3}))+
          #ggtitle("CAQU data")
        
        #str(s4)
        
}