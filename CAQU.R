#Read in data, renamer columns and delete empty columns
d<-read.csv("TNO2245Data_1429877310388.csv")
colnames(d)<-c("TimeStamp","TP","PM10","PM2.5","PM1","T","H","WS","WD","GPS")
d<-subset(d,select=-c(T,H,GPS))

#retain only values where all PM data are valid
dg<-subset(d,PM10 > 0 & PM2.5 > 0 & PM1 > 0 & WS>0)

#Convert TimeStamp to POSIXlt format
dg$TimeStamp<-as.POSIXlt(strptime(dg$TimeStamp, format= "%d/%m/%Y %T"))

nr<-nrow(dg)

#set up empty data frame for the mean values
PM.means <- data.frame(Time= numeric(0), PMall=numeric(0), PM10=numeric(0),
                       PM2_5=numeric(0),PM1=numeric(0),WS=numeric(0))


#create vector of times of day at which measurements are taken
times<-with(dg,paste(TimeStamp$hour,":",TimeStamp$min,sep=""))
dg<-cbind(dg,times)
daytimes<-unique(times)
nt=length(daytimes)

#find means for each variable at each time of day
for (i in 1:nt){
        PM.means[i,1]=i#as.character(daytimes[i])
        for (j in 2:6){
                PM.means[i,j]<-mean(subset(dg[,j],dg$times==daytimes[i]) )       
        }
        
}

ggplot(melt(PM.means,id.vars="Time"),aes(x=Time,y=value))+geom_point()+
        facet_wrap(~variable,scales="free_y")