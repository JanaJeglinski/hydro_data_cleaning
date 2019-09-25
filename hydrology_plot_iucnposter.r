
ed <- read.csv("file:///C:/Data/Hydrology/cleaned_site_logger/Edinglassie_EDINGLASS1_post2016.csv")
co<- read.csv("file:///C:/Data/Hydrology/cleaned_site_logger/Corrimony_CORRIMONY2_post2016.csv")
bl<- read.csv("file:///C:/Data/Hydrology/cleaned_site_logger/Ben Lawers_BENLAWERS2_pre2016.csv")

bl$date <- as.POSIXct(bl$date)
ed$date <- as.POSIXct(ed$date)
co$date <- as.POSIXct(co$date)

ed$level_corr <- -ed$level_corr
co$level_corr <- -co$level_corr
bl$level_corr <- -bl$level_corr

head(Bl)
png("C:/Data/Hydrology/IUCNplot_siteslong.png",res=500, width=30, height=15, units="cm",pointsize=12)       
par(mfrow=c(1,1),mar=c(4,4,2,1))
plot(bl$level_corr~bl$date,type="l",lwd=2,col="blue",ylim=c(-0.7,0.1),xlim=as.POSIXct(c("2015-07-01 GMT","2018-04-01 GMT")),xlab=c("Date"),ylab=c("Water level below surface (m)"))
lines(ed$level_corr~ed$date,col="turquoise1",lwd=2)
lines(co$level_corr~co$date,col="orange4",lwd=2)
abline(0,0,lty="dashed",col="red",lwd=2)
legend("bottomright",legend=c("Ben Lawers","Edinglassie","Corrimony", "Ground level"),lty=c("solid","solid","solid","dashed"), lwd=2,col=c("blue","turquoise1","orange4","red"),bty="n")
mtext("Hydrology data from three sites",side=3)
dev.off()


