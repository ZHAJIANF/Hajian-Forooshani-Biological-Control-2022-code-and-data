
######################################
########################### FIGURE 1 #
######################################

rm(list=ls())
library(ggplot2)
library(ggmap)
pr.roya.dat <- read.csv("2022_Hajian-Forooshani_Biological_control_fig1_data.csv")

## this seems to make our window in the map? bb stands for bounding box 
PR.bbox <- make_bbox(lon= pr.roya.dat$lon,lat= pr.roya.dat$lat,f=0.1)
PR.bbox

new_lon <- c((min(pr.roya.dat$lon) - 0.2), (max(pr.roya.dat$lon)+0.2))
new_lat <- c((min(pr.roya.dat$lat) - 0.2), (max(pr.roya.dat$lat)+0.2))
PR.bbox <- make_bbox(lon= new_lon,lat= new_lat,f=0.1)
PR.bbox


pr.map <- get_map(location = PR.bbox,maptype="satellite",source="google")
ggmap(pr.map) 

ggmap(pr.map) + geom_point(data= pr.roya.dat,mapping = aes(x=lon,y=lat),col="black",size=1,fill="black",shape=24) + geom_scatterpie (aes(x= lon, y=lat,group = region,r=log(roya)/80),data= pr.roya.dat, cols= colnames(pr.roya.dat)[3:6],color=NA,alpha=0.6) +  coord_equal() + scale_fill_manual(
breaks = c("myco","lecanii","mite","snail"),
labels = c("Mycodiplosis","L.lecanii","CLR-mite","CLR-snail"),
values = c("myco" = "red", "lecanii" = "gray", "mite" = "goldenrod", "snail" = "green")
) + theme(legend.position = c(0.96,0.03),legend.justification = c(1,0),legend.title=element_blank(),panel.grid = element_blank (),
panel.border = element_blank(), axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank() ) + geom_point(data= pr.roya.dat,mapping = aes(x=lon,y=lat),col="black",size=1.5,fill="black",shape=24)



######################################
########################### FIGURE 2 #
######################################

rm(list=ls())
data <- read.csv("2022_Hajian-Forooshani_Biological_control_fig2_data.csv")
all.site.vec <- as.character(unique(data$farm))
head(data)

par(mfrow=c(5,6),mai=c(0.04,0.04,0.04,0.04),oma=c(4,4,0,0),xpd=T)
for(xx in 1:25){
#xx <- 1
site.looper <- xx
a.site.dat <- data[data$farm == all.site.vec[site.looper], ]

site.visits <- a.site.dat$visit
roya.pres <- a.site.dat $prop.roya
myco.pres <- a.site.dat $prop.myco
lecanii.pres <- a.site.dat $prop.lecanii
snail.pres <- a.site.dat $prop.snail
mite.pres <- a.site.dat $prop.mite

plot(site.visits ,roya.pres,ylim=c(0,1),pch=19,col="dark orange",yaxt="n",xlab="",ylab="",xlim=c(1,12),xaxt="n")
points(site.visits ,roya.pres,pch=19,col="dark orange",type="l")
points(site.visits ,myco.pres,pch=19,col="dark red")
points(site.visits ,myco.pres,pch=19,col="dark red",type="l")
points(site.visits ,lecanii.pres,pch=19,col="grey")
points(site.visits ,lecanii.pres,pch=19,col="grey",type="l")
points(site.visits ,snail.pres,pch=19,col="dark green")
points(site.visits ,snail.pres,pch=19,col="dark green",type="l")
points(site.visits ,mite.pres,pch=19,col="goldenrod")
points(site.visits ,mite.pres,pch=19,col="goldenrod",type="l")


## add axes where they need to be
if(sum(xx == c(1,7,13,19,25)) > 0){
	axis(2,las=2,cex.axis=0.8)
}

x.axis.labels <- c("Aug","Sept","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","June","July")
if(sum(xx == c(25,24,23,22,21,20)) > 0){
	#axis(1,las=1,cex.axis=0.8)
	axis(1,labels=x.axis.labels,at=seq(1,12,1),cex.axis=0.7,las=2)
}


} ## this is the end of looping through the different farms

 
mtext("Propotion of plants",side=2,line=2.2,outer=T)
mtext("Monthly survey (Aug 2018 - July 2019)",side=1,line=1,outer=T)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")


legend("bottomright",inset=c(0.05,0.15),legend=c("CLR","Mycodiplosis","L.lecanii","CLR-mite","CLR-snail"),pch=19,col=c("dark orange","dark red","grey","goldenrod","dark green"), xpd = TRUE, horiz = T,cex=1.1 )





######################################
########################### FIGURE 3 #
######################################
rm(list=ls())
data <- read.csv("2022_Hajian-Forooshani_Biological_control_fig3_data.csv")
head(data)

par(mfrow=c(3,3),mai=c(0.3,0.5,0.1,0.1),oma=c(1,0,0,4))

for(loops in 1:9){
#loops <- 1
if(loops == 1){
	
	a.plot.dat <- data[data$site == "Tumba",]
	MX_dat <- T
}else if(loops == 2){
	a.plot.dat <- data[data$site == "amapola",]
	MX_dat <- T
}else if(loops == 3){
	a.plot.dat <- data[data$site == "primavera",]
	MX_dat <- T
}else if(loops == 4){
	a.plot.dat <- data[data$site == "che",]
	MX_dat <- T
}else if(loops == 5){
	a.plot.dat <- data[data$site == "sandino",]
	MX_dat <- T
}else if(loops == 6){
	
	a.plot.dat <- data[data$site == "leon",]
	MX_dat <- T
}else if(loops == 7){
	
	a.plot.dat <- data[data$site == "LOTTY",] 
	MX_dat <- F
}else if(loops == 8){
	a.plot.dat <- data[data$site == "TODY",]
	MX_dat <- F
}else if(loops == 9){
	
	a.plot.dat <- data[data$site == "JOHN",]
	MX_dat <- F
}


head(a.plot.dat)

site.visits <- a.plot.dat$site.visits
roya.pres <- a.plot.dat$prop.roya
myco.pres <- a.plot.dat$prop.myco
lecanii.pres <- a.plot.dat$prop.lecanii
mite.pres <- a.plot.dat$prop.mite
snail.pres <- a.plot.dat$prop.snail


plot(site.visits ,roya.pres,ylim=c(0,1),pch=19,col="dark orange",yaxt="n",xlab="",ylab="",xaxt="n")

if(loops < 4){
	axis(1,labels=c("June_7_2019","March_17_2020"),at=c(min(site.visits)+2,max(site.visits)-2))
	
}else if(loops == 4){
	
		axis(1,labels=c("June_5_2019","March_16_2020"),at=c(min(site.visits)+2,max(site.visits)-2))

}else if(loops == 5){
	
		axis(1,labels=c("June_4_2019","March_16_2020"),at=c(min(site.visits)+2,max(site.visits)-2))

}else if(loops == 6){
	
		axis(1,labels=c("June_3_2019","March_14_2020"),at=c(min(site.visits)+2,max(site.visits)-2))

}else{
		axis(1,labels=c("June_12_2019","March_3_2020"),at=c(min(site.visits)+2,max(site.visits)-2))
	
}
points(site.visits ,roya.pres,pch=19,col="dark orange",type="l")
points(site.visits ,myco.pres,pch=19,col="dark red")
points(site.visits ,myco.pres,pch=19,col="dark red",type="l")
points(site.visits ,lecanii.pres,pch=19,col="grey")
points(site.visits ,lecanii.pres,pch=19,col="grey",type="l")

if(loops > 6){
	points(site.visits ,snail.pres,pch=19,col="dark green")
	points(site.visits ,snail.pres,pch=19,col="dark green",type="l")
	points(site.visits ,mite.pres,pch=19,col="goldenrod")
	points(site.visits ,mite.pres,pch=19,col="goldenrod",type="l")
}

#mtext("Survey #",side=1,line=2.2,cex=0.75)
mtext("Prop plants",side=2,line=2.5,cex=0.75)
axis(2,las=2)
#mtext(unique(a.plot.dat $plot),cex=0.8)
if(loops == 3){
	mtext("Mexican",line=1,outer=F,side=4,las=0)
	mtext("Sun coffee landscape",line=2.5,outer=F,side=4,las=0)
}

if(loops == 6){
	mtext("Mexican",line=1,outer=F,side=4,las=0)
	mtext("Shade coffee landscape",line=2.5,outer=F,side=4,las=0)
}


if(loops == 9){
	mtext("Puerto Rican",line=1,outer=F,side=4,las=0)
	mtext("Coffee & citrus landscape",line=2.5,outer=F,side=4,las=0)
}


}






######################################
########################### FIGURE 4 #
######################################

data <- read.csv("2022_Hajian-Forooshani_Biological_control_fig4_data.csv")

col.fun <- colorRampPalette(c("grey","red"))
col.vec <- col.fun(12)

par(mfrow=c(2,2),mai=c(0.6,0.6,0.1,0.1),oma=c(0,0,0,0))
plot(data$rust.mean ,data$lecan.mean,cex=0.001,xlab="",ylab="",xlim=c(5,45),ylim=c(0,2),yaxt="n")
axis(2,las=2)
points(data$rust.mean ,data$lecan.mean,type="l",lwd= 1.5)
points(data$rust.mean ,data$lecan.mean,type="p",pch=19,col=col.vec)
arrows(data$rust.mean ,data$lecan.mean + data$lecan.error ,data$rust.mean ,data$lecan.mean- data$lecan.error,code=3,length=0,angle=90,col=col.vec,lwd=2)
arrows(data$rust.mean + data$rust.error ,data$lecan.mean ,data$rust.mean - data$rust.error,data$lecan.mean,code=3,length=0,angle=90,col=col.vec,lwd=2)
mtext(expression(italic(L.lecanii)),side=2,line=2.2)
mtext(expression(italic(CLR)),side=1,line=2.2)
text(min(data$rust.mean)-3,max(data$lecan.mean)+0.2,"a.)")

plot(data$rust.mean ,data$myco.mean,cex=0.001,xlab="",ylab="",xlim=c(5,45),ylim=c(0,1),yaxt="n")
axis(2,las=2)
points(data$rust.mean ,data$myco.mean,type="l",lwd= 1.5)
points(data$rust.mean ,data$myco.mean,type="p",pch=19,col=col.vec)
arrows(data$rust.mean ,data$myco.mean + data$myco.error ,data$rust.mean ,data$myco.mean- data$myco.error,code=3,length=0,angle=90,col=col.vec,lwd=2)
arrows(data$rust.mean + data$rust.error ,data$myco.mean ,data$rust.mean - data$rust.error,data$myco.mean,code=3,length=0,angle=90,col=col.vec,lwd=2)
mtext(expression(italic(Mycodiplosis)),side=2,line=2.2)
mtext(expression(italic(CLR)),side=1,line=2.2)
text(min(data$rust.mean)-3,max(data$myco.mean)+0.16,"b.)")


plot(data$rust.mean ,data$mite.mean,cex=0.001,xlab="",ylab="",xlim=c(5,45),ylim=c(0,0.4),yaxt="n")
axis(2,las=2)
points(data$rust.mean ,data$mite.mean,type="l",lwd= 1.5)
points(data$rust.mean ,data$mite.mean,type="p",pch=19,col=col.vec)
arrows(data$rust.mean ,data$mite.mean + data$mite.error ,data$rust.mean ,data$mite.mean- data$mite.error,code=3,length=0,angle=90,col=col.vec,lwd=2)
arrows(data$rust.mean + data$rust.error ,data$mite.mean ,data$rust.mean - data$rust.error,data$mite.mean,code=3,length=0,angle=90,col=col.vec,lwd=2)
mtext(expression(italic(CLR-mite)),side=2,line=2.2)
mtext(expression(italic(CLR)),side=1,line=2.2)
text(min(data$rust.mean)-3,max(data$mite.mean)+0.11,"c.)")


plot(data$rust.mean ,data$snail.mean,cex=0.001,xlab="",ylab="",xlim=c(5,45),ylim=c(0,0.1),yaxt="n")
axis(2,las=2)
points(data$rust.mean ,data$snail.mean,type="l",lwd=1.5)
points(data$rust.mean ,data$snail.mean,type="p",pch=19,col=col.vec)
arrows(data$rust.mean ,data$snail.mean + data$snail.error ,data$rust.mean ,data$snail.mean- data$snail.error,code=3,length=0,angle=90,col=col.vec,lwd=2)
arrows(data$rust.mean + data$rust.error ,data$snail.mean ,data$rust.mean - data$rust.error,data$snail.mean,code=3,length=0,angle=90,col=col.vec,lwd=2)
mtext(expression(italic(CLR-snail)),side=2,line=2.6)
mtext(expression(italic(CLR)),side=1,line=2.2)
text(min(data$rust.mean)-3,max(data$snail.mean)+0.039,"d.)")



