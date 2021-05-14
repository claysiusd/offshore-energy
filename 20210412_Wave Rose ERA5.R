#Library needed to open RNetcdffiles
library ("RNetCDF")
inputpath<-"F:/REM/Semester 2 (UPV)/OWEAOW - Ocean Wave Energy and Offshore Wind Energy Assessment/Block 2 - Wave Energy/Ibarra/Sailor data/"
inputfile<-paste(inputpath,"2013_BB.nc",sep="")

#1. Open the file
ERA_ini<-open.nc(inputfile)
#Have access to a summary
print.nc(ERA_ini)

#2. Read the content of the variables of interest
#1st variable:longitude
#2.1. 'var.get.nc' command
lonn<-var.get.nc(ERA_ini,"longitude")
lonn
#[1] 357.0 357.5
#Express longitude in degrees eastwards
lonn<-lonn-360
# [1] -3.0 -2.5
#2nd variable:Latitude
latt<-var.get.nc(ERA_ini,"latitude")
latt
# [1] 43.5
#Bilbao-Bizkaia Buoy [-3.05,43.64]

#We see that we have two gridpoints
#GP1=[-3,43.5] (nearest gridpointfrom the BB buoy)
#GP2=[-2.5,43.5] (also rather near from BB buoy)
#We want to calculate the wave roses at both locations and use the Sailor indicators and 
#diagram to determine which wave rose resembles most the buoy's wave rose.

#01_Nearest grid point ***************************************
#The nearest GP is -3 (pos#1 in lonn) 43.5 (pos#1 in latt)
#Read mwd, swhand mwpand retain only these positions

#3rd variable: mean wave direction
mwd0<-var.get.nc(ERA_ini, "mwd")
mwd1<-mwd0[1,]

#4th variable: mean wave period
mwp0<-var.get.nc(ERA_ini, "mwp")
mwp1<-mwp0[1,]

#5th variable: significant wave heigth
swh0<-var.get.nc(ERA_ini, "swh")
dim(swh0)
swh1<-swh0[1,]

#Dates
ttime<-var.get.nc(ERA_ini, "time")
ddates<-utcal.nc("hours since 1900-01-01 00:00:00.0",ttime,type="n")
ddates1<-ddates[,1:4]

#Calculate wef
wef1<-0.49*mwp1*(swh1^2)

#Combine merge
eragp1<-cbind(ddates1,swh1,mwp1,mwd1,wef1)
eragp1<-as.data.frame(eragp1)
names(eragp1)

wefu1<-matrix(NA,length(wef1),1)
wefv1<-matrix(NA,length(wef1),1)

for(iii in (1:length(wef1))){
  
  #If wef1 and direction <> NA
  if ((!is.na(wef1[iii]))&(!is.na(eragp1[iii,7]))){
    
    #Calculate zonal and meridional components of wef1
    #Depending on the direction, wefu1 and wefv1 are projected		
    
    #IF direction is below 90 degrees
    if (eragp1[iii,7]<=90){
      wefu1[iii,1]<--wef1[iii]*sin(eragp1[iii,7]*2*pi/360)
      wefv1[iii,1]<--wef1[iii]*cos(eragp1[iii,7]*2*pi/360)
    }
    #IF direction is below 90 degrees	
    
    #IF direction is between 90 degrees and 180 degrees
    if(eragp1[iii,7]>90 & eragp1[iii,7]<=180){
      wefu1[iii,1]<--wef1[iii]*sin((180-eragp1[iii,7])*2*pi/360)
      wefv1[iii,1]<-wef1[iii]*cos((180-eragp1[iii,7])*2*pi/360)
    }
    #IF direction is between 90 degrees and 180 degrees
    
    #IF direction is between 180 degrees and 270 degrees
    if(eragp1[iii,7]>180 & eragp1[iii,7]<=270){
      wefu1[iii,1]<-wef1[iii]*cos((270-eragp1[iii,7])*2*pi/360)
      wefv1[iii,1]<-wef1[iii]*sin((270-eragp1[iii,7])*2*pi/360)
    }
    #IF direction is between 180 degrees and 270 degrees
    
    #IF direction is between 270 degrees and 360 degrees
    if(eragp1[iii,7]>270 & eragp1[iii,7]<=360){
      wefu1[iii,1]<-wef1[iii]*sin((360-eragp1[iii,7])*2*pi/360)
      wefv1[iii,1]<--wef1[iii]*cos((360-eragp1[iii,7])*2*pi/360)
    }
    #IF direction is between 270 degrees and 360 degrees
    
  }else{
    wefu1[iii,1]<-NA
    wefv1[iii,1]<-NA
  }
  #IF NONE IS NA
}
#iii 

#Save the wefu1 and wefv1

eradata1<-cbind(ddates1,wefu1,wefv1)
eradata1<-as.data.frame(eradata1)
names(eradata1)

names(eradata1)[5]<-"wefu1"
names(eradata1)[6]<-"wefv1"

#We save wave data corresponding to the first GP
outputpath<-inputpath
outputfileERA<-paste(outputpath,"2013ERA_01.txt",sep="")
write.table(eradata1,outputfileERA,quote=F,col.names=T,row.names=F,append=F)

#Now we plot the wave rose corresponding to the first GP
library(openair)

#WEF, direction, WEF
roseERA<-cbind(wef,mwd1,wef)

#Force to be data frame
roseERA<-as.data.frame(roseERA)
names(roseERA)[1]<-"ws"
names(roseERA)[2]<-"wd"
names(roseERA)[3]<-"WEF"
nrow(roseERA)

#A text object with geographical coordinates
geokord<-"2013. ERA5 [-3,43.5]"
pollutionRose(roseERA, pollutant ="WEF",
              key=T,key.footer=NULL,key.header="WEF [Kw/m]",
              max.freq=60,
              sub="Frequency of counts by wave direction (%)",
              main=geokord)
outputpath<-"F:/REM/Semester 2 (UPV)/OWEAOW - Ocean Wave Energy and Offshore Wind Energy Assessment/Block 2 - Wave Energy/Ibarra/Sailor data/Result/"
nname<-"02_ERA2013_rose_01"

#Save as png
plotfile<-paste(outputpath,nname,".png",sep="")
dev.copy(png, plotfile)
dev.off()

#Save as eps
setEPS()
plotfile<-paste(outputpath,nname,".eps",sep="")
dev.copy(postscript, plotfile,horizontal=F)
dev.off()
dev.off()
#01_Nearest grid point ***************************************

#Now we do the same with the second GP which is position 2 in longitude

#02 Distant grid point ***************************************
ddates2<-ddates[,1:4]

#Significant wave height
swh2<-swh0[2,]

#Mean wave period
#The second GP is the second position
mwp2<-mwp0[2,]

#Mean wave direction
#The second GP is the second position
mwd2<-mwd0[2,]

#Calculate wef2
wef2<-0.49*mwp2*(swh2^2)

#Combine merge

eragp2<-cbind(ddates2,swh2,mwp2,mwd2,wef2)
eragp2<-as.data.frame(eragp2)
names(eragp2)

wefu2<-matrix(NA,length(wef2),1)
wefv2<-matrix(NA,length(wef2),1)

for(iii in (1:length(wef2))){
  
  #If wef2 and direction <> NA
  if ((!is.na(wef2[iii]))&(!is.na(eragp2[iii,7]))){
    
    #Calculate zonal and meridional components of wef2
    #Depending on the direction, wefu2 and wefv2 are projected		
    
    #IF direction is below 90 degrees
    if (eragp2[iii,7]<=90){
      wefu2[iii,1]<--wef2[iii]*sin(eragp2[iii,7]*2*pi/360)
      wefv2[iii,1]<--wef2[iii]*cos(eragp2[iii,7]*2*pi/360)
    }
    #IF direction is below 90 degrees	
    
    #IF direction is between 90 degrees and 180 degrees
    if(eragp2[iii,7]>90 & eragp2[iii,7]<=180){
      wefu2[iii,1]<--wef2[iii]*sin((180-eragp2[iii,7])*2*pi/360)
      wefv2[iii,1]<-wef2[iii]*cos((180-eragp2[iii,7])*2*pi/360)
    }
    #IF direction is between 90 degrees and 180 degrees
    
    #IF direction is between 180 degrees and 270 degrees
    if(eragp2[iii,7]>180 & eragp2[iii,7]<=270){
      wefu2[iii,1]<-wef2[iii]*cos((270-eragp2[iii,7])*2*pi/360)
      wefv2[iii,1]<-wef2[iii]*sin((270-eragp2[iii,7])*2*pi/360)
    }
    #IF direction is between 180 degrees and 270 degrees
    
    #IF direction is between 270 degrees and 360 degrees
    if(eragp2[iii,7]>270 & eragp2[iii,7]<=360){
      wefu2[iii,1]<-wef2[iii]*sin((360-eragp2[iii,7])*2*pi/360)
      wefv2[iii,1]<--wef2[iii]*cos((360-eragp2[iii,7])*2*pi/360)
    }
    #IF direction is between 270 degrees and 360 degrees
    
  }else{
    wefu2[iii,1]<-NA
    wefv2[iii,1]<-NA
  }
  #IF NONE IS NA
}
#iii 

#Save the wefu2 and wefv2

eradata2<-cbind(ddates1,wefu2,wefv2)
eradata2<-as.data.frame(eradata2)
names(eradata2)

names(eradata2)[5]<-"wefu2"
names(eradata2)[6]<-"wefv2"

#We calculate WEF, WEFu, and WEFv, and save data in a new file:"2013ERA_02.txt"
outputpath<-inputpath
outputfileERA<-paste(outputpath,"2013ERA_02.txt",sep="")
write.table(eradata2,outputfileERA,quote=F,col.names=T,row.names=F,append=F)

#Just like with first GP we plot the waverose
#A text object with geographical coordinates
geokord<-"2013. ERA5 [-2.5,43.5]"
pollutionRose(roseERA, pollutant ="WEF",
              key=T,key.footer=NULL,key.header="WEF [Kw/m]",
              max.freq=60,
              sub="Frequency of counts by wave direction (%)",
              main=geokord)
outputpath<-"F:/REM/Semester 2 (UPV)/OWEAOW - Ocean Wave Energy and Offshore Wind Energy Assessment/Block 2 - Wave Energy/Ibarra/Sailor data/Result/"
nname<-"02_ERA2013_rose_02"

#Save as png
plotfile<-paste(outputpath,nname,".png",sep="")
dev.copy(png, plotfile)
dev.off()

#Save as eps
setEPS()
plotfile<-paste(outputpath,nname,".eps",sep="")
dev.copy(postscript, plotfile,horizontal=F)
dev.off()