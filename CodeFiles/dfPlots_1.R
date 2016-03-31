##########################################################################################
# densityPlots.R
##########################################################################################
# Original script: Madeline Merck, 29 November 2015
# Last update: Madeline Merck, 3 December 2015
# Inputs for plots include densityGSL.csv, NorthArmLevelDay, and SouthArmLevelDay;
# outputs include TDS plots, 

##########################################################################################
# Installs
##########################################################################################
#install.packages("ggplot2")
require(ggplot2)
require(grid)

##########################################################################################
# Read data
##########################################################################################
NLevel=read.csv("/Users/merck/Documents/MFM/GSL/GSLBiowest/MyWork/Levels/NorthArmLevelDay.csv",
                stringsAsFactors=FALSE,header=T)
SLevel=read.csv("/Users/merck/Documents/MFM/GSL/GSLBiowest/MyWork/Levels/SouthArmLevelDay.csv",
                stringsAsFactors=FALSE,header=T)
UGSdata=read.csv("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/data/clean/UGS/2016/UGSdata.csv",
                    stringsAsFactors=FALSE,header=T)
USGSdata=read.csv("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/data/clean/USGS/USGSdata.csv",
                    stringsAsFactors=FALSE,header=T)

## To plot both USGS and UGS at once
densityGSL = UGSdata[,c("SITE","DATE","bathy_elev","bay","sample_elev_usgs","layerTDS_gL","layerSalinity_ppt")]
densityGSL = rbind(densityGSL,USGSdata[,c("SITE","DATE","bathy_elev","bay","sample_elev_usgs","layerTDS_gL","layerSalinity_ppt")])

# To plot UGS only
densityGSL = UGSdata[,c("SITE","DATE","bathy_elev","bay","sample_elev_usgs","layerTDS_gL","layerSalinity_ppt")]

# To plot USGS only
densityGSL = USGSdata[,c("SITE","DATE","bathy_elev","bay","sample_elev_usgs","layerTDS_gL","layerSalinity_ppt")]

# Set working directory for saving output plot files
setwd("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/plots")
# write.csv(densityGSL,"USGS_UGS_dfDensity.csv",row.names=FALSE)

# ##########################################################################################
# # Invalid Measurement Sites by arm
# ##########################################################################################
# NinvalidSites = c("01N","02N","03N","04N","01A","ECN")
# SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS","MI","SI")

##########################################################################################
# Salinity/TDS at Elevation...
##########################################################################################
# Plots of depth vs date with colorRamp markers based on measured TDS

# Set working directory for saving output plot files
setwd("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/plots")

## North and South Arm
sitePlots = c('N','S')
for(i in c(1:length(sitePlots))){
  df = densityGSL[which(densityGSL$bay == paste(sitePlots[i])),]
  TDS = df$layerSalinity_ppt
  date=as.Date(df$DATE) #must be in yyyy/mm/dd format
  elev = df$sample_elev_usgs
  if (df$bay[1]=='N'){
    dfL = data.frame(NLevel[which(!is.na(NLevel$Level_ft)),1:2])
    limits = c(45,290) #c(100,480)
    color = rainbow(10)
  } else {
    dfL = data.frame(SLevel[which(!is.na(SLevel$Level_ft)),1:2])
    limits = c(45, 290) # c(-5,320)
    color = rainbow(10)
  }
  
  p2 = ggplot(df,aes(x=date,y=elev,col=TDS)) + geom_point(size=3) + theme_bw() +
    coord_cartesian(ylim = c(4160, 4215), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
    scale_color_gradientn(limits=limits, colours=color, expression("Salinity (g/L)")) + # S
    labs(title = paste("All USGS & UGS Measurements in South Arm", sep=""), x="Year", y="Elevation (ft)") +
    geom_hline(yintercept=4177, linetype = 2, col="grey") + 
    geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
    theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
          axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
          legend.text=element_text(size=12),legend.title=element_text(size=14),
          legend.position = c(.06, .85),plot.margin = unit(c(0,6,1,1),units="points"))
  
  p2
  ggsave(paste("USGS_UGS_Sal_ALL_",df$bay[1],".jpg",sep = ""), p2, width=14, height=8.5)
}

## Individual Sites
# Loop through measurement sites or bays
sitePlots = unique(densityGSL$SITE) 
for(i in c(1:length(sitePlots))){
  df = densityGSL[which(densityGSL$SITE == paste(sitePlots[i])),]
  TDS = df$layerSalinity_ppt
  date=as.Date(df$DATE) #must be in yyyy/mm/dd format
  elev = df$sample_elev_usgs
  bathy = df$bathy_elev[1]
  if (df$bay[1]=='N'){
    dfL = data.frame(NLevel[which(!is.na(NLevel$Level_ft)),1:2])
    limits = c(100,480)
    color = rainbow(10)
    bay = "North"
  } else {
    dfL = data.frame(SLevel[which(!is.na(SLevel$Level_ft)),1:2])
    limits = c(-5, 320)
    color = rainbow(5)
    bay = "South"
  }

  p1 = ggplot(df,aes(x=date,y=elev,col=TDS)) + geom_point(size=5) + theme_bw() +
    coord_cartesian(ylim = c(4160, 4215), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
    scale_color_gradientn(limits=limits, colours=color, expression("Salinity (g/L)")) + # S
    labs(title = paste("USGS Measurement Site ",sitePlots[i]," (",bay," Arm)", sep=""), x="Year", y="Elevation (ft)") +
    geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
    geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
    theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),plot.title=element_text(size=20),
          text=element_text(size=18),
          legend.title=element_text(size=14),legend.position = c(.06, .85),plot.margin = unit(c(0,6,1,1),units="points"))

  p1
  
  ggsave(paste("USGS_Sal_",df$bay[1],"_",sitePlots[i],".jpg",sep = ""), p1, width=14, height=8.5)
}

##########################################################################################
# Mean TDS vs Surface TDS...
##########################################################################################
# Plot of volume weighted mean TDS vs Surface TDS 

# Set working directory for saving output plot files
setwd("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/plots")

# For UGS data
SaltLoad = read.csv("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/data/clean/UGS/2016/UGSload.csv", header = TRUE)

# For USGS
SaltLoad = read.csv("/Users/merck/Documents/MFM/GSL/GSLIntegratedModelingCH2M/MyWork/Density/data/clean/USGS/USGSload.csv", header = TRUE)

arm = "N"
# invalid = c("GSLB","BRBS","ABS","ABN","SS","01S","02S","03S","04S") # South sites removed
# NvalidSites = c("NML","RD2","LVG4","RT3","ECN")
# SvalidSites = c("RT4","FB2","NLN","RT2","IS1","IS2","AC1","AC2","AC3","RT1","SS")
SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
NinvalidSites = c("01N","02N","03N","04N")
# invalid = c("01N","02N","03N","04N") # North sites removed
dfS = SaltLoad[which(SaltLoad$bay == paste(arm)),]
dfS = dfS[-which(dfS$Site %in% invalid),]
dfS = dfS[which(dfS$Site %in% NinvalidSites),]
dfS = dfS[-which(dfS$Site %in% SinvalidSites),]

dateStart = min(as.Date(dfS$Date))
dateStop = as.Date("1984-08-01")

dateStart = as.Date("1984-08-01")
dateStop = as.Date("1990-01-01")

dateStart = as.Date("2000-01-01")
dateStop = max(as.Date(dfS$Date))

## UGS TDS with labels
## North
inds = which(SaltLoad$bay == "N")
p6.1 = ggplot(SaltLoad[inds,],aes(x=SaltLoad$AveTDS_gL[inds],
                                  y=SaltLoad$TopTDS_gL[inds],#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  labs(title = paste("Mean TDS vs Surface TDS in North Arm"), x="Mean TDS (g/L)", y="Surface TDS (g/L)") +
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        plot.margin = unit(c(0,6,1,1),units="points"))

p6.1

ggsave(paste("N_TDS_mean_surface_labels.jpg",sep = ""), p6.1, width=11, height=11)

## South
inds = which(SaltLoad$bay == "S")
p6.2 = ggplot(SaltLoad[inds,],aes(x=SaltLoad$AveTDS_gL[inds],
                                  y=SaltLoad$TopTDS_gL[inds],#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(100,200), xlim = c(100,200)) +
  labs(title = paste("Mean TDS vs Surface TDS in South Arm"), x="Mean TDS (g/L)", y="Surface TDS (g/L)") +
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        plot.margin = unit(c(0,6,1,1),units="points"))

p6.2

ggsave(paste("S_TDS_mean_surface_labels.jpg",sep = ""), p6.2, width=11, height=11)

## UGS Salinity with labels
## North
inds = which(SaltLoad$bay == "N")
p6.1 = ggplot(SaltLoad[inds,],aes(x=SaltLoad$AveSalinity_ppt[inds],
                                  y=SaltLoad$TopSalinity_ppt[inds],#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(100,200), xlim = c(100,200)) +
  labs(title = paste("Mean TDS vs Surface TDS in North Arm"), x="Mean TDS (g/L)", y="Surface TDS (g/L)") +
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        plot.margin = unit(c(0,6,1,1),units="points"))

p6.1

ggsave(paste("N_TDS_mean_surface_labels.jpg",sep = ""), p6.1, width=11, height=11)

## South
inds = which(SaltLoad$bay == "S")
p6.2 = ggplot(SaltLoad[inds,],aes(x=SaltLoad$AveSalinity_ppt[inds],
                                  y=SaltLoad$TopSalinity_ppt[inds],#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype = 2) +
  coord_cartesian(ylim = c(95,160), xlim = c(95,160)) +
  labs(title = paste("USGS Dataset for GSL:\nMean Salinity vs Surface Salinity in South Arm"), x="Mean TDS (g/L)", y="Surface TDS (g/L)") +
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        plot.margin = unit(c(0,6,1,1),units="points"))

p6.2

ggsave(paste("S_Sal_mean_sur_labels.jpg",sep = ""), p6.2, width=11, height=11)

# p5 = ggplot(dfS[inds,],aes(x=dfS$AveTDS_gL[inds],y=dfS$TopTDS_gL[inds],col=dfS$Site[inds],label=dfS$Site[inds])) +
#   geom_point(size=3) + theme_bw() + geom_text(aes(label=dfS$Site[inds]),hjust=0,just=0) +
#   labs(title = paste("Mean TDS vs Surface TDS in North Arm: ",dateStart," to ",dateStop), x="Mean TDS", y="Surface TDS",colour = "Site") +
#   # coord_cartesian(ylim = c(0, 500), xlim = c(0,500)) +
#   # labs(x="Year", y="Measurement Elevation (ft)") +
#   #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
#   #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
#   #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
#   #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
#   #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
#   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
#         axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
#         legend.text=element_text(size=12),legend.title=element_text(size=14),
#         # legend.position = c(.1, .92),
#         plot.margin = unit(c(0,6,1,1),units="points"))
# 
# p5
# 
# ggsave(paste(arm,"_Load_labels",dateStart,"_",dateStop,".jpg",sep = ""), p5, width=14, height=8.5)

## Select sites with shape and color
dateStart = min(as.Date(dfS$Date))
dateStop = as.Date("1984-08-01")

dateStart = as.Date("1984-08-01")
dateStop = as.Date("1990-01-01")

dateStart = as.Date("2000-01-01")
dateStop = max(as.Date(dfS$Date))
## North
# NinvalidSites = c("01N","02N","03N","04N","01A","ECN")
inds = which(SaltLoad$bay == "N" & !SaltLoad$Site %in% NinvalidSites) # x[!x %in% y] #--  x without y
p5.2 = ggplot(SaltLoad[inds,],aes(x=SaltLoad$AveTDS_gL[inds],y=SaltLoad$TopTDS_gL[inds],colour=SaltLoad$Site[inds])) + #,shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(length(unique(SaltLoad$Site[inds]))),breaks=SaltLoad$Site[inds]) +
  # scale_shape_manual(name="Sites",labels=NvalidSites,values=c(0:11),breaks=NvalidSites) +
  geom_point(size=5) + theme_bw() + #coord_cartesian(ylim = c(0, 350), xlim = c(0,350)) +
  geom_abline(intercept = 0, slope = 1,linetype = 2) +
  labs(title = paste("Mean TDS vs Surface TDS in North Arm at Selected Sites"),
       x="Mean TDS (g/l)", y="Surface TDS (g/L)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        legend.position = c(.06, .65),plot.margin = unit(c(0,6,1,1),units="points"))

p5.2

ggsave(paste("N_TDS_mean_surface_select.jpg",sep = ""), p5.2, width=11, height=11)

## South
# SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
# SinvalidSites = c()
inds = which(SaltLoad$bay == "S" & !SaltLoad$Site %in% SinvalidSites) # x[!x %in% y] #--  x without y 
p5.3 = ggplot(SaltLoad[inds,],aes(x=SaltLoad$AveTDS_gL[inds],y=SaltLoad$TopTDS_gL[inds],colour=SaltLoad$Site[inds])) + #,shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(length(unique(SaltLoad$Site[inds]))),breaks=SaltLoad$Site[inds]) +
  # scale_shape_manual(name="Sites",labels=NvalidSites,values=c(0:11),breaks=NvalidSites) +
  geom_point(size=5) + theme_bw() + #coord_cartesian(ylim = c(0, 350), xlim = c(0,350)) +
  geom_abline(intercept = 0, slope = 1,linetype = 2) +
  labs(title = paste("Mean TDS vs Surface TDS in South Arm at Selected Sites"),
       x="Mean TDS (g/l)", y="Surface TDS (g/L)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        legend.position = c(.06, .65),plot.margin = unit(c(0,6,1,1),units="points"))
# plot.margin = unit(c(0,6,1,1),units="points"))

p5.3

ggsave(paste("S_TDS_mean_surface_select.jpg",sep = ""), p5.3, width=11, height=11)

##########################################################################################
# Total Load vs Date...
##########################################################################################
## Plot both arms together
p3 = ggplot(SaltLoad,aes(x=as.Date(SaltLoad$Date),
                                y=(SaltLoad$TotalLoad_kg*0.00110231/1e9),col=SaltLoad$bay)) +
  geom_point(size=3) + theme_bw() +
  labs(title = paste("Total Salt Load in North and South Arms"), x="Year", y="Salt Load (billion tons)", colour ="Bay") +
  theme(text=element_text(size=50),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        legend.text=element_text(size=12),legend.title=element_text(size=14),
        legend.position = c(.06, .85),plot.margin = unit(c(0,6,1,1),units="points"))

p3

ggsave(paste("NS_Load_ALL.jpg",sep = ""), p3, width=14, height=8.5)

## Both arms using selected measurement sites
# validSites = c("NML","RD2","LVG4","RT3","ECN","RT4","FB2","NLN","RT2","IS1","IS2","AC1","AC2","AC3","RT1","SS")
# inds = which(SaltLoad$Site %in% validSites)

invalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
inds = which(!SaltLoad$Site %in% invalidSites) # x[!x %in% y] #--  x without y 

p3.1 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(40),breaks=SaltLoad$Site[inds]) +
  scale_shape_manual(name="Sites",labels=SaltLoad$Site[inds],values=c(0:40),breaks=SaltLoad$Site[inds]) +
  geom_point(size=5) + theme_bw() +
  coord_cartesian(ylim = c(0.85, 3.25)) +
  labs(title = paste("Total Salt Load in North and South Arms"), x="Year", y="Salt Load (billion tons)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        plot.margin = unit(c(0,6,1,1),units="points")) #,legend.position = c(.06, .15)))

p3.1

ggsave(paste("NS_Load_select.jpg",sep = ""), p3.1, width=14, height=8.5)

## South with point shapes in different colors
# SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
inds = which(SaltLoad$bay == "S" & !SaltLoad$Site %in% SinvalidSites) # x[!x %in% y] #--  x without y 

p3.1.1 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                  y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(21),breaks=SaltLoad$Site[inds]) +
  scale_shape_manual(name="Sites",labels=SaltLoad$Site[inds],values=c(0:20),breaks=SaltLoad$Site[inds]) +
  geom_point(size=5) + theme_bw() +
  # coord_cartesian(ylim = c(0.85, 3.25)) +
  labs(title = paste("Total Salt Load in South Arm at Selected Sites"), x="Year", y="Salt Load (billion tons)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        plot.margin = unit(c(0,6,1,1),units="points")) #,legend.position = c(.06, .15)))

p3.1.1

ggsave(paste("S_Load_select.jpg",sep = ""), p3.1.1, width=14, height=8.5)

## North with point shapes in different colors
# NinvalidSites = c("01N","02N","03N","04N")
inds = which(SaltLoad$bay == "N" & !SaltLoad$Site %in% NinvalidSites) # x[!x %in% y] #--  x without y 

p3.1.2 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                    y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(15),breaks=SaltLoad$Site[inds]) +
  scale_shape_manual(name="Sites",labels=SaltLoad$Site[inds],values=c(0:20),breaks=SaltLoad$Site[inds]) +
  geom_point(size=5) + theme_bw() +
  # coord_cartesian(ylim = c(0.85, 3.25)) +
  labs(title = paste("Total Salt Load in North Arm at Selected Sites"), x="Year", y="Salt Load (billion tons)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        plot.margin = unit(c(0,6,1,1),units="points")) #,legend.position = c(.06, .15)))

p3.1.2

ggsave(paste("N_Load_select.jpg",sep = ""), p3.1.2, width=14, height=8.5)

## Plot arms individually with labels
## North
inds = which(SaltLoad$bay == "N")
p3.2 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),#col=SaltLoad$bay[inds],
                                label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Total Salt Load in North Arm"), x="Year", y="Salt Load (billion tons)", colour ="Bay") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        # legend.text=element_text(size=12),legend.title=element_text(size=14),legend.position = c(.1, .92),
        plot.margin = unit(c(0,6,1,1),units="points"))

p3.2

ggsave(paste("N_Load_labels.jpg",sep = ""), p3.2, width=14, height=8.5)

## South
inds = which(SaltLoad$bay == "S")
p3.3 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),#col=SaltLoad$bay[inds],
                                label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Total Salt Load in South Arm"), x="Year", y="Salt Load (billion tons)", colour ="Bay") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        # legend.text=element_text(size=12),legend.title=element_text(size=14),legend.position = c(.1, .92),
        plot.margin = unit(c(0,6,1,1),units="points"))

p3.3

ggsave(paste("S_Load_labels.jpg",sep = ""), p3.3, width=14, height=8.5)

## Load vs Date: Plot arms individually with select sites
## North

inds = which(SaltLoad$bay == "N")
p3.2 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                  y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Total Salt Load in North Arm"), x="Year", y="Salt Load (billion tons)", colour ="Bay") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        # legend.text=element_text(size=12),legend.title=element_text(size=14),legend.position = c(.1, .92),
        plot.margin = unit(c(0,6,1,1),units="points"))

p3.2

ggsave(paste("N_Load_labels.jpg",sep = ""), p3.2, width=14, height=8.5)

## South
# inds = which(SaltLoad$bay == "S")
SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
# SvalidSites = c("RT4","FB2","NLN","RT2","IS1","IS2","AC1","AC2","AC3","RT1","SS") ### Add AS2
inds = which(SaltLoad$bay == "S" & !SaltLoad$Site %in% SinvalidSites) # x[!x %in% y] #--  x without y 
p3.3 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                  y=(SaltLoad$TotalLoad_kg[inds]*0.00110231/1e9),#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Total Salt Load in South Arm at Selected Sites"), x="Year", y="Salt Load (billion tons)", colour ="Bay") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        # legend.text=element_text(size=12),legend.title=element_text(size=14),legend.position = c(.1, .92),
        plot.margin = unit(c(0,6,1,1),units="points"))

p3.3

ggsave(paste("S_Load_labels.jpg",sep = ""), p3.3, width=14, height=8.5)

##########################################################################################
# TDS vs Date...
##########################################################################################

p5 = ggplot(SaltLoad,aes(x=as.Date(SaltLoad$Date),
                         y=(SaltLoad$AveTDS_gL),col=SaltLoad$bay)) +
  geom_point(size=3) + theme_bw() +
  labs(title = paste("Mean TDS in North and South Arms"), x="Year", y="Mean TDS (g/L)", colour ="Bay") +
  theme(text=element_text(size=50),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        legend.text=element_text(size=12),legend.title=element_text(size=14),
        legend.position = c(.06, .85),plot.margin = unit(c(0,6,1,1),units="points"))

p5

## MEAN
## Plot both arms together
validSites = c("NML","RD2","LVG4","RT3","ECN","RT4","FB2","NLN","RT2","IS1","IS2","AC1","AC2","AC3","RT1","SS")

inds = which(SaltLoad$Site %in% validSites)
# d = data.frame(p=c(0:14,35))
p5.1 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                         y=(SaltLoad$AveTDS_gL[inds]),colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=validSites,values=rainbow(16),breaks=validSites) +
  scale_shape_manual(name="Sites",labels=validSites,values=c(0:14,35),breaks=validSites) +
  # label=SaltLoad$Site)) +
  # geom_point(data=d, mapping=aes(shape=d$p),size=3) + theme_bw() +
  geom_point(size=5) + theme_bw() +
  # geom_text(aes(label=SaltLoad$Site),hjust=0,just=0) +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Mean TDS in North and South Arms"), x="Year", y="Mean TDS") + #, colour ="Sites") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        plot.margin = unit(c(0,6,1,1),units="points")) #,legend.position = c(.06, .15)))

p5.1

ggsave(paste("NS_TDS_select.jpg",sep = ""), p5.1, width=14, height=8.5)

## South with point shapes in different colors
# SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
inds = which(SaltLoad$bay == "S" & !SaltLoad$Site %in% SinvalidSites) # x[!x %in% y] #--  x without y 

p5.1.1 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                    y=(SaltLoad$AveTDS_gL[inds]),colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(21),breaks=SaltLoad$Site[inds]) +
  scale_shape_manual(name="Sites",labels=SaltLoad$Site[inds],values=c(0:20),breaks=SaltLoad$Site[inds]) +
  geom_point(size=5) + theme_bw() +
  # coord_cartesian(ylim = c(0.85, 3.25)) +
  labs(title = paste("Mean TDS in South Arm at Selected Sites"), x="Year", y="Mean TDS (g/L)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        plot.margin = unit(c(0,6,1,1),units="points")) #,legend.position = c(.06, .15)))

p5.1.1

ggsave(paste("S_TDS_select.jpg",sep = ""), p5.1.1, width=14, height=8.5)

## North with point shapes in different colors
# NinvalidSites = c("01N","02N","03N","04N")
inds = which(SaltLoad$bay == "N" & !SaltLoad$Site %in% NinvalidSites) # x[!x %in% y] #--  x without y 

p5.1.2 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                    y=(SaltLoad$AveTDS_gL[inds]),colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
  scale_color_manual(name="Sites",labels=SaltLoad$Site[inds],values=rainbow(length(unique(SaltLoad$Site[inds]))),breaks=SaltLoad$Site[inds]) +
  scale_shape_manual(name="Sites",labels=SaltLoad$Site[inds],values=c(0:20),breaks=SaltLoad$Site[inds]) +
  geom_point(size=5) + theme_bw() +
  # coord_cartesian(ylim = c(0.85, 3.25)) +
  labs(title = paste("Mean TDS in North Arm at Selected Sites"), x="Year", y="Mean TDS (g/L)") +
  theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=18), axis.text.y= element_text(size=18),plot.title=element_text(size=20),
        legend.text=element_text(size=16),legend.title=element_text(size=18),
        plot.margin = unit(c(0,6,1,1),units="points")) #,legend.position = c(.06, .15)))

p5.1.2

ggsave(paste("N_TDS_select.jpg",sep = ""), p5.1.2, width=14, height=8.5)

## TDS vs Date: Plot arms individually with labels
## North
inds = which(SaltLoad$bay == "N")
p5.2 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                  y=(SaltLoad$AveTDS_gL[inds]),#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Mean TDS in North Arm"), x="Year", y="Mean TDS (g/L)", colour ="Bay") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        # legend.text=element_text(size=12),legend.title=element_text(size=14),legend.position = c(.1, .92),
        plot.margin = unit(c(0,6,1,1),units="points"))

p5.2

ggsave(paste("N_TDS_labels.jpg",sep = ""), p5.2, width=14, height=8.5)

## South
# inds = which(SaltLoad$bay == "S")
SinvalidSites = c("GSLB","BRBS","01S","02S","03S","04S","ABN","ABS","SS")
# SvalidSites = c("RT4","FB2","NLN","RT2","IS1","IS2","AC1","AC2","AC3","RT1","SS") ### Add AS2
inds = which(SaltLoad$bay == "S" & !SaltLoad$Site %in% SinvalidSites) # x[!x %in% y] #--  x without y 

p5.3 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),
                                  y=(SaltLoad$AveTDS_gL[inds]),#col=SaltLoad$bay[inds],
                                  label=SaltLoad$Site[inds])) +
  geom_point(size=3) + geom_text(aes(label=SaltLoad$Site[inds]),hjust=0, vjust=0) + theme_bw() +
  # coord_cartesian(ylim = c(-0.1, 4.6), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #coord_cartesian(xlim = as.Date(c('1965-01-01','2016-10-01'))) +
  #scale_color_gradientn(limits=limits, colours=color, expression("TDS (g/L)")) + # S
  labs(title = paste("Mean TDS in South Arm at Selected Sites"), x="Year", y="Mean TDS (g/L)", colour ="Bay") +
  # labs(x="Year", y="Measurement Elevation (ft)") +
  #geom_hline(yintercept=bathy, linetype = 2, col="grey") + 
  #geom_line(aes(x=as.Date(dfL$Date), y=dfL$Level_ft),col="black", dfL) +
  #geom_line(aes(x=as.Date(dfS$Date), y=dfS$TotalLoad_kg),col="black", dfS) +
  #   theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
  #         legend.title=element_text(size=14),legend.position = c(.06, .9),plot.margin = unit(c(0,6,1,1),units="points"))
  theme(axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
        axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
        # legend.text=element_text(size=12),legend.title=element_text(size=14),legend.position = c(.1, .92),
        plot.margin = unit(c(0,6,1,1),units="points"))

p5.3

ggsave(paste("S_TDS_labels.jpg",sep = ""), p5.3, width=14, height=8.5)

## TDS vs Date: Plot arms individually with select sites
## South
# inds = which(SaltLoad$bay == "S")
# SvalidSites = c("RT4","FB2","NLN","RT2","IS1","IS2","AC1","AC2","AC3","RT1","SS") ### Add AS2
# inds = which(SaltLoad$Site %in% SvalidSites)
inds = which(SaltLoad$bay == "S" & !SaltLoad$Site %in% SinvalidSites) # x[!x %in% y] #--  x without y 

png("S_TDS_surface_mean_timeseries.png", width=11, height=8.5, units="in", res=300)
plot(as.Date(SaltLoad$Date[inds]),SaltLoad$AveTDS_gL[inds],type = "p",pch=19,cex=0.5,col="black",
     xlab="Year", ylab="TDS [g/L]", main="Mean TDS and Surface TDS in the South Arm",
     cex.lab = 1.5,cex.axis=1.5, ylim=c(0,350))
points(as.Date(SaltLoad$Date[inds]),SaltLoad$TopTDS_gL[inds],type = "p",pch=19,cex=0.5,col="red")
legend( x="topleft",legend=c("Surface TDS","Mean TDS"),col=c("red","black"), pch=c(19,19), cex = 1.25)
dev.off()

# p5.2 = ggplot(SaltLoad[inds,],aes(x=as.Date(SaltLoad$Date[inds]),y=(SaltLoad$AveTDS_gL[inds]))) +
# #,colour=SaltLoad$Site[inds],shape=SaltLoad$Site[inds])) +
# #   scale_color_manual(name="Sites",labels=SvalidSites,values=rainbow(11),breaks=SvalidSites) +
# #   scale_shape_manual(name="Sites",labels=SvalidSites,values=c(0:14,35),breaks=SvalidSites) +
#   geom_point(size=2) + theme_bw() +
#   geom_point(aes(x=as.Date(SaltLoad$Date[inds]),y=(SaltLoad$TopTDS_gL[inds]),col="red"),SaltLoad[inds,]) +
#   coord_cartesian(ylim = c(0, 350), xlim = as.Date(c('1965-01-01','2016-10-01'))) +
#   labs(title = paste("Surface and Mean TDS in the South Arm"), x="Year", y="Mean TDS")
#   theme(text=element_text(size=18),axis.title.x= element_text(size=18), axis.title.y= element_text(size=18),
#         axis.text.x= element_text(size=14), axis.text.y= element_text(size=14),plot.title=element_text(size=20),
#         legend.text=element_text(size=12),legend.title=element_text(size=14),plot.margin = unit(c(0,6,1,1),units="points"))
#         # legend.position = c(.1, .92),plot.margin = unit(c(0,6,1,1),units="points"))
# 
# p5.2
# 
# ggsave(paste("S_Load_labels.jpg",sep = ""), p5.2, width=14, height=8.5)

## North
# NvalidSites = c("NML","RD2","LVG4","RT3","ECN")
# inds = which(SaltLoad$Site %in% NvalidSites)
inds = which(SaltLoad$bay == "N" & !SaltLoad$Site %in% NinvalidSites) # x[!x %in% y] #--  x without y 

png("N_TDS_surface_mean_timeseries.png", width=11, height=8.5, units="in", res=300)
plot(as.Date(SaltLoad$Date[inds]),SaltLoad$AveTDS_gL[inds],type = "p",pch=19,cex=0.5,col="black",
     xlab="Year", ylab="TDS [g/L]", main="Mean TDS and Surface TDS in the North Arm",
     cex.lab = 1.5,cex.axis=1.5, ylim=c(150,415))
points(as.Date(SaltLoad$Date[inds]),SaltLoad$TopTDS_gL[inds],type = "p",pch=19,cex=0.5,col="red")
legend( x="topleft",legend=c("Surface TDS","Mean TDS"),col=c("red","black"), pch=c(19,19), cex = 1.25)
dev.off()


## Time series of surface and mean TDS for each arm