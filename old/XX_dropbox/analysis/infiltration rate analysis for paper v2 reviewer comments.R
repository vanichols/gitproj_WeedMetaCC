##final code for infiltration rate meta-analysis

setwd("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis")

setwd("~/Dropbox/UCS documents/Kendall Fellow project/Soil water lit review project/Data analysis")

library(lattice)
library(rmeta) #don't think I used this
library (nlme) #nonlinear mixed effects model - or this one
library(lme4)
library (plotrix) ##may not have used this
library(ggplot2)
library(boot)
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
library(grid)
library(lmerTest)




dat1 <- read.csv("sw ma v26.csv", na.strings='.', stringsAsFactors=FALSE)
dat1$Li <- log((dat1$Trt_Inf)/(dat1$Con_Inf))
dat1$Change <- (dat1$Trt_Inf)/(dat1$Con_Inf)
dat1$Wi <- (dat1$Reps * dat1$Reps) / (dat1$Reps + dat1$Reps)
dat1$Per <- ((exp(dat1$Li))-1)*100
till <- subset(dat1, Trt=='till')
res <- subset(dat1, Trt=='res')
cr <- subset(dat1, Trt=='cr')
int <- subset(dat1, Trt=='int')
per <- subset(dat1, Trt=='per')

length(unique(per$Study))


##overall means for first graph

dat1$Per <- ((exp(dat1$Li))-1)*100
dat1.o <- dat1[order(dat1$Per),]


fit <- lmer(Li ~ 1 + (1|Study), data=per,weights = Wi)
summary(fit)

##overall positive
pos.crop <- subset(dat1, Li > 0)
##246/391 = 62.9

##FIG 1 = MAP

##FIG 2 

overall <- read.csv ("overallmeansv2.csv", header=TRUE)

overall.o <- overall[order(overall$nn),]
overall.o$nn <- as.character(overall.o$nn)
overall.o$title <- factor(overall.o$title, levels=unique(overall.o$title))



scale <- 10
hpp <- 130 * scale
wpp <- 255 * scale
#MAY NEED TIFF FORMAT FOR GCB
##type=cairo worked in mac to change size of the file
#tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig2_draft.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
tiff("~/Dropbox/UCS documents/Kendall Fellow project/Soil water lit review project/Data analysis/fig2_revised.tiff", height= hpp, width=wpp, res=300, compression="lzw", type='cairo') 
limits2 <- aes(xmax = rr + se*1.96, xmin = rr - se*1.96)
ggplot(data = overall.o, aes_string(x = "rr", y = "title", color="pfm")) +
  ylab("  ") + 
  xlab("Percent Change in Infiltration Rate") +  
  geom_errorbarh(limits2, height=0.15) + 
  geom_vline(xintercept=0, lwd=0.5) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=5) + 
  annotate("text", x = 108, y = 1, label = paste("n = ",overall.o$nn[1],sep="")) +
  annotate("text", x = 108, y = 2, label = paste("n = ",overall.o$nn[2],sep="")) +
  annotate("text", x = 108, y = 3, label = paste("n = ",overall.o$nn[3],sep="")) +
  annotate("text", x = 108, y = 4, label = paste("n = ",overall.o$nn[4],sep="")) +
  annotate("text", x = 108, y = 5, label = paste("n = ",overall.o$nn[5],sep="")) 
dev.off()


##overall tillage mean
fit <- lmer(Li ~ 1 + (1|Study), data=till,weights = Wi)
summary(fit)



till$Length2[till$Yrs ==1 ] <- "1"
till$Length2[till$Yrs ==2 ] <- "2"
till$Length2[till$Yrs ==3 ] <- "3"
till$Length2[till$Yrs > 3] <- "> 3"
short <- subset(till, Length2== '1')
short1 <- subset(till, Length2== '2')
short2 <- subset(till, Length2== '3')
long <- subset(till, Length2== '> 3')

fit <- lmer(Li ~ Length2 + (1|Study), data=till,weights = Wi)
summary(fit)

##hard to see a clear effect of time! these are good reasonably even sized groups

till$Precip2[till$Avg.precip < 600 ] <- '< 600mm'
till$Precip2[till$Avg.precip > 600 &  till$Avg.precip < 1001 ] <- '600-1000mm'
till$Precip2[till$Avg.precip > 1000 &  till$Avg.precip < 1300 ] <- '1000-1300mm'
till$Precip2[till$Avg.precip > 1300 ] <- '> 1300mm'
dry <-subset(till, Precip2 == '< 600mm')
wet <-subset(till, Precip2 == '600-1000mm')
wetter <-subset(till, Precip2 == '1000-1300mm')
wettest <-subset(till, Precip2 == '> 1300mm')

fit <- lmer(Li ~ Precip2 + (1|Study), data=till,weights = Wi)
summary(fit)

##clear difference here with the drier areas !

length(unique(ct$Study))

fit <- lmer(Li ~ Tillage + (1|Study), data=till,weights = Wi)
summary(fit)

ct <-subset(till, Tillage == 'CT')
rt <-subset(till, Tillage == 'RT')



till.res <- rbind(wres, wores) ##all the missing values here are messing up the calcs

wres <-subset(till, Residue == 'with')
wores <-subset(till, Residue == 'without')

fit <- lmer(Li ~ Residue + (1|Study), data=till.res,weights = Wi)
summary(fit)



hisand <-subset(till, X.50Sand == 'Y')
losand <-subset(till, X.50Sand == 'N')
hiclay <-subset(till, X.30Clay == 'Y')
loclay <-subset(till, X.30Clay == 'N')

fit <- lmer(Li ~ X.50Sand + (1|Study), data=till,weights = Wi)
summary(fit)

length(unique(till.2crop$Study))

fit <- lmer(Li ~ CoverCrop + (1|Study), data=till,weights = Wi)
summary(fit)

cc <-subset(till, CoverCrop == "yes")

till$Crops2[till$No..cr ==1 ] <- '1 crop'
till$Crops2[till$No..cr > 1 ] <- '2 or more crops'

fit <- lmer(Li ~ Crops2 + (1|Study), data=till,weights = Wi)
summary(fit)

till.2crop <-subset(till, Crops2 == '2 or more crops')

##going to include the value of 2 or more crops in main table



##evaluating interactions - for review

fit <- lmer(Li ~ Aridity*X.Sand + (1|Study), data=res,weights = Wi)
summary(fit)

fit <- lmer(Li ~ Aridity*X.Clay + (1|Study), data=till,weights = Wi)
summary(fit)

##clay*aridity pvalue is 0.11 for tillage and 0.09 for cover crops
## no relationship for sand

##for adding regression coefficients to the charts
fit <- lmer(Li ~ Yrs + (1|Study), data=per,weights = Wi)
summary(fit)

##aridity
## no-till: 0.028
## cover crop: -0.009
## int:  0.430
## per: 0.011
## cr: 1.228

## count of tillage studies with 
argentina <- subset(till, Aridity > 0.65 & Li > 0.69)

##
till.graph <- read.csv ("tillage graph 3.csv", header=TRUE)


#till.graph.o <- till.graph[order(till.graph$nn),]
till.graph$nn <- as.character(till.graph$nn)
till.graph$title <- factor(till.graph$title, 
                           levels=unique(till.graph$title))

scale <- 10
hpp <- 230 * scale
wpp <- 210 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig3_draft.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
limits2 <- aes(xmax = rr + se*1.96, xmin = rr - se*1.96)
ggplot(data = till.graph, aes_string(x = "rr", y = "title", color="pfm")) +
  ylab("Study Length          Precip                   Texture                 Crops      Residue     Tillage  ") + 
  xlab("Percent Change in Infiltration Rate with No-Till") + 
  geom_errorbarh(limits2, height=0.15) + 
  geom_vline(xintercept=0, lwd=0.5) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=5) + 
  annotate("text", x = 155, y = 1, label = paste("n = ",till.graph$nn[1],sep="")) +
  annotate("text", x = 155, y = 2, label = paste("n = ",till.graph$nn[2],sep="")) +
  annotate("text", x = 155, y = 3, label = paste("n = ",till.graph$nn[3],sep="")) +
  annotate("text", x = 155, y = 4, label = paste("n = ",till.graph$nn[4],sep="")) +
  annotate("text", x = 155, y = 5, label = paste("n = ",till.graph$nn[5],sep="")) + 
  annotate("text", x = 155, y = 6, label = paste("n = ",till.graph$nn[6],sep="")) + 
  annotate("text", x = 155, y = 7, label = paste("n = ",till.graph$nn[7],sep="")) +
  annotate("text", x = 155, y = 8, label = paste("n = ",till.graph$nn[8],sep="")) +
  annotate("text", x = 155, y = 9, label = paste("n = ",till.graph$nn[9],sep="")) +
  annotate("text", x = 155, y = 10, label = paste("n = ",till.graph$nn[10],sep="")) +
  annotate("text", x = 155, y = 11, label = paste("n = ",till.graph$nn[11],sep="")) + 
  annotate("text", x = 155, y = 12, label = paste("n = ",till.graph$nn[12],sep="")) +
  annotate("text", x = 155, y = 13, label = paste("n = ",till.graph$nn[13],sep="")) +
  annotate("text", x = 155, y = 14, label = paste("n = ",till.graph$nn[14],sep="")) +
  annotate("text", x = 155, y = 15, label = paste("n = ",till.graph$nn[15],sep="")) + 
  annotate("text", x = 155, y = 16, label = paste("n = ",till.graph$nn[16],sep="")) +
  annotate("text", x = 155, y = 17, label = paste("n = ",till.graph$nn[17],sep="")) +
  annotate("text", x = 155, y = 18, label = paste("n = ",till.graph$nn[18],sep="")) +
  annotate("text", x = 155, y = 19, label = paste("n = ",till.graph$nn[19],sep=""))
dev.off()


##Aridity

dat1$names[dat1$Trt == "till" ] <- "No-Till"
dat1$names[dat1$Trt == "res" ] <- "Cover Crop"
dat1$names[dat1$Trt == "int" ] <- "Crop and Livestock"
dat1$names[dat1$Trt == "per" ] <- "Perennial"
dat1$names[dat1$Trt == "cr" ] <- "Crop Rotation"


dat2 <- subset(dat1, Aridity < 3)

scale <- 10
hpp <- 120 * scale
wpp <- 180 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/suppmaterialfig2.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
ggplot(data = dat2, aes(y = Li, x = Aridity)) +
  facet_wrap(~names, ncol=3) +
  geom_point(shape=1) + 
  geom_smooth() +
  xlab("Aridity Index") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_vline(xintercept=0.65, lwd=0.6, lty ="dotted") +
  theme_bw() 
dev.off()

##revised for the updated report

##aridity
## no-till: 0.028
## cover crop: -0.009
## int:  0.430
## per: 0.011
## cr: 1.228

##NOT GOING TO USE THIS

scale <- 10
hpp <- 230 * scale
wpp <- 210 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig4_revised.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
ggplot2.multiplot(p1, p2, p3, p4, p5, cols=3)
dev.off()

till2 <- subset(till, Aridity < 3)
p1<- ggplot(data = till2, aes(y = Li, x = Aridity)) +
  geom_point(shape=1) + 
  xlab("Aridity Index") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_hline(yintercept=0.05546, lwd=0.25, col="red") +
  geom_vline(xintercept=0.65, lwd=0.6, lty ="dotted") +
  annotate("text", label = "Beta = 0.028", x = 1.2, y = -2.0, color = "red") +
  ggtitle("No-Till") +
  theme_bw() 

res2 <- subset(res, Aridity < 3)
p2 <- ggplot(data = res2, aes(y = Li, x = Aridity)) +
  geom_point(shape=1) + 
  xlab("Aridity Index") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_hline(yintercept=0.29913, lwd=0.25, col="red") +
  geom_vline(xintercept=0.65, lwd=0.6, lty ="dotted") +
  annotate("text", label = "Beta = -0.009", x = 1.0, y = -1.0, color = "red") +
  ggtitle("Cover Crop") +
  theme_bw() 

cr2 <- subset(cr, Aridity < 3)
p3 <- ggplot(data = cr2, aes(y = Li, x = Aridity)) +
  geom_point(shape=1) + 
  xlab("Aridity Index") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_hline(yintercept=0.1696, lwd=0.25, col="red") +
  geom_vline(xintercept=0.65, lwd=0.6, lty ="dotted") +
  annotate("text", label = "Beta = 1.228", x = 0.8, y = -1.5, color = "red") +
  ggtitle("Crop Rotation") +
  theme_bw() 

int2 <- subset(int, Aridity < 3)
p4 <- ggplot(data = int2, aes(y = Li, x = Aridity)) +
  geom_point(shape=1) + 
  xlab("Aridity Index") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_hline(yintercept=-0.2393, lwd=0.25, col="red") +
  geom_vline(xintercept=0.65, lwd=0.6, lty ="dotted") +
  annotate("text", label = "Beta = 0.430", x = 0.40, y = 0.75, color = "red") +
  ggtitle("Crop and Livestock") +
  theme_bw() 

per2 <- subset(per, Aridity < 3)
p5 <- ggplot(data = per2, aes(y = Li, x = Aridity)) +
  geom_point(shape=1) + 
  xlab("Aridity Index") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_hline(yintercept=0.4650, lwd=0.25, col="red") +
  geom_vline(xintercept=0.65, lwd=0.6, lty ="dotted") +
  annotate("text", label = "Beta = 0.011", x = 0.50, y = 1.1, color = "red") +
  ggtitle("Perennial") +
  theme_bw()
  

##SOIL TEXTURE PLOTS


scale <- 10
hpp <- 230 * scale
wpp <- 210 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/suppmaterialfig3.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
ggplot2.multiplot(p1, p2, cols=1)
dev.off()

p1 <- ggplot(data = dat1, aes(y = Li, x = X.Sand)) +
  facet_wrap(~names, ncol=3) +
  geom_point(shape=1) + 
  geom_smooth() +
  xlab("Percent Sand") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_vline(xintercept=50, lwd=0.6, lty ="dotted") +
  theme_bw() 

p2 <- ggplot(data = dat1, aes(y = Li, x = X.Clay)) +
  facet_wrap(~names, ncol=3) +
  geom_point(shape=1) +
  geom_smooth() +
  xlab("Percent Clay") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  geom_vline(xintercept=30, lwd=0.6, lty ="dotted") +
  theme_bw() 




## time

dat3 <- subset(dat1, Yrs <40)

scale <- 10
hpp <- 120 * scale
wpp <- 180 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/suppmaterialfig4.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
ggplot(data = dat3, aes(y = Li, x = Yrs)) +
  facet_wrap(~names, ncol=3) +
  geom_point(shape=1) + 
  geom_smooth() +
  xlab("Study Length (Years)") +
  ylab("Natural Log Response Ratio") + 
  geom_hline(yintercept=0, lwd=0.5) +
  theme_bw() 
dev.off()




##RES STUDIES


fit <- lmer(Li ~ 1 + (1|Study), data=res, weights = Wi)
summary(fit)

res$Precip2[res$Avg.precip < 651 ] <- '< 650mm'
res$Precip2[res$Avg.precip > 651 ] <- '> 650mm'
dry <-subset(res, Precip2 == '< 650mm')
wet <-subset(res, Precip2 == '> 650mm')

fit <- lmer(Li ~ Precip2 + (1|Study), data=res, weights = Wi)
summary(fit)

res$Length2[res$Yrs < 4] <- "1-3 Yrs"
res$Length2[res$Yrs > 3] <- "4+ Yrs"
short <- subset(res, Length2== "1-3 Yrs")
long <- subset(res, Length2== "4+ Yrs")

fit <- lmer(Li ~ Length2 + (1|Study), data=res, weights = Wi)
summary(fit)

length(unique(long$Study))

fit <- lmer(Li ~ Tillage + (1|Study), data=res,weights = Wi)
summary(fit)
##groups are small and ns but there is a difference where there is NT + CC
## could be worth investigating a little more

hisand <-subset(res, X.50Sand == 'Y')
losand <-subset(res, X.50Sand == 'N')
hiclay <-subset(res, X.30Clay == 'Y')
loclay <-subset(res, X.30Clay == 'N')

fit <- lmer(Li ~ X.30Clay + (1|Study), data=res,weights = Wi)
summary(fit)

length(unique(hiclay$Study))



res$Tillage2[res$Tillage == 'CT' ] <- 'ctcc'
res$Tillage2[res$Tillage == 'RT' ] <- 'ctcc'
res$Tillage2[res$Tillage == 'NT' ] <- 'ntcc'
ctcc <-subset(res, Tillage2 == 'ctcc')
ntcc <-subset(res, Tillage2 == 'ntcc')


fit <- lmer(Li ~ Tillage2 + (1|Study), data=res,weights = Wi)
summary(fit)

length(unique(ntcc$Study))



res.graph <- read.csv ("cover crop graph 3.csv", header=TRUE)


#till.graph.o <- till.graph[order(till.graph$nn),]
res.graph$nn <- as.character(res.graph$nn)
res.graph$title <- factor(res.graph$title, 
                           levels=unique(res.graph$title))


scale <- 10
hpp <- 200 * scale
wpp <- 175 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig7_draft.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
limits2 <- aes(xmax = rr + se*1.96, xmin = rr - se*1.96)
ggplot(data = res.graph , aes_string(x = "rr", y = "title", color="pfm")) +
  ylab("  ") + 
  xlab("Percent Change in Infiltration Rate with Cover Crops") +  
  ylab("Tillage                      Precip       Study Length                        Texture                  ") +
  geom_errorbarh(limits2, height=0.15) + 
  geom_vline(xintercept=0, lwd=0.5) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=5) + 
  annotate("text", x = 85, y = 1, label = paste("n = ",res.graph$nn[1],sep="")) +
  annotate("text", x = 85, y = 2, label = paste("n = ",res.graph$nn[2],sep="")) +
  annotate("text", x = 85, y = 3, label = paste("n = ",res.graph$nn[3],sep="")) +
  annotate("text", x = 85, y = 4, label = paste("n = ",res.graph$nn[4],sep="")) +
  annotate("text", x = 85, y = 5, label = paste("n = ",res.graph$nn[5],sep="")) +
  annotate("text", x = 85, y = 6, label = paste("n = ",res.graph$nn[6],sep="")) +
  annotate("text", x = 85, y = 7, label = paste("n = ",res.graph$nn[7],sep="")) +
  annotate("text", x = 85, y = 8, label = paste("n = ",res.graph$nn[8],sep="")) +
  annotate("text", x = 85, y = 9, label = paste("n = ",res.graph$nn[9],sep="")) +
  annotate("text", x = 85, y = 10, label = paste("n = ",res.graph$nn[10],sep="")) +
  annotate("text", x = 85, y = 11, label = paste("n = ",res.graph$nn[11],sep=""))
dev.off()




##CR

twocrop <-subset(cr, No..cr == '2')
threecrop <-subset(cr, No..cr == '3')
fourcrop <-subset(cr, No..cr == '4')

cr$Crop[cr$No..cr ==2 ] <- "Two Crops"
cr$Crop[cr$No..cr ==3 ] <- "Three Crops"
cr$Crop[cr$No..cr ==4 ] <- "Four Crops"


p1<-ggplot(data = cr, aes(x = Per, y = Crop, color=Crop)) + 
  ylab(" ") + 
  xlab("Percent Change in Infiltration Rate with Crop Rotation") + 
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=3.5) +
  annotate("text", x = 425, y = 3, label = "n=22") +
  annotate("text", x = 425, y = 2, label = "n=14") +
  annotate("text", x = 425, y = 1, label = "n=2") 

length(unique(long$Study))


cr$mc[cr$Study ==1 ] <- "Maize"
# cr$mc[cr$Study ==2 ] <- "Maize" ##note this is the avg of wheat/maize so I will leave it out of this fig
cr$mc[cr$Study ==3 ] <- "Maize"
cr$mc[cr$Study ==4 ] <- "Maize"
cr$mc[cr$Study ==5 ] <- "Wheat"
cr$mc[cr$Study ==10 ] <- "Sorghum"
cr$mc[cr$Study ==11 ] <- "Maize"
cr$mc[cr$Study ==13 ] <- "Maize"
cr$mc[cr$StudyID =='14a' ] <- "Maize"
cr$mc[cr$StudyID =='14c' ] <- "Maize"
cr$mc[cr$StudyID =='13b' ] <- "Maize"
cr$mc[cr$StudyID =='14d' ] <- "Wheat"
cr$mc[cr$Study ==76 ] <- "Sorghum"
cr$mc[cr$Study ==86 ] <- "Wheat"


p2<- ggplot(data = cr, aes(x = Per, y = mc, color=mc)) + 
  ylab(" ") + 
  xlab("Percent Change in Infiltration Rate with Crop Rotation") + 
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=3.5) +
  annotate("text", x = 425, y = 3, label = "n=11") +
  annotate("text", x = 425, y = 2, label = "n=5") +
  annotate("text", x = 425, y = 1, label = "n=19") 


scale <- 10
hpp <- 230 * scale
wpp <- 210 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig8_draft.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
ggplot2.multiplot(p1, p2, cols=1)
dev.off()



##perennials

per <- subset (dat1, Trt=='per')

## for agroforestry center visit
per.o <- per[order(per$Li),]
per.o$StudyA[per.o$Study ==6 ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69a" ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69b" ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69c" ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69d" ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69e" ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69f" ] <- "Agroforestry"
per.o$StudyA[per.o$StudyID =="69g" ] <- "Forestry"
per.o$StudyA[per.o$StudyID =="69h" ] <- "Forestry"
per.o$StudyA[per.o$StudyID =="69i" ] <- "Forestry"
per.o$StudyA[per.o$StudyID =="69j" ] <- "Forestry"
per.o$StudyA[per.o$StudyID =="69k" ] <- "Forestry"
per.o$StudyA[per.o$StudyID =="69l" ] <- "Forestry"
per.o$StudyA[per.o$Study ==70 ] <- "Agroforestry"
per.o$StudyA[per.o$Study ==44 ] <- "Perennial Grass"
per.o$StudyA[per.o$StudyID =='44a' ] <- "Agroforestry"
per.o$StudyA[per.o$Study ==46 ] <- "Forestry"
per.o$StudyA[per.o$Study ==54 ] <- "Perennial Grass"
per.o$StudyA[per.o$Study ==64 ] <- "Perennial Grass"
per.o$StudyA[per.o$Study ==72 ] <- "Perennial Grass"


xyplot(1:nrow(per.o) ~ Per , data = per.o, 
       pch=20,
       cex=1.3, groups=StudyA, auto.key=TRUE,
       xlab= list('Percent Change in Infiltration Rate', cex=1.3),
       ylab= list('Observation Rank', cex=1.3),
       #     scales = list(y = list(tck=0, alternating=0),
       #      x= list(tck=0), cex=1.7),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)

scale <- 10
hpp <- 120 * scale
wpp <- 185 * scale
#tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig9_draft.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
tiff("~/Dropbox/UCS documents/Kendall Fellow project/Soil water lit review project/Data analysis/fig6_revised.tiff", height= hpp, width=wpp, res=300, compression="lzw", type='cairo') 
ggplot(data = per.o, aes(x = Per, y = StudyA, color=StudyA)) + 
  ylab(" ") + 
  xlab("Percent Change in Infiltration Rate with Perennials") +
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=3.5) +
  annotate("text", x = 500, y = 3, label = "n=15") +
  annotate("text", x = 500, y = 2, label = "n=8") +
  annotate("text", x = 500, y = 1, label = "n=17") 
dev.off()


##crop and livestock

int$PastureA[int$Pasture == "Yes" ] <- "Pasture"
int$PastureA[int$Pasture == "No" ] <- "Crop"
int$PastureA[int$Study == "47" ] <- "Crop"

scale <- 10
hpp <- 120 * scale
wpp <- 185 * scale
#tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/fig10_draft.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
tiff("~/Dropbox/UCS documents/Kendall Fellow project/Soil water lit review project/Data analysis/fig7_revised.tiff", height= hpp, width=wpp, res=300, compression="lzw", type='cairo') 
ggplot(data = int, aes(x = Per, y = PastureA, color=PastureA)) + 
  ylab(" ") + 
  xlab("Percent Change in Infiltration Rate with Crops and Livestock") +
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="none") + 
  geom_point(size=3.5) +
  annotate("text", x = 300, y = 2, label = "n=9") +
  annotate("text", x = 300, y = 1, label = "n=15") 
dev.off()


##looking at publication bias

dat1$TrtA[dat1$Trt =="till" ] <- "No-till"
dat1$TrtA[dat1$Trt =="res" ] <- "Cover Crop"
dat1$TrtA[dat1$Trt =="per" ] <- "Perennial"
dat1$TrtA[dat1$Trt =="cr" ] <- "Crop Rotation"
dat1$TrtA[dat1$Trt =="int" ] <- "Crop and Livestock"

scale <- 10
hpp <- 140 * scale
wpp <- 220 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/pubbias_analysis.tiff",height= hpp, width=wpp, res=300, compression = "lzw") 
histogram( ~ Li | TrtA,  data=dat1, xlab= list('Natural Log Response Ratio  
Infiltration Rate'), ylab="Percent of Response Ratios",
     main=" ", col="gray", type="percent")
dev.off()




##sensitivity analysis 

fit <- lmer(Li ~ 1 + (1|Study), data= till.89, weights = Wi)
summary(fit)

##overall mean = 0.05546, se = 0.09252 


##REMOVING ONE STUDY AT A TIME
till.2 <- till[!till$Study ==2, ]
till.3 <- till[!till$Study ==3, ]
till.4 <- till[!till$Study ==4, ]
till.8 <- till[!till$Study ==8, ]
till.9 <- till[!till$Study ==9, ]
till.10 <- till[!till$Study ==10, ]
till.12 <- till[!till$Study ==12, ]
till.13 <- till[!till$Study ==13, ]
till.14 <- till[!till$Study ==14, ]
till.15 <- till[!till$Study ==15, ]
till.16 <- till[!till$Study ==16, ]
till.17 <- till[!till$Study ==17, ]
till.26 <- till[!till$Study ==26, ]
till.30 <- till[!till$Study ==30, ]
till.33 <- till[!till$Study ==33, ]
till.34 <- till[!till$Study ==34, ]
till.35 <- till[!till$Study ==35, ]
till.36 <- till[!till$Study ==36, ]
till.37 <- till[!till$Study ==37, ]
till.38 <- till[!till$Study ==38, ]
till.39 <- till[!till$Study ==39, ]
till.40 <- till[!till$Study ==40, ]
till.41 <- till[!till$Study ==41, ]
till.42 <- till[!till$Study ==42, ]
till.43 <- till[!till$Study ==43, ]
till.45 <- till[!till$Study ==45, ]
till.48 <- till[!till$Study ==48, ]
till.49 <- till[!till$Study ==49, ]
till.50 <- till[!till$Study ==50, ]
till.51 <- till[!till$Study ==51, ]
till.56 <- till[!till$Study ==56, ]
till.57 <- till[!till$Study ==57, ]
till.58 <- till[!till$Study ==58, ]
till.59 <- till[!till$Study ==59, ]
till.60 <- till[!till$Study ==60, ]
till.61 <- till[!till$Study ==61, ]
till.62 <- till[!till$Study ==62, ]
till.63 <- till[!till$Study ==63, ]
till.64 <- till[!till$Study ==64, ]
till.65 <- till[!till$Study ==65, ]
till.66 <- till[!till$Study ==66, ]
till.67 <- till[!till$Study ==67, ]
till.78 <- till[!till$Study ==78, ]
till.79 <- till[!till$Study ==79, ]
till.80 <- till[!till$Study ==80, ]
till.81 <- till[!till$Study ==81, ]
till.82 <- till[!till$Study ==82, ]
till.83 <- till[!till$Study ==83, ]
till.84 <- till[!till$Study ==84, ]
till.85 <- till[!till$Study ==85, ]
till.87 <- till[!till$Study ==87, ]
till.89 <- till[!till$Study ==89, ]

##crop rotation

fit <- lmer(Li ~ 1 + (1|Study), data= cr.86, weights = Wi)
summary(fit)

##overall mean 0.1696  ##stderr  0.1243

cr.1 <- cr[!cr$Study ==1, ]
cr.2 <- cr[!cr$Study ==2, ]
cr.3 <- cr[!cr$Study ==3, ]
cr.4 <- cr[!cr$Study ==4, ]
cr.5 <- cr[!cr$Study ==5, ]
cr.10 <- cr[!cr$Study ==10, ]
cr.11 <- cr[!cr$Study ==11, ]
cr.13 <- cr[!cr$Study ==13, ]
cr.14 <- cr[!cr$Study ==14, ]
cr.76 <- cr[!cr$Study ==76, ]
cr.86 <- cr[!cr$Study ==86, ]


##crop and livestock

fit <- lmer(Li ~ 1 + (1|Study), data= int.73, weights = Wi)
summary(fit)

##overall mean -0.2393 stderr 0.1388
int.47 <- int[!int$Study ==47, ]
int.52 <- int[!int$Study ==52, ]
int.53 <- int[!int$Study ==53, ]
int.54 <- int[!int$Study ==54, ]
int.55 <- int[!int$Study ==55, ]
int.71 <- int[!int$Study ==71, ]
int.73 <- int[!int$Study ==73, ]

##perennial

fit <- lmer(Li ~ 1 + (1|Study), data= per.72, weights = Wi)
summary(fit)

##overall mean 0.4650 stderr 0.1899
per.6 <- per[!per$Study ==6, ]
per.44 <- per[!per$Study ==44, ]
per.46 <- per[!per$Study ==46, ]
per.54 <- per[!per$Study ==54, ]
per.64 <- per[!per$Study ==64, ]
per.69 <- per[!per$Study ==69, ]
per.70 <- per[!per$Study ==70, ]
per.72 <- per[!per$Study ==72, ]

##cover crop
fit <- lmer(Li ~ 1 + (1|Study), data= res.88, weights = Wi)
summary(fit)

##overall mean  0.29913  stderr 0.07419
res.7 <- res[!res$Study ==7, ]
res.17 <- res[!res$Study ==17, ]
res.18 <- res[!res$Study ==18, ]
res.19 <- res[!res$Study ==19, ]
res.20 <- res[!res$Study ==20, ]
res.21 <- res[!res$Study ==21, ]
res.22 <- res[!res$Study ==22, ]
res.23 <- res[!res$Study ==23, ]
res.24 <- res[!res$Study ==24, ]
res.25 <- res[!res$Study ==25, ]
res.27 <- res[!res$Study ==27, ]
res.28 <- res[!res$Study ==28, ]
res.29 <- res[!res$Study ==29, ]
res.30 <- res[!res$Study ==30, ]
res.31 <- res[!res$Study ==31, ]
res.32 <- res[!res$Study ==32, ]
res.33 <- res[!res$Study ==33, ]
res.68 <- res[!res$Study ==68, ]
res.74 <- res[!res$Study ==74, ]
res.75 <- res[!res$Study ==75, ]
res.77 <- res[!res$Study ==77, ]
res.87 <- res[!res$Study ==87, ]
res.88 <- res[!res$Study ==88, ]
##calculations above convereted into the below csv file

sens.ir <- read.csv ("sens_IR.csv", na.strings = ".")

till.sens <- subset(sens.ir, trt== "No-Till")
cr.sens <- subset(sens.ir, trt == "Crop Rotation")
int.sens <-subset(sens.ir, trt == "Crop and Livestock")
per.sens <-subset(sens.ir, trt == "Perennial")
res.sens <-subset(sens.ir, trt == "Cover Crops")
limits2 <- aes(xmax = lrr + stderr * 1.96, xmin = lrr - stderr * 1.96)

scale <- 10
hpp <- 140 * scale
wpp <- 220 * scale
tiff("P:/CURRENT & RECENT FY PROJECTS/FY16 Projects/Kendall Fellow project/Soil water lit review project/Data analysis/sens_analysis.tiff",height= hpp, width=wpp, res=300, compression = "lzw")
ggplot2.multiplot(p1, p2, p3, p4, p5, cols=3)
dev.off()

p1 <- ggplot(data = res.sens, aes_string(x = "lrr", y = "obs")) +
  ylab("Study No. Removed") + 
  xlab("LRR") +  
  xlim(-0.02, 0.5) + 
  geom_errorbarh(limits2, height=0.15) + 
  geom_vline(xintercept= 0, lwd=0.5, col="blue") + 
  geom_vline(xintercept= 0.29913 , lwd=0.5) + 
  geom_vline(xintercept= 0.1537176, lty="dotted") + 
  geom_vline(xintercept= 0.4445424, lty="dotted") + 
  theme_bw() +
  theme(legend.position="none") + 
  ggtitle("Cover Crops") +
  theme(text = element_text(size = 10)) +
  geom_point(size=1.5)

p2 <- ggplot(data = per.sens, aes_string(x = "lrr", y = "obs")) +
  ylab("Study No. Removed") + 
  xlab("LRR") +  
  xlim(-0.1, 1.0) + 
  geom_errorbarh(limits2, height=0.15) +
  geom_vline(xintercept= 0, lwd=0.5,col="blue") + 
  geom_vline(xintercept= 0.4650, lwd=0.5) + 
  geom_vline(xintercept= 0.092796, lty="dotted") + 
  geom_vline(xintercept=0.837204, lty="dotted") + 
  theme_bw() +
  theme(legend.position="none") + 
  ggtitle("Perennials") +
  theme(text = element_text(size = 10)) +
  geom_point(size=1.5) 
##removing 2 & 70 would make these NS- interesting bc they are the livestock experiments

p3 <- ggplot(data = int.sens, aes_string(x = "lrr", y = "obs")) +
  ylab("Study No. Removed") + 
  xlab("LRR") +  
  #  xlim(0, 0.15) + 
  geom_errorbarh(limits2, height=0.15) + 
  geom_vline(xintercept= 0, lwd=0.5, col="blue") + 
  geom_vline(xintercept= -0.2393, lwd=0.5) + 
  geom_vline(xintercept= 0.032748, lty="dotted") + 
  geom_vline(xintercept=-0.511348, lty="dotted") + 
  theme_bw() +
  theme(legend.position="none") + 
  ggtitle("Crop and Livestock") +
  theme(text = element_text(size = 10)) +
  geom_point(size=1.5)

##removing the studies did not change the NS effect (study 3 was close!)

p4 <- ggplot(data = cr.sens, aes_string(x = "lrr", y = "obs")) +
  ylab("Study No. Removed") + 
  xlab("LRR") +  
#  xlim(0, 0.15) + 
  geom_errorbarh(limits2, height=0.15) +
  geom_vline(xintercept= 0, lwd=0.5, col="blue") + 
  geom_vline(xintercept= 0.1696, lwd=0.5) + 
  geom_vline(xintercept= -0.074028, lty="dotted") + 
  geom_vline(xintercept=0.413228, lty="dotted") + 
  theme_bw() +
  theme(legend.position="none") + 
  ggtitle("Crop Rotation") +
  theme(text = element_text(size = 10)) +
  geom_point(size=1.5)
##removing study #5 would make this a sig diff from zero

p5 <- ggplot(data = till.sens, aes_string(x = "lrr", y = "obs")) +
  ylab("Study No. Removed") + 
  xlab("LRR") +  
  #  xlim(0, 0.15) + 
  geom_errorbarh(limits2, height=0.15) + 
  geom_vline(xintercept= 0, lwd=0.5, col="blue") + 
  geom_vline(xintercept=0.05546, lwd=0.5) + 
  geom_vline(xintercept=-0.1258792, lty="dotted") + 
  geom_vline(xintercept=0.2367992, lty="dotted") + 
  theme_bw() +
  theme(legend.position="none") + 
  ggtitle("No-Till") +
  theme(text = element_text(size = 10)) +
  geom_point(size=1.5)
