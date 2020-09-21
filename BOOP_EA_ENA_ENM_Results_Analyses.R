###Anthony E. Melton
###PhD candidate, UFL
###2018
###This script was written to help summarize and visualize outputs from ENM analyses.
# Messy right now.... 

library(wesanderson)
#Load the libraries!
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(cowplot)
library(data.table)

#First is the estimated distribution sizes and raster.breadth outputs, then the EcoSpat output
#model.dat <- read.csv("~/Dropbox/Manuscripts/EA_ENA_ENM/models_stats.csv")
dist.dat <- read.csv("file with results for enm stats")
ovrlp.dat <- read.csv("background test results")

dist.dat.trim <- read.csv("same as above but with just genus name instead of species....")
ovrlp.dat.trim <- read.csv("same as above but with just genus name instead of species....")

# Separate data by region
dist.dat.ea <- dist.dat[dist.dat[, "Region"] == "EA",]
dist.dat.ena <- dist.dat[dist.dat[, "Region"] == "ENA",]

ddply(dist.dat.ena, ~Region, summarise, mean=mean(HyperVolume), median=median(HyperVolume), sd=sd(HyperVolume))
ddply(b2.dat.sort, ~Region, summarise, mean=mean(B2), median=median(B2), sd=sd(B2))

#Ordered the data
dist.dat.sort <- dist.dat[order(-dist.dat$Est..Dist.),]
dist.dat.sort
head(dist.dat.sort)
tail(dist.dat.sort)

dist.wilc <- wilcox.test(dist.dat.sort$Est..Dist. ~ dist.dat.sort$Region)

#
b1.dat.sort <- dist.dat[order(-dist.dat$B1), ]
b1.dat.sort
bone.wilc <- wilcox.test(b1.dat.sort$B1 ~ b1.dat.sort$Region)

#
b2.dat.sort <- dist.dat[order(-dist.dat$B2), ]
b2.dat.sort
btwo.wilc <- wilcox.test(b2.dat.sort$B2 ~ b2.dat.sort$Region)


#
vol.dat.sort <- dist.dat[order(-dist.dat$HyperVolume), ]
vol.dat.sort
hv.wilc <- wilcox.test(vol.dat.sort$HyperVolume ~ vol.dat.sort$Region)

#Let's do a multiple correstions for p-values....
d.dat.sort <- ovrlp.dat[order(ovrlp.dat$p.D),]
i.dat.sort <- ovrlp.dat[order(ovrlp.dat$p.I),]

d.dat.sort$Holm <- p.adjust(d.dat.sort$p.D, method = "holm")
i.dat.sort$Holm <- p.adjust(i.dat.sort$p.I, method = "holm")
d.dat.sort
i.dat.sort

# Niche Widths
tw.sort <- dist.dat[order(-dist.dat$Tw),]
tw.sort
head(tw.sort)
tail(tw.sort)

#
tw.wilc <- wilcox.test(dist.dat$Tw ~ dist.dat$Region)

#
pw.sort <- dist.dat[order(-dist.dat$Pw),]
pw.sort
head(pw.sort)
tail(pw.sort)

#
pw.wilc <- wilcox.test(dist.dat.sort$Pw ~ dist.dat.sort$Region)

#
aw.sort <- dist.dat[order(-dist.dat$Aw),]
aw.sort
head(aw.sort)
tail(aw.sort)

#
aw.wilc <- wilcox.test(dist.dat.sort$Aw ~ dist.dat.sort$Region)

#
w.scores <- rbind(dist.wilc$statistic, 
                  bone.wilc$statistic, 
                  btwo.wilc$statistic, 
                  hv.wilc$statistic, 
                  tw.wilc$statistic, 
                  pw.wilc$statistic, 
                  aw.wilc$statistic)

p.vals <- rbind(dist.wilc$p.value, bone.wilc$p.value, btwo.wilc$p.value, hv.wilc$p.value, 
tw.wilc$p.value, pw.wilc$p.value, aw.wilc$p.value)

wilc.out <- cbind(w.scores, p.vals)
rownames(wilc.out) <- c("Distribution", "B1", "B2", "Hypervolume", "Temperature", "Precipitation", "Altitude")
wilc.out

write.csv(x = wilc.out, file = "Wilcoxon_Tests.csv")

### 
#Checking for correlation between est. dist. size and B1 and B2.
r1 <- lm(dist.dat$B1 ~ dist.dat$Est..Dist.)
summary(r1)
plot(x = dist.dat$Est..Dist., y = dist.dat$B1)
abline(r1)

r2 <- lm(dist.dat$B2 ~ dist.dat$Est..Dist.)
summary(r2)
plot(x = dist.dat$Est..Dist., y = dist.dat$B2)
abline(r2)

r3 <- lm(dist.dat$B2 ~ dist.dat$B1)
summary(r3)
plot(x = dist.dat$B1, y = dist.dat$B2)
abline(r3)

### Plots for Melton et al 2020
### Plots for dist sizes
pdf("Dist_Violin.pdf") #, width = 8.5, height = 8.5, units = 'in', res = 300)
p.a <- ggplot(dist.dat, aes(x = Region, y = Est..Dist...km.2., fill=Region)) +
  geom_violin() +
  ylab("Estimated Distribution (km^2)") +
  xlab("Region") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")
plot(p.a)
dev.off()


### Niche "Breadth" Plots
B1 <- ggplot(dist.dat, aes(x = Region, y = B1, fill=Region)) +
  geom_violin() +
  ylab("B1") +
  xlab("Region") +
  #ggtitle("B1") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")

B2 <- ggplot(dist.dat, aes(x = Region, y = B2, fill=Region)) +
  geom_violin() +
  ylab("B2") +
  xlab("Region") +
  #ggtitle("B2") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")

b_grid <- plot_grid(B1, B2, ncol = 2)

pdf("Breadth_violin.pdf") #, width = 8.5, height = 8.5, units = 'in') #, res = 300)
ggdraw(b_grid)
dev.off()

### Overlap Plots

pdf("OvrLap_Violin.pdf") #, width = 8.5, height = 8.5, units = 'in', res = 300)
d.plot <- ggplot(ovrlp.dat, aes(x = comparison, y = emp.D, fill=comparison)) +
  geom_violin() +
  ylab("Schoener's D") +
  xlab("Regional Comparison") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")
ggdraw(d.plot)
dev.off()

#i.plot <- ggplot(ovrlp.dat, aes(x = comparison, y = emp.I, fill=comparison)) +
#  geom_violin() +
#  ylab("I") +
#  xlab("Regions") +
#  #scale_fill_grey(start=0.75, end=0.5) + 
#  theme_classic()

#ovrlp_grid <- plot_grid(d.plot, i.plot, ncol = 2, labels = "AUTO")
#tiff("OvrLap_Violin.tiff", width = 8.5, height = 8.5, units = 'in', res = 300)
#ggdraw(d.plot)
#dev.off()

### niche space plot
pdf("HypVolPlot_Violin.pdf") #, width = 8.5, height = 8.5, units = 'in', res = 300)
hypVol <- ggplot(dist.dat, aes(x = Region, y = HyperVolume, fill=Region)) +
  geom_violin() +
  ylab("Hypervolume") +
  xlab("Region") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")
plot(hypVol)
dev.off()

### Niche "Width" Plots
Tw <- ggplot(dist.dat, aes(x = Region, y = Tw, fill=Region)) +
  geom_violin() +
  ylab("Tw") +
  xlab("Region") +
  #ggtitle("B1") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")

Pw <- ggplot(dist.dat, aes(x = Region, y = Pw, fill=Region)) +
  geom_violin() +
  ylab("Pw") +
  xlab("Region") +
  #ggtitle("B2") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 19, size = 2, color = "black") +
  theme(legend.position="none")


width_grid <- plot_grid(Tw, Pw, ncol = 2)

pdf("Width_violin.pdf") #, width = 8.5, height = 8.5, units = 'in', res = 300)
ggdraw(width_grid)
dev.off()


###
all_plot_grid <- plot_grid(p.a, b_grid, hypVol, width_grid, ncol = 2, labels = "AUTO")
tiff("Grid_Violin.tiff", width = 8.5, height = 8.5, units = 'in', res = 300)
ggdraw(all_plot_grid)
dev.off()

###

r.ea <- lm(dist.dat.ea$B2 ~ dist.dat.ea$B1)
summary(r.ea)
plot(x = dist.dat.ea$B1, y = dist.dat.ea$B2)
abline(r.ea)

ggplot(dist.dat.ea,aes(B1,B2)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_point(color='black') +
  geom_smooth(method='lm')

r.ena <- lm(dist.dat.ena$B2 ~ dist.dat.ena$B1)
summary(r.ena)
plot(x = dist.dat.ena$B1, y = dist.dat.ena$B2)
abline(r.ena)

ggplot(dist.dat.ena,aes(B1,B2)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_point(color='black') +
  geom_smooth(method='lm')


##### Breaking down significance patterns for BG tests
### Who and how many are statistically significant?
sig.p.d <- ovrlp.dat[ovrlp.dat$p.D<=0.05, ]
sig.p.i <- ovrlp.dat[ovrlp.dat$p.I<=0.05, ]
sig.p.all <- ovrlp.dat[ovrlp.dat$p.D<=0.05 & ovrlp.dat$p.I<=0.05, ]

sig.p.d
sig.p.i
sig.p.all

sig.d.counts <- table(unlist(sig.p.d$comparison))
sig.i.counts <- table(unlist(sig.p.i$comparison))
sig.all.counts <- table(unlist(sig.p.all$comparison))

sig.d.counts
sig.i.counts
sig.all.counts

dist <- ddply(sig.p.d, ~comparison, summarise, mean=mean(emp.D), median.Dist=median(emp.D), sd.Dist=sd(emp.D))
rownames(dist) <- dist$Genus

#Let's make a table with those counts
sig.table <- rbind(sig.d.counts, sig.i.counts, sig.all.counts)
rownames(sig.table) <- c('D', 'I', 'Both')
sig.table
write.csv(sig.table, file = "Sig_diff_counts.csv")

### Who and how many are NOT statistically significant?
not.sig.p.d <- ovrlp.dat[ovrlp.dat$p.D>0.05, ]
not.sig.p.i <- ovrlp.dat[ovrlp.dat$p.I>0.05, ]
not.sig.p.all <- ovrlp.dat[ovrlp.dat$p.D>0.05 & ovrlp.dat$p.I>0.05, ]

not.sig.p.d
not.sig.p.i
not.sig.p.all

not.sig.d.counts <- table(unlist(not.sig.p.d$comparison))
not.sig.i.counts <- table(unlist(not.sig.p.i$comparison))
not.sig.all.counts <- table(unlist(not.sig.p.all$comparison))

not.sig.d.counts
not.sig.i.counts
not.sig.all.counts

sig.d.counts
not.sig.d.counts
#Let's make another table!
not.sig.table <- rbind(not.sig.d.counts, not.sig.i.counts, not.sig.all.counts)
rownames(not.sig.table) <- c('D', 'I', 'Both')
not.sig.table
write.csv(not.sig.table, file = "Not_Sig_diff_counts.csv")

##########################################################################################
############################## Climatic Niche Widths ##############################
# Temp
temp.dat.sort <- dist.dat[order(-dist.dat$Tw),]
temp.dat.sort
head(temp.dat.sort)
tail(temp.dat.sort)

wilcox.test(temp.dat.sort$Tw ~ temp.dat.sort$Region)


# Precipitation
precip.dat.sort <- dist.dat[order(-dist.dat$Pw),]
precip.dat.sort
head(precip.dat.sort)
tail(precip.dat.sort)

wilcox.test(precip.dat.sort$Pw ~ precip.dat.sort$Region)

### Niche Width Plots
T.plot <- ggplot(data = dist.dat, aes(x = Region, y = Tw, fill = Region)) +
  geom_violin() +
  ylab("Temperature (°C*)") +
  #xlab("Regions") +
  scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(legend.position="none")

P.plot <- ggplot(dist.dat, aes(x = Region, y = Pw, fill = Region)) +
  geom_violin() +
  ylab("Precipitation (mm)") +
  xlab("Region") +
  scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(legend.position="none")


width_grid <- plot_grid(T.plot, P.plot, ncol = 2, labels = "AUTO")

tiff("Width_Grid_Violin.tiff", res = 300, width = 8.5, height = 8.5, units = 'in')
ggdraw(width_grid)
dev.off()

#Get some descriptive stats for the niche widths
dist.dat.ea
dist.dat.ena

mean(dist.dat.ea$Tw)
mean(dist.dat.ea$Pw)


sd(dist.dat.ea$Tw)
sd(dist.dat.ea$Pw)


mean(dist.dat.ena$Tw)
mean(dist.dat.ena$Pw)


sd(dist.dat.ena$Tw)
sd(dist.dat.ena$Pw)


min(dist.dat.ea$Tw)
min(dist.dat.ea$Pw)


max(dist.dat.ea$Tw)
max(dist.dat.ea$Pw)


### Summarize stats by region
dist.dat.ea <- dist.dat.trim[dist.dat.trim[, "Region"] == "EA",]
dist.dat.ena <- dist.dat.trim[dist.dat.trim[, "Region"] == "ENA",]
ovrlp.dat.ea <- ovrlp.dat[ovrlp.dat[, "comparison"] == "ea-ea",]
ovrlp.dat.ena <- ovrlp.dat[ovrlp.dat[, "comparison"] == "ena-ena",]
ovrlp.dat.ea.ena <- ovrlp.dat[ovrlp.dat[, "comparison"] == "ea-ena",]
ovrlp.dat.ena.ea <- ovrlp.dat[ovrlp.dat[, "comparison"] == "ena-ea",]

# Dist
ea.d <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(Est..Dist...km.2.), median=median(Est..Dist...km.2.), sd=sd(Est..Dist...km.2.))
ena.d <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(Est..Dist...km.2.), median=median(Est..Dist...km.2.), sd=sd(Est..Dist...km.2.))

# B1
ea.b1 <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(B1), median=median(B1), sd=sd(B1))
ena.b1 <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(B1), median=median(B1), sd=sd(B1))

# B2
ea.b2 <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(B2), median=median(B2), sd=sd(B2))
ena.b2 <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(B2), median=median(B2), sd=sd(B2))

# hypervolume
ea.hv <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(HyperVolume), median=median(HyperVolume), sd=sd(HyperVolume))
ena.hv <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(HyperVolume), median=median(HyperVolume), sd=sd(HyperVolume))

#Width
ea.tw <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(Tw), median=median(Tw), sd=sd(Tw))
ea.pw <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(Pw), median=median(Pw), sd=sd(Pw))
ea.aw <- ddply(dist.dat.ea, ~Genus, summarise, mean=mean(Aw), median=median(Aw), sd=sd(Aw))

ena.tw <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(Tw), median=median(Tw), sd=sd(Tw))
ena.pw <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(Pw), median=median(Pw), sd=sd(Pw))
ena.aw <- ddply(dist.dat.ena, ~Genus, summarise, mean=mean(Aw), median=median(Aw), sd=sd(Aw))

## Overlap
D.ea <- ddply(ovrlp.dat.ea, ~comparison, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))
D.ena <- ddply(ovrlp.dat.ena, ~comparison, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))
D.ea.ena <- ddply(ovrlp.dat.ea.ena, ~comparison, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))
D.ena.ea <- ddply(ovrlp.dat.ena.ea, ~comparison, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))

D.df <- rbind(D.ea, D.ena, D.ea.ena, D.ena.ea)   
write.csv(x = D.df, file = "Overlap_By_Region.csv")
   
a <- ddply(ovrlp.dat.ea, ~Genus, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))
b <- ddply(ovrlp.dat.ena, ~Genus, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))
c <- ddply(ovrlp.dat.ea.ena, ~Genus, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))
d <- ddply(ovrlp.dat.ena.ea, ~Genus, summarise, mean=mean(emp.D), median=median(emp.D), sd=sd(emp.D))

write.csv(x = boop, file = "EcoSpat_by_genus_and_region.csv", row.names = T)

## By Genus
by.genus <- ddply(ovrlp.dat.trim, ~Genus, summarise, mean=mean(Schoener.sD), median=median(Schoener.sD), sd=sd(Schoener.sD))
write.csv(x = by.genus, file = "TABLE_by_genus.csv", row.names = T)

#
dist <- ddply(dist.dat.trim, ~Genus, summarise, mean.Dist=mean(Est..Dist...km.2.), median.Dist=median(Est..Dist...km.2.), sd.Dist=sd(Est..Dist...km.2.))
rownames(dist) <- dist$Genus

Ba <- ddply(dist.dat.trim, ~Genus, summarise, mean.B1=mean(B1), median.B1=median(B1), sd.B1=sd(B1))
rownames(Ba) <- Ba$Genus

Bb <- ddply(dist.dat.trim, ~Genus, summarise, mean.B2=mean(B2), median.B2=median(B2), sd.B2=sd(B2))
rownames(Bb) <- Bb$Genus

HV <- ddply(dist.dat.trim, ~Genus, summarise, mean.HV=mean(HyperVolume), median.HV=median(HyperVolume), sd.HV=sd(HyperVolume))
rownames(HV) <- HV$Genus

Tw <- ddply(dist.dat.trim, ~Genus, summarise, mean.Tw=mean(Tw), median.Tw=median(Tw), sd.Tw=sd(Tw))
rownames(Tw) <- Tw$Genus

Pw <- ddply(dist.dat.trim, ~Genus, summarise, mean.Pw=mean(Pw), median.Pw=median(Pw), sd.Pw=sd(Pw))
rownames(Pw) <- Pw$Genus

list <- list(ena.d, ena.b1, ena.b2, ena.hv, ena.tw, ena.pw)
by.genus.table <- join_all(dfs = list, by = "Genus")
write.csv(by.genus.table, "results_by_genus_and_region_ENA.csv")

# By comparison
by.comp <- ddply(ovrlp.dat.trim, ~comparison, summarise, mean=mean(Schoener.sD), median=median(Schoener.sD), sd=sd(Schoener.sD))
write.csv(x = by.comp, file = "TABLE_by_Comp.csv", row.names = T)

# By region/comparison within genus
dist.dat.trim.ea <- dist.dat.trim[dist.dat.trim[, "Region"] == "EA",]
dist.dat.trim.ena <- dist.dat.trim[dist.dat.trim[, "Region"] == "ENA",]

# Lots of commands again for per genus stuff because I haven't learned how to do loops well enough....
# Aesculus
aesculus.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Aesculus",]
aesculus.df <- ddply(aesculus.dat, ~Region, 
                       summarise, mean.Dist=mean(Est..Dist...km.2.), 
                       median.Dist=median(Est..Dist...km.2.), 
                       sd.Dist=sd(Est..Dist...km.2.),
                       mean.B1=mean(B1), 
                       median.B1=median(B1), 
                       sd.B1=sd(B1),
                       mean.B2=mean(B2), 
                       median.B2=median(B2), 
                       sd.B2=sd(B2),
                       mean.HV=mean(HyperVolume), 
                       median.HV=median(HyperVolume), 
                       sd.HV=sd(HyperVolume),
                       mean.Tw=mean(Tw), 
                       median.Tw=median(Tw), 
                       sd.Tw=sd(Tw),
                       mean.Pw=mean(Pw), 
                       median.Pw=median(Pw), 
                       sd.Pw=sd(Pw),
                       mean.Aw=mean(Aw), 
                       median.Aw=median(Aw), 
                       sd.Aw=sd(Aw))

Castanea.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Castanea",]
Castanea.df <- ddply(Castanea.dat, ~Region, 
                     summarise, mean.Dist=mean(Est..Dist...km.2.), 
                     median.Dist=median(Est..Dist...km.2.), 
                     sd.Dist=sd(Est..Dist...km.2.),
                     mean.B1=mean(B1), 
                     median.B1=median(B1), 
                     sd.B1=sd(B1),
                     mean.B2=mean(B2), 
                     median.B2=median(B2), 
                     sd.B2=sd(B2),
                     mean.HV=mean(HyperVolume), 
                     median.HV=median(HyperVolume), 
                     sd.HV=sd(HyperVolume),
                     mean.Tw=mean(Tw), 
                     median.Tw=median(Tw), 
                     sd.Tw=sd(Tw),
                     mean.Pw=mean(Pw), 
                     median.Pw=median(Pw), 
                     sd.Pw=sd(Pw),
                     mean.Aw=mean(Aw), 
                     median.Aw=median(Aw), 
                     sd.Aw=sd(Aw))

Catalpa.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Catalpa",]
Catalpa.df <- ddply(Catalpa.dat, ~Region, 
                     summarise, mean.Dist=mean(Est..Dist...km.2.), 
                     median.Dist=median(Est..Dist...km.2.), 
                     sd.Dist=sd(Est..Dist...km.2.),
                     mean.B1=mean(B1), 
                     median.B1=median(B1), 
                     sd.B1=sd(B1),
                     mean.B2=mean(B2), 
                     median.B2=median(B2), 
                     sd.B2=sd(B2),
                     mean.HV=mean(HyperVolume), 
                     median.HV=median(HyperVolume), 
                     sd.HV=sd(HyperVolume),
                     mean.Tw=mean(Tw), 
                     median.Tw=median(Tw), 
                     sd.Tw=sd(Tw),
                     mean.Pw=mean(Pw), 
                     median.Pw=median(Pw), 
                     sd.Pw=sd(Pw),
                     mean.Aw=mean(Aw), 
                     median.Aw=median(Aw), 
                     sd.Aw=sd(Aw))

Cercis.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Cercis",]
Cercis.df <- ddply(Cercis.dat, ~Region, 
                    summarise, mean.Dist=mean(Est..Dist...km.2.), 
                    median.Dist=median(Est..Dist...km.2.), 
                    sd.Dist=sd(Est..Dist...km.2.),
                    mean.B1=mean(B1), 
                    median.B1=median(B1), 
                    sd.B1=sd(B1),
                    mean.B2=mean(B2), 
                    median.B2=median(B2), 
                    sd.B2=sd(B2),
                    mean.HV=mean(HyperVolume), 
                    median.HV=median(HyperVolume), 
                    sd.HV=sd(HyperVolume),
                    mean.Tw=mean(Tw), 
                    median.Tw=median(Tw), 
                    sd.Tw=sd(Tw),
                    mean.Pw=mean(Pw), 
                    median.Pw=median(Pw), 
                    sd.Pw=sd(Pw),
                    mean.Aw=mean(Aw), 
                    median.Aw=median(Aw), 
                    sd.Aw=sd(Aw))

Corylus.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Corylus",]
Corylus.df <- ddply(Corylus.dat, ~Region, 
                   summarise, mean.Dist=mean(Est..Dist...km.2.), 
                   median.Dist=median(Est..Dist...km.2.), 
                   sd.Dist=sd(Est..Dist...km.2.),
                   mean.B1=mean(B1), 
                   median.B1=median(B1), 
                   sd.B1=sd(B1),
                   mean.B2=mean(B2), 
                   median.B2=median(B2), 
                   sd.B2=sd(B2),
                   mean.HV=mean(HyperVolume), 
                   median.HV=median(HyperVolume), 
                   sd.HV=sd(HyperVolume),
                   mean.Tw=mean(Tw), 
                   median.Tw=median(Tw), 
                   sd.Tw=sd(Tw),
                   mean.Pw=mean(Pw), 
                   median.Pw=median(Pw), 
                   sd.Pw=sd(Pw),
                   mean.Aw=mean(Aw), 
                   median.Aw=median(Aw), 
                   sd.Aw=sd(Aw))
 
Pieris.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Pieris",]
Pieris.df <- ddply(Pieris.dat, ~Region, 
                   summarise, mean.Dist=mean(Est..Dist...km.2.), 
                   median.Dist=median(Est..Dist...km.2.), 
                   sd.Dist=sd(Est..Dist...km.2.),
                   mean.B1=mean(B1), 
                   median.B1=median(B1), 
                   sd.B1=sd(B1),
                   mean.B2=mean(B2), 
                   median.B2=median(B2), 
                   sd.B2=sd(B2),
                   mean.HV=mean(HyperVolume), 
                   median.HV=median(HyperVolume), 
                   sd.HV=sd(HyperVolume),
                   mean.Tw=mean(Tw), 
                   median.Tw=median(Tw), 
                   sd.Tw=sd(Tw),
                   mean.Pw=mean(Pw), 
                   median.Pw=median(Pw), 
                   sd.Pw=sd(Pw),
                   mean.Aw=mean(Aw), 
                   median.Aw=median(Aw), 
                   sd.Aw=sd(Aw))

Sassafras.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Sassafras",]
Sassafras.df <- ddply(Sassafras.dat, ~Region, 
                   summarise, mean.Dist=mean(Est..Dist...km.2.), 
                   median.Dist=median(Est..Dist...km.2.), 
                   sd.Dist=sd(Est..Dist...km.2.),
                   mean.B1=mean(B1), 
                   median.B1=median(B1), 
                   sd.B1=sd(B1),
                   mean.B2=mean(B2), 
                   median.B2=median(B2), 
                   sd.B2=sd(B2),
                   mean.HV=mean(HyperVolume), 
                   median.HV=median(HyperVolume), 
                   sd.HV=sd(HyperVolume),
                   mean.Tw=mean(Tw), 
                   median.Tw=median(Tw), 
                   sd.Tw=sd(Tw),
                   mean.Pw=mean(Pw), 
                   median.Pw=median(Pw), 
                   sd.Pw=sd(Pw),
                   mean.Aw=mean(Aw), 
                   median.Aw=median(Aw), 
                   sd.Aw=sd(Aw))

Torreya.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Torreya",]
Torreya.df <- ddply(Torreya.dat, ~Region, 
                   summarise, mean.Dist=mean(Est..Dist...km.2.), 
                   median.Dist=median(Est..Dist...km.2.), 
                   sd.Dist=sd(Est..Dist...km.2.),
                   mean.B1=mean(B1), 
                   median.B1=median(B1), 
                   sd.B1=sd(B1),
                   mean.B2=mean(B2), 
                   median.B2=median(B2), 
                   sd.B2=sd(B2),
                   mean.HV=mean(HyperVolume), 
                   median.HV=median(HyperVolume), 
                   sd.HV=sd(HyperVolume),
                   mean.Tw=mean(Tw), 
                   median.Tw=median(Tw), 
                   sd.Tw=sd(Tw),
                   mean.Pw=mean(Pw), 
                   median.Pw=median(Pw), 
                   sd.Pw=sd(Pw),
                   mean.Aw=mean(Aw), 
                   median.Aw=median(Aw), 
                   sd.Aw=sd(Aw))

Wisteria.dat <- dist.dat.trim[dist.dat.trim[, "Genus"] == "Wisteria",]
Wisteria.df <- ddply(Wisteria.dat, ~Region, 
                   summarise, mean.Dist=mean(Est..Dist...km.2.), 
                   median.Dist=median(Est..Dist...km.2.), 
                   sd.Dist=sd(Est..Dist...km.2.),
                   mean.B1=mean(B1), 
                   median.B1=median(B1), 
                   sd.B1=sd(B1),
                   mean.B2=mean(B2), 
                   median.B2=median(B2), 
                   sd.B2=sd(B2),
                   mean.HV=mean(HyperVolume), 
                   median.HV=median(HyperVolume), 
                   sd.HV=sd(HyperVolume),
                   mean.Tw=mean(Tw), 
                   median.Tw=median(Tw), 
                   sd.Tw=sd(Tw),
                   mean.Pw=mean(Pw), 
                   median.Pw=median(Pw), 
                   sd.Pw=sd(Pw),
                   mean.Aw=mean(Aw), 
                   median.Aw=median(Aw), 
                   sd.Aw=sd(Aw))

# Add them all to a table
Genus <- c("Aesculus", "Aesculus", "Castanea", "Castanea", "Catalpa", "Catalpa", "Cercis", "Cercis",
           "Corylus", "Corylus", "Pieris", "Pieris", "Sassafras", "Sassafras", "Torreya", "Torreya",
           "Wisteria", "Wisteria")

big.by.genus.table <- rbind(aesculus.df, Castanea.df, Catalpa.df, Cercis.df, Corylus.df,
                             Pieris.df, Sassafras.df, Torreya.df, Wisteria.df)
big.by.genus.table$Genus <- cbind(Genus)
write.csv(big.by.genus.table, "results_by_genus_and_region.csv")

### Summarize stats by region then genus
#ovrlp.dat.trim.ea <- ovrlp.dat.trim[ovrlp.dat.trim[, "comparison"] == "ea-ea",]
#ovrlp.dat.trim.ena <- ovrlp.dat.trim[ovrlp.dat.trim[, "comparison"] == "ena-ena",]
#ovrlp.dat.trim.ea.ena <- ovrlp.dat.trim[ovrlp.dat.trim[, "comparison"] == "ea-ena",]
#ovrlp.dat.trim.ena.ea <- ovrlp.dat.trim[ovrlp.dat.trim[, "comparison"] == "ena-ea",]


