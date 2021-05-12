### Revised by AEM

##################################################################################################################################
##################################################################################################################################
#
# Suggested analyses and figures for GEB_2020_0117R2
#
# Files needed: "NicheAreaDif.csv" (derived from Table S1.3) and "NicheOverlap.csv" (derived from Table 3)
#
# Script sections:
#
# 1) Read data and load packages
# 2) Analsyis of the relationship between ?sampled geographic areas? and ?climatic niche areas?
# 3) Graphs illustrating the results about niche conservatism
#
##################################################################################################################################
##################################################################################################################################


##################################################################################################################################
# 1) Read data and load packages
##################################################################################################################################

#load packages
library(nlme)
library(ggplot2)

#set your working directory
setwd("~/Dropbox/UF_Research/EA_ENA_ENM/R3/")
#make sure the working directory includes files "NicheAreaDif.csv" and "NicheOverlap.csv"
dir()

#read and examine "NicheAreaDif.csv"
niche.area <- read.csv("NicheArea.csv", header=T)
head(niche.area)
dim(niche.area)
summary(niche.area)

#read and examine "NicheOverlap.csv"
niche.overlap <- read.csv("NicheOverlap.csv", header=T)
head(niche.overlap)
dim(niche.overlap)
summary(niche.overlap)


##################################################################################################################################
# 2) Analsyis of the relationship between ?sampled geographic areas? and ?climatic niche areas?
##################################################################################################################################

#attach data frame "niche.area"
attach(niche.area)

#examine genus data
length(unique(Genus))
table(Genus)

# examine data
hist(x = Niche.Area, breaks = 60) # left skew
hist(x = Geographic.Area, breaks = 60) # left skew

#graph the relationship between ?sampled geographic areas? and ?climatic niche areas?
plot(log(Geographic.Area), log(Niche.Area), type="n", bty="n")
points(log(Geographic.Area)[Region=="ENA"], log(Niche.Area)[Region=="ENA"], col="blue", pch=19, cex=1)
points(log(Geographic.Area)[Region=="EA"], log(Niche.Area)[Region=="EA"], col="red", pch=21, cex=1.3)
legend(14,-7, c("ENA","EA"), col=c("blue","red"), pch=c(19,21), pt.cex=c(1,1.5))

# do the same as above, but in ggplot2
p <- ggplot(niche.area, aes(x = log(Geographic.Area), y = log(Niche.Area),color = Region)) +
  geom_point()
plot(p)

#graph, for each genus, the relationship between ?sampled geographic areas? and ?climatic niche areas?
#the loop below suspends execution for 5 seconds after graphing each genus, allowing time to examine each graph
for(i in unique(Genus)){
	plot(log(Geographic.Area), log(Niche.Area), type="n", main=paste("Focal genus:", i, sep=" "), bty="n")
	points(log(Geographic.Area)[Region=="ENA"], log(Niche.Area)[Region=="ENA"], col="gray", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA"], log(Niche.Area)[Region=="EA"], col="gray", pch=21, cex=1.3)
	points(log(Geographic.Area)[Region=="ENA" & Genus==i], log(Niche.Area)[Region=="ENA" & Genus==i], col="blue", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA" & Genus==i], log(Niche.Area)[Region=="EA" & Genus==i], col="red", pch=21, cex=1.3)
	legend(12,-6, c("ENA focal genus","EA focal genus", "ENA", "EA"), col=c("blue","red", "gray", "gray"), pch=c(19,21), pt.cex=c(1,1.5))
	Sys.sleep(5)
}

#the loop below is the same as above, except that it saves pdf files with the graphs for each genus
#and does not display the graphs in a graphical window
setwd("lmePlots/") #the directory where figures will be saved
for(i in unique(Genus)){
	pdf(paste(i, "_plot.pdf", sep=""))
	plot(log(Geographic.Area), log(Niche.Area), type="n", main=paste("Focal genus:", i, sep=" "), bty="n")
	points(log(Geographic.Area)[Region=="ENA"], log(Niche.Area)[Region=="ENA"], col="gray", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA"], log(Niche.Area)[Region=="EA"], col="gray", pch=21, cex=1.3)
	points(log(Geographic.Area)[Region=="ENA" & Genus==i], log(Niche.Area)[Region=="ENA" & Genus==i], col="blue", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA" & Genus==i], log(Niche.Area)[Region=="EA" & Genus==i], col="red", pch=21, cex=1.3)
	legend(12,-6, c("ENA focal genus","EA focal genus", "ENA", "EA"), col=c("blue","red", "gray", "gray"), pch=c(19,21), pt.cex=c(1,1.5))
	dev.off()
}


# fixed effects models
fixed.1 <- lm(log(Niche.Area) ~ log(Geographic.Area) + Region)
summary(fixed.1)

fixed.2 <- lm(log(Niche.Area) ~ log(Geographic.Area) + Region + log(Geographic.Area)*Region)
summary(fixed.2)

#mixed random effects models
random.1 <- lme(fixed = log(Niche.Area) ~ log(Geographic.Area) + Region, random = ~ 1 | Genus)
summary(random.1)

random.2 <- lme(fixed = log(Niche.Area) ~ log(Geographic.Area) + Region + log(Geographic.Area)*Region, random = ~ 1 | Genus)
summary(random.2)

# compare fixed vs. random effect models
anova(random.1, fixed.1)
anova(random.2, fixed.2) # random effects with interaction is best

#examine model assumptions (i.e., model criticism)
plot(random.2) #residuals
plot(random.2, log(Niche.Area) ~ fitted(.)) #fit (or lack thereof)
hist(residuals(random.2))
shapiro.test(residuals(random.2))

qqnorm(residuals(random.2), pch = 1, frame = FALSE)
qqline(residuals(random.2), col = "steelblue", lwd = 2)

library(car)
pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_R3/qqplot_for_supp.pdf")
qqPlot(residuals(random.2))
dev.off()

#plot the overall model
LGA.ENA <- seq(min(log(Geographic.Area[Region=="ENA"])), max(log(Geographic.Area[Region=="ENA"])), 0.1)
prediction.ENA <- fixed.effects(random.2)[1] + LGA.ENA * fixed.effects(random.2)[2] + fixed.effects(random.2)[3] + LGA.ENA * fixed.effects(random.2)[4]
LGA.EA <- seq(min(log(Geographic.Area[Region=="EA"])), max(log(Geographic.Area[Region=="EA"])), 0.1)
prediction.EA <- fixed.effects(random.2)[1] + LGA.EA * fixed.effects(random.2)[2]
plot(log(Geographic.Area), log(Niche.Area), type="n", bty="n")
points(log(Geographic.Area)[Region=="ENA"], log(Niche.Area)[Region=="ENA"], col="blue", pch=19, cex=1)
points(log(Geographic.Area)[Region=="EA"], log(Niche.Area)[Region=="EA"], col="red", pch=21, cex=1.3)
points(LGA.ENA, prediction.ENA, type="l", col="blue")
points(LGA.EA, prediction.EA, type="l", col="red")
axis(2, at=seq(-10, 0, 1), labels=F) 
legend(13,-7, c("ENA","EA"), col=c("blue","red"), pch=c(19,21), pt.cex=c(1,1.5), lwd=1)

# Same as above, but in ggplot2
LGA.ENA <- seq(min(log(Geographic.Area[Region=="ENA"])), max(log(Geographic.Area[Region=="ENA"])), 0.1)
prediction.ENA <- fixed.effects(random.2)[1] + LGA.ENA * fixed.effects(random.2)[2] + fixed.effects(random.2)[3] + LGA.ENA * fixed.effects(random.2)[4]
LGA.EA <- seq(min(log(Geographic.Area[Region=="EA"])), max(log(Geographic.Area[Region=="EA"])), 0.1)
prediction.EA <- fixed.effects(random.2)[1] + LGA.EA * fixed.effects(random.2)[2]

length(LGA.ENA)
length(prediction.ENA)

length(LGA.EA)
length(prediction.EA)

ea.df <- data.frame(LGA.EA, prediction.EA)
ea.df$Region <- rep(x = "EA", 66)
colnames(ea.df) <- c("LGA", "Prediction", "Region")

ena.df <- data.frame(LGA.ENA, prediction.ENA)
ena.df$Region <- rep(x = "ENA", 88)
colnames(ena.df) <- c("LGA", "Prediction", "Region")

predictions <- data.frame(LGA = numeric(), Prediction = numeric(), Region = character())
predictions <- rbind(ea.df, ena.df)

p <- ggplot(niche.area, aes(x = log(Geographic.Area), y = log(Niche.Area), color = Region)) +
  geom_point() +
  xlab(label = "log(Geographic Area)") +
  ylab(label = "log(Niche Area)")

pp <- p + geom_line(data = predictions, aes(x = LGA, y = Prediction, color = Region)) # add line for predictions

pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_R3/PredictedNicheArea.pdf")
plot(pp)
dev.off()

#calculate differences in predicted log(?climatic niche areas?) when log(?sampled geographic areas?) equals 5 
#ENA
ENA.5 <- fixed.effects(random.2)[1] + 5 * fixed.effects(random.2)[2] + fixed.effects(random.2)[3] + 5 * fixed.effects(random.2)[4]
#EA
EA.5 <- fixed.effects(random.2)[1] + 5 * fixed.effects(random.2)[2]
#difference ENA and EA
EA.5 - ENA.5 # 4.566154

#calculate differences in predicted log(?climatic niche areas?) when log(?sampled geographic areas?) equals 10 
#ENA
ENA.10 <- fixed.effects(random.2)[1] + 10 * fixed.effects(random.2)[2] + fixed.effects(random.2)[3] + 10 * fixed.effects(random.2)[4]
#EA
EA.10 <- fixed.effects(random.2)[1] + 10 * fixed.effects(random.2)[2]
#difference ENA and EA
EA.10 - ENA.10 # 2.588342  

#calculate differences in predicted log(?climatic niche areas?) when log(?sampled geographic areas?) equals 15 
#ENA
ENA.15 <- fixed.effects(random.2)[1] + 15 * fixed.effects(random.2)[2] + fixed.effects(random.2)[3] + 15 * fixed.effects(random.2)[4]
#EA
EA.15 <- fixed.effects(random.2)[1] + 15 * fixed.effects(random.2)[2]
#difference ENA and EA
EA.15 - ENA.15 # 0.6105305 

#graph model for each genus,
#the loop below suspends execution for 5 seconds after graphing each genus, allowing time to examine each graph
for(i in unique(Genus)){
	plot(log(Geographic.Area), log(Niche.Area), type="n", main=paste("Focal genus:", i, sep=" "), bty="n")
	points(log(Geographic.Area)[Region=="ENA"], log(Niche.Area)[Region=="ENA"], col="gray", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA"], log(Niche.Area)[Region=="EA"], col="gray", pch=21, cex=1.3)
	points(LGA.ENA, prediction.ENA, type="l", col="gray")
	points(LGA.EA, prediction.EA, type="l", col="gray", lty=3)
	points(log(Geographic.Area)[Region=="ENA" & Genus==i], log(Niche.Area)[Region=="ENA" & Genus==i], col="blue", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA" & Genus==i], log(Niche.Area)[Region=="EA" & Genus==i], col="red", pch=21, cex=1.3)
	#prediction lines for genus
	points(LGA.ENA, prediction.ENA + random.effects(random.2)[i,], type="l", col="blue")
	points(LGA.EA, prediction.EA + random.effects(random.2)[i,], type="l", col="red")
	axis(2, at=seq(-10, 0, 1), labels=F)
	legend(11,-6, c("ENA focal genus","EA focal genus", "ENA", "EA"), col=c("blue","red", "gray", "gray"), pch=c(19,21), pt.cex=c(1,1.5), lty=c(1,1,1,3))
	Sys.sleep(5)
}

#graph model for each genus,
#the loop below is the same as above, except that it saves pdf files with the graphs for each genus
#and does not display the graphs in a graphical window

# add in par to make one big plot
pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_R3/Online_Supplemental_Materials_R3/Per_Genus_Model_Plot.pdf")
par(mfrow=c(3,2))
for(i in unique(Genus)){
	plot(log(Geographic.Area), log(Niche.Area), type="n", main=paste("Focal genus:", i, sep=" "), bty="n", xlab="log(Geographic Area)", ylab="log(Niche Area)")
	points(log(Geographic.Area)[Region=="ENA"], log(Niche.Area)[Region=="ENA"], col="gray", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA"], log(Niche.Area)[Region=="EA"], col="gray", pch=21, cex=1.3)
	points(LGA.ENA, prediction.ENA, type="l", col="gray")
	points(LGA.EA, prediction.EA, type="l", col="gray", lty=3)
	points(log(Geographic.Area)[Region=="ENA" & Genus==i], log(Niche.Area)[Region=="ENA" & Genus==i], col="blue", pch=19, cex=1)
	points(log(Geographic.Area)[Region=="EA" & Genus==i], log(Niche.Area)[Region=="EA" & Genus==i], col="red", pch=21, cex=1.3)
	#prediction lines for genus
	points(LGA.ENA, prediction.ENA + random.effects(random.2)[i,], type="l", col="blue")
	points(LGA.EA, prediction.EA + random.effects(random.2)[i,], type="l", col="red")
	#legend(11,-6, c("ENA focal genus","EA focal genus", "ENA", "EA"), col=c("blue","red", "gray", "gray"), pch=c(19,21), pt.cex=c(1,1.5), lty=c(1,1,1,3))
}
dev.off()

#calculate partial residuals
partial.residuals.region.interaction <- residuals(random.2) + (Region == "ENA")*fixed.effects(random.2)[3] + (Region == "ENA")*log(Geographic.Area)*fixed.effects(random.2)[4] 

#the following loop plots partial residuals for each genus,
#it suspends execution for 5 seconds after graphing each genus,
#thus allowing time to examine each graph
for(i in unique(Genus)){
	boxplot(partial.residuals.region.interaction ~ Region, ylab="Partial residuals of log (Niche Area)", main=paste("Focal genus:", i, sep=" "))
	points(rep(1, times=length(Genus))[Genus==i & Region=="EA"], partial.residuals.region.interaction[Genus==i & Region=="EA"], col="red", pch=19)
	points(rep(2, times=length(Genus))[Genus==i & Region=="ENA"], partial.residuals.region.interaction[Genus==i & Region=="ENA"], col="blue", pch=19)
	abline(h=c(partial.residuals.region.interaction[Genus==i & Region=="EA"]), col="red", lty=3)
	abline(h=c(partial.residuals.region.interaction[Genus==i & Region=="ENA"]), col="blue", lty=3)
	Sys.sleep(5)
}
	
#the loop below is the same as above, except that it saves pdf files with the graphs for each genus
#and does not display the graphs in a graphical window
setwd("~/Dropbox/UF_Research/EA_ENA_ENM/R3/lmePlots/") #the directory where figures will be saved
for(i in unique(Genus)){
	pdf(paste(i, "_PartialResiduals.pdf", sep=""))
	boxplot(partial.residuals.region.interaction ~ Region, ylab="Partial residuals of log (Niche Area)", main=paste("Focal genus:", i, sep=" "))
	points(rep(1, times=length(Genus))[Genus==i & Region=="EA"], partial.residuals.region.interaction[Genus==i & Region=="EA"], col="red", pch=19)
	points(rep(2, times=length(Genus))[Genus==i & Region=="ENA"], partial.residuals.region.interaction[Genus==i & Region=="ENA"], col="blue", pch=19)
	abline(h=c(partial.residuals.region.interaction[Genus==i & Region=="EA"]), col="red", lty=3)
	abline(h=c(partial.residuals.region.interaction[Genus==i & Region=="ENA"]), col="blue", lty=3)
	dev.off()
}

detach(niche.area)


##################################################################################################################################
# 3) Graphs illustrating the results about niche conservatism
##################################################################################################################################

attach(niche.overlap)

#plot relationship between Observed Shoeners' D and the p-value of the niche conservatism tests
plot(Observed.Schoener.s.D, P.value)

#graph the frequency of statistically significant and non-significant statistical tests for each genus
#in pair-wise comparison of EA species
#Genus.EA.EA <- tapply(Comparison=="EA-EA", Genus.1, sum)
#significant.EA.EA <- tapply(P.value<0.05 & Comparison == "EA-EA", Genus.1, sum)
#barplot(rbind(Genus.EA.EA,significant.EA.EA), names=unique(Genus.1), ylab="Pair-wise species comparisons", col=c("white", "gray"), las=3, cex.names=0.7)
#legend(12, 3.5, c("p >= 0.05","p < 0.05"), fill=c("white","gray"), title="EA-EA")

#graph the frequency of statistically significant and non-significant statistical tests for each genus
#in pair-wise comparison of ENA species
#Genus.ENA.ENA <- tapply(Comparison=="ENA-ENA", Genus.1, sum)
#significant.ENA.ENA <- tapply(P.value<0.05 & Comparison == "ENA-ENA", Genus.1, sum)
#barplot(rbind(Genus.ENA.ENA,significant.ENA.ENA), names=unique(Genus.1), ylab="Pair-wise species comparisons", col=c("white", "gray"), las=3, cex.names=0.7)
#legend(12, 3.5, c("p >= 0.05","p < 0.05"), fill=c("white","gray"), title="ENA-ENA")

#graph the frequency of statistically significant and non-significant statistical tests for each genus
#in pair-wise comparison of EA - ENA species
#Genus.EA.ENA <- tapply(Comparison=="EA-ENA", Genus.1, sum)
#significant.EA.ENA <- tapply(P.value<0.05 & Comparison == "EA-ENA", Genus.1, sum)
#barplot(rbind(Genus.EA.ENA,significant.EA.ENA), names=unique(Genus.1), ylab="Pair-wise species comparisons", col=c("white", "gray"), las=3, cex.names=0.7)
#legend(12, 8.5, c("p >= 0.05","p < 0.05"), fill=c("white","gray"), title="EA-ENA")

### Same as above, but in ggplot
# Kind of annoying, but break down each object to rename columns, add a column, and recombine.

#graph the frequency of statistically significant and non-significant statistical tests for each genus
#in pair-wise comparison of EA species
#Genus.EA.EA <- tapply(Comparison=="EA-EA", Genus.1, sum)
significant.EA.EA <- tapply(P.value<0.05 & Comparison == "EA-EA", Genus.1, sum)
insignificant.EA.EA <- tapply(P.value>=0.05 & Comparison == "EA-EA", Genus.1, sum)

df <- t(rbind(insignificant.EA.EA, significant.EA.EA))
df <- as.data.frame(df)

insig.df <- data.frame(rownames(df), df$insignificant.EA.EA)
sig.df <- data.frame(rownames(df), df$significant.EA.EA)

insig.df$Significance <- rep(x = "p >= 0.05", nrow(insig.df))
colnames(insig.df) <- c("Genus", "Count", "Significance")
sig.df$Significance <- rep(x = "p < 0.05", 31)
colnames(sig.df) <- c("Genus", "Count", "Significance")

df <- rbind(insig.df, sig.df)

p1 <- ggplot(data = df, aes(x = Genus, y = Count, fill = Significance)) +
  geom_bar(stat = "identity") +
  ggtitle("EA-EA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, face = "italic")) +
  theme(legend.position = c(0.8, 0.75), legend.title = element_text(size = 7), legend.text = element_text(size = 6), legend.background = element_rect(fill = "transparent"))

#graph the frequency of statistically significant and non-significant statistical tests for each genus
#in pair-wise comparison of ENA species
#Genus.ENA.ENA <- tapply(Comparison=="ENA-ENA", Genus.1, sum)
significant.ENA.ENA <- tapply(P.value<0.05 & Comparison == "ENA-ENA", Genus.1, sum)
insignificant.ENA.ENA <- tapply(P.value>=0.05 & Comparison == "ENA-ENA", Genus.1, sum)


df <- t(rbind(insignificant.ENA.ENA, significant.ENA.ENA))
df <- as.data.frame(df)

insig.df <- data.frame(rownames(df), df$insignificant.ENA.ENA)
sig.df <- data.frame(rownames(df), df$significant.ENA.ENA)

insig.df$Significance <- rep(x = "p >= 0.05", nrow(insig.df))
colnames(insig.df) <- c("Genus", "Count", "Significance")
sig.df$Significance <- rep(x = "p < 0.05", 31)
colnames(sig.df) <- c("Genus", "Count", "Significance")

df <- rbind(insig.df, sig.df)

p2 <- ggplot(data = df, aes(x = Genus, y = Count, fill = Significance)) +
  geom_bar(stat = "identity") +
  ggtitle("ENA-ENA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, face = "italic")) +
  theme(legend.position = "none")

#graph the frequency of statistically significant and non-significant statistical tests for each genus
#in pair-wise comparison of EA - ENA species
#Genus.EA.ENA <- tapply(Comparison=="EA-ENA", Genus.1, sum)
significant.EA.ENA <- tapply(P.value<0.05 & Comparison == "EA-ENA", Genus.1, sum)
insignificant.EA.ENA <- tapply(P.value>=0.05 & Comparison == "EA-ENA", Genus.1, sum)

df <- t(rbind(insignificant.EA.ENA, significant.EA.ENA))
df <- as.data.frame(df)

insig.df <- data.frame(rownames(df), df$insignificant.EA.ENA)
sig.df <- data.frame(rownames(df), df$significant.EA.ENA)

insig.df$Significance <- rep(x = "p >= 0.05", nrow(insig.df))
colnames(insig.df) <- c("Genus", "Count", "Significance")
sig.df$Significance <- rep(x = "p < 0.05", 31)
colnames(sig.df) <- c("Genus", "Count", "Significance")

df <- rbind(insig.df, sig.df)

p3 <- ggplot(data = df, aes(x = Genus, y = Count, fill = Significance)) +
  geom_bar(stat = "identity") +
  ggtitle("EA-ENA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, face = "italic")) +
  theme(legend.position = "none")
#

pdf(file = "~/Dropbox/UF_Research/EA_ENA_ENM/R3/NicheOverlapPlots/NicheOverlap.pdf", width = 5, height = 8)
cowplot::plot_grid(p1, p2, p3, ncol = 1, labels = "AUTO")
dev.off()
#

detach(niche.overlap)


