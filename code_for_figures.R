library(gridExtra)
library(ggplot2)
library(grid)
library(partykit)

##############################################################################################

blsdata<-read.csv("blsdata.csv", sep = ",")
hunger<-blsdata$tfactor3.0
disinhibition<-blsdata$tfactor2.0
rest.eating<-blsdata$tfactor1.0
rrvf<-blsdata$rrvfood
liking<-blsdata$ml.hf.bias0
wanting<-blsdata$fwa.hf.bias0
kcal<-blsdata$kcal24h0
weighfreq<-blsdata$weighfreq
#be<-as.numeric(blsdata$edeq15.0)
#bs<-as.numeric(blsdata$edeq23.0)
#misc<-as.numeric(blsdata$edeq01.0)
sex<-as.numeric(blsdata$sex)
bmi<-as.numeric(blsdata$bmi0)
age<-as.numeric(blsdata$age)
# Scale the dataset
covariates<-data.frame(hunger, disinhibition, rest.eating, rrvf, liking, wanting, kcal)
covariates<-data.frame(scale(covariates))

# Decision tree - Figure 1
p2<-partykit::ctree(kcal~hunger+ disinhibition+rest.eating+rrvf+ liking+wanting, data = covariates, control = ctree_control(mincriterion = 0.95))
tab <- tapply(p2$fitted[["(response)"]],
              factor(p2$fitted[["(fitted)"]], levels = 1:length(p2)),
              FUN = mean)
number<-table(p2$fitted[["(fitted)"]])
tab <- format(round(tab, 2))
mlab = function(id, nobs){
  p1<-paste("Mean = ", tab[id])
  p2<-c(paste0("Node ID = ", id,", ",paste0("(","n = ", number[[id]],")")))
  tt1<-ttheme_default(base_size = 14)
  p<-tableGrob(p2, rows = c(" "), theme = tt1)
  grid.text(x = 0.51, y = -7.25, label = p1, gp = gpar(fontsize = 18))
  grid.draw(p)
}
plot(p2, gp = gpar(fontsize = 19, fontface = "bold"), type = "extended",tp_args = list(mainlab = mlab), ep_args = list(digits = 2))

###############################################################################################
# CART - Figure 2

library(rpart)
library(rpart.plot)
p4<-rpart(kcal~., model = TRUE, data = covariates,control = rpart.control(cp = 0.0185))
prp(p4, extra = 1, cex = 1.5,split.cex = 1.25, branch.lwd = 4, type = 3)

##################################################################################################

# Visualization - Figure 3
setwd("./simulation_study/code")
source("visTree.R")
Y<-covariates$kcal
visTree(p2)

###################################################################################################

# Application
setwd("..")
blsdata<-read.csv("blsdata.csv", header = T)
names(blsdata)

#reg<-lm(kcal24h0~., data = blsdata)
# Stepwise regression
#step(reg)
# Regression
#reg2<-lm(formula = kcal24h0 ~ sex + bmi0 + snackkcal0 + srvgfv0 + srvgssb0 + cdrsbody0 + weighfreq0 + freqff0 + tfactor3.0 + fwa.hf.bias0, data = blsdata)
#summary(reg2)

blsdata$trt<-as.numeric(blsdata$trt)
# Female is 1 and Male is 2.
#blsdata$sex<-as.numeric(blsdata$sex)
names(blsdata)[c(4, 6, 12, 19, 21, 22, 23, 24, 25)]<-c("skcal", "srvgssb", "edeq15.0", "freqff","rest.eating", "disinhibition", "hunger", "liking", "wanting")

blsdata$edeq01.0<-as.numeric(blsdata$edeq01.0)
blsdata$edeq02.0<-as.numeric(blsdata$edeq02.0)
blsdata$edeq13.0<-as.numeric(blsdata$edeq13.0)
blsdata$edeq14.0<-as.numeric(blsdata$edeq14.0)
blsdata$edeq15.0<-as.numeric(blsdata$edeq15.0)
blsdata$edeq23.0<-as.numeric(blsdata$edeq23.0)
blsdata$edeq25.0<-as.numeric(blsdata$edeq25.0)
blsdata$edeq26.0<-as.numeric(blsdata$edeq26.0)
blsdata$weighfreq0<-as.numeric(blsdata$weighfreq0)
blsdata$freqff<-as.numeric(blsdata$freqff)

library(gridExtra)
resid<-lm(kcal24h0~sex+age+bmi0, data = blsdata) 
blsdata$kcal24h0<-resid$residuals
blsdata<-blsdata[,-2]
blsdata<-blsdata[,-2]
blsdata<-blsdata[, -18]

####################################################################################################

# Figure 6

potentialtree<-ctree(kcal24h0~., data = blsdata, control = ctree_control(mincriterion = 0.95))
tab <- tapply(potentialtree$fitted[["(response)"]],
              factor(potentialtree$fitted[["(fitted)"]], levels = 1:length(potentialtree)),
              FUN = mean)
number<-table(potentialtree$fitted[["(fitted)"]])
tab <- format(round(tab, 2))
mlab = function(id, nobs){
  p1<-paste("Mean = ", tab[id])
  p2<-c(paste0("Node ID = ", id,", ",paste0("(","n = ", number[[id]],")")))
  tt1<-ttheme_default(base_size = 14)
  p<-tableGrob(p2, rows = c(" "), theme = tt1)
  grid.text(x = 0.51,y = -7.5, label = p1, gp = gpar(fontsize = 18))
  grid.draw(p)
}
plot(potentialtree, gp = gpar(fontsize = 19, fontface = "bold"), type = "extended",tp_args = list(mainlab = mlab), ep_args = list(digits = 2))

#blsdata$kcal24h0<-as.numeric(scale(blsdata$kcal24h0))
#potentialtree<-ctree(kcal24h0~., data = blsdata, control = ctree_control(mincriterion = 0.95))
#plot(potentialtree, gp = gpar(fontsize = 19, fontface = "bold"), ep_args = list(digits = 2))
#pdf("residplot.pdf", width=20, height=10) 
#resid<-as.numeric(residuals(lm(kcal~blsdata$age + blsdata$bmi0 + blsdata$sex)))

############################################################################################

# Figure 7 
Y<-blsdata$kcal24h0
visTree(potentialtree)

############################################################################################

# Figure 8
# CART
tree2<-rpart(kcal24h0~., data = blsdata, control = rpart.control(cp = 0.015))
#prp(tree2,cex= 1.95, extra = 1, digits = 1)
prp(tree2, extra = 1, cex = 1,split.cex = 1.15, branch.lwd = 4, type = 3)

#############################################################################################

