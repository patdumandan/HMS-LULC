library(readxl)
library(dplyr)
library(reshape2)
require(standardize)
require(rstanarm)
require(tidyr)
require(mgcv)

full_data=read.csv("./Data/chap2-hmscovarv3-final.csv")

full_data$years = (full_data$year - mean(full_data$year))/(2 *sd(full_data$year))
full_data$stdpasture = (full_data$weighted.pasture - mean(full_data$weighted.pasture))/(2 *sd(full_data$weighted.pasture))
full_data$stdurban = (full_data$weighted.urban - mean(full_data$weighted.urban))/(2 *sd(full_data$weighted.urban))
full_data$stdforest = (full_data$weighed.forest - mean(full_data$weighed.forest))/(2 *sd(full_data$weighed.forest))


amke=full_data%>%filter(species=="AMKE")
noha=full_data%>%filter(species=="NOHA")
coha=full_data%>%filter(species=="COHA")
rtha=full_data%>%filter(species=="RTHA")
ssha=full_data%>%filter(species=="SSHA")
nogo=full_data%>%filter(species=="NOGO")

m1=gam(count~s(stdpasture)+s(years)+offset(log(obs.hours)), data=amke, method="REML",family =nb(theta = NULL, link = "log"))
m2=gam(count~s(stdpasture)+s(years)+offset(log(obs.hours)), data=noha, method="REML",family =nb(theta = NULL, link = "log"))
m3=gam(count~s(stdforest)+s(years)+offset(log(obs.hours)), data=nogo, method="REML",family =nb(theta = NULL, link = "log"))
m4=gam(count~s(stdforest)+s(years)+offset(log(obs.hours)), data=ssha, method="REML",family =nb(theta = NULL, link = "log"))
m5=gam(count~s(stdurban)+s(years)+offset(log(obs.hours)), data=coha, method="REML",family =nb(theta = NULL, link = "log"))
m6=gam(count~s(stdurban)+s(years)+offset(log(obs.hours)), data=rtha, method="REML", family =nb(theta = NULL, link = "log"))

plot.gam(m1, shade=T, jit=T, select=1, bty="n",pch=20, font.main = 1,res=1200, xlab="Pasture",main="American Kestrel")
box(bty="l", lwd=2)
title("C", adj=0)
summary(m6)

#model viz####

visreg(m6, xvar="stdurban",type="conditional",scale="response", jitter=T,
       trans=exp, line=list(col="black"),fill=list(col="gray"), xlab="urban")

#PERIODS OF CHANGE FOR LULC####

tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",tmpf)
source(tmpf)

#forest####

forest=gam(amke$forest.cov~s(year), data=amke, method="REML")


want=seq(1, nrow(amke), length.out = 200)
pdat1=with(amke, data.frame(year=year[want]))
p1=predict(forest, newdata=pdat1, type="terms", se.fit = TRUE)
pdat1=transform(pdat1, p1=p1$fit[,1], se1=p1$se.fit[,1]) #p2=fit, se2=std.error
df.res=df.residual(forest)
crit.t=qt(0.025, df.res, lower.tail = F)
pdat1=transform(pdat1, upper=p1+(crit.t*se1), lower=p1-(crit.t*se1))
p1.d <- Deriv(forest)
Term="year"
p1.dci <- confint(p1.d, term = Term)
p1.dsig <- signifD(pdat1$p1, d = p1.d[[Term]]$deriv,
                   +p1.dci[[Term]]$upper, p1.dci[[Term]]$lower)
plot.Deriv(p1.d, sizer=T, term=Term, ylab="f'(year)")
summary(forest)

ylim <- with(pdat1, range(upper, lower, p1))
plot(p1~year, data=pdat1, type="n", ylim=ylim, ylab="forest",
     bty="n")
box(bty="l", lwd=2)
lines(p1~year, data=pdat1)

lines(p1~year, data=pdat1)
lines(upper~year, data=pdat1, lty="dashed")
lines(lower~year, data=pdat1, lty="dashed")
lines(unlist(p1.dsig$incr) ~ year, data = pdat1, col = "grey", lwd = 3)
lines(unlist(p1.dsig$decr) ~ year, data = pdat1, col = "black", lwd = 3)

#urban####

urb=gam(amke$urban.cov~s(year), data=amke, method="REML")


want=seq(1, nrow(amke), length.out = 200)
pdat2=with(amke, data.frame(year=year[want]))
p2=predict(urb, newdata=pdat2, type="terms", se.fit = TRUE)
pdat2=transform(pdat2, p2=p2$fit[,1], se2=p2$se.fit[,1]) #p2=fit, se2=std.error
df.res=df.residual(forest)
crit.t=qt(0.025, df.res, lower.tail = F)
pdat2=transform(pdat2, upper=p2+(crit.t*se2), lower=p2-(crit.t*se2))

p2.d <- Deriv(urb)
Term="year"
p2.dci <- confint(p2.d, term = Term)
p2.dsig <- signifD(pdat2$p2, d = p2.d[[Term]]$deriv,
                   +p2.dci[[Term]]$upper, p2.dci[[Term]]$lower)
plot.Deriv(p2.d, sizer=T, term=Term, ylab="f'(year)")
summary(forest)

ylim <- with(pdat2, range(upper, lower, p2))
plot(p2~year, data=pdat2, type="n", ylim=ylim, ylab="urban",
     bty="n")
box(bty="l", lwd=2)
lines(p2~year, data=pdat2)
lines(upper~year, data=pdat2, lty="dashed")
lines(lower~year, data=pdat2, lty="dashed")
lines(unlist(p2.dsig$incr) ~ year, data = pdat2, col = "grey", lwd = 3)
lines(unlist(p2.dsig$decr) ~ year, data = pdat2, col = "black", lwd = 3)

#MODEL PERFORMANCE EVALUATION####
m1a=gam(count~s(stdurban)+s(years)+offset(log(obs.hours)), data=merl, method="REML",family =nb(theta = NULL, link = "log"))
m1b=gam(count~stdurban+s(years)+offset(log(obs.hours)), data=merl, method="REML",family =nb(theta = NULL, link = "log"))
m1c=gam(count~s(stdurban)+years+offset(log(obs.hours)), data=merl, method="REML",family =nb(theta = NULL, link = "log"))
m1d=gam(count~stdurban+years+offset(log(obs.hours)), data=merl, method="REML",family =nb(theta = NULL, link = "log"))

AIC(m1a)
AIC(m1b)
AIC(m1c)
AIC(m1d)

summary(m1a)
summary(m1b)
summary(m1c)
summary(m1d)

visreg(m6, "stdurban", jitter=TRUE, line=list(col="black"),fill=list(col="gray"),
       xlab="standardized urban cover", main="Red-tailed Hawk")
