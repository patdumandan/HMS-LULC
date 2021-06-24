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
merl=full_data%>%filter(species=="MERL")
ssha=full_data%>%filter(species=="SSHA")
nogo=full_data%>%filter(species=="NOGO")

m1=gam(count~s(stdpasture)+s(years, bs="cc")+offset(log(obs.hours)), data=amke, method="REML",family =nb(theta = NULL, link = "log"))
m2=gam(count~s(stdpasture)+s(years)+offset(log(obs.hours)), data=noha, family =nb(theta = NULL, link = "log"))
m3=gam(count~s(stdforest)+s(years)+offset(log(obs.hours)), data=nogo, family =nb(theta = NULL, link = "log"))
m4=gam(count~s(stdforest)+s(years)+offset(log(obs.hours)), data=ssha, family =nb(theta = NULL, link = "log"))
m5=gam(count~s(stdurban)+s(years)+offset(log(obs.hours)), data=coha, family =nb(theta = NULL, link = "log"))
m6=gam(count~s(stdurban)+s(years)+offset(log(obs.hours)), data=merl, family =nb(theta = NULL, link = "log"))

plot.gam(m2, shade=T, pages=1)
summary(m1)

#model viz####

visreg(m1, "stdpasture", jitter=TRUE, line=list(col="black"),fill=list(col="gray"), xlab="standardized pasture cover")
past=gam(amke$pasture.cov~s(year), data=amke, method="REML")
tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",tmpf)
source(tmpf)

want=seq(1, nrow(amke), length.out = 200)
pdat2=with(amke, data.frame(year=year[want]))
p3=predict(past, newdata=pdat2, type="terms", se.fit = TRUE)
pdat2=transform(pdat2, p3=p3$fit[,1], se3=p3$se.fit[,1]) #p2=fit, se2=std.error
df.res=df.residual(past)
crit.t=qt(0.025, df.res, lower.tail = F)
pdat2=transform(pdat2, upper=p3+(crit.t*se3), lower=p3-(crit.t*se3))
p1.d <- Deriv(past)
Term="year"
m1.dci <- confint(p1.d, term = Term)
m1.dsig <- signifD(pdat2$p3, d = p1.d[[Term]]$deriv,
                   +m1.dci[[Term]]$upper, m1.dci[[Term]]$lower)
plot.Deriv(p1.d, sizer=T, term=Term, ylab="f'(year)")
summary(past)

ylim <- with(pdat2, range(upper, lower, p3))
plot(p3~year, data=pdat2, type="n", ylim=ylim, ylab="proportional cover of pasture (standardized)")
lines(p3~year, data=pdat2)
lines(upper~year, data=pdat2, lty="dashed")
lines(lower~year, data=pdat2, lty="dashed")
lines(unlist(m1.dsig$incr) ~ year, data = pdat2, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ year, data = pdat2, col = "red", lwd = 3)
