
#OPEN AREA SPECIES####

full_dat$years = (full_dat$year - mean(full_dat$year))/(2 *sd(full_dat$year))
full_dat$stdpasture = (full_dat$pasture - mean(full_dat$pasture))/(2 *sd(full_dat$pasture))
full_dat$stdcrop= (full_dat$cropland - mean(full_dat$cropland))/(2 *sd(full_dat$cropland))


amke=full_dat%>%filter(species=="AMKE")
noha=full_dat%>%filter(species=="NOHA")

require(mgcv)

m1=gam(count~s(stdpasture)+years+offset(log(obs.hours)), data=amke, family =nb(theta = NULL, link = "log"))
m2=gam(count~s(stdpasture)+years+offset(log(obs.hours)), data=noha, family =nb(theta = NULL, link = "log"))

plot.gam(m1, shade=T)
plot.gam(m2, shade=T, residuals=T, pch=20, bty="n", xlab="pasture")
box(bty="l", lwd=2)
summary(m1)
summary(m2)


#FOREST SPECIES####
full_dat$stdforest = (full_dat$forest - mean(full_dat$forest))/(2 *sd(full_dat$forest))

nogo=full_dat%>%filter(species=="NOGO")
ssha=full_dat%>%filter(species=="SSHA")

m3=gam(count~s(stdforest)+s(years)+offset(log(obs.hours)), data=nogo,method="REML", family =nb(theta = NULL, link = "log"))
m4=gam(count~s(stdforest)+s(years)+offset(log(obs.hours)), data=ssha,method="REML", family =nb(theta = NULL, link = "log"))

plot.gam(m3, shade=T)
plot.gam(m4, shade=T)

summary(m3)
summary(m4)

#URBAN SPECIES####
full_dat$stdurban = (full_dat$urban - mean(full_dat$urban))/(2 *sd(full_dat$urban))

coha=full_dat%>%filter(species=="COHA")
merl=full_dat%>%filter(species=="MERL")

m5=gam(count~s(stdurban)+years+offset(log(obs.hours)), data=coha, family =nb(theta = NULL, link = "log"))
m6=gam(count~s(stdurban)+years+offset(log(obs.hours)), data=merl, family =nb(theta = NULL, link = "log"))

plot.gam(m5, shade=T)
plot.gam(m6, shade=T)

summary(m5)
summary(m6)
