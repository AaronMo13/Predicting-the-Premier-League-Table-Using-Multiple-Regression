library(car)
library(leaps)
library(ggpubr)

datapremtrain = read.csv("premdatatrain1.csv", header=T) #this is the uncleaned data
datapremtest = read.csv("21-22test.csv", header=T) #the test data

lm.fit = lm(points~.-club, data=datapremtrain) #we remove the club variable first 
#use VIF to check for any multicollinearity, we remove the varaibles with values higher than 10
vif(lm.fit)
lm.fit = lm(points~.-club-PSG, data=datapremtrain)
vif(lm.fit)
lm.fit = lm(points~.-club-PSG-STPG, data=datapremtrain)
vif(lm.fit)
lm.fit = lm(points~.-club-PSG-STPG-TPG, data=datapremtrain)
vif(lm.fit)
lm.fit = lm(points~.-club-PSG-STPG-TPG-GPG, data=datapremtrain)
vif(lm.fit)
lm.fit = lm(points~.-club-PSG-STPG-TPG-GPG-PPG, data=datapremtrain)
vif(lm.fit)

#---------------checking for outliers and influential points-----------------------

#checking for outliers using studentised residuals, if bigger than 3 then possible outlier
plot(predict(lm.fit), rstudent(lm.fit))
tab = round(table(rstudent(lm.fit)>3,predict(lm.fit)),digits=2)
x = ifelse(rstudent(lm.fit)>3,predict(lm.fit),0)

outliers_points = predict(lm.fit)
outliers_rstudent = rstudent(lm.fit)
outliers = data.frame(outliers_points,outliers_rstudent)
ggplot(outliers,aes(x=outliers_points,y=outliers_rstudent))+geom_point()+
  xlab("Predicted Points")+ylab("Studentized Residuals")

#checking for influential points, and is explained in the final report
pr <- residuals(lm.fit)/(1-lm.influence(lm.fit)$hat)
influence.measures(lm.fit)
y = ifelse(abs(pr)>10,pr,0)
options(max.print=1000000)

#new data without outliers and influential points
df2 = datapremtrain[-c(107),]


#----------------------------subset selection------------------------------------
regfit.fwd = regsubsets(points~.-PSG-STPG-TPG-GPG-PPG-club, data=df2, nvmax = 19, method = "forward")
reg.fwd.sum = summary(regfit.fwd)
regfit.bwd = regsubsets(points~.-PSG-STPG-TPG-GPG-PPG-club, data=df2, nvmax = 19, method = "backward")
reg.bwd.sum = summary(regfit.bwd)

mod = regsubsets(points~SPG+PSTG+PCS+PSV+PTS+CRPG+GCPG+PST,data=df2,nvmax=8)
mod.sum = summary(mod)
which.max(mod.sum$adjr2)
which.min(mod.sum$cp)
which.min(mod.sum$bic)
coef(mod,7)
coef(mod,8)
#coef(mod,9)

#forward selection
par(mfrow=c(2,2))
plot(reg.fwd.sum$rss,xlab = "Number of Variables",ylab="RSS",type="b")
plot(reg.fwd.sum$adjr2,xlab = "Number of variables",ylab="ARSq",type="b")
best_adjr2 = which.max(reg.fwd.sum$adjr2)
points(best_adjr2, reg.fwd.sum$adjr2[best_adjr2], 
       col = "red", cex = 2, pch = 20)
plot(reg.fwd.sum$bic,xlab = "Number of variables",ylab="bic",type="b")
best_bic = which.min(reg.fwd.sum$bic)
points(best_bic, reg.fwd.sum$bic[best_bic], 
       col = "red", cex = 2, pch = 20)
plot(reg.fwd.sum$cp,xlab = "Number of variables",ylab="cp",type="b")
best_cp = which.min(reg.fwd.sum$cp)
points(best_cp, reg.fwd.sum$cp[best_cp], 
       col = "red", cex = 2, pch = 20)
plot(reg.fwd.sum$rsq,xlab = "Number of variables",ylab="RSq",type="b")

which.max(reg.fwd.sum$adjr2)
which.min(reg.fwd.sum$cp)
which.min(reg.fwd.sum$bic)
coef(regfit.fwd,7)
coef(regfit.fwd,8)
coef(regfit.fwd,9)

#backward selection
par(mfrow=c(2,2))
plot(reg.bwd.sum$rss,xlab = "Number of Variables",ylab="RSS",type="b")
plot(reg.bwd.sum$adjr2,xlab = "Number of variables",ylab="ARSq",type="b")
best_adjr2 = which.max(reg.bwd.sum$adjr2)
points(best_adjr2, reg.bwd.sum$adjr2[best_adjr2], 
       col = "red", cex = 2, pch = 20)
plot(reg.bwd.sum$bic,xlab = "Number of variables",ylab="bic",type="b")
best_bic = which.min(reg.bwd.sum$bic)
points(best_bic, reg.bwd.sum$bic[best_bic], 
       col = "red", cex = 2, pch = 20)
plot(reg.bwd.sum$cp,xlab = "Number of variables",ylab="cp",type="b")
best_cp = which.min(reg.bwd.sum$cp)
points(best_cp, reg.bwd.sum$cp[best_cp], 
       col = "red", cex = 2, pch = 20)
plot(reg.bwd.sum$rsq,xlab = "Number of variables",ylab="RSq",type="b")

which.max(reg.bwd.sum$adjr2)
which.min(reg.bwd.sum$cp)
which.min(reg.bwd.sum$bic)
coef(regfit.bwd,9)
coef(regfit.bwd,8)
coef(regfit.bwd,7)

#--------------the 3 different models pulled out by subset selection methods----------

#9 var model
lm.fit9 = lm(points~SPG + PST + PSTG + PR+ PCS + GCPG + CRPG + PTS + PSV, data=df2)
summary(lm.fit9)
lm.pred9.train = predict(lm.fit9,df2,type="response")
mean((df2$points-lm.pred9.train)^2) #training mse
lm.pred9.test = predict(lm.fit9,datapremtest,type="response")
mean((datapremtest$points-lm.pred9.test)^2) #testing mse
table(lm.pred9.test,datapremtest$points)

#get number of positions exactly correct
ordered = read.csv("orders.csv",header=T)
tab9 = data.frame(club = datapremtest$club, points = datapremtest$points, pred_points9 = lm.pred9.test)
tab9 = tab9[order(tab9$pred_points9,decreasing=TRUE),]
sum9 = 0
for (i in 1:20){
  if (tab9[i,1] == ordered[i,1]){
    sum9 = sum9+1
  }
}

#get total number of positions out
tot_pos_out9 = 0
for (i in 1:20){
  club_i = tab9[i,1]
  pos_1 = as.numeric(which(tab9$club ==club_i))
  pos_2 = as.numeric(which(ordered$X21.22==club_i))
  inc = abs(pos_1-pos_2)
  tot_pos_out9 = tot_pos_out9 + inc
}

#8 var model
lm.fit8 = lm(points~SPG + PST + PSTG + PCS + GCPG + CRPG +PTS +PSV+I(PTS^2), data=df2)
lm.fit8n = lm(points~SPG + PST + PSTG + PCS + GCPG + CRPG + PTS +PSV+I(PTS^2), data=df2)
summary(lm.fit8)
lm.pred8.train = predict(lm.fit8,df2,type="response")
mean((df2$points-lm.pred8.train)^2)
lm.pred8.test = predict(lm.fit8,datapremtest,type="response")
mean((datapremtest$points-lm.pred8.test)^2)
table(lm.pred8.test,datapremtest$points)

tab8 = data.frame(club = datapremtest$club, points = datapremtest$points, pred_points8 = lm.pred8.test)
tab8 = tab8[order(tab8$pred_points8,decreasing=TRUE),]
sum8 = 0
for (i in 1:20){
  if (tab8[i,1] == ordered[i,1]){
    sum8 = sum8+1
  }
}

#get total number of positions out
tot_pos_out8 = 0
for (i in 1:20){
  club_i = tab8[i,1]
  pos_1 = as.numeric(which(tab8$club ==club_i))
  pos_2 = as.numeric(which(ordered$X21.22==club_i))
  inc = abs(pos_1-pos_2)
  tot_pos_out8 = tot_pos_out8 + inc
}

#7 var model
lm.fit7 = lm(points~SPG + PST + PSTG + PCS + GCPG + PTS + PSV, data=df2)
summary(lm.fit7)
lm.pred7.train = predict(lm.fit7,df2,type="response")
mean((df2$points-lm.pred7.train)^2)
lm.pred7.test = predict(lm.fit7,datapremtest,type="response")
mean((datapremtest$points-lm.pred7.test)^2)
table(lm.pred7.test,datapremtest$points)

tab7 = data.frame(club = datapremtest$club, points = datapremtest$points, pred_points7 = lm.pred7.test)
tab7 = tab7[order(tab7$pred_points7,decreasing=TRUE),]
sum7 = 0
for (i in 1:20){
  if (tab7[i,1] == ordered[i,1]){
    sum7 = sum7+1
  }
}

#get total number of positions out
tot_pos_out7 = 0
for (i in 1:20){
  club_i = tab7[i,1]
  pos_1 = as.numeric(which(tab7$club ==club_i))
  pos_2 = as.numeric(which(ordered$X21.22==club_i))
  inc = abs(pos_1-pos_2)
  tot_pos_out7 = tot_pos_out7 + inc
}


#-----------------partial regression plots------------------------------------

library(tidyverse)
partial_regression = df2

#partial regression plots 8 var model
#SPG
mod_wo_SPG = lm(points~PST+PSTG+PCS+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$SPG_residuals = residuals(mod_wo_SPG)
mod_SPG_against = lm(SPG~PST+PSTG+PCS+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$SPG_against_residuals = residuals(mod_SPG_against)
PRSPG = ggplot(partial_regression,aes(x=SPG_against_residuals,y=SPG_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: SPG removed")+ xlab("RES: SPG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PST
mod_wo_PST = lm(points~SPG+PSTG+PCS+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$PST_residuals = residuals(mod_wo_PST)
mod_PST_against = lm(PST~SPG+PSTG+PCS+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$PST_against_residuals = residuals(mod_PST_against)
PRPST = ggplot(partial_regression,aes(x=PST_against_residuals,y=PST_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PST removed")+ xlab("RES: PST versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PSTG
mod_wo_PSTG = lm(points~SPG+PST+PCS+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$PSTG_residuals = residuals(mod_wo_PSTG)
mod_PSTG_against = lm(PSTG~SPG+PST+PCS+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$PSTG_against_residuals = residuals(mod_PSTG_against)
PRPSTG = ggplot(partial_regression,aes(x=PSTG_against_residuals,y=PSTG_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PSTG removed")+ xlab("RES: PSTG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PCS
mod_wo_PCS = lm(points~SPG+PST+PSTG+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$PCS_residuals = residuals(mod_wo_PCS)
mod_PCS_against = lm(PCS~SPG+PST+PSTG+GCPG+CRPG+PTS+PSV,data=df2)
partial_regression$PCS_against_residuals = residuals(mod_PCS_against)
PRPCS = ggplot(partial_regression,aes(x=PCS_against_residuals,y=PCS_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PCS removed")+ xlab("RES: PCS versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#GCPG
mod_wo_GCPG = lm(points~SPG+PST+PSTG+PCS+CRPG+PTS+PSV,data=df2)
partial_regression$GCPG_residuals = residuals(mod_wo_GCPG)
mod_GCPG_against = lm(GCPG~SPG+PST+PSTG+PCS+CRPG+PTS+PSV,data=df2)
partial_regression$GCPG_against_residuals = residuals(mod_GCPG_against)
PRGCPG = ggplot(partial_regression,aes(x=GCPG_against_residuals,y=GCPG_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: GCPG removed")+ xlab("RES: GCPG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#CRPG
mod_wo_CRPG = lm(points~SPG+PST+PSTG+PCS+GCPG+PTS+PSV,data=df2)
partial_regression$CRPG_residuals = residuals(mod_wo_CRPG)
mod_CRPG_against = lm(CRPG~SPG+PST+PSTG+PCS+GCPG+PTS+PSV,data=df2)
partial_regression$CRPG_against_residuals = residuals(mod_CRPG_against)
PRCRPG = ggplot(partial_regression,aes(x=CRPG_against_residuals,y=CRPG_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: CRPG removed")+ xlab("RES: CRPG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PTS
mod_wo_PTS = lm(points~SPG+PST+PSTG+PCS+GCPG+CRPG+PSV,data=df2)
partial_regression$PTS_residuals = residuals(mod_wo_PTS)
mod_PTS_against = lm(PTS~SPG+PST+PSTG+PCS+GCPG+CRPG+PSV,data=df2)
partial_regression$PTS_against_residuals = residuals(mod_PTS_against)
PRPTS = ggplot(partial_regression,aes(x=PTS_against_residuals,y=PTS_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PTS removed")+ xlab("RES: PTS versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PSV
mod_wo_PSV = lm(points~SPG+PST+PSTG+PCS+GCPG+CRPG+PTS,data=df2)
partial_regression$PSV_residuals = residuals(mod_wo_PSV)
mod_PSV_against = lm(PSV~SPG+PST+PSTG+PCS+GCPG+CRPG+PTS,data=df2)
partial_regression$PSV_against_residuals = residuals(mod_PSV_against)
PRPSV = ggplot(partial_regression,aes(x=PSV_against_residuals,y=PSV_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PSV removed")+ xlab("RES: PSV versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#UPDATED partial regression plots from new non-linear model
#SPG
mod_wo_SPG1 = lm(points~PST+PSTG+PCS+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$SPG1_residuals = residuals(mod_wo_SPG1)
mod_SPG1_against = lm(SPG~PST+PSTG+PCS+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$SPG1_against_residuals = residuals(mod_SPG1_against)
PRSPG1 = ggplot(partial_regression,aes(x=SPG1_against_residuals,y=SPG1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: SPG removed")+ xlab("RES: SPG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PST
mod_wo_PST1 = lm(points~SPG+PSTG+PCS+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$PST1_residuals = residuals(mod_wo_PST1)
mod_PST1_against = lm(PST~SPG+PSTG+PCS+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$PST1_against_residuals = residuals(mod_PST1_against)
PRPST1 = ggplot(partial_regression,aes(x=PST1_against_residuals,y=PST1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PST removed")+ xlab("RES: PST versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PSTG
mod_wo_PSTG1 = lm(points~SPG+PST+PCS+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$PSTG1_residuals = residuals(mod_wo_PSTG1)
mod_PSTG1_against = lm(PSTG~SPG+PST+PCS+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$PSTG1_against_residuals = residuals(mod_PSTG1_against)
PRPSTG1 = ggplot(partial_regression,aes(x=PSTG1_against_residuals,y=PSTG1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PSTG removed")+ xlab("RES: PSTG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PCS
mod_wo_PCS1 = lm(points~SPG+PST+PSTG+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$PCS1_residuals = residuals(mod_wo_PCS1)
mod_PCS1_against = lm(PCS~SPG+PST+PSTG+GCPG+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$PCS1_against_residuals = residuals(mod_PCS1_against)
PRPCS1 = ggplot(partial_regression,aes(x=PCS1_against_residuals,y=PCS1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PCS removed")+ xlab("RES: PCS versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#GCPG
mod_wo_GCPG1 = lm(points~SPG+PST+PSTG+PCS+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$GCPG1_residuals = residuals(mod_wo_GCPG1)
mod_GCPG1_against = lm(GCPG~SPG+PST+PSTG+PCS+CRPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$GCPG1_against_residuals = residuals(mod_GCPG1_against)
PRGCPG1 = ggplot(partial_regression,aes(x=GCPG1_against_residuals,y=GCPG1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: GCPG removed")+ xlab("RES: GCPG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#CRPG
mod_wo_CRPG1 = lm(points~SPG+PST+PSTG+PCS+GCPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$CRPG1_residuals = residuals(mod_wo_CRPG1)
mod_CRPG1_against = lm(CRPG~SPG+PST+PSTG+PCS+GCPG+PTS+PSV+I(PTS^2),data=df2)
partial_regression$CRPG1_against_residuals = residuals(mod_CRPG1_against)
PRCRPG1 = ggplot(partial_regression,aes(x=CRPG1_against_residuals,y=CRPG1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: CRPG removed")+ xlab("RES: CRPG versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PTS
mod_wo_PTS1 = lm(points~SPG+PST+PSTG+PCS+GCPG+CRPG+PSV+I(PTS^2),data=df2)
partial_regression$PTS1_residuals = residuals(mod_wo_PTS1)
mod_PTS1_against = lm(PTS~SPG+PST+PSTG+PCS+GCPG+CRPG+PSV+I(PTS^2),data=df2)
partial_regression$PTS1_against_residuals = residuals(mod_PTS1_against)
PRPTS1 = ggplot(partial_regression,aes(x=PTS1_against_residuals,y=PTS1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PTS removed")+ xlab("RES: PTS versus other predictors")+
  theme(axis.title = element_text(size = 8)) 

#PSV
mod_wo_PSV1 = lm(points~SPG+PST+PSTG+PCS+GCPG+CRPG+PTS+I(PTS^2),data=df2)
partial_regression$PSV1_residuals = residuals(mod_wo_PSV1)
mod_PSV1_against = lm(PSV~SPG+PST+PSTG+PCS+GCPG+CRPG+PTS+I(PTS^2),data=df2)
partial_regression$PSV1_against_residuals = residuals(mod_PSV1_against)
PRPSV1 = ggplot(partial_regression,aes(x=PSV1_against_residuals,y=PSV1_residuals))+geom_point()+
  geom_smooth(method='lm',se=F)+
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y~x + I(x^2))+
  ylab("RES: PSV removed")+ xlab("RES: PSV versus other predictors")+
  theme(axis.title = element_text(size = 8)) 


ggarrange(PRSPG, PRPST, PRPSTG, PRPCS, PRGCPG, PRCRPG, PRPTS, PRPSV + rremove("x.text"), 
          ncol = 3, nrow = 3)

ggarrange(PRSPG1, PRPST1, PRPSTG1, PRPCS1, PRGCPG1, PRCRPG1, PRPTS1, PRPSV1 + rremove("x.text"), 
          ncol = 3, nrow = 3)


#----------------------results for updated 8 variable model-----------------------
#8 var model
lm.fit8 = lm(points~SPG + PST + PSTG + PCS + GCPG + CRPG +PTS +PSV+I(PTS^2), data=df2)
summary(lm.fit8)
lm.pred8.train = predict(lm.fit8,df2,type="response")
mean((df2$points-lm.pred8.train)^2)
lm.pred8.test = predict(lm.fit8,datapremtest,type="response")
mean((datapremtest$points-lm.pred8.test)^2)
table(lm.pred8.test,datapremtest$points)

tab8 = data.frame(club = datapremtest$club, points = datapremtest$points, pred_points8 = lm.pred8.test)
tab8 = tab8[order(tab8$pred_points8,decreasing=TRUE),]
sum8 = 0
for (i in 1:20){
  if (tab8[i,1] == ordered[i,1]){
    sum8 = sum8+1
  }
}

#get total number of positions out
tot_pos_out8 = 0
for (i in 1:20){
  club_i = tab8[i,1]
  pos_1 = as.numeric(which(tab8$club ==club_i))
  pos_2 = as.numeric(which(ordered$X21.22==club_i))
  inc = abs(pos_1-pos_2)
  tot_pos_out8 = tot_pos_out8 + inc
}
