library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(tibble)
mydata<- read.csv('socclay.csv')
View(mydata)
names(mydata)
myformula<- y~x
mydata %>%
  ggplot(aes(clay, soc, fill=landuse))+ geom_point(size=1, aes(color=landuse))+
  geom_smooth(method='lm', formula = myformula, se=F, level=0.95, aes(color=landuse))+
  theme_classic()+ 
  stat_poly_eq(formula = myformula, aes(label= paste(..eq.label.., ..rr.label.., 
                                                     sep='~~~')), parse=TRUE, colour='blue')+
  facet_wrap(~landuse)+ coord_cartesian(xlim = c(0, 40))+ labs(x='Clay %', 
                                                               y='SOC (Mg.C/ha) ')
                           
library(ggcorrplot)
library(gridExtra)
library(cowplot)
amos<-read.csv('correlE.csv')
names(amos)
View(amos)
car<- round(cor(amos),1)
head(car)
p.mat<-cor_pmat(amos)
head(p.mat[, 1:4])
ggcorrplot(car, hc.order = TRUE, type='lower',lab=T)
kip<-select(amos, 1,4,5)
names(kip)
myformula<-y~x
par(mfrow=c(2,2))

p1<- ggplot(kip, aes(ph, contamination))+ geom_point(size=2, alpha=0.7)+ theme_classic()+
  geom_smooth(method='lm', se=F, formula=myformula,col='red')+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label..,
                                                  sep='~~~')),parse=TRUE, color='black')+
  labs(x='Ph of Enturire',y='Contamination')
p1
p2<-ggplot(kip, aes(amino.acids, contamination))+ geom_point()+ theme_bw()+
  geom_smooth(method='lm',se=F, formula=myformula)+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label.., 
                                                  sep='~~~')), parse=TRUE, color='blue' )+
  labs(x='Amino Acids concetration', y='contamination of Enturire')
p2
myd<-read.csv('corEF.csv')
names(myd)
p3<-ggplot(myd, aes(amin, cont))+ geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method='lm', se=F, formula=myformula, color='green')+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label.., 
                                                  sep='~~~')), parse=T)+
  theme_classic()+ labs(x='Amino acid concentration in Enturire',y='Contamination')
p3
p4<- ggplot(myd, aes(phh, cont))+ geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method='lm', se=F, formula=myformula)+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label.., 
                                                  sep='~~~')), parse=T)+
  theme_classic()+ labs(x='Ph of Enturire', y='Contamination')

p4
plot_grid(p1,p3,p4,labels='AUTO', hjust = c(-1,-1), vjust = c(1, 1))
dev.off()

model<- lm(contamination~ph+amino.acids, data=amos)
summary(model)
library(car)
avPlots(model)
anova(model)
plot(model)
mine<- read.csv('corEkit.csv')
names(mine)
p5<- ggplot(mine, aes(red, ekit))+ geom_point(size=2, alpha=0.7)+
  geom_smooth(method='lm', formula=myformula, se=F)+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label..,
                                                  sep='~~~')), parse=T)+
  theme_classic()+ labs(x='Reducing sugars of Ekitiribiita at room temp', y='Contamination')
p5
p6<-ggplot(mine, aes(amino, ekit))+ geom_point(size=2, alpha=0.7)+
  geom_smooth(method='lm', formula=myformula, se=F, color='red')+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label..,
                                                  sep='~~~')), parse=T)+
  theme_classic()+ labs(x='Amino acid in Ekitiribiita at Room temp', y='Contamination')
p6
p7<-ggplot(mine, aes(ph, ekit))+ geom_point(size=2, alpha=0.7)+
  geom_smooth(method='lm', formula=myformula, se=F, color='black')+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label..,
                                                  sep='~~~')), parse=T)+
  theme_classic()+ labs(x='Ph of Ekitiribita at Room temp', y='Contamination')

p8<-ggplot(mine, aes(phh, ekif))+ geom_point(size=2, alpha=0.7)+
  geom_smooth(method='lm', formula=myformula, se=F, color='yellow')+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label.., ..rr.label..,
                                                  sep='~~~')), parse=T)+
  theme_classic()+ labs(x='Ph of Ekitiribiita at Fridge temp', y='Contamination')
p8
plot_grid(p5,p6,p7,p8,labels='AUTO')
library(pastecs)
today<- read.csv('Room.csv')
names(today)
stat.desc(today)
res<- var.test(contamination~type, data=today)
res
re<- t.test(contamination~type, data=today, var.equal=TRUE)
re

getwd()
library(graphics)
mydata<- read.csv('food.csv')
frame<- as.table(as.matrix(mydata))
frame
mosaicplot(mydata, shade=TRUE, las=2, main='Food handling')
library(vcd)
assoc(mydata, shade=TRUE, las=3)
chisq.test(mydata)
mini<- read.csv('mini.csv')
names(mini)
mini %>%
  ggplot(aes(Days, logs.of.CFU))+ geom_point(size=1, aes(color=microbes))+
  geom_smooth(aes(color=microbes), se=F)+ theme_classic()+
  labs(x='Days', y='Mean log of CFU/ml')+coord_cartesian(xlim =c(1, 15))
mini %>%
  ggplot(aes(x=Days, y=microbes, fill=logs.of.CFU))+
  geom_tile()+
 theme_classic()+
  scale_fill_gradient(name='logs of\nCFU/ml',low='white',high='black', limits=c(0, NA))+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())
last<-read.csv('last.csv')
names(last)
myformula=y~x
last %>%
  ggplot(aes(x=days, y=microbes, fill=cfu)) +geom_tile() +theme_classic()+
  scale_fill_gradient(name='log of\nCFU/ml',low='white',high='red', limits=c(0,NA))+
  theme(axis.line = element_blank(),axis.ticks = element_blank())
obw<- read.csv('obw.csv')
names(obw)
p11<- ggplot(obw, aes(redu, obw))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Reducing sugars in Obwenkiga at Room Temp',y='Contamination')
p11
p12<-ggplot(obw, aes(obw, amine))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='blue')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acids in Obwenkiga at Room Temp',y='Contamination')
p12
p13<-ggplot(obw, aes(ph, amine))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='yellow')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH of Obwenkiga at Room Temp',y='Contamination')
p13
p14<-ggplot(obw, aes(phh, con))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH of Obwenkiga at Fridge Temp',y='Contamination')
p14
plot_grid(p11,p12,p13,p14, labels='AUTO')
obu<-read.csv('obut.csv')
names(obu)
p15<-ggplot(obu, aes(re, obu))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Reducing sugars in Obutiire at Room Temp',y='Contamination')
p16<-ggplot(obu, aes(amin, obu))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acids in Obutiire at Room Temp',y='Contamination')
p17<-ggplot(obu, aes(ph, obu))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='blue')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Obutiire at Room Temp',y='Contamination')
p17
plot_grid(p15,p16,p17, labels='AUTO')
p18<-ggplot(obu, aes(res, com))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Reducing sugars in Obutiire at Fridge Temp',y='Contamination')
p18
p19<-ggplot(obu, aes(amino, com))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acid in Obutiire at Fridge Temp',y='Contamination')
p20<-ggplot(obu, aes(phh, com))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='blue')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Obutiire at Fridge Temp',y='Contamination')
p20
plot_grid(p18,p19,p20, labels='AUTO')
fn<- read.csv('fungiE.csv')
names(fn)
p21<-ggplot(fn, aes(amino, fung))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acid in Enturire at Room Temp',y='Fungi Contamination')
p21
p22<-ggplot(fn, aes(ph, fung))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='blue')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Enturire at Room Temp',y='Fungi Contamination')
p23<-ggplot(fn, aes(amin, fun))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acid in Enturire at Fridge Temp',y='Fungi Contamination')
p24<-ggplot(fn, aes(phh, fun))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='yellow')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Enturire at Fridge Temp',y='Fungi Contamination')
p24
plot_grid(p21,p22,p23,p24, labels='AUTO',hjust = c(-2, -2), vjust = c(1,1))
ek<-read.csv('fungiekit.csv')
names(ek)
p25<-ggplot(ek, aes(red, ekitf))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Reducing sugars in Ekitibita at Room Temp',y='Fungi Contamination')
p25
p26<-ggplot(ek, aes(amino, ekitf))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acids in Ekitibita at Room Temp',y='Fungi Contamination')
p27<-ggplot(ek, aes(ph, ekitf))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='orange')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Ekitibita at Room Temp',y='Fungi Contamination')
p28<-ggplot(ek, aes(phh, ef))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='blue')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Ekitibita at Fridge Temp',y='Fungi Contamination')
p28
plot_grid(p25, p26, p27, p28, labels='AUTO', hjust = c(-2,-2))
oby<-read.csv('oby.csv')
names(oby)
p29<-ggplot(oby, aes(redu, obw))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Reducing sugars in Obwenkiga at Room Temp',y='Fungal Contamination')
p30<-ggplot(oby, aes(amine, obw))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acids in Obwenkiga at Room Temp',y='Fungal Contamination')
p31<-ggplot(oby, aes(ph, obw))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='orange')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Obwenkiga at Room Temp',y='Fungal Contamination')
p32<-ggplot(oby, aes(phh, ol))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Obwenkiga at Fridge Temp',y='Fungal Contamination')

plot_grid(p29, p30,p31,p32, labels='AUTO', hjust = c(-2,-2))
fin<- read.csv('fin.csv')
names(fin)
p33<-ggplot(fin, aes(amin, obi))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='black')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acids in Obutire at Room Temp',y='Fungal Contamination')
p34<-ggplot(fin, aes(ph, obi))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='orange')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pHin Obutire at Room Temp',y='Fungal Contamination')
p35<-ggplot(fin, aes(amino, ibu))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='red')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='Amino acids in Obutire at Fridge Temp',y='Fungal Contamination')
p36<-ggplot(fin, aes(phh, ibu))+geom_point(size=2, alpha=0.7)+ 
  geom_smooth(method = 'lm', se=F, formula=myformula, color='green')+theme_classic()+
  stat_poly_eq(formula=myformula, aes(label=paste(..eq.label..,..rr.label..,
                                                  sep='~~~')), parse=T)+
  labs(x='pH in Obutire at Fridge Temp',y='Fungal Contamination')
p36
plot_grid(p33, p34,p35,p36, labels='AUTO', hjust = c(-2,-2))
library(GGally)
library(scatterplot3d)
rele<- read.csv('relations.csv')
names(rele)
ggpairs(data=rele, columns = 1:5, title = 'contamination')
fit<-lm(bushera~ph+amino, data=rele)
fit
summary(fit)
amino<- seq(0.01, 5, by=0.0005)
ph<- seq(1,7, by=0.5)
prd<-expand.grid(amino=amino, ph=ph)
predco<- predict(fit, new=prd)
scr<- scatterplot3d(prd$ph,prd$amino,predco, 
                    angle = 60, color='dodgerblue',pch = 1,
                    xlab = 'pH', ylab='amino acids',zlab = 'contamination')
scr$points3d(rele$ph, rele$bushera, rele$bushera, pch=16)
predict(fit, data.frame(amino=1.1645, ph=4.64))
fiting<-lm(bushera~ph*amino, data=rele)
summary(fiting)
amin<- seq(0.01,5, by=0.005)
PH<-seq(1,7, by=0.5)
predg<-expand.grid(amin=amin,PH=PH)
predv<- predict(fiting, new=predg)
