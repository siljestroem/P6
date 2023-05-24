
################################################################################
###########################  Kode til data analyse  ############################
################################################################################

# Pakker indlæses 
library(dplyr)
library(car)
library(magrittr)
library(ggplot2)
library(patchwork)
library(heplots)

# Dataen indlæses
fish = read.csv("fish.csv")

# Filterer datasættet, så det kun indeholder de ønskede observationer
fish = fish %>% filter(Species %in% c("Bream", "Parkki", "Perch", "Pike", 
                                      "Roach", "Smelt")) %>%
  filter(Length1>0, Length2>0, Length3>0, Height>0, Weight>0, Width>0)

# Datasættet for hver art oprettes
Bream = fish %>% filter(Species=="Bream") %>% select(-Species)
Parkki = fish %>% filter(Species=="Parkki") %>% select(-Species)
Perch = fish %>% filter(Species=="Perch") %>% select(-Species)
Pike = fish %>% filter(Species=="Pike") %>% select(-Species)
Roach = fish %>% filter(Species=="Roach") %>% select(-Species)
Smelt = fish %>% filter(Species=="Smelt") %>% select(-Species) 

# Antallet af observationer for hver art 
summary(factor(fish$Species))

## Boksplot ##
df=data.frame(fish)
names(df) = c("Arter", "(a) Vægt", "(b) Længde1","(c) Længde2","(d) Længde3", "(e) Højde", "(f) Bredde")
long <- reshape2::melt(df, id.vars = c("Arter"))
ggplot(long, aes( x = Arter, y = value, group = interaction(Arter), fill=Arter)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap( ~ variable, scales = "free")+labs(y="Værdier")

## Scatterplot ##

names(df) = c("Arter", "Vægt", "Længde1","Længde2","Længde3", "Højde", "Bredde")

#Længdemålingerne overfor hinanden
L1 = ggplot(df, aes(Længde1, Længde2, color=Arter))+geom_point()+ggtitle("(a)")+theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  stat_ellipse()
L2 = ggplot(df, aes(Længde1, Længde3, color=Arter))+geom_point()+ggtitle("(b)")+theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  stat_ellipse()
L3 = ggplot(df, aes(Længde2, Længde3, color=Arter))+geom_point()+ggtitle("(c)")+theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()
#Plotter de tre ovenstående sammen
L1+L2+L3+plot_layout(ncol = 3, nrow = 1, guides = "collect")

#Længdemålingerne, Højde og Bredde
L1_H=ggplot(df, aes(Længde1, Højde, color=Arter)) + geom_point()+ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()
L2_H=ggplot(df, aes(Længde2, Højde, color=Arter)) + geom_point()+ ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()
L3_H=ggplot(df, aes(Længde3, Højde, color=Arter)) + geom_point()+ ggtitle("(c)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()
H_B=ggplot(df, aes(Bredde, Højde, color=Arter))+geom_point()+ ggtitle("(d)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()
#Plotter de fire ovenstående sammen
L1_H+L2_H+L3_H+H_B+plot_layout(ncol = 2, nrow = 2, guides = "collect")

#Bredde og Længdemålingerne 
B1 = ggplot(df, aes(Længde1, Bredde, color=Arter))+geom_point()+ ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  stat_ellipse()
B2 = ggplot(df, aes(Længde2, Bredde, color=Arter))+geom_point()+ ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  stat_ellipse()
B3 = ggplot(df, aes(Længde3, Bredde, color=Arter))+geom_point()+ ggtitle("(c)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()
#Plotter de tre ovenstående sammen
B1+B2+B3+plot_layout(ncol = 3, nrow = 1, guides = "collect")

#Vægt og Længde-målingerne 
V1 = ggplot(df, aes(Længde1, Vægt, color=Arter))+geom_point()+ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  stat_ellipse()+ylab("Vægt")
V2 = ggplot(df, aes(Længde2, Vægt, color=Arter))+geom_point()+ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  stat_ellipse()+ylab("Vægt")
V3 = ggplot(df, aes(Længde3, Vægt, color=Arter))+geom_point()+ggtitle("(c)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()+ylab("Vægt")
#Plotter de tre ovenstående sammen
V1+V2+V3+plot_layout(ncol = 3, nrow = 1, guides = "collect")

#Logaritmen af Vægt og Længde-målingerne 
V1 = ggplot(df, aes(Længde1, log(Vægt), color=Arter))+geom_point()+ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  stat_ellipse()+ylab("Vægt")
V2 = ggplot(df, aes(Længde2, log(Vægt), color=Arter))+geom_point()+ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  stat_ellipse()+ylab("Vægt")
V3 = ggplot(df, aes(Længde3, log(Vægt), color=Arter))+geom_point()+ggtitle("(c)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()+ylab("Vægt")
#Plotter de tre ovenstående sammen
V1+V2+V3+plot_layout(ncol = 3, nrow = 1, guides = "collect")

#Vægt og Bredde
VB = ggplot(df, aes(Bredde, log(Vægt), color=Arter)) + geom_point() + ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+ 
  stat_ellipse()+ylab("Vægt") 
#Vægt og Højde 
VH = ggplot(df, aes(Højde, log(Vægt), color=Arter))+geom_point()+ ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()+ylab("Vægt")
#Plotter de to ovenstående sammen
VB+VH+plot_layout(ncol = 2, nrow = 1, guides = "collect")

#Logaritmen af Vægt og Bredde
VB = ggplot(df, aes(Bredde, Vægt, color=Arter)) + geom_point() + ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+ 
  stat_ellipse()+ylab("Vægt") 
#Logaritmen af Vægt og Højde 
VH = ggplot(df, aes(Højde, Vægt, color=Arter))+geom_point()+ ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_ellipse()+ylab("Vægt")
#Plotter de to ovenstående sammen
VB+VH+plot_layout(ncol = 2, nrow = 1, guides = "collect")

## Log transformering af Vægt ##
fish = fish %>% mutate(Weight_log = log(Weight)) %>% select(-Weight)
fish = fish[,c(1,6,5,2,3,4,7)]

## Datasæt for hver art med log-transformerede vægte ##
Bream = fish %>% filter(Species=="Bream") %>% select(-Species) 
Parkki = fish %>% filter(Species=="Parkki") %>% select(-Species)
Perch = fish %>% filter(Species=="Perch") %>% select(-Species)
Pike = fish %>% filter(Species=="Pike") %>% select(-Species)
Roach = fish %>% filter(Species=="Roach") %>% select(-Species)
Smelt = fish %>% filter(Species=="Smelt") %>% select(-Species) 

## Middelværdi estimater ##
colMeans(Bream)
colMeans(Parkki)
colMeans(Perch)
colMeans(Pike)
colMeans(Roach)
colMeans(Smelt)

## Varians estimater ##
diag(cov(Bream))
diag(cov(Parkki))
diag(cov(Perch))
diag(cov(Pike))
diag(cov(Roach))
diag(cov(Smelt))

## QQ-plots med konfidens intervaller ##
par(mfrow = c(2,3))
Variables = c("Bredde", "Højde", "Længde1", "Længde2", "Længde3", "Vægt")

for (i in 1:6){
  qqPlot(Bream[,i], main=Variables[i], pch=16, lwd=1, xlab="Teoretiske fraktiler", 
         ylab="Stikprøve fraktiler", envelope=c(alpha=0.05, style="lines"), 
         id=FALSE, cex=1, line = "robust"); 
  mtext("    Bream", side = 3, line = -1.5, outer = TRUE)}

for (i in 1:6){
  qqPlot(Parkki[,i], main=Variables[i], pch=16, lwd=1, xlab="Teoretiske fraktiler", 
         ylab="Stikprøve fraktiler", envelope=c(alpha=0.05, style="lines"), 
         id=FALSE, cex=1, line = "robust"); 
  mtext("    Parkki", side = 3, line = - 1.5, outer = TRUE)}

for (i in 1:6){
  qqPlot(Perch[,i], main=Variables[i], pch=16, lwd=1, xlab="Teoretiske fraktiler", 
         ylab="Stikprøve fraktiler", envelope=c(alpha=0.05, style="lines"), 
         id=FALSE, cex=1, line = "robust"); 
  mtext("    Perch", side = 3, line = - 1.5, outer = TRUE)}

for (i in 1:6){
  qqPlot(Pike[,i], main=Variables[i], pch=16, lwd=1, xlab="Teoretiske fraktiler", 
         ylab="Stikprøve fraktiler", envelope=c(alpha=0.05, style="lines"), 
         id=FALSE, cex=1, line = "robust"); 
  mtext("    Pike", side = 3, line = - 1.5, outer = TRUE)}

for (i in 1:6){
  qqPlot(Roach[,i], main=Variables[i], pch=16, lwd=1, xlab="Teoretiske fraktiler", 
         ylab="Stikprøve fraktiler", envelope=c(alpha=0.05, style="lines"), 
         id=FALSE, cex=1, line = c("robust")); 
  mtext("    Roach", side = 3, line = - 1.5, outer = TRUE)}

for (i in 1:6){
  qqPlot(Smelt[,i], main=Variables[i], pch=16, lwd=1, xlab="Teoretiske fraktiler", 
         ylab="Stikprøve fraktiler", envelope=c(alpha=0.05, style="lines"), 
         id=FALSE, cex=1, line = "robust"); 
  mtext("    Smelt", side = 3, line = - 1.5, outer = TRUE)}

## Varians inflations faktor ##

#Bream:
Multikolin_breambredde <- lm(Width ~ Length1 + Length2 + Length3 + Height + Weight_log, data = Bream)
summary(Multikolin_breambredde)$r.squared #R^2=0.9103
1/(1-summary(Multikolin_breambredde)$r.squared) #VIF 11.14401
Multikolin_breamhøjde <- lm(Height ~ Length1 + Length2 + Length3 + Width + Weight_log, data = Bream)
summary(Multikolin_breamhøjde)$r.squared #R^2=0.9422
1/(1-summary(Multikolin_breamhøjde)$r.squared) #VIF 17.29098
Multikolin_breamlængde1 <- lm(Length1 ~ Height + Length2 + Length3 + Width + Weight_log, data = Bream)
summary(Multikolin_breamlængde1)$r.squared #R^2=0.9973
1/(1-summary(Multikolin_breamlængde1)$r.squared) #VIF 374.2365
Multikolin_breamlængde2 <- lm(Length2 ~ Height + Length1 + Length3 + Width + Weight_log, data = Bream)
summary(Multikolin_breamlængde2)$r.squared #R^2=0.9981
1/(1-summary(Multikolin_breamlængde2)$r.squared) #VIF 530.2595
Multikolin_breamlængde3 <- lm(Length3 ~ Height + Length1 + Length2 + Width + Weight_log, data = Bream)
summary(Multikolin_breamlængde3)$r.squared #R^2=0.9973
1/(1-summary(Multikolin_breamlængde3)$r.squared) #VIF 365.84
Multikolin_breamvægt <- lm(Weight_log ~ Height + Length1 + Length2 + Length3 + Width, data = Bream)
summary(Multikolin_breamvægt)$r.squared #R^2=0.9098
1/(1-summary(Multikolin_breamvægt)$r.squared) #VIF 11.08752

#Parkki:
Multikolin_parkkibredde <- lm(Width ~ Length1 + Length2 + Length3 + Height + Weight_log, data = Parkki)
summary(Multikolin_parkkibredde)$r.squared #R^2=0.9814
1/(1-summary(Multikolin_parkkibredde)$r.squared) #VIF 53.83946
Multikolin_parkkihøjde <- lm(Height ~ Length1 + Length2 + Length3 + Width + Weight_log, data = Parkki)
summary(Multikolin_parkkihøjde)$r.squared #R^2=0.9720
1/(1-summary(Multikolin_parkkihøjde)$r.squared) #VIF 35.77676
Multikolin_parkkilængde1 <- lm(Length1 ~ Height + Length2 + Length3 + Width + Weight_log, data = Parkki)
summary(Multikolin_parkkilængde1)$r.squared #R^2=0.9999
1/(1-summary(Multikolin_parkkilængde1)$r.squared) #VIF 15890.68
Multikolin_parkkilængde2 <- lm(Length2 ~ Height + Length1 + Length3 + Width + Weight_log, data = Parkki)
summary(Multikolin_parkkilængde2)$r.squared #R^2=1
1/(1-summary(Multikolin_parkkilængde2)$r.squared) #VIF 51585.45
Multikolin_parkkilængde3 <- lm(Length3 ~ Height + Length1 + Length2 + Width + Weight_log, data = Parkki)
summary(Multikolin_parkkilængde3)$r.squared #R^2=0.9999
1/(1-summary(Multikolin_parkkilængde3)$r.squared) #VIF 17527.66
Multikolin_parkkivægt <- lm(Weight_log ~ Height + Length1 + Length2 + Length3 + Width, data = Parkki)
summary(Multikolin_parkkivægt)$r.squared #R^2=0.9836
1/(1-summary(Multikolin_parkkivægt)$r.squared) #VIF 61.01805

#Perch
Multikolin_perchbredde <- lm(Width ~ Length1 + Length2 + Length3 + Height + Weight_log, data = Perch)
summary(Multikolin_perchbredde)$r.squared #R^2=0.9677
1/(1-summary(Multikolin_perchbredde)$r.squared) #VIF 30.96359
Multikolin_perchhøjde <- lm(Height ~ Length1 + Length2 + Length3 + Width + Weight_log, data = Perch)
summary(Multikolin_perchhøjde)$r.squared #R^2=0.9816
1/(1-summary(Multikolin_perchhøjde)$r.squared) #VIF 54.24102
Multikolin_perchlængde1 <- lm(Length1 ~ Height + Length2 + Length3 + Width + Weight_log, data = Perch)
summary(Multikolin_perchlængde1)$r.squared #R^2=0.9994
1/(1-summary(Multikolin_perchlængde1)$r.squared) #VIF 1805.269
Multikolin_perchlængde2 <- lm(Length2 ~ Height + Length1 + Length3 + Width + Weight_log, data = Perch)
summary(Multikolin_perchlængde2)$r.squared #R^2=0.9998
1/(1-summary(Multikolin_perchlængde2)$r.squared) #VIF 4659.689
Multikolin_perchlængde3 <- lm(Length3 ~ Height + Length1 + Length2 + Width + Weight_log, data = Perch)
summary(Multikolin_perchlængde3)$r.squared #R^2=0.9996
1/(1-summary(Multikolin_perchlængde3)$r.squared) #VIF 2383.533
Multikolin_perchvægt <- lm(Weight_log ~ Height + Length1 + Length2 + Length3 + Width, data = Perch)
summary(Multikolin_perchvægt)$r.squared #R^2=0.9443
1/(1-summary(Multikolin_perchvægt)$r.squared) #VIF 17.94468

#Pike
Multikolin_pikebredde <- lm(Width ~ Length1 + Length2 + Length3 + Height + Weight_log, data = Pike)
summary(Multikolin_pikebredde)$r.squared #R^2=0.9682
1/(1-summary(Multikolin_pikebredde)$r.squared) #VIF 31.45057
Multikolin_pikehøjde <- lm(Height ~ Length1 + Length2 + Length3 + Width + Weight_log, data = Pike)
summary(Multikolin_pikehøjde)$r.squared #R^2=0.9692
1/(1-summary(Multikolin_pikehøjde)$r.squared) #VIF 32.42524
Multikolin_pikelængde1 <- lm(Length1 ~ Height + Length2 + Length3 + Width + Weight_log, data = Pike)
summary(Multikolin_pikelængde1)$r.squared #R^2=0.9998
1/(1-summary(Multikolin_pikelængde1)$r.squared) #VIF 5698.096
Multikolin_pikelængde2 <- lm(Length2 ~ Height + Length1 + Length3 + Width + Weight_log, data = Pike)
summary(Multikolin_pikelængde2)$r.squared #R^2=0.9999
1/(1-summary(Multikolin_pikelængde2)$r.squared) #VIF 6889.708
Multikolin_pikelængde3 <- lm(Length3 ~ Height + Length1 + Length2 + Width + Weight_log, data = Pike)
summary(Multikolin_pikelængde3)$r.squared #R^2=0.9987
1/(1-summary(Multikolin_pikelængde3)$r.squared) #VIF  785.558
Multikolin_pikevægt <- lm(Weight_log ~ Height + Length1 + Length2 + Length3 + Width, data = Pike)
summary(Multikolin_pikevægt)$r.squared #R^2=0.9806
1/(1-summary(Multikolin_pikevægt)$r.squared) #VIF 51.4982

#Roach
Multikolin_roachbredde <- lm(Width ~ Length1 + Length2 + Length3 + Height + Weight_log, data = Roach)
summary(Multikolin_roachbredde)$r.squared #R^2=0.9547
1/(1-summary(Multikolin_roachbredde)$r.squared) #VIF 22.07548
Multikolin_roachhøjde <- lm(Height ~ Length1 + Length2 + Length3 + Width + Weight_log, data = Roach)
summary(Multikolin_roachhøjde)$r.squared #R^2=0.9495
1/(1-summary(Multikolin_roachhøjde)$r.squared) #VIF 19.80229
Multikolin_roachlængde1 <- lm(Length1 ~ Height + Length2 + Length3 + Width + Weight_log, data = Roach)
summary(Multikolin_roachlængde1)$r.squared #R^2=0.9977
1/(1-summary(Multikolin_roachlængde1)$r.squared) #VIF 432.9214
Multikolin_roachlængde2 <- lm(Length2 ~ Height + Length1 + Length3 + Width + Weight_log, data = Roach)
summary(Multikolin_roachlængde2)$r.squared #R^2=0.9983
1/(1-summary(Multikolin_roachlængde2)$r.squared) #VIF 595.5434
Multikolin_roachlængde3 <- lm(Length3 ~ Height + Length1 + Length2 + Width + Weight_log, data = Roach)
summary(Multikolin_roachlængde3)$r.squared #R^2=0.9960
1/(1-summary(Multikolin_roachlængde2)$r.squared) #VIF 595.5434
Multikolin_roachvægt <- lm(Weight_log ~ Height + Length1 + Length2 + Length3 + Width, data = Roach)
summary(Multikolin_roachvægt)$r.squared #R^2=0.9667
1/(1-summary(Multikolin_roachvægt)$r.squared) #VIF 30.00057

#Smelt
Multikolin_smeltbredde <- lm(Width ~ Length1 + Length2 + Length3 + Height + Weight_log, data = Smelt)
summary(Multikolin_smeltbredde)$r.squared #R^2=0.8325
1/(1-summary(Multikolin_smeltbredde)$r.squared) #VIF 5.968886
Multikolin_smelthøjde <- lm(Height ~ Length1 + Length2 + Length3 + Width + Weight_log, data = Smelt)
summary(Multikolin_smelthøjde)$r.squared #R^2=0.9307
1/(1-summary(Multikolin_smelthøjde)$r.squared) #VIF 14.43154
Multikolin_smeltlængde1 <- lm(Length1 ~ Height + Length2 + Length3 + Width + Weight_log, data = Smelt)
summary(Multikolin_smeltlængde1)$r.squared #R^2=0.9983
1/(1-summary(Multikolin_smeltlængde1)$r.squared) #VIF 578.5999
Multikolin_smeltlængde2 <- lm(Length2 ~ Height + Length1 + Length3 + Width + Weight_log, data = Smelt)
summary(Multikolin_smeltlængde2)$r.squared #R^2=0.9962
1/(1-summary(Multikolin_smeltlængde2)$r.squared) #VIF 261.9151
Multikolin_smeltlængde3 <- lm(Length3 ~ Height + Length1 + Length2 + Width + Weight_log, data = Smelt)
summary(Multikolin_smeltlængde3)$r.squared #R^2=0.9956
1/(1-summary(Multikolin_smeltlængde3)$r.squared) #VIF 228.5777
Multikolin_smeltvægt <- lm(Weight_log ~ Height + Length1 + Length2 + Length3 + Width, data = Smelt)
summary(Multikolin_smeltvægt)$r.squared #R^2=0.9617
1/(1-summary(Multikolin_smeltvægt)$r.squared) #VIF 26.10086

## Hotellings T^2 ##
Par_Roa = fish %>% filter(Species %in% c("Parkki", "Roach"))
fit_1 = lm(cbind(Weight_log, Height, Length1, Length2, Length3, Width)~Species-1, data = Par_Roa)
fit_2 = lm(cbind(Weight_log, Height, Length1, Length2, Length3, Width)~1, data = Par_Roa)
anova(fit_1, fit_2, test="Wilks")

## Estimat af kovarianser ##
fish.RoachParkki=cbind("Parkki","Roach")
for (n in fish.RoachParkki){
  lm.fit=lm(cbind(Width,Height,Length1,Length2,Length3,Weight_log)~1, data=fish[fish$Species==n,])
  SSD.mat =SSD(lm.fit)$SSD
  df =SSD(lm.fit)$df
  S=SSD.mat/df
  print(n)
  print(S)
}

## Test for identitet af kovariansmatricer ##
lm.fit=lm(cbind(Weight_log,Length1,Length2,Length3,Height,Width)~Species, data=fish)
boxM(lm.fit) #p-value = < 2.2e-16 hermed forkastes hypotesen. 

lm.fit=lm(cbind(Weight_log,Length1,Length2,Length3,Height,Width)~Species, 
          data=fish[fish$Species=="Parkki" | fish$Species=="Roach" ,])
boxM(lm.fit) #p-value = 9.452*10^{-5} hermed forkastes hypotesen.  

## Test for uafhængighed mellem komponenter ##
#Første hypotese med Bream
lm.fit=lm(cbind(Width,Height,Length1,Length2,Length3,Weight_log)~1, data=Bream)
SSD=SSD(lm.fit)$SSD #X^T(I_35-P)X for Bream
for (x in 1:6) {
  SSD1=SSD[x,x] #Kovariansmatricen for den første komponent bestående af 1 variabel.
  SSD2=SSD[-x,-x] #Kovariansmatricen for den anden komponent bestående af 5 variable. 
  Q2n=det(SSD)/(SSD1*det(SSD2)) 
  n=nrow(Bream)
  print(colnames(Bream[x])) 
  print(Q2n^(n/2)) #Printer Likelihood ratio testoren.
  if (x==6) {
    lm.fitalt1= lm(cbind(Length1,Length2,Length3,Height,Width)~1+Weight_log, data=Bream)
    lm.fitalt2= lm(cbind(Length1,Length2,Length3,Height,Width)~1, data=Bream)
  }
  if (x==3) {
    lm.fitalt1= lm(cbind(Weight_log,Length2,Length3,Height,Width)~1+Length1,data=Bream)
    lm.fitalt2= lm(cbind(Weight_log,Length2,Length3,Height,Width)~1,data=Bream)
  }
  if (x==4) {
    lm.fitalt1= lm(cbind(Weight_log,Length1,Length3,Height,Width)~1+Length2,data=Bream)
    lm.fitalt2= lm(cbind(Weight_log,Length1,Length3,Height,Width)~1,data=Bream)
  }
  if (x==5) {
    lm.fitalt1= lm(cbind(Weight_log,Length1,Length2,Height,Width)~1+Length3,data=Bream)
    lm.fitalt2= lm(cbind(Weight_log,Length1,Length2,Height,Width)~1,data=Bream)
  }
  if (x==2) {
    lm.fitalt1= lm(cbind(Weight_log,Length1,Length2,Length3,Width)~1+Height,data=Bream)
    lm.fitalt2= lm(cbind(Weight_log,Length1,Length2,Length3,Width)~1,data=Bream)
  }
  if (x==1) {
    lm.fitalt1= lm(cbind(Weight_log,Length1,Length2,Length3,Height)~1+Width,data=Bream)
    lm.fitalt2= lm(cbind(Weight_log,Length1,Length2,Length3,Height)~1,data=Bream)
  }
  print(anova(lm.fitalt1,lm.fitalt2,test="Wilks"))
}

# Anden hypotese for hver art 
fishspecies=cbind("Bream","Parkki","Perch","Pike","Roach","Smelt")
for (h in fishspecies){
  lm.fit=lm(cbind(Weight_log,Length1,Length2,Length3,Height,Width)~1, data=fish[fish$Species==h,])
  SSD=SSD(lm.fit)$SSD #X^T(I_n-P)X for den pågældende art
  print(h) 
  MSSDL1L2L3=SSD[2:4,2:4] #X^T(I_n-P)X delen for komponenten bestående af længderne
  USSDL1L2L3=SSD[-(2:4),-(2:4)] #X^T(I_n-P)X delen for komponenten uden længderne
  n=nrow(fish[fish$Species==h,]) #Antallet af observationer for den pågældende art
  Q2nL1L2L3=det(SSD)/(det(MSSDL1L2L3)*det(USSDL1L2L3))
  print(Q2nL1L2L3^(n/2))  #Printer Likelihood ratio testoren.
  print("Anova") 
  Mlm.fitL1L2L3=lm(cbind(Weight_log,Height,Width)~1+Length1+Length2+Length3, data=fish[fish$Species==h,])
  Ulm.fitL1L2L3=lm(cbind(Weight_log,Height,Width)~1, data=fish[fish$Species==h,])
  print(anova(Mlm.fitL1L2L3,Ulm.fitL1L2L3,test="Wilks")) 
}

## GLS og Ridge Regression ##

#Opsætning af seed, trænings og test datasættene. 
set.seed(123)
dt = sort(sample(nrow(Bream), nrow(Bream)*.8))
Train<-Bream[dt,] #Træningsdatasæt.
Test<-Bream[-dt,] #Test datasæt.
Dtrain=cbind(Train$Length1,Train$Length2,Train$Length3);Dtrain #Trænings designmatrix.
Xtrain=cbind(Train$Width,Train$Height,Train$Weight_log);Xtrain #Trænings responsvariblen.
Dtest=cbind(Test$Length1,Test$Length2,Test$Length3);Dtest #Test designmatrix.

#GLS prædiktion.
Bgls=(solve(t(Dtrain)%*%Dtrain))%*%t(Dtrain)%*%Xtrain;Bgls #GLS estimatet af regressionsparametermatricen.
XhGLS=Dtest%*%Bgls;XhGLS #GLS prædiktion.

#Ridge prædiktion.
n=ncol(Dtrain) 
I=diag(n) #Identitetsmaticen.
lamda=1 #lambda værdien.
Bhrid=solve(t(Dtrain)%*%Dtrain+lamda*I)%*%t(Dtrain)%*%Xtrain;Bhrid #Ridge estimatet af regressionsparametermatricen.
Xhrid=Dtest%*%Bhrid;Xhrid #Ridge prædiktion.

#Sammenligning af prædiktionerne.
Xtest=cbind(Test$Width,Test$Height,Test$Weight_log);Xtest #Den rigtige responsvariabel af test datasættet.
colSums((Xtest-XhGLS)^2) #RSS af hver variabel bredde, højde og vægt for GLS.
colSums((Xtest-Xhrid)^2) #RSS af hver variabel bredde, højde og vægt for GLS.

GLSRSS = 0
for(i in 1:nrow(Xtest)){
  current = (Xtest[i,]-XhGLS[i,])%*%(Xtest[i,]-XhGLS[i,])
  GLSRSS = GLSRSS + current
}
GLSRSS #Den totale RSS for GLS.

RIDGERSS = 0
for(i in 1:nrow(Xtest)){
  current = (Xtest[i,]-Xhrid[i,])%*%(Xtest[i,]-Xhrid[i,])
  RIDGERSS = RIDGERSS + current
}
RIDGERSS #Den totale RSS for ridge.


## Test for individuelle regressionsparametre ##

# Beregning af første hypotese
#Bream
fit_1 = lm(cbind(Length1, Length2, Length3) ~ Weight_log + Height + Width, data = Bream)
fit_2 = lm(cbind(Length1, Length2, Length3) ~ Weight_log, data = Bream)
anova(fit_1, fit_2, test = "Wilks")
#Parkki
fit_1 = lm(cbind(Length1, Length2, Length3) ~ Weight_log + Height + Width, data = Parkki)
fit_2 = lm(cbind(Length1, Length2, Length3) ~ Weight_log, data = Parkki)
anova(fit_1, fit_2, test = "Wilks")
#Perch
fit_1 = lm(cbind(Length1, Length2, Length3) ~ Weight_log + Height + Width, data = Perch)
fit_2 = lm(cbind(Length1, Length2, Length3) ~ Weight_log, data = Perch)
anova(fit_1, fit_2, test = "Wilks")
#Pike 
fit_1 = lm(cbind(Length1, Length2, Length3) ~ Weight_log + Height + Width, data = Pike)
fit_2 = lm(cbind(Length1, Length2, Length3) ~ Weight_log, data = Pike)
anova(fit_1, fit_2, test = "Wilks")
#Roach
fit_1 = lm(cbind(Length1, Length2, Length3) ~ Weight_log + Height + Width, data = Roach)
fit_2 = lm(cbind(Length1, Length2, Length3) ~ Weight_log, data = Roach)
anova(fit_1, fit_2, test = "Wilks")
#Smelt
fit_1 = lm(cbind(Length1, Length2, Length3) ~ Weight_log + Height + Width, data = Smelt)
fit_2 = lm(cbind(Length1, Length2, Length3) ~ Weight_log, data = Smelt)
anova(fit_1, fit_2, test = "Wilks")

# Beregning af anden hypotese
#Bream
fit_1 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3 + Height + Width, data = Bream)
fit_2 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3, data = Bream)
anova(fit_1, fit_2, test = "Wilks")
#Parkki
fit_1 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3 + Height + Width, data = Parkki)
fit_2 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3, data = Parkki)
anova(fit_1, fit_2, test = "Wilks")
#Perch
fit_1 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3 + Height + Width, data = Perch)
fit_2 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3, data = Perch)
anova(fit_1, fit_2, test = "Wilks")
#Pike
fit_1 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3 + Height + Width, data = Pike)
fit_2 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3, data = Pike)
anova(fit_1, fit_2, test = "Wilks")
#Roach
fit_1 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3 + Height + Width, data = Roach)
fit_2 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3, data = Roach)
anova(fit_1, fit_2, test = "Wilks")
#Smelt
fit_1 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3 + Height + Width, data = Smelt)
fit_2 = lm(cbind(Weight_log, Length1) ~ Length2 + Length3, data = Smelt)
anova(fit_1, fit_2, test = "Wilks")


