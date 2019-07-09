#########
#########
# Copier dans le dossier projet R :
# - le script Source.R,
# - et les fichers Data.csv (à remplir avec les absorbances moyennes),
# - ainsi que sd.csv (à remplir avec les écarts-type)
#
# Puis ajuster les 2 paramètres suivants :

# Nbr minimum de points utilisés : 

a=5

# Seuil inférieur pour le R² : 

seuil=0.96


#########
#########

library(ggplot2)
library(tidyverse)
theme_set(theme_gray())

### Import

Data=read.csv("data.csv", head=TRUE, sep=";")
SD=read.csv("sd.csv", head=TRUE, sep=";")

for (n in (2:ncol(Data)) )
{
Data[,n]=sub(",",".", Data[,n])
Data[,n]<- as.numeric(Data[,n])
SD[,n]=sub(",",".", SD[,n])
SD[,n]<- as.numeric(SD[,n])
}


### Boucle

lm = rep(1, ncol(Data)-1)

for (n in (1:(ncol(Data)-1)))
  {
  t=a
  repeat {
    if ((lm[n] <= seuil) | (t==nrow(Data))){break}
    Tab=Data[1:t,]
    lm[n] = summary(lm(Tab[,n+1] ~ Time, data = Tab))$r.squared
    t=t+1
  } 
  cat("Points conservés pour ", colnames(Tab)[n+1], " : ", t, "\n")
  }


#### Plot
col=ncol(Data)
row=nrow(Data)
Data=gather(Data, key=type, value=value, -Time)
SD=gather(SD, key=type, value=value, -Time)
Data$sd=SD[,3]

slope = vector ()
intercept = vector ()
vitesse = vector()
for (n in (2:col)) {
  slope=c(slope, rep(lm(Tab[,n] ~ Time, data = Tab)$coefficients[2], times=row))
  intercept=c(intercept, rep(Tab[1,n], times=row))
  vitesse[n-1]=lm(Tab[,n] ~ Time, data = Tab)$coefficients[2]
}

ggplot(Data, aes(x=Time, y=value, colour=type, group=type)) + 
  geom_line() +
  geom_point() +
  labs(title="GAPDH Activity Assay", 
     y="Absorbance",
     x="Time (min)") +
  scale_colour_discrete(name="Samples")+
  geom_linerange(aes(ymin=value-sd, ymax=value+sd)) +
  geom_abline(aes(intercept = intercept, slope = slope, color=type))

### Histo vitesses

histo = data.frame("Sample"=colnames(Tab)[-1], "Initial_speed"=vitesse)
ggplot(histo, aes(x=Sample, y=Initial_speed, colour=Sample, group=Sample)) +
  geom_col()