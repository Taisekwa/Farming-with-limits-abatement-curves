library(ggplot2)
library(readr) 

mt<-read.csv("C:/Users/chikazhet/MACChauraki.csv")



mt$mitigation <- factor(mt$mitigation, levels= c("Nitrogen use efficiency", "nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"))
## plotting profit and N leaching
ggplot(data =mt, mapping = aes(y = cost, x = abatement, group= mitigation)) +
  scale_x_continuous(position="bottom") +
  scale_y_continuous(position="right")+
  geom_point(aes(color= mitigation),size=3)+ scale_colour_manual(values = c( "green","cyan","blue","red", "gray" ))+
  scale_fill_discrete(limits = c("Nitogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structures")) +
  labs(y="MAC($/kg N reduced)", x="Abatement pontential(% N leaching reduced)")  +
  theme(legend.position = "left") 


df<-read.csv("C:/Users/chikazhet/MACC2.csv")
df$mitigation <- factor(df$mitigation, levels= c("Nitrogen use efficiency", "nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"))
## plotting profit and N leaching
ggplot(data =df, mapping = aes(y = cost, x = abatement, group= mitigation)) +
  scale_x_continuous(position="bottom") +
  scale_y_continuous(position="right")+
  geom_point(aes(color= mitigation),size=3)+ scale_colour_manual(values = c( "green","cyan","blue","red", "gray" ))+
  scale_fill_discrete(limits = c("Nitogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structures")) +
  labs(y="MAC($/kg N reduced)", x="Abatement pontential(% N leaching reduced)")  +
  theme(legend.position = "left") +
  facet_wrap(~region)


