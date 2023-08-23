
#Load the necessary libraries
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)

df<-read.csv("C:/Users/chikazhet/Otagofwl.csv")

# All mitigations plotted
MyData2<- df

## plotting profit and N leaching
ggplot(data = MyData2, mapping = aes(y = profit_change, x = N_reduction, group= Farm)) +scale_x_reverse()+
  geom_line(aes(color= Farm),size=1)+ scale_colour_manual(values = c("green","red","yellow","pink","blue", "orange", "grey", "black","cyan", "purple","brown" ))+
  geom_point(aes(shape=Mitigation), size=3)+scale_shape_manual(values=c("base"=8, "GMP"=16,"GMP_plus"=7))+  
  labs(y="Change in operating profit(%)", x="Change in N leaching(%)") + #+ #theme(legend.title=element_blank()) #+ #theme(legend.position = "bottom")+
theme(axis.line = element_line(color="black")) +
  ggtitle(" Figure 1, mitigation impacts on N leaching and operating profit")

##plotting profit and GHG
ggplot(data = MyData2, mapping = aes(y = profit_change, x = GHG_reduction, group= Farm)) +scale_x_reverse()+
  geom_line(aes(color=Farm), size=1)+ scale_colour_manual(values = c("green","red","yellow","pink","blue", "orange", "grey", "black","cyan", "purple","brown" ))+
  geom_point(aes(shape=Mitigation), size=3)+scale_shape_manual(values=c("base"=8, "GMP"=16,"GMP_plus"=7))+  
  labs(y="Change in operating profit(%)", x="Change in GHG(%)")+ #+ #theme(legend.title=element_blank()) #+ #theme(legend.position = "bottom")+
  theme(axis.line = element_line(color="black")) +
  ggtitle("Figure 2, mitigation impacts on GHG emmissions and operating profit")

## Plotting profit and p loss
ggplot(data = MyData2, mapping = aes(y = profit_change, x = P_reduction, group= Farm)) +scale_x_reverse()+
  geom_line(aes(color=Farm), size=1)+ scale_colour_manual(values = c("green","red","yellow","pink","blue", "orange", "grey", "black","cyan", "purple","brown" ))+
  geom_point(aes(shape=Mitigation), size=3)+scale_shape_manual(values=c("base"=8, "GMP"=16,"GMP_plus"=7))+  
  labs(y="Change in operating profit(%)", x="Change in P loss(%)")+ #+ theme(legend.title=element_blank()) + theme(legend.position = "bottom")+
  theme(axis.line = element_line(color="black")) +
  ggtitle("Figure 3, mitigation impacts on P loss and operating profit")