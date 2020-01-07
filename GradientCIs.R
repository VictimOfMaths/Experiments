#Attempting to visualise 95% Confidence Intervals around a line using gradient fills

library(lofi)
library(minisvg)
library(devout)
library(devoutsvg)
library(svgpatternsimple)
library(poissoned)
library(ggplot2)

fadedown <- encode_pattern_params_as_hex_colour(pattern_name="gradient",angle=90, colour1="White", colour2="#0570b0")
fadeup <- encode_pattern_params_as_hex_colour(pattern_name="gradient",angle=270,  colour1="White", colour2="#0570b0")

grads <- c(fadedown, fadeup)

temp2 <- data.frame(x=c(0,1), y=c(0.5,0.5), ymin=c(0,0), ymax=c(1,1))

svgout(filename = "test.svg", pattern_pkg="svgpatternsimple", width=6, height=4)
ggplot(data=temp2)+
  geom_ribbon(aes(x, ymin=ymin, ymax=y, fill=grads[1]))+
  geom_ribbon(aes(x, ymin=y, ymax=ymax, fill=grads[2]))+
  geom_line(aes(x,y), colour="Black")+
  theme_classic()+
  ylim(0,1)+
  scale_fill_manual(values=grads, guide=FALSE)
invisible(dev.off()) 
  
