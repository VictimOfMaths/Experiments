#Attempting to visualise 95% Confidence Intervals around a line using gradient fills

library(lofi)
library(minisvg)
library(devout)
library(devoutsvg)
library(svgpatternsimple)
library(poissoned)
library(ggplot2)
library(tidyverse)

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

#############################################
#Hacky solution using lots of geom_ribbons:

n_ribbons <- 100 #This is probably overkill

#Generate the palette
colfunc <- colorRampPalette(c("#0570b0", "white"))
Gradpal <- colfunc(n_ribbons)

#Make up some data
data <- data.frame(x=c(0:10), y=c(30,19,18,15,37,7,34,48,31,25,27))
data$lowCI <- data$y-1.96*sqrt(data$y)
data$highCI <- data$y+1.96*sqrt(data$y)

#generate the max/min values for each ribbon
for (i in 1:n_ribbons){
  data[paste0("ylo", i)] <- data$y-(data$y-data$lowCI)*(i)/n_ribbons
  data[paste0("yhi", i)] <- data$y+(data$highCI-data$y)*(i)/n_ribbons
}

#plot the central ribbon
plot <- ggplot()+
  geom_ribbon(data=data, aes(x=x, ymin=ylo1, ymax=yhi1), fill=Gradpal[1])

#add in the rest
for (i in 1:(n_ribbons-1)){
  loopdata <- select(data, c("x", paste0("ylo", i), paste0("ylo", i+1), 
                                          paste0("yhi", i), paste0("yhi", i+1)))
  colnames(loopdata) <- c("x","ymin1", "ymax1", "ymin2", "ymax2")

  plot <- plot+
    geom_ribbon(data=loopdata, aes(x=x, ymin=ymin1, ymax=ymax1), fill=Gradpal[i+1])+
    geom_ribbon(data=loopdata, aes(x=x, ymin=ymin2, ymax=ymax2), fill=Gradpal[i+1])
}

#add the central line
plot <- plot+
  geom_line(data=data, aes(x=x, y=y), colour="Black")+
  theme_classic()

plot
