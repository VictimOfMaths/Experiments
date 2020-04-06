map4.data <- map.data

map4.data$IMDquart <- quantcut(-map4.data$rank, q=4, labels=FALSE)
map4.data$mortquart <- quantcut(map4.data$mortrate, q=4, labels=FALSE)

#generate 9-category index for map key
key4data <- data.frame(IMDquart=rep(1:4, each=4, times=1),
                       mortquart=rep(1:4, each=1, times=4),
                       key=c(1:16),
                       RGB=c("#e8e8e8","#B9DDDD","#89D3D3","#5ac8c8","#DABCD4","#ACB2CA",
                                "#7EA8C1","#509EB7","#CC90C0","#9F86B7","#727DAE","#4573A5",
                                "#be64ac","#925BA4","#67529C","#3b4994"))

#Bring colours into main data for plotting
map4.data <- left_join(map4.data, key4data, by=c("IMDquart", "mortquart"))

plot <- ggplot(subset(map4.data, LAname=="Sheffield"), aes(fill=RGB, geometry=geometry))+
  geom_sf()+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title="Mapping potential COVID-19 risk across Sheffield",
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs from Istituto Superiore di Sanità\nPlot by @VictimOfMaths")+
  annotate("text", x=-1.38, y=53.45, label="High deprivation,\nyoung population", size=3)+
  annotate("text", x=-1.34, y=53.38, label="High deprivation,\nold population", size=3)+
  annotate("text", x=-1.75, y=53.4, label="Low deprivation,\nold population", size=3)+
  geom_curve(aes(x=-1.38, y=53.44, xend=-1.4, yend=53.42), curvature=-0.15)+
  geom_curve(aes(x=-1.345, y=53.37, xend=-1.37, yend=53.355), curvature=-0.15)+
  geom_curve(aes(x=-1.725, y=53.4, xend=-1.62, yend=53.36), curvature=0.15)


key <- ggplot(key4data)+
  geom_tile(aes(x=mortquart, y=IMDquart, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater age-based COVID-19 risk" %->%  ""),
       y = expression("Greter health deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()


tiff("Outputs/COVIDBivariateSheff4x4.tiff", units="in", width=12, height=8, res=500)
ggdraw()+
  draw_plot(plot, 0,0,1,1)+
  draw_plot(key, 0.03,0.03,0.32,0.32)
dev.off()
