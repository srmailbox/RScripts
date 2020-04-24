transparent_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.line.y = element_line(colour="grey"),
  axis.line.x = element_line(colour ="grey"),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.position="none")

nHeads=6
nTosses=10
p = seq(0,1,.1)
lklhd = d.f(pval = p, lklhd=dbinom(nHeads, nTosses, p))

basePlot = ggplot(lklhd, aes(x=factor(pval), y=lklhd))+geom_col(fill="red")+APA$Theme+ylim(0,.4)+
    ylab("Likelihood [P( 6 Heads | p)]")+xlab("P(Heads) = p")+ggtitle("Likelihood Function for 6 Heads")

nHeads = 0:10

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .0))
p0 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))
d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .1))
p.1 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .2))
p.2 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .3))
p.3 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .4))
p.4 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .5))
p.5 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .6))
p.6 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .7))
p.7 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .8))
p.8 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, .9))
p.9 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

d = d.f(nHeads, lklhd = dbinom(nHeads, nTosses, 1))
p1 = ggplot(d, aes(x=nHeads, y=lklhd, fill=nHeads==6))+geom_col()+transparent_theme+
    scale_fill_manual(values=c("black", "red"))

require(gridExtra)
p0Grob = ggplotGrob(p0)
p.1Grob = ggplotGrob(p.1)
p.2Grob = ggplotGrob(p.2)
p.3Grob = ggplotGrob(p.3)
p.4Grob = ggplotGrob(p.4)
p.5Grob = ggplotGrob(p.5)
p.6Grob = ggplotGrob(p.6)
p.7Grob = ggplotGrob(p.7)
p.8Grob = ggplotGrob(p.8)
p.9Grob = ggplotGrob(p.9)
p1Grob = ggplotGrob(p1)

width = 1
nudge_y = .015
basePlot + annotation_custom(p0Grob, xmin=1-width/2, xmax=1 + width/2, ymin=nudge_y+lklhd$lklhd[1], ymax=nudge_y+.1+lklhd$lklhd[1])+
  annotation_custom(p.1Grob, xmin=2-width/2, xmax=2 + width/2, ymin=nudge_y+lklhd$lklhd[2], ymax=nudge_y+.1+lklhd$lklhd[2])+
  annotation_custom(p.2Grob, xmin=3-width/2, xmax=3 + width/2, ymin=nudge_y+lklhd$lklhd[3], ymax=nudge_y+.1+lklhd$lklhd[3])+
  annotation_custom(p.3Grob, xmin=4-width/2, xmax=4 + width/2, ymin=nudge_y+lklhd$lklhd[4], ymax=nudge_y+.1+lklhd$lklhd[4])+
  annotation_custom(p.4Grob, xmin=5-width/2, xmax=5 + width/2, ymin=nudge_y+lklhd$lklhd[5], ymax=nudge_y+.1+lklhd$lklhd[5])+
  annotation_custom(p.5Grob, xmin=6-width/2, xmax=6 + width/2, ymin=nudge_y+lklhd$lklhd[6], ymax=nudge_y+.1+lklhd$lklhd[6])+
  annotation_custom(p.6Grob, xmin=7-width/2, xmax=7 + width/2, ymin=nudge_y+lklhd$lklhd[7], ymax=nudge_y+.1+lklhd$lklhd[7])+
  annotation_custom(p.7Grob, xmin=8-width/2, xmax=8 + width/2, ymin=nudge_y+lklhd$lklhd[8], ymax=nudge_y+.1+lklhd$lklhd[8])+
  annotation_custom(p.8Grob, xmin=9-width/2, xmax=9 + width/2, ymin=nudge_y+lklhd$lklhd[9], ymax=nudge_y+.1+lklhd$lklhd[9])+
  annotation_custom(p.9Grob, xmin=10-width/2, xmax=10 + width/2, ymin=nudge_y+lklhd$lklhd[10], ymax=nudge_y+.1+lklhd$lklhd[10])+
  annotation_custom(p1Grob, xmin=11-width/2, xmax=11 + width/2, ymin=nudge_y+lklhd$lklhd[11], ymax=nudge_y+.1+lklhd$lklhd[11])
