df = data.frame(DV=rnorm(8)+5, Cond1 = gl(2,4), Cond2=gl(2,2), Cond3= factor(c(1,2)))

library(ggplot2)

ggplot(df, aes(x=Cond1, y=DV, fill=Cond2))+geom_bar(stat="identity", position = "dodge")+
  facet_grid(Cond3~.)
