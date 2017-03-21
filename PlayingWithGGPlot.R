require(ggplot2)
require(plyr)

CCD=list(
  Theme = theme(text = element_text(size=18, vjust=.25, colour=ccd.cols["red"]),
                title = element_text(vjust=1),
                rect = element_rect(colour=ccd.cols["grey"], fill=ccd.cols["grey"]),
                axis.title = element_text(vjust=.25),
                axis.text = element_text(colour=ccd.cols["red"]),
                axis.line = element_line(colour=ccd.cols["red"]),
                axis.ticks = element_line(colour=ccd.cols["red"]),
                legend.text = element_text(size=18),
                panel.grid.major=element_line(colour=NA), 
                panel.grid.minor=element_line(colour=NA), 
                panel.background=element_rect(colour="white", fill="white"),
                panel.border=element_rect(colour=ccd.cols["grey"], fill=NA),
                strip.background=element_rect(colour=ccd.cols["grey"], fill=ccd.cols["red"]),
                strip.text = element_text(colour=ccd.cols["grey"], face="bold"),
                legend.key=element_rect(colour=NA, fill=NA)),
  LineType = scale_linetype_manual(values = c("solid", "52", "dashed", "dot", "dotdash")),
  Colour = scale_colour_manual(values=ccd.cols))


temp = data.frame(RT=c(450,550)+rnorm(100)*100, Condition=factor(gl(2,50)), Gender = rep(c("Male", "Female"), 50))
temp.d = ddply(temp, .(Gender, Condition), summarize, meanRT = mean(RT))
ggplot(temp.d, aes(x=Condition, y=meanRT)) + facet_grid(.~Gender) + 
  geom_point(stat="identity", fill=ccd.cols[2:1], size=10, colour=ccd.cols[2:3]) + labs(title="test") + CCD$Theme + CCD$LineType + CCD$Colour

# Plotting LMEs

library(lme4)
library(multcomp)
dataset <- expand.grid(experiment = factor(seq_len(10)), status = factor(c("N", "D", "R"), levels = c("N", "D", "R")), reps = seq_len(10))
dataset$value <- rnorm(nrow(dataset), sd = 0.23) + with(dataset, rnorm(length(levels(experiment)), sd = 0.256)[experiment] + ifelse(status == "D", 0.205, ifelse(status == "R", 0.887, 0))) + 2.78
model <- lmer(value~status+(1|experiment), data = dataset)
tmp <- as.data.frame(confint(glht(model, mcp(status = "Tukey")))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()

tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()

model <- lmer(value ~ 0 + status + (1|experiment), data = dataset)
tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()