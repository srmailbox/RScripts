rgba<- function(col, alpha){
       c<- col2rgb(col)
       if(length(alpha)<ncol(c))alpha<- rep(alpha, ncol(c))
       col_with_alpha<- NULL
       for(i in 1:ncol(c)){
               col_with_alpha[i]<- rgb(c[1,i], c[2,i], c[3,i], alpha[i],
maxColorValue = 255)
       }
       return(col_with_alpha)
}


addBars2 <- function( x, y, upper, lower, angle = 90, code = 3, length
= .1, ... ){
       arrows( x, upper, x, lower, angle=angle, code=code, length=length, ...)
}

hc<- heat.colors(20)
greenWhite<- colorRampPalette(c("green", "white"))
blueGrey<- colorRampPalette(c("blue", "grey"))
gw<- greenWhite(20)
blg<- blueGrey(20)
colarr<- matrix(c(hc,gw,blg),c(3,length(hc)), byrow = T)




#plot 1
bars<- bars[order(bars$Type, bars$HiN_LoN, bars$Quantile),]
par(bg = "black", fg = "white", col.axis = "white", col.lab = "white")
plot(bars$LiftOff.mean, bars$predval.mean, type = 'n', ylim = c(-2,
12), xlab = "LiftOff Latency (ms)", ylab = "")
for(i in levels(bars$Type)){
       for(j in levels(bars$HiN_LoN)){
	               points(bars$LiftOff.mean[bars$Type == i & bars$HiN_LoN == j],
bars$predval.mean[bars$Type == i & bars$HiN_LoN == j],
               pch = as.numeric(bars$HiN_LoN[bars$HiN_LoN == j])+15,cex =
1.3, col = rgba(colarr[unique(as.numeric(bars$Type[bars$Type ==
i])),], 185))


       addBars2(bars$LiftOff.mean[bars$Type == i & bars$HiN_LoN == j],
bars$predval.mean[bars$Type == i & bars$HiN_LoN == j],
               bars$predval.upper[bars$Type == i & bars$HiN_LoN ==
j],bars$predval.lower[bars$Type == i & bars$HiN_LoN == j], col =
rgba(colarr[unique(as.numeric(bars$Type[bars$Type == i])),], 185),
length = .05)
       }
}

legend(x=-50,y=12, levels(bars$Type), lty = 1, lwd = 2, cex = 1.3, col
= c(colarr[1,10],colarr[2,10], colarr[3,10]), bty = 'n')
legend(x=-50,y=9, levels(bars$HiN_LoN), pch = c(16,17), cex = 1.3, col
= "grey", bty = 'n')
