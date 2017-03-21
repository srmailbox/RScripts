
plot(x, type="b", ylim=c(5,15), pch=1:5)
opar=par(usr=c(par("usr")[1:2], 0, .3))
barplot(y, width=.3, axes=F, add=T, space=c(10/3, 8.5/3,8.5/3,8.5/3,8.5/3)-.5)
axis(4, at=c(0,.05,.1), labels=c("", "5%", "10%"))
