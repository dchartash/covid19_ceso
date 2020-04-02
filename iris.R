require('ggplot2')

g<-ggplot(iris) + geom_point(aes(x=Petal.Length,y=Petal.Width,fill=Species))
png("gfx/iris.png",width=10,height=10)
plot(g)
dev.off()
