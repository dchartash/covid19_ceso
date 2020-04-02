require('ggplot2')

g<-ggplot(iris) + geom_point(aes(x=Petal.Length,y=Petal.Width,color=Species))
png("gfx/iris.png",width=10,height=10,units="in",res=400)
plot(g)
dev.off()
