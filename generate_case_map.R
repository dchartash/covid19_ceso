#packages
require('tidyverse')
require('rgdal')
#require('tmap')
#require('rgeos')

#load covid data
covidloc<-read_csv("data/conposcovidloc.csv") %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{3}",Reporting_PHU_Postal_Code))
#create postal code forwarding data
covidloc_pcfa<-covidloc %>% group_by(Reporting_PHU_PCFA) %>% summarize(recovered=sum(OUTCOME1=="RECOVERED",na.rm=TRUE),cases=n(),deaths=sum(OUTCOME1=="FATAL",na.rm=TRUE),Reporting_PHU=unique(Reporting_PHU))

#load geographical data for postal code forward sortation area from <https://www150.statcan.gc.ca/n1/en/catalogue/92-179-X>
can<-readOGR(dsn='data',layer="lfsa000a16a_e")
#select only Ontario
on<-can[can$PRNAME == "Ontario",] 
#add covid reporting data
on@data<-on@data %>% left_join(covidloc_pcfa,by=c("CFSAUID"="Reporting_PHU_PCFA"))
#fortify ontario data
on_f<-fortify(on)
#add covid reporting data back to on_f
on$id<-row.names(on)
on_f<- on_f %>% left_join(on@data)
#add GTA variable to on_f to identify GTA locales from <https://www.ic.gc.ca/eic/site/bsf-osb.nsf/eng/br03396.html>
on_f<- on_f %>% mutate(Region=ifelse(grepl("^M",CFSAUID),"Metropolitan Toronto",
    ifelse(grepl("^K",CFSAUID),"Eastern Ontario",
    ifelse(grepl("^L",CFSAUID),"Central Ontario",
    ifelse(grepl("^N",CFSAUID),"Southwestern Ontario",
    ifelse(grepl("^P",CFSAUID),"Northern Ontario","Other")))))
)

#using ggplot, plot polygons of postal code forwarding sortation areas, bounded by black line-paths, fill with number of cases, separate regions identified above
g<-ggplot(on_f,aes(long,lat,group=group,fill=cases)) + geom_polygon() + scale_fill_gradient2(low='white',mid='orange',high='red') + geom_path(color="black",lwd=0.05) + labs(fill = "Number of Cases") + theme_bw() + theme(panel.border = element_blank(),axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio=1) + facet_wrap(~Region,scales="free")
#facet_wrap(~GTA,scale="free",labeller=labeller(GTA=c("TRUE"="Toronto","FALSE"="Rest of Province")))

#generate plot, save to svg
svg("gfx/covid_prov.svg",height=20,width=40)
plot(g)
dev.off()
