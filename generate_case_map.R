#packages
require('tidyverse')
require('rgdal')
#require('tmap')
#require('rgeos')

#load covid data
covidloc<-read_csv("data/conposcovidloc.csv") 

#identify coverage of public health units (based on precision of first three letters of postal code area
#get public health unit coverage areas by postal code forwarding sortation area
PHU_pcfa<-split(covidloc,covidloc %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{1}",Reporting_PHU_Postal_Code)) %>% pull("Reporting_PHU_PCFA"))
#identify which 1 character sortation areas are covered by more than one unit
PHU_ur<-which(sapply(PHU_pcfa,function(x) length(unique(x$Reporting_PHU))) > 1) %>% names()
#get all 2 and greater character sortation areas
PHU_pcfa_ur<-split(covidloc,covidloc %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{2}",Reporting_PHU_Postal_Code)) %>% pull("Reporting_PHU_PCFA"))
PHU_ur_2char<-(PHU_pcfa_ur %>% names())[grepl(paste0(PHU_ur,collapse="|"),PHU_pcfa_ur %>% names())]
#identify which 2 character sortation areas are covered by more than one unit
PHU_regional<-which(sapply(PHU_pcfa_ur,function(x) length(unique(x$Reporting_PHU))) > 1) %>% names()
#map PHU to 3 character sortation areas
PHU_3char<-covidloc %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{2}",Reporting_PHU_Postal_Code)) %>% filter(grepl(pattern=paste0(PHU_regional,collapse="|"),Reporting_PHU_PCFA)) %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{3}",Reporting_PHU_Postal_Code)) %>% select(Reporting_PHU_PCFA,Reporting_PHU) %>% distinct()
#map PHU to 2 character sortation areas 
PHU_2char<-covidloc %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{2}",Reporting_PHU_Postal_Code)) %>% filter(grepl(pattern=PHU_ur_2char %>% paste0(collapse="|"),Reporting_PHU_PCFA)) %>% select(Reporting_PHU_PCFA,Reporting_PHU) %>% distinct()
#map PHU to 1 character sortation areas 
PHU_1char<-covidloc %>% mutate(Reporting_PHU_PCFA=str_extract(pattern="[A-Z0-9]{1}",Reporting_PHU_Postal_Code)) %>% filter(grepl(pattern=(PHU_pcfa %>% names())[which(!(PHU_pcfa %>% names()) %in% c(PHU_regional,PHU_ur))] %>% paste0(collapse="|"),Reporting_PHU_PCFA)) %>% select(Reporting_PHU_PCFA,Reporting_PHU) %>% distinct()
#concatenate all mapped sortation areas, make sure that the longest PCFA sequence is selected for each public health unit
PHU_PCFA<-bind_rows(PHU_1char,PHU_2char,PHU_3char) %>% mutate(L=str_length(Reporting_PHU_PCFA)) %>% group_by(Reporting_PHU) %>% summarize(Reporting_PHU_PCFA=Reporting_PHU_PCFA[which.max(L)])

#load geographical data for postal code forward sortation area from <https://www150.statcan.gc.ca/n1/en/catalogue/92-179-X>
can<-readOGR(dsn='data',layer="lfsa000a16a_e")
#select only Ontario
on<-can[can$PRNAME == "Ontario",] 

#create postal code forwarding data
covidloc_phu<-covidloc %>% group_by(Reporting_PHU) %>% summarize(recovered=sum(OUTCOME1=="RECOVERED",na.rm=TRUE),cases=n(),deaths=sum(OUTCOME1=="FATAL",na.rm=TRUE)) %>% left_join(PHU_PCFA)
#get all postal codes covered by the pattern match of each postal code
phu_postal_codes<-sapply(seq(1:dim(covidloc_phu)[1]),function(x) on@data$CFSAUID[which(grepl(paste0("^",covidloc_phu[x,"Reporting_PHU_PCFA"]),on@data$CFSAUID))])
names(phu_postal_codes)<-covidloc_phu$Reporting_PHU
#create dataframe to join postal codes with public health unit
phu_cfsauid<-data.frame(CFSAUID=phu_postal_codes %>% unlist(use.names=FALSE),Reporting_PHU=lapply(names(phu_postal_codes),function(x) rep(x,times=length(phu_postal_codes[[x]]))) %>% unlist() )
#join in case data
phu_cfsauid <- phu_cfsauid %>% left_join(covidloc_phu %>% select(-Reporting_PHU_PCFA)) 
#add remaining CFSAUID that are not assigned to a public health unit
phu_cfsauid <- phu_cfsauid %>% bind_rows( data.frame(CFSAUID=on@data$CFSAUID[which(!on@data$CFSAUID %in% (phu_cfsauid %>% pull("CFSAUID")))]) )

#add covid reporting data
on@data<-on@data %>% left_join(phu_cfsauid)
#fortify ontario data
on_f<-fortify(on)
#add covid reporting data back to on_f
on$id<-row.names(on)
on_f<- on_f %>% left_join(on@data)
#add GTA variable to on_f to identify regions from <https://www.ic.gc.ca/eic/site/bsf-osb.nsf/eng/br03396.html>
on_f<- on_f %>% mutate(Region=ifelse(grepl("^M",CFSAUID),"Metropolitan Toronto",
    ifelse(grepl("^K",CFSAUID),"Eastern Ontario",
    ifelse(grepl("^L",CFSAUID),"Central Ontario",
    ifelse(grepl("^N",CFSAUID),"Southwestern Ontario",
    ifelse(grepl("^P",CFSAUID),"Northern Ontario","Other")))))
)
#

#using ggplot, plot polygons of postal code forwarding sortation areas, bounded by black line-paths, fill with number of cases, separate regions identified above
g<-ggplot(on_f,aes(long,lat,group=group,fill=cases)) + geom_polygon() + scale_fill_gradient2(low='white',mid='orange',high='red') + geom_path(color="black",lwd=0.05) + labs(fill = "Number of Cases") + theme_bw() + theme(panel.border = element_blank(),axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio=1) + facet_wrap(~Region,scales="free")
#facet_wrap(~GTA,scale="free",labeller=labeller(GTA=c("TRUE"="Toronto","FALSE"="Rest of Province")))

#generate plot, save to svg
svg("gfx/covid_prov.svg",height=20,width=40)
plot(g)
dev.off()
