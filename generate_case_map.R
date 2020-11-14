#packages
require('tidyverse')
require('rgdal')
require('raster')
require('reshape2')

#load covid data
covidloc<-read_csv("data/conposcovidloc.csv") 

#create postal code forwarding data
#OUTCOME -> Outcome
covidloc_phu<-covidloc %>% group_by(Reporting_PHU) %>% summarize(recovered=sum(Outcome1=="Resolved",na.rm=TRUE),active=sum(Outcome1=="Not Resolved",na.rm=TRUE),cases=n(),deaths=sum(Outcome1=="Fatal",na.rm=TRUE),CFSAUID=unique(str_extract(pattern="[A-Z0-9]{3}",Reporting_PHU_Postal_Code))) %>% mutate(Region=ifelse(grepl("^M",CFSAUID),"Metropolitan Toronto",
#covidloc_phu<-covidloc %>% group_by(Reporting_PHU) %>% summarize(recovered=sum(RESOLVED=="Yes",na.rm=TRUE),cases=n(),deaths=sum(RESOLVED=="Fatal",na.rm=TRUE),CFSAUID=unique(str_extract(pattern="[A-Z0-9]{3}",Reporting_PHU_Postal_Code))) %>% mutate(Region=ifelse(grepl("^M",CFSAUID),"Metropolitan Toronto",
    ifelse(grepl("^K",CFSAUID),"Eastern Ontario",
    ifelse(grepl("^L",CFSAUID),"Central Ontario",
    ifelse(grepl("^N",CFSAUID),"Southwestern Ontario",
    ifelse(grepl("^P",CFSAUID),"Northern Ontario","Other")))))
)

#load geographical data for postal code forward sortation area from <https://www150.statcan.gc.ca/n1/en/catalogue/92-179-X>
can<-readOGR(dsn='data',layer="lfsa000b16a_e")
#select only Ontario
on<-can[can$PRNAME == "Ontario",] 
#fortify ontario data
on_f<-fortify(on)
#add covid reporting data back to on_f
on$id<-row.names(on)
on_f<- on_f %>% left_join(on@data)
#add region by CFAUID
on_f<- on_f %>% mutate(Region=ifelse(grepl("^M",CFSAUID),"Metropolitan Toronto",
    ifelse(grepl("^K",CFSAUID),"Eastern Ontario",
    ifelse(grepl("^L",CFSAUID),"Central Ontario",
    ifelse(grepl("^N",CFSAUID),"Southwestern Ontario",
    ifelse(grepl("^P",CFSAUID),"Northern Ontario","Other")))))
)

#load health facility data from the ministry of health and long term care; add region
hspl<-read_csv("data/Ministry_of_Health_Service_Provider_Locations.csv") %>% mutate(Region=ifelse(grepl("^M",POSTAL_CODE),"Metropolitan Toronto",
    ifelse(grepl("^K",POSTAL_CODE),"Eastern Ontario",
    ifelse(grepl("^L",POSTAL_CODE),"Central Ontario",
    ifelse(grepl("^N",POSTAL_CODE),"Southwestern Ontario",
    ifelse(grepl("^P",POSTAL_CODE),"Northern Ontario","Other")))))
)
#filter to hospitals or labs, remove anywhere without a long/lat location 
hspl_sel<-hspl %>% filter(grepl("^Hospital|^Laboratory|^Retirement|^Senior",SERVICE_TYPE)) %>% filter(!is.na(X),!is.na(Y))
#get X and Y coordinates (lat/long)
hspl_sel_coord<-hspl_sel[c('X','Y')]
coordinates(hspl_sel_coord)=~X+Y
# set it to lat-long
proj4string(hspl_sel_coord)=CRS("+init=epsg:4326") 
# align with on
hspl_sel_coord = spTransform(hspl_sel_coord,crs(on))
#assign to hspl_sel, and correct column names
hspl_sel_coord_ll<-coordinates(hspl_sel_coord)
colnames(hspl_sel_coord_ll)<-c("long","lat")
hspl_sel<-cbind(hspl_sel_coord_ll,hspl_sel)



#map public health unit data names from shape file to names from covid data
phu_map<-c(
"The District of Algoma Health Unit"="Algoma Public Health Unit",
"Brant County Health Unit"="Brant County Health Unit",
"Durham Regional Health Unit"="Durham Region Health Department",
"Elgin-St. Thomas Health Unit"="Southwestern Public Health",
"Grey Bruce Health Unit"="Grey Bruce Health Unit",
"Haldimand-Norfolk Health Unit"="Haldimand-Norfolk Health Unit",
"Haliburton, Kawartha, Pine Ridge District Health Unit"="Haliburton, Kawartha, Pine Ridge District Health Unit",
"Halton Regional Health Unit"="Halton Region Health Department",
"City of Hamilton Health Unit"="Hamilton Public Health Services",
"Hastings and Prince Edward Counties Health Unit"="Hastings and Prince Edward Counties Health Unit",
"Huron County Health Unit"="Huron Perth District Health Unit",
"Chatham-Kent Health Unit"="Chatham-Kent Health Unit",
"Kingston, Frontenac, and Lennox and Addington Health Unit"="Kingston, Frontenac and Lennox & Addington Public Health",
"Lambton Health Unit"="Lambton Public Health",
"Leeds, Grenville and Lanark District Health Unit"="Leeds, Grenville and Lanark District Health Unit",
"Middlesex-London Health Unit"="Middlesex-London Health Unit",
"Niagara Regional Area Health Unit"="Niagara Region Public Health Department",
"North Bay Parry Sound District Health Unit"="North Bay Parry Sound District Health Unit",
"Northwestern Health Unit"="Northwestern Health Unit",
"City of Ottawa Health Unit"="Ottawa Public Health",
"Oxford County Health Unit"="Middlesex-London Health Unit",
"Peel Regional Health Unit"="Peel Public Health",
"Perth District Health Unit"="Huron Perth District Health Unit",
"Peterborough County-City Health Unit"="Peterborough Public Health",
"Porcupine Health Unit"="Porcupine Health Unit",
"Renfrew County and District Health Unit"="Renfrew County and District Health Unit",
"The Eastern Ontario Health Unit"="Eastern Ontario Health Unit",
"Simcoe Muskoka District Health Unit"="Simcoe Muskoka District Health Unit",
"Sudbury and District Health Unit"="Sudbury & District Health Unit",
"Thunder Bay District Health Unit"="Thunder Bay District Health Unit",
"Timiskaming Health Unit"="Timiskaming Health Unit",
"Waterloo Health Unit"="Wellington-Dufferin-Guelph Public Health",
"Wellington-Dufferin-Guelph Health Unit"="Wellington-Dufferin-Guelph Public Health",
"Windsor-Essex County Health Unit"="Windsor-Essex County Health Unit",
"York Regional Health Unit"="York Region Public Health Services",
"City of Toronto Health Unit"="Toronto Public Health"
)

phu_codes<-c("Algoma Public Health"="ALG",
"Algoma Public Health Unit"="ALG",
"Brant County Health Unit"="BRN",
"Chatham-Kent Public Health"="CHK",
"Chatham-Kent Health Unit"="CHK",
"City of Hamilton Public Health Services"="HAM",
"Hamilton Public Health Services"="HAM",
"Ottawa Public Health"="OTT",
"Toronto Public Health"="TOR",
"Durham Region Health Department"="DUR",
"Eastern Ontario Health Unit"="EOH",
"Grey Bruce Health Unit"="GBO",
"Haldimand-Norfolk Health Unit"="HDN",
"Haliburton, Kawartha, Pine Ridge District Health Unit"="HKP",
"Halton Region Public Health"="HAL",
"Halton Region Health Department"="HAL",
"Hastings Prince Edward Public Health"="HPE",
"Hastings and Prince Edward Counties Health Unit"="HPE",
"Huron County Health Unit"="HUR",
"Huron Perth District Health Unit"="HUR",
"Kingston, Frontenac and Lennox & Addington Public Health"="KFL",
"Lambton Public Health"="LAM",
"Leeds, Grenville & Lanark District Health Unit"="LGL",
"Leeds, Grenville and Lanark District Health Unit"="LGL",
"Middlesex-London Health Unit"="MSL",
"Niagara Region Public Health"="NIA",
"Niagara Region Public Health Department"="NIA",
"North Bay Parry Sound District Health Unit"="NPS",
"Northwestern Health Unit"="NWR",
"Southwestern Public Health"="OXE",
"Peel Public Health"="PEL",
"Perth District Health Unit"="PDH",
"Peterborough Public Health"="PTC",
"Porcupine Health Unit"="PQP",
"Renfrew County and District Health Unit"="REN",
"Simcoe Muskoka District Health Unit"="SMD",
"Public Health Sudbury & Districts"="SUD",
"Sudbury & District Health Unit"="SUD",
"Thunder Bay District Health Unit"="THB",
"Timiskaming Health Unit"="TSK",
"Region of Waterloo Public Health and Emergency Services"="WAT",
"Region of Waterloo, Public Health"="WAT",
"Wellington-Dufferin-Guelph Public Health"="WDG",
"Windsor-Essex County Health Unit"="WEC",
"York Region Public Health"="YRK",
"York Region Public Health Services"="YRK",
"York Regional Health Unit"="YRK"
)


#read in public health office region geometry data
on_phu<-readOGR(dsn='data',layer="lhrp035b06a_e_Oct2011")
#fortify public health region
on_phu_f<-fortify(on_phu)
#add data back to on_phu_f
on_phu$id<-row.names(on_phu)
on_phu_f<- on_phu_f %>% left_join(on_phu@data)
#assign public health unit names to public health unit geometries
on_phu_f<-on_phu_f %>% mutate(Reporting_PHU=phu_map[as.character(ENG_LABEL)])
#join covid case reporting data
on_phu_f<- on_phu_f %>% left_join(covidloc_phu)
#get centroid of PHU for text plotting
on_phu.cent<-coordinates(on_phu) %>% as.data.frame()
names(on_phu.cent)<-c("long","lat")
on_phu.cent['Reporting_PHU'] <- on_phu@data$ENG_LABEL
on_phu.cent['Reporting_PHU_Code'] <- phu_codes[phu_map[on_phu.cent %>% pull("Reporting_PHU") %>% as.character()]]
#add region based on phu_codes joining of covidloc_phu
on_phu.cent<-on_phu.cent %>% left_join(covidloc_phu %>% mutate(Reporting_PHU_Code=phu_codes[Reporting_PHU]) %>% dplyr::select(Reporting_PHU_Code,Region))

#using ggplot, plot boundaries of public health units
g<-ggplot() + geom_polygon(data=on_phu_f,aes(long,lat,group=group,fill=active)) + scale_fill_distiller(palette="RdYlBu") + labs(fill = paste0("Number of Active Cases\n",as.character(Sys.Date(),format="%B %d %Y"))) + geom_path(data=on_phu_f,aes(long,lat,group=group),color='blue',lwd=0.25,linetype='dashed') + geom_text(data=on_phu.cent,aes(long,lat,label=Reporting_PHU_Code)) + theme_bw() + theme(panel.border = element_blank(),axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),aspect.ratio=1) 
#using ggplot, plot polygons of postal code forwarding sortation areas, bounded by black line-paths, fill with number of cases, separate regions identified above
g2<-geom_path(data=on_f,aes(x=long,y=lat,group=group),color="black",lwd=0.05)
#plot all hospitals
g3<-geom_point(data=hspl_sel,aes(x=long,y=lat,shape=SERVICE_TYPE),color='black',alpha=0.5) 

#generate plot, save to svg
svg("gfx/covid_prov.svg",height=20,width=40)
plot(g + g2 + g3 + scale_shape_manual("Healthcare Facility",values=c("Hospital - Site"=2, "Hospital - Corporation"=6, "Laboratory - Hospital"=3, "Laboratory - Specimen Collection Centre"=4, "Laboratory - Community Private"=0,"Retirement Home"=1,"Senior Active Living Centre"=19)) + facet_wrap(~Region,scale="free"))
dev.off()
