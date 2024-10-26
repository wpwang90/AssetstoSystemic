#Source code for Railway system climate risk management

#Main functions for import datasets and preprocessing.

#Copyright (C) 2024 Weiping Wang. All versions released under the GNU Affero General Public License v3.0 license.


library(raster)
library(rgdal)
library(ggplot2)
library(maptools)  
library(stars)
library(sf)
library(dplyr)
library(tidyr)
library(car)
library(reshape2)
library(data.table)
library(ggthemes)
library(RColorBrewer)
library(purrr)
library(proj4)
library(rineq)
library(ncdf4)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggExtra)
library(raster)
library(igraph)
library(scales)


source("produceSpatialLines.R")

load("../res/resStationToStationwithID.RData")


load("../res/resRailwaywithID.RData")

load("../res/resRailwaywithTime.RData")
#resRailwaywithID$as.numeric.sPaths.k..

#########init stage #########


#######from railway shapefile data to network########
 
#    These codes convert a railway network from a shapefile format into an igraph format for storage.
#    First read shapefile data and then covert
 
dataRailway=fread("../Data/openRailwayBothEnds.txt",header = TRUE,sep = ",")

dataStationList=read.csv("../Data/车站信息.csv",stringsAsFactors=FALSE,fileEncoding = 'GBK')

dimRoad=dim(dataRailway);
#uniqueXY=unique(dataRailway[,c(11,12)])
uniqueXY=unique(dataRailway[,c("POINT_X","POINT_Y")])

dimUniqueXY=dim(uniqueXY)
nodeIndex=c(1:dimRoad[1])*0;
wError=0.0001
for(i in 1:dimUniqueXY[1]){
  lSet=which(abs(dataRailway$POINT_X-uniqueXY[i,]$POINT_X)<wError);
  rSet=which(abs(dataRailway$POINT_Y-uniqueXY[i,]$POINT_Y)<wError);
  nodeIndex[unique(intersect(lSet,rSet))]=i;
}
#gRoad <- make_empty_graph(n =length(unique(dataRailway$ORIG_FID)),directed = FALSE)
edgeList=data.frame(nodeIndex[(c(1:(dimRoad[1]/2))-1)*2+1],nodeIndex[(c(1:(dimRoad[1]/2)))*2]);


########covert edgelist to spatialdataframe#################
   
#    These codes convert edge list of network with igraph format to shapfile format.
 

railway_line_DF=produceSpatialLines(edgeList,uniqueXY)

writeOGR(railway_line_DF,"railway_line.shp","railway", driver="ESRI Shapefile",overwrite_layer=TRUE)

############################giant connected component##############################################
  
#    These codes calculate giant connected componentt from origin network.


gRoad=graph_from_edgelist(as.matrix(edgeList),directed = FALSE)
V(gRoad)$id <- 1:gorder(gRoad)

edge_attr(gRoad, "weight")=dataRailway$length[c(1:(dimRoad[1]/2))*2];

temp=components(gRoad);
maxCize=max(temp$csize)
tempIndex=which(temp$membership==1)

gLargest <- induced_subgraph(gRoad, tempIndex,impl="copy_and_delete")

edge_List=get.edgelist(gLargest)

gRoadNode=gLargest;

f_uniqueXY=uniqueXY[tempIndex,]

f_uniqueXY=uniqueXY[tempIndex,]

########covert edgelist to spatialdataframe#################
railway_line_giant_DF=produceSpatialLines(edge_List,f_uniqueXY)
writeOGR(railway_line_giant_DF,"railway_line_giant.shp","railway", driver="ESRI Shapefile",overwrite_layer=TRUE)





 
 #   Read province boundary data.

region_shp <- readOGR("../Data/省级行政区.shp",encoding ="GBK")
region_province_shp_m <- spTransform(region_shp, "+proj=merc +units=m")
region_province_shp_m$pID=c(1:34)
writeOGR(region_province_shp_m,"province_tf.shp","province", driver="ESRI Shapefile",overwrite_layer=TRUE)






#mean(province_railwaylinecost_k,na.rm =TRUE)





#railway_shp <- readOGR("../Data/主要铁路.shp",encoding ="GBK")

###########draw figure##########
  
#    draw province boundary map.

par(mar=c(0,0,0,0))
plot(region_shp, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

AG <- fortify(region_shp)
tempplot=ggplot()+ geom_polygon(data=AG, aes(long, lat, group = group), 
                                colour = alpha("darkred", 1/2), size = 0.2,fill="#69B3A2", alpha = .5)+
  theme_void() 
print(tempplot)
fileDisName="Display"
#ggsave(file=paste0("..\\Res\\",paste0(fileDisName,"region.pdf")))



#g <- getData("GADM", country="China", level=1)

###############create raster with region shapefile############
 
#    Based on the spatial extent of the railway lines, a 1 km by 1 km grid raster is constructed.

m <- spTransform(region_shp, "+proj=merc +units=m")
r <- raster(m, res=10000)
vals=1:ncell(r)
r <- setValues(r, vals)
out_raster_file='China1000.tif'
writeRaster(r,out_raster_file,overwrite=TRUE)
rdf <- as.data.frame(r, xy=TRUE) #Convert raster to data.frame
names(rdf)[3] <- 'value' #Name value column
AG <- fortify(m)


 
# The initial railway lines retain only those within the largest connected subgraph, and isolated lines are removed. 
# If there are no isolated lines, this operation results in the same network as the original railway lines.

railway_shp <- readOGR("railway_line_giant.shp",encoding ="GBK")

railway_m <- spTransform(railway_shp, "+proj=merc +units=m")
writeOGR(railway_m,"railway_line_giant_tf.shp","railway", driver="ESRI Shapefile",overwrite_layer=TRUE)


railway_all_shp <- readOGR("railway_line.shp",encoding ="GBK")

railway_all_m <- spTransform(railway_all_shp, "+proj=merc +units=m")
writeOGR(railway_all_m,"railway_line_tf.shp","railway", driver="ESRI Shapefile",overwrite_layer=TRUE)


###import county-level data
region_county_shp <- readOGR("../Data/2023年县级/县级.shp",encoding ="utf-8")
region_county_shp_m <- spTransform(region_county_shp, "+proj=merc +units=m")
#region_county_shp_m$FID=c(1:2391)
region_county_shp_m$FID=c(1:2853)


region_county_shp = st_as_sf(region_county_shp_m)

#region_county_shp_m_new=region_county_shp_m[c(1:6)]



#writeOGR(region_county_shp_m_new,"county_tf.shp","county",encoding ="utf-8", driver="ESRI Shapefile",overwrite_layer=TRUE)



###################calculate overlap length for each link########
###Spatial analysis to calculate railway lines falling within different spatial extents

tif=read_stars(out_raster_file, package = "stars")
sf=st_as_sf(tif)

polys = st_read("railway_line_giant_tf.shp", quiet=TRUE)


#region_county_shp= st_read("county_tf.shp", quiet=TRUE)
region_province_shp= st_read("province_tf.shp", quiet=TRUE)
region_province_shp$NAME=c("Heilongjiang","Xinjiang","Shanxi","Ningxia","Tibet","Shandong",
                           "Henan","Jiangsu","Anhui","Hubei","Zhejiang","Jiangxi","Hunan","Yunnan","Guizhou","Fujian",
                           "Guangxi","Guangdong","Hainan","Jilin","Liaoning","Tianjin","Qinghai","Gansu","Shaanxi","Inner Mongolia",
                           "Chongqing","Hebei","Shanghai","Beijing","Taiwan","Hong Kong","Macao","Sichuan")





ints = st_intersection(polys, sf)%>% 
  mutate(lenght = st_length(.)) %>% 
  
  st_drop_geometry()


ints_county = st_intersection(region_county_shp, sf)%>% 

  st_drop_geometry()

ints_province= st_intersection(region_province_shp, sf)%>% 
  
  st_drop_geometry()



########all points#########
polys_all= st_read("railway_line_tf.shp", quiet=TRUE)


ints_all = st_intersection(polys_all, sf)%>% 
  mutate(lenght = st_length(.)) %>% 
  
  st_drop_geometry()



all_df <- merge(ints, resRailwaywithID, by.x ="ID", by.y ="as.numeric.sPaths.k..")

#############calculated length for county#########
###Spatial analysis to calculate railway length within each county

ints_all_county_length = st_intersection(polys_all, region_county_shp)%>% 
  mutate(lenght = st_length(.)) %>% 
  
  st_drop_geometry()

Length_tvals=region_county_shp$FID*0

for (i in 1:length(ints_all_county_length$FID)){
  Length_tvals[ints_all_county_length$FID[i]]=Length_tvals[ints_all_county_length$FID[i]]+as.numeric(ints_all_county_length$lenght[i])/1000
}




#########
Ct_tvals
Ct_Gvals

province_railwaylinecost_k


region_county_shp$Length_tvals=Length_tvals*0.1





###Draw map for railway length within each county

tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill = ifelse(Length_tvals != 0, Length_tvals, NA)),colour = alpha("black",0))+
  geom_sf(data=region_province_shp, fill = NA,size = 0.2,colour = alpha("black"))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")), name='Physical damage loss (billion RMB)',na.value = "white")+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few(base_size = 12, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'))


print(tempplot)
ggsave(wid=16,hei=5,paste0('../Res/','length_in_county.pdf'))


#############calculated length for province#########
###Spatial analysis to calculate railway length within each province

ints_all_province_length = st_intersection(polys_all, region_province_shp)%>% 
  mutate(lenght = st_length(.)) %>% 
  
  st_drop_geometry()

Length_tvals=region_province_shp$pID*0

for (i in 1:length(ints_all_province_length$pID)){
  Length_tvals[ints_all_province_length$pID[i]]=Length_tvals[ints_all_province_length$pID[i]]+as.numeric(ints_all_province_length$lenght[i])/1000
}



region_province_shp$Length_tvals=Length_tvals*0.099



region_province_shp<-region_province_shp%>%
  arrange(Length_tvals)
region_province_shp1=region_province_shp[c(4:34),]

###Draw bar figure for railway length within each province

tempplot=ggplot(data=region_province_shp1, aes(x=NAME, y=Length_tvals, fill = Length_tvals)) +
  geom_bar(stat="identity")+
  #scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
  scale_fill_gradient(low="white",high="darkred")+
  coord_flip()+
  scale_x_discrete(name="",limits = region_province_shp1$NAME)+
  #theme()+
  theme_few(base_size = 12, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(legend.position="none" )+
  ylab("Physical damage loss (billion RMB)") 
print(tempplot)
ggsave(wid=4,hei=5,paste0('../Res/','length_in_province_bar.pdf'))



# tempplot=ggplot()+theme_few(base_size = 12)+
#   geom_sf(data=region_county_shp,aes(fill = ifelse(Ct_tvals != 0, Ct_tvals, NA)),colour = alpha("black", 1/10), size = 0.2)+
#   scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",trans = "log",name='Ct_tvals')
# 
# 
# print(tempplot)




###Convert the length unit in the 1km*1km grid from meters to kilometers

tvals_len=vals*0
for (i in 1:length(ints$China1000.tif)){
  tvals_len[ints$China1000.tif[i]]=tvals_len[ints$China1000.tif[i]]+as.numeric(ints$lenght[i])/1000
}

tvals_all_len=vals*0
for (i in 1:length(ints_all$China1000.tif)){
  tvals_all_len[ints_all$China1000.tif[i]]=tvals_all_len[ints_all$China1000.tif[i]]+as.numeric(ints_all$lenght[i])/1000
}



###Draw map for railway length within each 1km*1km grid

rr <- setValues(r, tvals_all_len)
rrdf = as.data.frame(rr, xy=TRUE)
names(rrdf)[3] <- 'value'
tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+
  geom_raster(data = rrdf,mapping=aes(x=x, y=y,  fill= ifelse(value != 0, value, NA)))+
  geom_sf(data=region_province_shp, fill = NA,size = 0.1,alpha=0.01)+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")), name='Railway length (km)',na.value = "white")+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  xlab("")+
  theme_few(base_size = 14, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(legend.position="bottom" )
    

print(tempplot)
ggsave(wid=16,hei=9,paste0('../Res/','Railwaylengthingrid.pdf'))
ggsave(wid=16,hei=9,paste0('../Res/','Railwaylengthingrid.jpg'))


# tempplot=ggplot()+theme_few(base_size = 12)+
#   geom_sf(data=region_county_shp,aes(fill = ifelse(Ct_tvals != 0, Ct_tvals, NA)),colour = alpha("black", 1/10), size = 0.2)+
#   scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",trans = "log",name='Ct_tvals')
# 
# 
# print(tempplot)




writeRaster(rr,'China1000_all_lengths.tif',overwrite=TRUE)

################

########merge all dataframe##########
##Match the train schedules passing through each grid

all_df <- merge(ints, resRailwaywithID, by.x ="ID", by.y ="as.numeric.sPaths.k..")
oall_df=all_df
all_df=all_df[!duplicated(all_df),]



##Match the train schedules passing through each county
county_all_df <- merge(all_df, ints_county, by ="China1000.tif")

county_all_df_alllines <- merge(ints_all, ints_county, by ="China1000.tif")


county_all_df=county_all_df[,c("China1000.tif","ID","tempIDNameList","lenght","FID")]
county_all_df=county_all_df[!duplicated(county_all_df),]


##Match the train schedules passing through each province
province_all_df <- merge(all_df, ints_province, by ="China1000.tif")

province_all_df_alllines <- merge(ints_all, ints_province, by ="China1000.tif")


province_all_df=province_all_df[,c("China1000.tif","ID","tempIDNameList","lenght","pID")]
province_all_df=province_all_df[!duplicated(province_all_df),]





##Obtain the number of high-speed trains and regular trains passing through each grid

tvals=vals*0
#6
Gvals=vals*0
#14
for (i in 1:length(all_df$China1000.tif)){
  if(!is.na(all_df$tempIDNameList[i])){
    tvals[all_df$China1000.tif[i]]=tvals[all_df$China1000.tif[i]]+1
    if(substr(all_df$tempIDNameList[i],1,1)=="G"){
      Gvals[all_df$China1000.tif[i]]=Gvals[all_df$China1000.tif[i]]+1  
    }
  }
}

##Obtain the number of high-speed trains and regular trains passing through each county

Ct_tvals=region_county_shp$FID*0
#6
Ct_Gvals=region_county_shp$FID*0
#14
for (i in 1:length(county_all_df$FID)){
  if(!is.na(county_all_df$tempIDNameList[i])){
    Ct_tvals[county_all_df$FID[i]]=Ct_tvals[county_all_df$FID[i]]+1
    if(substr(county_all_df$tempIDNameList[i],1,1)=="G"){
      Ct_Gvals[county_all_df$FID[i]]=Ct_Gvals[county_all_df$FID[i]]+1  
    }
  }
}

##Obtain the number of high-speed trains and regular trains passing through each province

Pt_tvals=region_province_shp$pID*0
#6
Pt_Gvals=region_province_shp$pID*0
#14
for (i in 1:length(province_all_df$pID)){
  if(!is.na(province_all_df$tempIDNameList[i])){
    Pt_tvals[province_all_df$pID[i]]=Pt_tvals[province_all_df$pID[i]]+1
    if(substr(province_all_df$tempIDNameList[i],1,1)=="G"){
      Pt_Gvals[province_all_df$pID[i]]=Pt_Gvals[province_all_df$pID[i]]+1  
    }
  }
}


region_county_shp$Ct_tvals=Ct_tvals
region_county_shp$Ct_Gvals=Ct_Gvals


region_province_shp$Pt_tvals=Pt_tvals
region_province_shp$Pt_Gvals=Pt_Gvals

rr <- setValues(r, tvals)
rrdf = as.data.frame(rr, xy=TRUE)
names(rrdf)[3] <- 'value'

Grr <- setValues(r, Gvals)
Grrdf = as.data.frame(Grr, xy=TRUE)
names(Grrdf)[3] <- 'value'

writeRaster(rr,'China1000_all_links.tif',overwrite=TRUE)

writeRaster(Grr,'China1000_G_links.tif',overwrite=TRUE)


my_breaks = c(10, 100, 1000, 10000, 100000, 1000000)
region_shp <- readOGR("../Data/省级行政区.shp",encoding ="GBK")
region_AG <- fortify(region_shp)
region_m <- spTransform(region_shp, "+proj=merc +units=m")#墨卡托投影


########draw map for all lines for each grid 

tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = rrdf,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= brewer.pal(11, "YlGn"),  na.value = "white",trans = "log",breaks = my_breaks, labels = my_breaks,name='value')
tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
print(tempplot)

ggsave(wid=16,hei=10,"../Res/all_lines.pdf")



########draw map for all lines for each county

tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_sf(data=region_county_shp, colour = alpha("black", 1/2), size = 0.2,aes(fill = Ct_tvals),alpha = .01)+ 
  scale_fill_gradientn(colours= brewer.pal(11, "YlGn"),  na.value = "white",trans = "log",name='Ct_tvals')
print(tempplot)

ggsave(wid=16,hei=10,"../Res/all_lines.pdf")


########draw map for high speed lines for each grid 

tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = Grrdf,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",trans = "log",breaks = my_breaks, labels = my_breaks,name='value')
tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("darkred", 1/2), size = 0.2,fill="white", alpha = .01)
print(tempplot)

ggsave(wid=16,hei=10,"../Res/Gao_lines.pdf")


########calculate systemic functions loss caused by each  gird

Ollval=vals*0
uID=unique(all_df$China1000.tif)
tempTrainsList=list()
tempLengthList=c(1:length(uID))
for (i in 1:length(uID)){
  if(i%%500==0){print(i)}
  iIndex=which(all_df$China1000.tif==uID[i]);
  tempTrains=unique(all_df$tempIDNameList[iIndex])
  tempTrainsList=append(tempTrainsList,list(tempTrains))
  #iIndex=which(all_df$ID==i);
  tempValue=as.numeric(all_df$lenght[iIndex[1]])
  if(is.na(tempValue)){tempValue=0}
  tempLengthList[i]=tempValue
  
}

for (i in 1:length(uID)){
  if(i%%500==0){print(i)}
  iTrains=tempTrainsList[[i]]
  Ollval[uID[i]]=Ollval[uID[i]]+tempLengthList[i]
  for(j in 1:length(uID)){
    if(i!=j){
      jTrains=tempTrainsList[[j]]
      if(length(jTrains)==0){
        disl=0
      }
      else{
        disl=as.numeric(length(intersect(iTrains,jTrains))/length(jTrains)*tempLengthList[j])
      }
      Ollval[uID[i]]=Ollval[uID[i]]+disl   
      }
  }
}


for (i in 1:length(ints_all$China1000.tif)){
  if(Ollval[ints_all$China1000.tif[i]]==0)
    Ollval[ints_all$China1000.tif[i]]=ints_all$lenght[i]

}




Ct_Ollval=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Ct_Ollval[county_all_df_alllines$FID[i]]=Ct_Ollval[county_all_df_alllines$FID[i]]+Ollval[county_all_df_alllines$China1000.tif[i]]
}



Pt_Ollval=region_province_shp$pID*0

for (i in 1:length(province_all_df_alllines$pID)){
  Pt_Ollval[province_all_df_alllines$pID[i]]=Pt_Ollval[province_all_df_alllines$pID[i]]+Ollval[province_all_df_alllines$China1000.tif[i]]
}

region_county_shp$Ct_Ollval=Ct_Ollval*0.099/1000

region_province_shp<-region_province_shp%>%
  arrange(pID)
region_province_shp$Pt_Ollval=Pt_Ollval*0.099/1000



#############draw_functional loss for county#########3

tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill = ifelse(Ct_Ollval != 0, Ct_Ollval, NA)),colour = alpha("black",0))+
  geom_sf(data=region_province_shp, fill = NA,size = 0.2,colour = alpha("black"))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")), name='Systematic damage loss (billion RMB)',na.value = "white")+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few(base_size = 12, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'))


print(tempplot)
ggsave(wid=16,hei=5,paste0('../Res/','Systematicloss_in_county.pdf'))


#############draw_functional loss for province#########3




region_province_shp<-region_province_shp%>%
  arrange(Pt_Ollval)
region_province_shp1=region_province_shp[c(4:34),]
tempplot=ggplot(data=region_province_shp1, aes(x=NAME, y=Pt_Ollval, fill = Pt_Ollval)) +
  geom_bar(stat="identity")+
  #scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
  scale_fill_gradient(low="white",high="darkred")+
  coord_flip()+
  scale_x_discrete(name="",limits = region_province_shp1$NAME)+
  #theme()+
  theme_few(base_size = 12, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(legend.position="none" )+
  ylab("Systematic damage loss (billion RMB)") 
print(tempplot)
ggsave(wid=4,hei=5,paste0('../Res/','Systematicloss_in_province_bar.pdf'))



# temp_df=all_df[1:5000,]
# 
# res=temp_df %>% 
#   group_by(ID,tempIDNameList)%>% 
#   mutate(intersect = list(reduce(tempIDNameList, intersect)))%>% 
#   select(ID,intersect)
# 
# library(palmerpenguins)
# 
# all_df %>% max(ID)


Orr <- setValues(r, Ollval)
Orrdf = as.data.frame(Orr, xy=TRUE)
names(Orrdf)[3] <- 'value'

writeRaster(Orr,'China1000_O_linkslosscausedbyonegrid.tif',overwrite=TRUE)


my_breaks = c(1000, 10000, 100000, 1000000,10000000)
region_shp <- readOGR("../Data/省级行政区.shp",encoding ="GBK")
region_AG <- fortify(region_shp)
region_m <- spTransform(region_shp, "+proj=merc +units=m")#墨卡托投影



tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = Orrdf,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",trans = "log",breaks = my_breaks, labels = my_breaks,name='value')
tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
print(tempplot)



########calculate all travel loss between provinces caused by one gird

TRLlval=vals*0 ######reduce rate of number of lines between provinces#####
TULlval=vals*0 ######reduce rate of passed lines between provinces#####
TPTLlval=vals*0 ######rate of minum times of passed lines between provinces#####





#Big_value=10e5

for (ui in 1:length(uID)){
  iTrains=tempTrainsList[[ui]]
  if(ui%%500==0){print(ui)}
  
  gridCityToCityLines=matrix(NA,cityNumbers,cityNumbers);
  gridCityToCityMinumTime=matrix(NA,cityNumbers,cityNumbers);
  
  for(i in 1:(cityNumbers)){
    for(j in 1:cityNumbers){
      if(i!=j){
        tempIndex=which(resStationToStationwithID$from==i&resStationToStationwithID$to==j);
        jTrains=IDNameList[tempIndex]
        jTime=resStationToStationwithID$time[tempIndex]
        if(length(tempIndex)>0){
          
          zinter=setdiff(jTrains,iTrains)
          if(length(zinter)==0){
            gridCityToCityLines[i,j]=NA;
            gridCityToCityMinumTime[i,j]=NA;
          }
          else{
            zmatch=match(zinter,jTrains)
            gridCityToCityLines[i,j]=length(zmatch);
            gridCityToCityMinumTime[i,j]=min(jTime[zmatch]);
          }
        }
      }
    }
  }
  TRLlval[uID[ui]]=sum(gridCityToCityLines[!is.na(gridCityToCityLines)])/sum(normalrCityToCityLines[!is.na(normalrCityToCityLines)])
  TULlval[uID[ui]]=length(!is.na(gridCityToCityLines))/length(!is.na(normalrCityToCityLines))
  allNaIndex=(!is.na(gridCityToCityMinumTime))&(!is.na(normalrCityToCityMinumTime))
  TPTLlval[uID[ui]]=sum(gridCityToCityMinumTime[allNaIndex])/sum(normalrCityToCityMinumTime[allNaIndex])
}



for (i in 1:length(ints_all$China1000.tif)){
  if(TRLlval[ints_all$China1000.tif[i]]==0)
    TRLlval[ints_all$China1000.tif[i]]=0.9967152
  
}


for (i in 1:length(ints_all$China1000.tif)){
  if(TPTLlval[ints_all$China1000.tif[i]]==0)
    TPTLlval[ints_all$China1000.tif[i]]=1.000125
  
}



TRLlvalr <- setValues(r, TRLlval)
TRLlvaldf = as.data.frame(TRLlvalr, xy=TRUE)
names(TRLlvaldf)[3] <- 'value'


TPTLlvalr <- setValues(r, TPTLlval)
TPTLlvaldf = as.data.frame(TPTLlvalr, xy=TRUE)
names(TPTLlvaldf)[3] <- 'value'

writeRaster(TRLlvalr,'China1000_lines_provinces.tif',overwrite=TRUE)

writeRaster(TPTLlvalr,'China1000_times_provinces.tif',overwrite=TRUE)




Ct_TPTLlval=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  if(Ct_TPTLlval[county_all_df_alllines$FID[i]]<TPTLlval[county_all_df_alllines$China1000.tif[i]])
  Ct_TPTLlval[county_all_df_alllines$FID[i]]=TPTLlval[county_all_df_alllines$China1000.tif[i]]
}



Pt_TPTLlval=region_province_shp$pID*0

for (i in 1:length(province_all_df_alllines$pID)){
  
  if(Pt_TPTLlval[province_all_df_alllines$pID[i]]<TPTLlval[province_all_df_alllines$China1000.tif[i]])
    Pt_TPTLlval[province_all_df_alllines$pID[i]]=TPTLlval[province_all_df_alllines$China1000.tif[i]]
}

region_county_shp$Ct_TPTLlval=Ct_TPTLlval



region_province_shp<-region_province_shp%>%
  arrange(pID)
region_province_shp$Pt_TPTLlval=Pt_TPTLlval

#############draw_functional loss for county#########3

tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill = ifelse(Ct_TPTLlval != 0, Ct_TPTLlval, NA)),colour = alpha("black",0))+
  geom_sf(data=region_province_shp, fill = NA,size = 0.2,colour = alpha("black"))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")), name='Ratio of the minimum travel time',na.value = "white")+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few(base_size = 12, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'))


print(tempplot)
ggsave(wid=16,hei=5,paste0('../Res/','Functionalloss_in_county.pdf'))


#############draw_functional loss for province#########3

region_province_shp<-region_province_shp%>%
  arrange(Pt_TPTLlval)
region_province_shp1=region_province_shp[c(2,5:34),]
tempplot=ggplot(data=region_province_shp1, aes(x=NAME, y=Pt_TPTLlval, fill = Pt_TPTLlval)) +
  geom_bar(stat="identity")+
  #scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
  scale_fill_gradient(low="white",high="darkred")+
  coord_flip()+
  scale_x_discrete(name="",limits = region_province_shp1$NAME)+
  #theme()+
  theme_few(base_size = 12, base_family = "sans")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  theme(legend.position="none" )+
  ylab("Ratio of the minimum travel time") 
print(tempplot)
ggsave(wid=4,hei=5,paste0('../Res/','Functionalloss_in_province_bar.pdf'))





#writeRaster(Orr,'China1000_O_linkslosscausedbyonegrid.tif',overwrite=TRUE)


#my_breaks = c(1000, 10000, 100000, 1000000,10000000)
# region_shp <- readOGR("../Data/省级行政区.shp",encoding ="GBK")
# region_AG <- fortify(region_shp)
# region_m <- spTransform(region_shp, "+proj=merc +units=m")#墨卡托投影



tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = TRLlvaldf,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= brewer.pal(11, "Spectral"),  na.value = "white",name='value')
tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
print(tempplot)


tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = TPTLlvaldf,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",name='value')
tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
print(tempplot)



##########calculated flood#################

flood_tif <- raster("../Data/EuropeanCommission/floodMapGL_rp20y/floodMapGL_rp20y.tif")
grid_tif=raster(out_raster_file)

#r4 <- overlay(flood_tif, grid_tif,  fun=function(x,y){return(x*y)} )

flood_tif <- projectRaster(flood_tif,grid_tif,method = 'ngb')

flood_mask <- mask(flood_tif,grid_tif)
flood_mask_values=values(flood_mask)

  
flood_mask_as<- setValues(r, flood_mask_values)
flood_mask_df = as.data.frame(flood_mask_as, xy=TRUE)
names(flood_mask_df)[3] <- 'value'


tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = flood_mask_df,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",name='value')
tempplot=tempplot+geom_sf(data=region_province_shp,fill="white",colour = alpha("black", 1/10), size = 0.2,alpha = .1)
print(tempplot)


Ollval_flood=flood_mask_values*Ollval
flood_mask_values<- setValues(r, Ollval_flood)
flood_mask_df = as.data.frame(flood_mask_values, xy=TRUE)
names(flood_mask_df)[3] <- 'value'

tempplot=ggplot()+theme_few(base_size = 18, base_family = "sans")+geom_raster(data = flood_mask_df,mapping=aes(x=x, y=y, fill= ifelse(value != 0, value, NA)))+
  scale_fill_gradientn(colours= rev(brewer.pal(11, "Spectral")),  na.value = "white",name='value')
tempplot=tempplot+geom_sf(data=region_province_shp,fill="white",colour = alpha("black", 1/10), size = 0.2,alpha = .1)
print(tempplot)




##############calculated 


mesh_grid_shp <- readOGR("../Data/JP/grid/grid.shp",encoding ="GBK")
mesh_grid_shp_m <- spTransform(mesh_grid_shp, "+proj=merc +units=m")
#region_county_shp_m$FID=c(1:2391)
writeOGR(mesh_grid_shp_m,"mesh_grid_tf.shp","mash_grid", driver="ESRI Shapefile",overwrite_layer=TRUE)

mesh_grid = st_read("mesh_grid_tf.shp", quiet=TRUE)


ints_mesh_grid= st_intersection(mesh_grid, region_county_shp)%>% 
  st_drop_geometry()




####################################calculated_p1###########################
pop_ssp1_data<-read.csv("../Data/JP/pop_ssp1.csv")
index_col=c(6,8,13)+4
county_pop_ssp1_data=pop_ssp1_data[1:2391,index_col]*0

for(l in 1:length(index_col)){
  i=index_col[l]
  for(j in 1:length(ints_mesh_grid$FID)){
    county_pop_ssp1_data[ints_mesh_grid$FID[j],l]=county_pop_ssp1_data[ints_mesh_grid$FID[j],l]+pop_ssp1_data[ints_mesh_grid$gID[j],i]
  }
}
ocounty_pop_ssp1_data=county_pop_ssp1_data
ocounty_pop_ssp1_data$Physcal=region_county_shp$Length_tvals
#county_pop_ssp1_data$Systematic=region_county_shp$Ct_Ollval
#county_pop_ssp1_data$Functional=region_county_shp$Ct_TPTLlval


county_pop_ssp1_df <- melt(ocounty_pop_ssp1_data, id.vars="Physcal", variable.name="Population", value.name="p1")

county_pop_ssp1_df=county_pop_ssp1_df[which(county_pop_ssp1_df$Physcal!=0),]

# ggplot(data = county_pop_ssp1_df, mapping = aes(x = Physcal, y = p1, linetype = Population, colour = Population, shape = Population, fill = Population))+ 
#   geom_violin(alpha=0.9,size=2)+theme_few(base_size = 18, base_family = "sans")+
#   scale_x_continuous(trans='log10')
# 
# ggplot(data = county_pop_ssp1_data,aes(x = Physcal, y = p1_2030))+ 
#   geom_line(alpha=0.4) + geom_point(alpha=0.9,size=2)+theme_few(base_size = 18, base_family = "sans")


county_pop_ssp1_df <- county_pop_ssp1_df %>%
  mutate( bin=cut_interval(p1, 4) ) 

tempplot=ggplot(data = county_pop_ssp1_df, mapping = aes(x = bin, y = Physcal, fill = Population))+ 
  geom_boxplot(alpha=0.9)+theme_few(base_size = 10, base_family = "sans")+
  ylab("Physical damage(billion RMB)")+
  xlab("Population(million)")+
  theme(legend.position = c(.99, 0.99),legend.justification = c("right", "top"))
print(tempplot)
ggsave(wid=4,hei=3,paste0('../Res/','physical_Population.pdf'))

# scatterplotMatrix(~ Physcal + p1_2030 + p1_2050 + p1_2100, data=county_pop_ssp1_data, spread=FALSE)
# scatterplotMatrix(~ Systematic + p1_2030 + p1_2050 + p1_2100, data=county_pop_ssp1_data, spread=FALSE)






ocounty_pop_ssp1_data=county_pop_ssp1_data

ocounty_pop_ssp1_data$Systematic=region_county_shp$Ct_Ollval
#county_pop_ssp1_data$Functional=region_county_shp$Ct_TPTLlval

county_pop_ssp1_df <- melt(ocounty_pop_ssp1_data, id.vars="Systematic", variable.name="Population", value.name="p1")

county_pop_ssp1_df=county_pop_ssp1_df[which(county_pop_ssp1_df$Systematic!=0),]

county_pop_ssp1_df <- county_pop_ssp1_df %>%
  mutate( bin=cut_interval(p1, 4) ) 

tempplot=ggplot(data = county_pop_ssp1_df, mapping = aes(x = bin, y = Systematic, fill = Population))+ 
  geom_boxplot(alpha=0.9)+theme_few(base_size =8, base_family = "sans")+
  ylab("Systematic damage(billion RMB)")+
  xlab("Population(million)")+
  theme(legend.position = c(.99, 0.99),legend.justification = c("right", "top"))
print(tempplot)
ggsave(wid=4,hei=3,paste0('../Res/','systematic_Population.pdf'))

# scatterplotMatrix(~ Physcal + p1_2030 + p1_2050 + p1_2100, data=county_pop_ssp1_data, spread=FALSE)
# scatterplotMatrix(~ Systematic + p1_2030 + p1_2050 + p1_2100, data=county_pop_ssp1_data, spread=FALSE)


ocounty_pop_ssp1_data=county_pop_ssp1_data

ocounty_pop_ssp1_data$Functional=region_county_shp$Ct_TPTLlval

county_pop_ssp1_df <- melt(ocounty_pop_ssp1_data, id.vars="Functional", variable.name="Population", value.name="p1")

county_pop_ssp1_df=county_pop_ssp1_df[which(county_pop_ssp1_df$Functional!=0),]

county_pop_ssp1_df <- county_pop_ssp1_df %>%
  mutate( bin=cut_interval(p1, 4) ) 

tempplot=ggplot(data = county_pop_ssp1_df, mapping = aes(x = bin, y = Functional, fill = Population))+ 
  geom_boxplot(alpha=0.9)+theme_few(base_size =8, base_family = "sans")+
  ylab("Functional damage")+
  xlab("Population(million)")+
  theme(legend.position = c(.99, 0.99),legend.justification = c("right", "top"))
print(tempplot)
ggsave(wid=4,hei=3,paste0('../Res/','Population_functional.pdf'))





