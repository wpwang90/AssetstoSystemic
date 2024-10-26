#Source code for Railway system climate risk management

#Calculate three types of railway lossess under landslides and floods.

#Copyright (C) 2024 Weiping Wang. All versions released under the GNU Affero General Public License v3.0 license.

##Calculate three types of railwaylosses under landslides: physical assets, systemic functions and infrastructure services. 
##First, import the landslide susceptibility and frequency of rainfall events exceeding thresholds data, and compute the probability of asset loss. 
##Next, calculate the three types of losses—railway physical assets, system functionality, 
#and infrastructure services—across different counties based on the probability of asset loss from landslide disasters."
library(terra)
#rcp=c(1,2,3,5)
rcp=c(1,2,3)
rcp_length=c("1-2.6","2-4.5","3-7.0","5-8.5")
period=c("f","m","n")
period_length=c("2080-2099","2041-2060","2021-2040")
period_name=c(2100,2050,2030)
ssp_namelist=c('ssp1','ssp2','ssp3')

slr_flood_len_list=list()
rcpi=2
period_i=2

xyr=xyFromCell(r,1:ncell(r))
xyr_df=data.frame(xyr)
p1 <- st_as_sf(xyr_df, coords = c("x", "y"), crs = crs(r))
p2 <- st_transform(p1, crs= crs(raster(paste0('../Data/landslides/freq_s',rcp[rcpi],period[period_i],'_median.tif'))))
p_coord=st_coordinates(p2)



freq_slr_flood_file=paste0('../Data/landslides/freq_s',rcp[rcpi],period[period_i],'_median.tif')
suscep_slr_flood_file=paste0('../Data/landslides/suscep_ca2_s',rcp[rcpi],period[period_i],'_median.tif')


freq_flood_tif=raster(freq_slr_flood_file)

suscep_flood_tif=raster(suscep_slr_flood_file)

# compute the probability of railway system loss under landsildes
grid2 <- projectRaster(suscep_flood_tif*freq_flood_tif/100, r, method = "ngb")



slr_flood_len=values(grid2)
slr_flood_len[is.na(slr_flood_len)] = 0


slr_flood_len_list=append(slr_flood_len_list,list(slr_flood_len))




final_df=data.frame()


total_i=(rcpi-1)*length(period)+period_i
total_i=1

##Calculate the loss of physical assets for railway system under landslides

Length_tvals_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Length_tvals_slr[county_all_df_alllines$FID[i]]=Length_tvals_slr[county_all_df_alllines$FID[i]]+
    as.numeric(county_all_df_alllines$lenght[i]*slr_flood_len_list[[total_i]][county_all_df_alllines$China1000.tif[i]])/1000
}

##Calculate the loss of physical assets for railway system under landslides

region_county_shp$Length_tvals_slr=Length_tvals_slr*county_railwayline_cost*0.1


Land_Length_tvals_slr=region_county_shp$Length_tvals_slr
region_county_shp$Length_tvals_land=region_county_shp$Length_tvals_slr

##Calculate the loss of systemic functions for railway system under landslides

Ct_Ollval_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Ct_Ollval_slr[county_all_df_alllines$FID[i]]=Ct_Ollval_slr[county_all_df_alllines$FID[i]]+
    Ollval[county_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[total_i]][county_all_df_alllines$China1000.tif[i]]
}



region_county_shp$Ct_Ollval_slr=Ct_Ollval_slr*0.1/1000*county_railwayline_cost

region_county_shp$Ct_Ollval_land=region_county_shp$Ct_Ollval_slr
Land_Ct_Ollval_slr=region_county_shp$Ct_Ollval_slr


##Calculate the loss of infrastructure services for railway system under landslides

Ct_TPTLlval_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  
  temp=TFulLlval[county_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[total_i]][county_all_df_alllines$China1000.tif[i]]
  
  if(Ct_TPTLlval_slr[county_all_df_alllines$FID[i]]<temp)
    Ct_TPTLlval_slr[county_all_df_alllines$FID[i]]=temp
}


region_county_shp$Ct_TPTLlval_slr=Ct_TPTLlval_slr

region_county_shp$Ct_TPTLlval_land=region_county_shp$Ct_TPTLlval_slr
Land_Ct_TPTLlval_slr=region_county_shp$Ct_TPTLlval_slr   


##Calculate three types of railwaylosses under river flooding: physical assets, systemic functions and infrastructure services. 
##First, import the inundated depth caused by river flooding, and then we combine flood depth-damage function to compute the probability of asset loss. 
##Next, calculate the three types of losses—railway physical assets, system functionality, 
#and infrastructure services—across different counties based on the probability of asset loss from river flooding."

library(terra)
library(ncdf4)
library(raster)

#rcp=c(1,2,3,5)
rcp_len=c(3,3,2,3)
model_namelist=c("MPI-ESM1-2-LR","NorESM2-LM","KIOST-ESM")
rcp_length=c("1-2.6","2-4.5","3-7.0","5-8.5")




period_length=c("2040","2060","2100")
period_name=c(2030,2050,2090)

#period_name=c(2090,2050,2030)
ssp_namelist=c('ssp1','ssp2','ssp3')

ssp_namelist_file=c('ssp126','ssp245','ssp370')

slr_flood_len_list=list()
rcpi=2
period_i=2

    
river_flood_len_list=list()

for(model_i in 1:rcp_len[rcpi]){
  
  ncfile_name=paste0('../Data/CaMa-flood/',model_namelist[model_i],"_",ssp_namelist_file[rcpi],'_o_flddph',period_length[period_i],'return100.nc')
  print(ncfile_name)
  temp_flood_depth=cal_flood_depth_r(ncfile_name,r)
  river_flood_len_list=append(river_flood_len_list,list(temp_flood_depth))
}
temp_river_len_list=river_flood_len_list[[1]]
for(j in 1:length(river_flood_len_list[[1]])){
  temp_value=1:rcp_len[rcpi]
  for(model_i in 1:rcp_len[rcpi]){
    temp_value[model_i]=river_flood_len_list[[model_i]][j]
  }
  temp_river_len_list[j]=stats::median(temp_value,na.rm = TRUE)
}
flddph_damage=temp_river_len_list*0
for(i in 1:length(temp_river_len_list)){
  flddph_damage[i]=flood_damage(temp_river_len_list[i]-5)
}

slr_flood_len_list=append(slr_flood_len_list,list(flddph_damage))
    
    




final_df=data.frame()


total_i=(rcpi-1)*length(period_length)+period_i
total_i=1


##Calculate the loss of physical assets for railway system under river flooding

Length_tvals_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Length_tvals_slr[county_all_df_alllines$FID[i]]=Length_tvals_slr[county_all_df_alllines$FID[i]]+
    as.numeric(county_all_df_alllines$lenght[i]*slr_flood_len_list[[total_i]][county_all_df_alllines$China1000.tif[i]])/1000
}


region_county_shp$Length_tvals_slr=Length_tvals_slr*county_railwayline_cost*0.1

region_county_shp$Length_tvals_river=region_county_shp$Length_tvals_slr
River_Length_tvals_slr=region_county_shp$Length_tvals_slr

##Calculate the loss of systemic functions for railway system under landslides

Ct_Ollval_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Ct_Ollval_slr[county_all_df_alllines$FID[i]]=Ct_Ollval_slr[county_all_df_alllines$FID[i]]+
    Ollval[county_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[total_i]][county_all_df_alllines$China1000.tif[i]]
}



region_county_shp$Ct_Ollval_slr=Ct_Ollval_slr*0.1/1000*county_railwayline_cost


region_county_shp$Ct_Ollval_river=region_county_shp$Ct_Ollval_slr
River_Ct_Ollval_slr=region_county_shp$Ct_Ollval_slr

##Calculate the loss of infrastructure services for railway system under river flooding

Ct_TPTLlval_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  
  temp=TFulLlval[county_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[total_i]][county_all_df_alllines$China1000.tif[i]]
  
  if(Ct_TPTLlval_slr[county_all_df_alllines$FID[i]]<temp)
    Ct_TPTLlval_slr[county_all_df_alllines$FID[i]]=temp
}


region_county_shp$Ct_TPTLlval_slr=Ct_TPTLlval_slr


region_county_shp$Ct_TPTLlval_river=region_county_shp$Ct_TPTLlval_slr
River_Ct_TPTLlval_slr=region_county_shp$Ct_TPTLlval_slr


###########################calculate coastal hazard#######
rcp=c(26,45,85)
slr_flood_len_list=list()
rcpi=45

slr_flood_file=paste0('../Data/slr_scenario_dem_rc_rg_rtp_sbl/slr',rcpi,'_dem_rc_rg_rtp_sbl.shp')
slr_flood_shp= readOGR(slr_flood_file)

slr_flood_shp_m <- spTransform(slr_flood_shp, "+proj=merc +units=m")
writeOGR(slr_flood_shp_m,paste0("slr_flood_tf",rcpi,".shp"),"slr_flood", driver="ESRI Shapefile",overwrite_layer=TRUE)

slr_flood = st_read(paste0("slr_flood_tf",rcpi,".shp"), quiet=TRUE)


slr_flood_int = st_intersection(slr_flood, sf)%>% 
  st_drop_geometry()

slr_flood_len=vals*0
for (i in 1:length(slr_flood_int$China1000.tif)){
  slr_flood_len[slr_flood_int$China1000.tif[i]]=1
}
slr_flood_len_list=append(slr_flood_len_list,list(slr_flood_len))




rcpi=1

###############cal physcial lossess###########



Length_tvals_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Length_tvals_slr[county_all_df_alllines$FID[i]]=Length_tvals_slr[county_all_df_alllines$FID[i]]+
    as.numeric(county_all_df_alllines$lenght[i]*slr_flood_len_list[[rcpi]][county_all_df_alllines$China1000.tif[i]])/1000
}


region_county_shp$Length_tvals_slr=Length_tvals_slr*county_railwayline_cost*0.1


Coast_Length_tvals_slr=region_county_shp$Length_tvals_slr

#############calculate systematic lossess##########3
Ct_Ollval_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  Ct_Ollval_slr[county_all_df_alllines$FID[i]]=Ct_Ollval_slr[county_all_df_alllines$FID[i]]+
    Ollval[county_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[rcpi]][county_all_df_alllines$China1000.tif[i]]
}



Pt_Ollval_slr=region_province_shp$pID*0

for (i in 1:length(province_all_df_alllines$pID)){
  Pt_Ollval_slr[province_all_df_alllines$pID[i]]=Pt_Ollval_slr[province_all_df_alllines$pID[i]]+
    Ollval[province_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[rcpi]][province_all_df_alllines$China1000.tif[i]]
}


region_county_shp$Ct_Ollval_slr=Ct_Ollval_slr*0.1/1000*county_railwayline_cost




Coast_Ct_Ollval_slr=region_county_shp$Ct_Ollval_slr


#########calculated function loss for aggregated 

Ct_TPTLlval_slr=region_county_shp$FID*0

for (i in 1:length(county_all_df_alllines$FID)){
  
  temp=TFulLlval[county_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[rcpi]][county_all_df_alllines$China1000.tif[i]]
  
  if(Ct_TPTLlval_slr[county_all_df_alllines$FID[i]]<temp)
    Ct_TPTLlval_slr[county_all_df_alllines$FID[i]]=temp
}



Pt_TPTLlval_slr=region_province_shp$pID*0

for (i in 1:length(province_all_df_alllines$pID)){
  
  temp=TFulLlval[province_all_df_alllines$China1000.tif[i]]*slr_flood_len_list[[rcpi]][province_all_df_alllines$China1000.tif[i]]
  
  if(Pt_TPTLlval_slr[province_all_df_alllines$pID[i]]<temp)
    Pt_TPTLlval_slr[province_all_df_alllines$pID[i]]=temp
}

region_county_shp$Ct_TPTLlval_slr=Ct_TPTLlval_slr



region_province_shp<-region_province_shp%>%
  arrange(pID)
region_province_shp$Pt_TPTLlval_slr=Pt_TPTLlval_slr

Coast_Ct_TPTLlval_slr=region_county_shp$Ct_TPTLlval_slr






par(family='STKaiti')

rcpi=2
###########################draw figure###############

Total_Length_tvals_slr=pmax(Land_Length_tvals_slr/3+River_Length_tvals_slr/3+Coast_Length_tvals_slr/3)
temp_Length_tvals_slr=calMaxIndex(Land_Length_tvals_slr,River_Length_tvals_slr,Coast_Length_tvals_slr)
Max_Length_tvals_slr=temp_Length_tvals_slr[[1]]
alpha_value=temp_Length_tvals_slr[[2]]
#######physical########
tempplot=ggplot()+
  #geom_sf(data=region_county_shp,aes(fill = ifelse(Total_Length_tvals_slr != 0, Total_Length_tvals_slr, NA)),colour = alpha("black",0))+
  #geom_sf(data=region_county_shp,aes(fill = rescale_Value(Total_Length_tvals_slr)),colour = alpha("black",0))+
  geom_sf(data=region_county_shp,aes(fill =cut(Total_Length_tvals_slr,quantile(Total_Length_tvals_slr,probs=c(0,0.25,0.5,0.75,0.95,1),na.rm=TRUE))),lwd = 0)+
  
  #geom_sf(data=region_county_shp,aes(fill = (Total_Length_tvals_slr),colour = alpha("black",0))+
  
  geom_sf(data=region_province_shp, fill = NA,lwd = 0.05,colour = alpha("black"))+
  #scale_fill_gradientn(colours= brewer.pal(4, "YlGnBu"),name='',na.value = "white")+
  scale_fill_manual(values= brewer.pal(6, "Blues"),name='',na.value = "#f0f0f0")+
  
  #scale_fill_paletteer_c("ggthemes::Classic Blue", name="",na.value = "white")+

  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few()+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  ggtitle(paste0('Physical damage loss (billion RMB)',' under multi hazards with RCP',rcp[rcpi]))+
  #theme(legend.direction = "vertical")
  theme(legend.position="bottom",legend.key.size = unit(0.1, "inches"),legend.text=element_text(size=6),
        plot.title = element_text(size=8))



print(tempplot)

plot1=tempplot

ggsave(wid=6,hei=5,paste0('../Res/','Multihazard',rcp[rcpi],'length_in_county','.pdf'))

ggsave(wid=6,hei=4, dpi = 450,paste0('../Res/','Multihazard',rcp[rcpi],'length_in_county','.jpg'))

##############max
#labs <- c(1, 2, 3,4)
#labs_plot=c('None','Landslides','River flooding','Coastal flooding')
labs_plot=c('None','Landslides<1/2','Landslides<3/4','Landslides','River flooding<1/2','River flooding<3/4','River flooding','Coastal flooding<1/2','Coastal flooding<3/4','Coastal flooding')

tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill = factor(alpha_value)),lwd = 0)+
  geom_sf(data=region_province_shp, fill = NA,lwd = 0.05,colour = alpha("black"))+
  #scale_fill_manual(values=pal, name="",na.value = "white")+
  scale_fill_manual(breaks = c("0","1", "2", "3","4", "5", "6","7","8", "9"), 
                    #values=c("#EEEEEE","#fc8d62", "#66c2a5", "#8da0cb"),
                    #values=c("#EEEEEE","#e41a1c", "#377eb8", "#4daf4a"),
                    
                    values=c("#EEEEEE", "#F2E98E","#C9C875", "#A19F5A",
                             "#7AC0B7", "#788695","#416B7D",
                             "#E45C3F", "#B86D79", "#AA2B39"
                    ),
                    # values=c("#EEEEEE", "#F986B4","#FAC3D8", "#FEEFF5",
                    #          "#BE3298", "#DE99C9","#F8E5F2",
                    #          "#6C3389", "#B599C3", "#ECE6F1"
                    # ),
                    #na.value = "grey80",
                    name="",
                    #na.value = "white",
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom"))+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  guides(alpha="none")+
  ylab("")+
  #xlab("none")+
  theme_few()+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  ggtitle(paste0('Physical damage loss (billion RMB)',' caused by dominated hazard with RCP',rcp[rcpi]))+
  # theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
  #       legend.key.width= unit(1.5, 'cm'))

  theme(legend.position="bottom",legend.key.size = unit(0.1, "inches"),legend.text=element_text(size=6),
        plot.title = element_text(size=8))


print(tempplot)

plot2=tempplot


ggsave(wid=6,hei=5,paste0('../Res/','MaxMultihazard',rcp[rcpi],'length_in_county','.pdf'))

ggsave(wid=6,hei=4,dpi = 450,paste0('../Res/','MaxMultihazard',rcp[rcpi],'length_in_county','.jpg'))

#ggsave(wid=6,hei=4, dpi = 450,paste0('../Res/','Multihazard',rcp[rcpi],'length_in_county','.jpg'))



Total_Ct_Ollval_slr=pmax(Land_Ct_Ollval_slr+River_Ct_Ollval_slr+Coast_Ct_Ollval_slr)

temp_Max_Ct_Ollval_slr=calMaxIndex(Land_Ct_Ollval_slr,River_Ct_Ollval_slr,Coast_Ct_Ollval_slr)
Max_Ct_Ollval_slr=temp_Max_Ct_Ollval_slr[[1]]
alpha_value=temp_Max_Ct_Ollval_slr[[2]]

######systematic######
tempplot=ggplot()+
  #geom_sf(data=region_county_shp,aes(fill = ifelse(Total_Ct_Ollval_slr != 0, Total_Ct_Ollval_slr, NA)),colour = alpha("black",0))+
  geom_sf(data=region_county_shp,aes(fill =cut(Total_Ct_Ollval_slr,quantile(Total_Ct_Ollval_slr,probs=c(0,0.25,0.5,0.75,0.95,1),na.rm=TRUE))),lwd=0)+
  
  geom_sf(data=region_province_shp, fill = NA,lwd = 0.05,colour = alpha("black"))+
  #scale_fill_manual(colours= brewer.pal(4, "YlGn"), name='',na.value = "white")+
  #scale_fill_manual(values= brewer.pal(5, "YlGn"),name='',na.value = "white")+
  scale_fill_manual(values= brewer.pal(5, "Greens"),name='',na.value = "#f0f0f0")+
  
  #scale_fill_paletteer_c("ggthemes::Classic Green",name="",na.value = "white")+
  
  
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few()+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  ggtitle(paste0('Systematic damage loss (billion RMB)',' under multi hazards with RCP',rcp[rcpi]))+
  
  # theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
  #       legend.key.width= unit(1.5, 'cm'))
  #theme(legend.direction = "vertical")
  theme(legend.position="bottom",legend.key.size = unit(0.1, "inches"),legend.text=element_text(size=6),
        plot.title = element_text(size=8))


print(tempplot)
plot3=tempplot

ggsave(wid=6,hei=5,paste0('../Res/','Multihazard',rcp[rcpi],'Systematicloss_in_county.pdf'))

ggsave(wid=6,hei=4,paste0('../Res/','Multihazard',rcp[rcpi],'Systematicloss_in_county.jpg'))

#####max


tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill = factor(alpha_value)),lwd = 0)+
  geom_sf(data=region_province_shp, fill = NA,lwd = 0.05,colour = alpha("black"))+
  #scale_fill_manual(values=pal, name="",na.value = "white")+
  scale_fill_manual(breaks = c("0","1", "2", "3","4", "5", "6","7","8", "9"), 
                    #values=c("#EEEEEE","#fc8d62", "#66c2a5", "#8da0cb"),
                    #values=c("#EEEEEE","#e41a1c", "#377eb8", "#4daf4a"),
                    values=c("#EEEEEE", "#F2E98E","#C9C875", "#A19F5A",
                             "#7AC0B7", "#788695","#416B7D",
                             "#E45C3F", "#B86D79", "#AA2B39"
                    ),
                    # values=c("#EEEEEE", "#F986B4","#FAC3D8", "#FEEFF5",
                    #          "#BE3298", "#DE99C9","#F8E5F2",
                    #          "#6C3389", "#B599C3", "#ECE6F1"
                    # ),
                    #na.value = "grey80",
                    name="",
                    #na.value = "white",
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom"))+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few()+
  guides(alpha="none")+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  ggtitle(paste0('Systematic damage loss (billion RMB)',' under dominated hazard with RCP',rcp[rcpi]))+
  
  # theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
  #       legend.key.width= unit(1.5, 'cm'))
  theme(legend.position="bottom",legend.key.size = unit(0.1, "inches"),legend.text=element_text(size=6),
        plot.title = element_text(size=8))

print(tempplot)
plot4=tempplot
ggsave(wid=6,hei=5,paste0('../Res/','MaxMultihazard',rcp[rcpi],'Systematicloss_in_county.pdf'))


Total_Ct_TPTLlval_slr=pmax(Land_Ct_TPTLlval_slr+River_Ct_TPTLlval_slr+Coast_Ct_TPTLlval_slr)
#Max_Ct_TPTLlval_slr=calMaxIndex(Land_Ct_TPTLlval_slr,River_Ct_TPTLlval_slr,Coast_Ct_TPTLlval_slr)

temp_Max_Ct_TPTLlval_slr=calMaxIndex(Land_Ct_TPTLlval_slr,River_Ct_TPTLlval_slr,Coast_Ct_TPTLlval_slr)

Max_Ct_TPTLlval_slr=temp_Max_Ct_TPTLlval_slr[[1]]
alpha=temp_Max_Ct_TPTLlval_slr[[2]]

#############draw_functional loss for county#########3

tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill =cut(Total_Ct_TPTLlval_slr,quantile(Total_Ct_TPTLlval_slr,probs=c(0,0.25,0.5,0.75,0.95,1),na.rm=TRUE))),lwd=0)+
  
  geom_sf(data=region_province_shp, fill = NA,size = 0.2,colour = alpha("black"),lwd=0.05)+
  #scale_fill_manual(colours= brewer.pal(4, "YlGn"), name='',na.value = "white")+
  #scale_fill_manual(values= brewer.pal(5, "YlOrBr"),name='',na.value = "white")+
  scale_fill_manual(values= brewer.pal(5, "Reds"),name='',na.value = "#f0f0f0")+
  
  #scale_fill_gradientn(colours= brewer.pal(30, "YlOrRd"), name='',na.value = "white")+
  #scale_fill_paletteer_c(rev("grDevices::BluYl"),name="",na.value = "white")+
  #scale_fill_brewer()

  
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  #xlab("none")+
  theme_few()+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  ggtitle(paste0('Daily functional damage loss (billion RMB)',' under multi hazards with RCP',rcp[rcpi]))+
  #theme(legend.direction = "vertical")
  theme(legend.position="bottom",legend.key.size = unit(0.1, "inches"),legend.text=element_text(size=6),
        plot.title = element_text(size=8))

  # theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
  #       legend.key.width= unit(1.5, 'cm'))


print(tempplot)
plot5=tempplot

ggsave(wid=6,hei=5,paste0('../Res/','Multihazard',rcp[rcpi],'Functionalloss_in_county.pdf'))

#######max
tempplot=ggplot()+
  geom_sf(data=region_county_shp,aes(fill = factor(alpha)),lwd=0)+
  geom_sf(data=region_province_shp, fill = NA,size = 0.01,colour = alpha("black"),lwd=0.05)+
  #scale_fill_manual(values=pal, name="",na.value = "white")+
  scale_fill_manual(breaks = c("0","1", "2", "3","4", "5", "6","7","8", "9"), 
                    #values=c("#EEEEEE","#fc8d62", "#66c2a5", "#8da0cb"),
                    #values=c("#EEEEEE","#e41a1c", "#377eb8", "#4daf4a"),
                    values=c("#EEEEEE", "#F2E98E","#C9C875", "#A19F5A",
                             "#7AC0B7", "#788695","#416B7D",
                             "#E45C3F", "#B86D79", "#AA2B39"
                    ),
                    # values=c("#EEEEEE", "#F986B4","#FAC3D8", "#FEEFF5",
                    #          "#BE3298", "#DE99C9","#F8E5F2",
                    #          "#6C3389", "#B599C3", "#ECE6F1"
                    # ),
                    
                    #na.value = "grey80",
                    name="",
                    #na.value = "white",
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom"))+
  #tempplot=tempplot+geom_polygon(data=region_m, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.2,fill="white", alpha = .01)
  ylab("")+
  guides(alpha="none")+
  #xlab("none")+
  theme_few()+
  # theme(legend.position = c(.99, 0.4),
  #       legend.justification = c("right", "top"))
  ggtitle(paste0('Daily functional damage loss (billion RMB)',' under dominated hazard with RCP',rcp[rcpi]))+
  
  # theme(axis.title.x = element_blank(),legend.position="bottom",legend.key.height= unit(0.5, 'cm'),
  #       legend.key.width= unit(1.5, 'cm'))
  theme(legend.position="bottom",legend.key.size = unit(0.1, "inches"),legend.text=element_text(size=6),
        plot.title = element_text(size=8))


print(tempplot)
ggsave(wid=6,hei=5,paste0('../Res/','MaxMultihazard',rcp[rcpi],'MaxFunctionalloss_in_county.pdf'))

plot6=tempplot
finalp <- ggpubr::ggarrange(plot1, plot2, plot3, plot4,plot5,plot6, nrow = 3, ncol=2,labels = letters[1:6])#将p1-p4四幅图组合成一幅图，按照两行两列排列，标签分别为A、B、C、D。（LETTERS[1:4] 意为提取26个大写英文字母的前四个:A、B、C、D）
finalp
ggsave(wid=10,hei=12,dpi=450,paste0('../Res/','MaxMultihazard',rcp[rcpi],'MaxFinalRe.jpg'))

ggsave(wid=10,hei=12,paste0('../Res/','MaxMultihazard',rcp[rcpi],'MaxFinalRe.pdf'))

