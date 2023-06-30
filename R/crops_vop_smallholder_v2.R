# Subset mapspam VoP data to cumulative farm size and caculate VoP per: 1) cell (total VoP); 2) per harvested area of crop(s) = economic yield per ha of crop; and 3) per cell area (less water bodies) = VoP per ha of terrestrial land in cell.

require(terra)
require(foreign)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Read in boundary of subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Create cellsize raster
cellsize_ha<-terra::cellSize(base_raster,unit="ha")

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Create a higher resolution raster to work out area of waterbody per pixel
#cellsize_da<-terra::disagg(cellsize_ha,fact=10)
#water_rast<-terra::rasterize(waterbodies,cellsize_da)

# Work out proportion of cell that is not water and multiply cellsize_ha by this value
#water_rast<-(100-terra::aggregate(water_rast,fact=10,fun=sum,na.rm=T))/100
#water_rast[is.na(water_rast)]<-1
#cellsize_ha<-cellsize_ha*water_rast

# Set directory for atlas IFPRI mapspam data by farm size
SpamDir<-paste0(DataDir,"/atlas_mapspam/intermediate/data_v4")

IFPRIDirInt_ep<-paste0(DataDir,"/atlas_mapspam/intermediate/vop_per_harv_area_v4")
if(!dir.exists(IFPRIDirInt_ep)){
  dir.create(IFPRIDirInt_ep,recursive=T)
}

IFPRIDirInt_cell<-paste0(DataDir,"/atlas_mapspam/intermediate/vop_per_cell_area_v4")
if(!dir.exists(IFPRIDirInt_cell)){
  dir.create(IFPRIDirInt_cell,recursive=T)
}

IFPRIDirInt_total<-paste0(DataDir,"/atlas_mapspam/intermediate/vop_total_v4")
if(!dir.exists(IFPRIDirInt_total)){
  dir.create(IFPRIDirInt_total,recursive=T)
}

IFPRIDirInt_ha<-paste0(DataDir,"/atlas_mapspam/intermediate/harvested_area_v4")
if(!dir.exists(IFPRIDirInt_ha)){
  dir.create(IFPRIDirInt_ha,recursive=T)
}

IFPRIDirInt_yield<-paste0(DataDir,"/atlas_mapspam/intermediate/yield_v4")
if(!dir.exists(IFPRIDirInt_yield)){
  dir.create(IFPRIDirInt_yield,recursive=T)
}

IFPRIDirInt_prod<-paste0(DataDir,"/atlas_mapspam/intermediate/production_v4")
if(!dir.exists(IFPRIDirInt_prod)){
  dir.create(IFPRIDirInt_prod,recursive=T)
}

Vop_Files<-list.files(SpamDir,"_V_",full.names=T)
Vop_Files<-grep("_cum",Vop_Files,value=T)
Vop_Files<-grep(".DBF",Vop_Files,value=T)

HA_Files<-list.files(SpamDir,"_H_",full.names=T)
HA_Files<-grep("_cum",HA_Files,value=T)
HA_Files<-grep(".DBF",HA_Files,value=T)

Y_Files<-list.files(SpamDir,"_Y_",full.names=T)
Y_Files<-grep("_cum",Y_Files,value=T)
Y_Files<-grep(".DBF",Y_Files,value=T)


for(i in 1:length(Vop_Files)){
  Vop_data<-suppressWarnings(read.dbf(Vop_Files[i], as.is = T))
  HA_data<-suppressWarnings(read.dbf(HA_Files[i], as.is = T))
  Yield_data<-suppressWarnings(read.dbf(Y_Files[i], as.is = T))
  
  
  Vop_data[HA_data$CELL5M==4966310, "MAIZ_A"]
  HA_data[HA_data$CELL5M==4966310, "MAIZ_A"]
  Yield_data[Yield_data$CELL5M==4966310, "MAIZ_A"]
  
  Cols<-grep("_A",colnames(Vop_data),value=T)
  Cols<-Cols[!grepl("NAME_",Cols)]
  
  FS<-paste0("h",i+1)
  
  for(j in 1:length(Cols)){
    # Display progress
    cat('\r                                                                                                                                          ')
    cat('\r',paste0(Vop_Files[i],"-",Cols[j]))
    flush.console()
    
    crop_ha<-terra::rast(HA_data[,c("X","Y",Cols[j])],crs=crs(water_mask)) # ha
    crop_vop<-terra::rast(Vop_data[,c("X","Y",Cols[j])],crs=crs(water_mask)) # USD INT
    crop_yield<-terra::rast(Yield_data[,c("X","Y",Cols[j])],crs=crs(water_mask)) # kg/ha
    
    crop_ha<-terra::resample(crop_ha,water_mask,method="near")
    crop_vop<-terra::resample(crop_vop,water_mask,method="near")
    crop_yield<-terra::resample(crop_yield,water_mask,method="near")
    
    crop_vop<-terra::mask(terra::crop(crop_vop,adm1_africa),adm1_africa)*water_mask
    crop_ha<-terra::mask(terra::crop(crop_ha,adm1_africa),adm1_africa)*water_mask
    crop_yield<-terra::mask(terra::crop(crop_yield,adm1_africa),adm1_africa)*water_mask
    
    crop_production<-crop_yield*crop_ha # kg
    
    crop_vop_ha<-crop_vop/crop_ha # USD INT/ha
    crop_vop_ha[is.infinite(crop_vop_ha)]<-NA
    crop_vop_cell<-crop_vop/cellsize_ha
    
    CROP<-gsub("_A","",names(crop_vop_ha))
    
    names(crop_production)<-paste0(CROP,"-",FS,"-tons")
    names(crop_yield)<-paste0(CROP,"-",FS,"-ton_ha")
    names(crop_ha)<-paste0(CROP,"-",FS,"-ha")
    names(crop_vop)<-paste0(CROP,"-",FS,"-IND_total")
    names(crop_vop_ha)<-paste0(CROP,"-",FS,"-IND_ha")
    names(crop_vop_cell)<-paste0(CROP,"-",FS,"-IND_cell")
    
    suppressWarnings(terra::writeRaster(crop_production,file=paste0(IFPRIDirInt_prod,"/",FS,"-",CROP,"-prod.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_yield,file=paste0(IFPRIDirInt_yield,"/",FS,"-",CROP,"-yield.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_ha,file=paste0(IFPRIDirInt_ha,"/",FS,"-",CROP,"-harv_area.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_vop,file=paste0(IFPRIDirInt_total,"/",FS,"-",CROP,"-vop.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_vop_ha,file=paste0(IFPRIDirInt_ep,"/",FS,"-",CROP,"-vop_per_harv_ha.tif"),overwrite=T))
    suppressWarnings(terra::writeRaster(crop_vop_cell,file=paste0(IFPRIDirInt_cell,"/",FS,"-",CROP,"-vop_per_cell_ha.tif"),overwrite=T))
    
    list(crop_vop=crop_vop, crop_vop_ha=crop_vop_ha,crop_vop_cell=crop_vop_cell)
    
  }
  
}

# Validation: Check bar plots

# Total VoP
focal_crop<-"CER"

tot_vop_files<-list.files(IFPRIDirInt_total,focal_crop,full.names=T)

tot_vop_Stack<-terra::rast(tot_vop_files)

Angola<-adm1_africa[adm1_africa$COUNTRY=="Angola"]

tot_vop_Stack<-terra::mask(terra::crop(tot_vop_Stack,Angola),Angola)

vals<-unlist(lapply(1:terra::nlyr(tot_vop_Stack),FUN=function(i){
  sum(values(tot_vop_Stack[[i]]),na.rm=T)
}))

vals<-data.frame(Size=paste0("h",2:7),VoP=vals,Crop=focal_crop)

require(ggplot2)

ggplot(vals, aes(fill=Size, y=VoP, x=Crop)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()

# Mean VoP/ha
focal_crop<-"BANPL"

tot_vop_files<-list.files(IFPRIDirInt_ep,focal_crop,full.names=T)

tot_vop_Stack<-terra::rast(tot_vop_files)

Angola<-adm1_africa[adm1_africa$COUNTRY=="Angola"]

tot_vop_Stack<-terra::mask(terra::crop(tot_vop_Stack,Angola),Angola)

vals<-unlist(lapply(1:terra::nlyr(tot_vop_Stack),FUN=function(i){
  mean(values(tot_vop_Stack[[i]]),na.rm=T)
}))

vals<-data.frame(Size=paste0("h",2:7),VoP=vals,Crop=focal_crop)


ggplot(vals, aes(fill=Size, y=VoP, x=Crop)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()



for(i in 1:length(Vop_Files)){
  Vop_data<-suppressWarnings(read.dbf(Vop_Files[i], as.is = T))
  HA_data<-suppressWarnings(read.dbf(HA_Files[i], as.is = T))
  Yield_data<-suppressWarnings(read.dbf(Y_Files[i], as.is = T))
  
  Cols<-grep("_A",colnames(Vop_data),value=T)
  Cols<-Cols[!grepl("NAME_",Cols)]
  
  FS<-paste0("h",i+1)
  
  for(j in 1:length(Cols)){
    # Display progress
    cat('\r                                                                                                                                          ')
    cat('\r',paste0(Vop_Files[i],"-",Cols[j]))
    flush.console()
    
    crop_ha<-terra::rast(HA_data[,c("X","Y",Cols[j])],crs=crs(water_mask))
    crop_vop<-terra::rast(Vop_data[,c("X","Y",Cols[j])],crs=crs(water_mask))
    crop_yield<-terra::rast(Yield_data[,c("X","Y",Cols[j])],crs=crs(water_mask))
    
    
    crop_ha<-terra::resample(crop_ha,water_mask,method="near")
    crop_vop<-terra::resample(crop_vop,water_mask,method="near")
    crop_yield<-terra::resample(crop_yield,water_mask,method="near")
    
    crop_vop<-terra::mask(terra::crop(crop_vop,adm1_africa),adm1_africa)*water_mask
    crop_ha<-terra::mask(terra::crop(crop_ha,adm1_africa),adm1_africa)*water_mask
    crop_yield<-terra::mask(terra::crop(crop_yield,adm1_africa),adm1_africa)*water_mask
    
    crop_production<-crop_yield*crop_ha
    
    crop_vop_ha<-crop_vop/crop_ha
    crop_vop_ha[is.infinite(crop_vop_ha)]<-NA
    crop_vop_cell<-crop_vop/cellsize_ha
    
    CROP<-gsub("_A","",names(crop_vop_ha))
    
    names(crop_production)<-paste0(CROP,"-",FS,"-tons")
    names(crop_yield)<-paste0(CROP,"-",FS,"-ton_ha")
    names(crop_ha)<-paste0(CROP,"-",FS,"-ha")
    names(crop_vop)<-paste0(CROP,"-",FS,"-IND_total")
    names(crop_vop_ha)<-paste0(CROP,"-",FS,"-IND_ha")
    names(crop_vop_cell)<-paste0(CROP,"-",FS,"-IND_cell")
    
    suppressWarnings(terra::writeRaster(crop_production,file=paste0(IFPRIDirInt_prod,"/",FS,"-",CROP,"-prod.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_yield,file=paste0(IFPRIDirInt_yield,"/",FS,"-",CROP,"-yield.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_ha,file=paste0(IFPRIDirInt_ha,"/",FS,"-",CROP,"-harv_area.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_vop,file=paste0(IFPRIDirInt_total,"/",FS,"-",CROP,"-vop.tif"), overwrite=T))       
    suppressWarnings(terra::writeRaster(crop_vop_ha,file=paste0(IFPRIDirInt_ep,"/",FS,"-",CROP,"-vop_per_harv_ha.tif"),overwrite=T))
    suppressWarnings(terra::writeRaster(crop_vop_cell,file=paste0(IFPRIDirInt_cell,"/",FS,"-",CROP,"-vop_per_cell_ha.tif"),overwrite=T))
    
    list(crop_vop=crop_vop, crop_vop_ha=crop_vop_ha,crop_vop_cell=crop_vop_cell)
    
  }
  
}

# Validation: Check bar plots

# Total VoP
focal_crop<-"CER"

tot_vop_files<-list.files(IFPRIDirInt_total,focal_crop,full.names=T)

tot_vop_Stack<-terra::rast(tot_vop_files)

Angola<-adm1_africa[adm1_africa$COUNTRY=="Angola"]

tot_vop_Stack<-terra::mask(terra::crop(tot_vop_Stack,Angola),Angola)

vals<-unlist(lapply(1:terra::nlyr(tot_vop_Stack),FUN=function(i){
  sum(values(tot_vop_Stack[[i]]),na.rm=T)
}))

vals<-data.frame(Size=paste0("h",2:7),VoP=vals,Crop=focal_crop)

require(ggplot2)

ggplot(vals, aes(fill=Size, y=VoP, x=Crop)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()

# Mean VoP/ha
focal_crop<-"BANPL"

tot_vop_files<-list.files(IFPRIDirInt_ep,focal_crop,full.names=T)

tot_vop_Stack<-terra::rast(tot_vop_files)

Angola<-adm1_africa[adm1_africa$COUNTRY=="Angola"]

tot_vop_Stack<-terra::mask(terra::crop(tot_vop_Stack,Angola),Angola)

vals<-unlist(lapply(1:terra::nlyr(tot_vop_Stack),FUN=function(i){
  mean(values(tot_vop_Stack[[i]]),na.rm=T)
}))

vals<-data.frame(Size=paste0("h",2:7),VoP=vals,Crop=focal_crop)


ggplot(vals, aes(fill=Size, y=VoP, x=Crop)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()


