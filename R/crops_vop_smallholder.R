# Intersect crop VoP with smallholder size classes
require(data.table)
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# Set directory for crops data
CropDir<-paste0(DataDir,"/mapspam_2017/raw")
# Set save directory for livestock x smallholder data
CropDirInt<-paste0(DataDir,"/mapspam_2017/intermediate/atlas_smallholders")
if(!dir.exists(CropDirInt)){
    dir.create(CropDirInt,recursive=T)
    }

# Load crop meta-data
MS_metadata<-data.table::fread(paste0(CropDir,"/mapspam_meta.csv"))

# Convert Crop VoP per pixel to per ha of harvested area ####

# Create a list of crop groupings
cereals<-c("barley","maize","other cereals","pearl millet","rice","small millet","sorghum","wheat")
oil_crops<-c("oilpalm","other oil crops","rapeseed","sesameseed","sunflower","coconut","groundnut","soybean")
pulses<-c("bean","chickpea","cowpea","lentil","other pulses","pigeonpea")
roots<-c("cassava","other roots","potato","sweet potato","yams")
total<-MS_metadata[,unique(Commodity)]

crop_groups<-list(cereals=cereals,oil_crops=oil_crops,pulses=pulses,roots=roots,total=total)

# VoP files
files_vop<-MS_metadata[Variable=="value of production" & Subvarible == "all technologies"]
# Harvested area files
files_ha<-MS_metadata[Variable=="havested area" & Subvarible == "all technologies"]

# Load and mask vop & harvest area per crop group, divide vop by harvested area and sum the resulting stack
crop_vop_ha<-lapply(1:length(crop_groups),FUN=function(i){
    print(names(crop_groups)[i])
    CROPS<-crop_groups[[i]]
    crop_vop<-terra::rast(paste0(CropDir,"/",files_vop[Commodity %in% CROPS,File]))
    crop_vop<-terra::mask(terra::crop(crop_vop,sh_africa),sh_africa)
    
    cellsize_ha<-terra::cellSize(crop_vop[[1]],unit="ha",mask=F)
    
    crop_ha<-terra::rast(paste0(CropDir,"/",files_ha[Commodity %in% CROPS,File]))
    crop_ha<-terra::mask(terra::crop(crop_ha,sh_africa),sh_africa)
      
    crop_vop_ha2<-sum(crop_vop)/sum(crop_ha)
    
    crop_vop<-sum(crop_vop)
    crop_vop_ha<-crop_vop/cellsize_ha
       
    names(crop_vop_ha)<-names(crop_groups)[i]
    names(crop_vop_ha2)<-names(crop_groups)[i]
    names(crop_vop)<-names(crop_groups)[i]
    
    list(crop_vop_ha_cell=crop_vop_ha,crop_vop_ha_crop=crop_vop_ha2,crop_vop=crop_vop)
    
    })

crop_vop_ha_cell<-terra::rast(lapply(crop_vop_ha,"[[","crop_vop_ha_cell"))
crop_vop_ha_crop<-terra::rast(lapply(crop_vop_ha,"[[","crop_vop_ha_crop"))
crop_vop<-terra::rast(lapply(crop_vop_ha,"[[","crop_vop"))

# Subset VoP for different smallholder farm sizes ####

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::resample(SmallHolders,crop_vop_ha_cell,method="near")
SmallHolders<-terra::mask(terra::crop(SmallHolders,sh_africa),sh_africa)

# Create vector of smallholder values
Values<-c(1,2,5,10,20,9999999)
ValNames<-paste0("h",c(2,3,4,5,6,7))

# Create Save Directories
IFPRIDirInt_ep<-paste0(DataDir,"/atlas_mapspam/intermediate/vop_per_harv_area")
if(!dir.exists(IFPRIDirInt_ep)){
    dir.create(IFPRIDirInt_ep,recursive=T)
    }

IFPRIDirInt_cell<-paste0(DataDir,"/atlas_mapspam/intermediate/vop_per_cell_area")
if(!dir.exists(IFPRIDirInt_cell)){
    dir.create(IFPRIDirInt_cell,recursive=T)
    }

IFPRIDirInt_total<-paste0(DataDir,"/atlas_mapspam/intermediate/vop_total")
if(!dir.exists(IFPRIDirInt_total)){
    dir.create(IFPRIDirInt_total,recursive=T)
    }

# Create mask for each smallholder value, multiply CropVop stack by mask and save each layer
Y<-lapply(1:length(Values),FUN=function(j){
    
    VAL<-Values[j]
    SH<-SmallHolders
    SH[SH>VAL]<-NA
    SH[!is.na(SH)]<-1
    crop_vop_ha_sh_cell<-crop_vop_ha_cell*SH
    crop_vop_ha_sh_crop<-crop_vop_ha_crop*SH
    
  
    names(crop_vop)<- paste0(names(crop_vop),"-",ValNames[j],"-IND_total")
    names(crop_vop_ha_sh_cell)<- paste0(names(crop_vop_ha_sh_cell),"-",ValNames[j],"-IND_cell")
    names(crop_vop_ha_sh_crop)<- paste0(names(crop_vop_ha_sh_crop),"-",ValNames[j],"-IND_ha")
    
    X<-lapply(1:terra::nlyr(crop_vop_ha_sh_cell),FUN=function(i){
        
       suppressWarnings(terra::writeRaster(crop_vop[[i]],file=paste0(IFPRIDirInt_total,"/",names(crop_vop)[i],"-cell.tif"),overwrite=T))        
       suppressWarnings(terra::writeRaster(crop_vop_ha_sh_cell[[i]],file=paste0(IFPRIDirInt_cell,"/",names(crop_vop_ha_sh_cell)[i],"-cell.tif"),overwrite=T))        
       suppressWarnings(terra::writeRaster(crop_vop_ha_sh_crop[[i]],file=paste0(IFPRIDirInt_ep,"/",names(crop_vop_ha_sh_crop)[i],"-crop.tif"),overwrite=T)) 
        })
    X
    })

names(Y)<-ValNames

terra::plot(terra::rast(list.files(CropDirInt,"total",full.names=T)[1:2]))

