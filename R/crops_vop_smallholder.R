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
oil_crops<-c("oilpalm","other oil crops","rapeseed","sesameseed","sunflower")
pulses<-c("bean","chickpea","cowpea","groundnut","lentil","other pulses","pigeonpea","soybean")
roots<-c("cassava","other roots","plantain","potato","sweet potato","yams")
total<-MS_metadata[,unique(Commodity)]

crop_groups<-list(cereals=cereals,oil_crops=oil_crops,pulses=pulses,roots=roots,total=total)

# VoP files
files_vop<-MS_metadata[Variable=="value of production" & Subvarible == "all technologies"]
# Harvested area files
files_ha<-MS_metadata[Variable=="havested area" & Subvarible == "all technologies"]

# Load and mask vop & harvest area per crop group, divide vop by harvested area and sum the resulting stack
crop_vop_ha<-terra::rast(lapply(1:length(crop_groups),FUN=function(i){
    print(names(crop_groups)[i])
    CROPS<-crop_groups[[i]]
    crop_vop<-terra::rast(paste0(CropDir,"/",files_vop[Commodity %in% CROPS,File]))
    crop_vop<-terra::mask(terra::crop(crop_vop,sh_africa),sh_africa)
    
    cellsize_ha<-terra::cellSize(crop_vop[[1]],unit="ha",mask=F)
    #crop_ha<-terra::rast(paste0(CropDir,"/",files_ha[Commodity %in% CROPS,File]))
    #crop_ha<-terra::mask(terra::crop(crop_ha,sh_africa),sh_africa)
       
    crop_vop<-sum(crop_vop)
    
    crop_vop_ha<-crop_vop/cellsize_ha
    
    names(crop_vop_ha)<-paste0(names(crop_groups)[i],"-USD_ha")
    
    crop_vop_ha
    
    }))

# Subset VoP for different smallholder farm sizes ####

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::resample(SmallHolders,crop_vop_ha,method="near")
SmallHolders<-terra::mask(terra::crop(SmallHolders,sh_africa),sh_africa)

# Create vector of smallholder values
Values<-unique(terra::values(SmallHolders))
Values<-sort(Values[!is.na(Values)])

# Create mask for each smallholder value, multiply CropVop stack by mask and save each layer
lapply(Values,FUN=function(VAL){
    SH<-SmallHolders
    SH[SH>VAL]<-NA
    SH[!is.na(SH)]<-1
    crop_vop_ha_sh<-crop_vop_ha*SH
    names(crop_vop_ha_sh)<- paste0(names(crop_vop_ha_sh),"-sh",VAL)
    lapply(names(crop_vop_ha_sh),FUN=function(Layer){
       suppressWarnings(terra::writeRaster(crop_vop_ha_sh[[Layer]],file=paste0(CropDirInt,"/",Layer,".tif"),overwrite=T))        
        })
    })

terra::plot(terra::rast(list.files(CropDirInt,"total",full.names=T)[1:2]))