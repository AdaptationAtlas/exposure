# Intersect livestock VoP with smallholder size classes
require(future.apply)
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# Set directory for livestock data
LivestockDir<-paste0(DataDir,"/livestock_vop/raw")
# Set save directory for livestock x smallholder data
LivestockDirInt<-paste0(DataDir,"/livestock_vop/intermediate/atlas_smallholders")
if(!dir.exists(LivestockDirInt)){
    dir.create(LivestockDirInt,recursive=T)
    }

# Load & mask livestock data
LivestockVoP<-terra::rast(list.files(LivestockDir,full.names=T))
LivestockVoP<-terra::mask(terra::crop(LivestockVoP,sh_africa),sh_africa)

# Rename livestock layers
Names1<-unlist(data.table::tstrsplit(names(LivestockVoP),"_",keep=5))
Names2<-unlist(data.table::tstrsplit(names(LivestockVoP),"_",keep=6))
Names<-paste0(Names1,"_",Names2)
Names<-gsub("_NA","",gsub(" ","_",Names))
names(LivestockVoP)<-Names

# Load map of agricultural area
ag_landDirInt<-paste0(DataDir,"/af_agri_land/intermediate/atlas")
if(!dir.exists(ag_landDirInt)){
    dir.create(ag_landDirInt)
}

# Load agricultural land dataset
ag_land<-terra::rast(paste0(DataDir,"/af_agri_land/raw/MAL_AFRICA1.tif"))

# Dataset is projected and resolution is 100x100m = 1 ha cells, so resampled summed layer will give ha/agriculture per grid cell of LivestockVoP
ag_land_area<-terra::resample(ag_land,LivestockVoP,method="sum")

# Load pasture land dataset
pasture<-terra::rast(paste0(DataDir,"/pasture_ramankutty/raw/af_pasture.tif"))
pasture_resamp<-terra::resample(pasture,LivestockVoP)
pasture_area<-pasture_resamp*terra::cellSize(pasture_resamp,unit="ha")

# Determine livestock VoP per ha agricultural land
LivestockVoP_area<-LivestockVoP/pasture_area
terra::plot(LivestockVoP_area$total)

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::resample(SmallHolders,LivestockVoP,method="near")
SmallHolders<-terra::mask(terra::crop(SmallHolders,sh_africa),sh_africa)

# Create vector of smallholder values
Values<-unique(terra::values(SmallHolders))
Values<-sort(Values[!is.na(Values)])

# Create mask for each smallholder value, multiply LSVop stack by mask and save each layer

# Agricultural land is not implemented as livestock can be produced in rangelands which contain zero agriculture
lapply(Values,FUN=function(VAL){
    SH<-SmallHolders
    SH[SH>VAL]<-NA
    SH[!is.na(SH)]<-1
    
    cellsize_ha<-terra::cellSize(LivestockVoP[[1]],unit="ha",mask=F)
    
    ls_vop_ha_cell<-LivestockVoP/cellsize_ha
   # ls_vop_ha_ag<-LivestockVoP/ag_land_area
    
    ls_vop_ha_sh_cell<-ls_vop_ha_cell*SH
   # ls_vop_ha_sh_ag<-ls_vop_ha_ag*SH
    
    names(ls_vop_ha_sh_cell)<- paste0(names(ls_vop_ha_sh_cell),"-USD_ha-sh",VAL)
   # names(ls_vop_ha_sh_ag)<- paste0(names(ls_vop_ha_sh_ag),"-USD_ha-sh",VAL)
    
    lapply(names(ls_vop_ha_sh_cell),FUN=function(Layer){
        suppressWarnings(terra::writeRaster(ls_vop_ha_sh_cell[[Layer]],file=paste0(LivestockDirInt,"/",Layer,"-cell.tif"),overwrite=T))    
       # suppressWarnings(terra::writeRaster(ls_vop_ha_sh_ag[[Layer]],file=paste0(LivestockDirInt,"/",Layer,"-agland.tif"),overwrite=T)) 
        })
    
    VAL
    
    })