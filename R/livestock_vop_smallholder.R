# Intersect livestock VoP with smallholder size classes
require(future.apply)
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))

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

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::resample(SmallHolders,LivestockVoP,method="near")
SmallHolders<-terra::mask(terra::crop(SmallHolders,sh_africa),sh_africa)

# Create vector of smallholder values
Values<-unique(terra::values(SmallHolders))
Values<-sort(Values[!is.na(Values)])

# Work out pixel cell size
cellsize_ha<-terra::cellSize(LivestockVoP[[1]],unit="ha",mask=F)

# Create a higher resolution raster to work out area of waterbody per pixel
cellsize_da<-terra::disagg(cellsize_ha,fact=10)
water_rast<-terra::rasterize(waterbodies,cellsize_da)

# Work out proportion of cell that is not water and multiply cellsize_ha by this value
water_rast<-(100-terra::aggregate(water_rast,fact=10,fun=sum,na.rm=T))/100
water_rast[is.na(water_rast)]<-1
cellsize_ha<-cellsize_ha*water_rast

# Create mask for each smallholder value, multiply LSVop stack by mask and save each layer

lapply(Values,FUN=function(VAL){
    SH<-SmallHolders
    SH[SH>VAL]<-NA
    SH[!is.na(SH)]<-1
        
    ls_vop_ha_cell<-LivestockVoP/cellsize_ha
    
    ls_vop_ha_sh_cell<-ls_vop_ha_cell*SH
    
    names(ls_vop_ha_sh_cell)<- paste0(names(ls_vop_ha_sh_cell),"-IND_ha-sh",VAL)
    
    lapply(names(ls_vop_ha_sh_cell),FUN=function(Layer){
        suppressWarnings(terra::writeRaster(ls_vop_ha_sh_cell[[Layer]],file=paste0(LivestockDirInt,"/",Layer,"-cell.tif"),overwrite=T))    
        })
    
    VAL
    
    })