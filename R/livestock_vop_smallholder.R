# Intersect livestock VoP with smallholder size classes
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Read in boundary of subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))

water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Set directory for livestock data
LivestockDir<-paste0(DataDir,"/atlas_livestock/raw")
# Set save directory for livestock x smallholder data
LivestockDirInt_cell<-paste0(DataDir,"/atlas_livestock/intermediate/vop_per_cell_km")
if(!dir.exists(LivestockDirInt_cell)){
    dir.create(LivestockDirInt_cell,recursive=T)
    }

LivestockDirInt_total<-paste0(DataDir,"/atlas_livestock/intermediate/vop_total")
if(!dir.exists(LivestockDirInt_total)){
    dir.create(LivestockDirInt_total,recursive=T)
    }

# Load & mask livestock data
LivestockVoP<-terra::rast(list.files(LivestockDir,full.names=T))
LivestockVoP<-terra::mask(terra::crop(LivestockVoP,adm1_africa),adm1_africa)*water_mask

# Rename livestock layers
Names1<-unlist(data.table::tstrsplit(names(LivestockVoP),"_",keep=5))
Names2<-unlist(data.table::tstrsplit(names(LivestockVoP),"_",keep=6))
Names<-paste0(Names1,"_",Names2)
Names<-gsub("_NA","",gsub(" ","_",Names))
names(LivestockVoP)<-Names
LivestockVoP<-LivestockVoP[[c("cattle","chicken","sheep_goat","pig","total")]]

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::project(SmallHolders,crs(base_raster))
SmallHolders<-terra::mask(terra::crop(SmallHolders,adm1_africa),adm1_africa)
SmallHolders<-terra::resample(SmallHolders,base_raster,method="near")

# Create vector of smallholder size classes
Values<-c(1,2,5,10,20,999999999)
names(Values)<-paste0("h",2:7)

# Work out pixel cell size
cellsize_ha<-terra::cellSize(LivestockVoP[[1]],unit="km",mask=F)

# Create a higher resolution raster to work out area of waterbody per pixel
#cellsize_da<-terra::disagg(cellsize_ha,fact=10)
#water_rast<-terra::rasterize(waterbodies,cellsize_da)

# Work out proportion of cell that is not water and multiply cellsize_ha by this value
#water_rast<-(100-terra::aggregate(water_rast,fact=10,fun=sum,na.rm=T))/100
#water_rast[is.na(water_rast)]<-1
#cellsize_ha<-cellsize_ha*water_rast

# Create mask for each smallholder value, multiply LSVop stack by mask and save each layer

LS<-lapply(1:length(Values),FUN=function(i){
    VAL<-Values[i]
    SH<-SmallHolders
    SH[!is.na(SH) & SH<=VAL]<-1
    SH[SH>VAL]<-0
        
    LivestockVoP_SH<-LivestockVoP*SH
    
    ls_vop_ha_sh_cell<-LivestockVoP_SH/cellsize_ha
       
    names(ls_vop_ha_sh_cell)<- paste0(names(Values)[i],"-",names(LivestockVoP),"-vop_per_cell_km")
    names(LivestockVoP_SH)<- paste0(names(Values)[i],"-",names(LivestockVoP),"-vop_total")
    
    lapply(1:terra::nlyr(ls_vop_ha_sh_cell),FUN=function(j){
        suppressWarnings(
            terra::writeRaster(ls_vop_ha_sh_cell[[j]],
                               file=paste0(LivestockDirInt_cell,"/",names(ls_vop_ha_sh_cell)[j],".tif"),
                               overwrite=T))
        suppressWarnings(
            terra::writeRaster(LivestockVoP_SH[[j]],
                               file=paste0(LivestockDirInt_total,"/",names(LivestockVoP_SH)[j],".tif"),
                               overwrite=T))   
    })
    
    list(LivestockVoP_SH=LivestockVoP_SH,ls_vop_ha_sh_cell=ls_vop_ha_sh_cell)
    

    
    })

names(LS)<-Values