# Intersect livestock VoP with smallholder size classes

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

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::resample(SmallHolders,LivestockVoP,method="near")
SmallHolders<-terra::mask(terra::crop(SmallHolders,sh_africa),sh_africa)

# Create vector of smallholder values
Values<-unique(terra::values(SmallHolders))
Values<-sort(Values[!is.na(Values)])

# Create mask for each smallholder value, multiply LSVop stack by mask and save each layer
lapply(Values,FUN=function(VAL){
    SH<-SmallHolders
    SH[SH>VAL]<-NA
    SH[!is.na(SH)]<-1
    LSVop<-LivestockVoP*SH
    
    cellsize_ha<-terra::cellSize(LSVop[[1]],unit="ha",mask=F)

    LSVop<-LSVop/cellsize_ha
    
    names(LSVop)<-paste0(names(LSVop),"-USD_ha-sh",VAL)
    
    lapply(names(LSVop),FUN=function(Layer){
       suppressWarnings(terra::writeRaster(LSVop[[Layer]],file=paste0(LivestockDirInt,"/",Layer,".tif")))        
        })
    })

terra::plot(terra::rast(list.files(LivestockDirInt,"total",full.names=T)[1:4]))

# Add meta.data
SizeClasses<-paste0(0,"-",Values)
