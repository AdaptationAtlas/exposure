require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt_total<-paste0(DataDir,"/atlas_pop/intermediate/pop_total")
if(!dir.exists(DataDirInt_total)){
    dir.create(DataDirInt_total,recursive=T)
    }

DataDirInt_cell<-paste0(DataDir,"/atlas_pop/intermediate/pop_density")
if(!dir.exists(DataDirInt_cell)){
    dir.create(DataDirInt_cell,recursive=T)
    }
    
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

# Read in population data
pop<-terra::rast(list.files(paste0(DataDir,"/atlas_pop/raw"),".tif",full.names=T))
pop<-terra::resample(pop,base_raster,method="near")
pop<-terra::mask(terra::crop(pop,adm1_africa),adm1_africa)*water_mask
names(pop)<-c("rural","total","urban")

cellsize_ha<-terra::cellSize(pop[[1]],unit="ha")

# Create a higher resolution raster to work out area of waterbody per pixel
cellsize_da<-terra::disagg(cellsize_ha,fact=10)
water_rast<-terra::rasterize(waterbodies,cellsize_da)

# Work out proportion of cell that is not water and multiply cellsize_ha by this value
water_rast<-(100-terra::aggregate(water_rast,fact=10,fun=sum,na.rm=T))/100
water_rast[is.na(water_rast)]<-1
cellsize_ha<-cellsize_ha*water_rast

# Load, mask, and resample smallholder data
SmallHolders<-terra::rast(paste0(DataDir,"/atlas_smallholders/raw/farmSize_agarea_20210505_1.tif"))
SmallHolders<-terra::project(SmallHolders,crs(base_raster))
SmallHolders<-terra::mask(terra::crop(SmallHolders,adm1_africa),adm1_africa)
SmallHolders<-terra::resample(SH_Tot,base_raster,method="near")

# Create vector of smallholder size classes
Values<-c(1,2,5,10,20,999999999)
names(Values)<-paste0("h",2:7)

PopList<-lapply(1:length(Values),FUN=function(i){
    VAL<-Values[i]
    SH<-SmallHolders
    SH[SH>VAL]<-0
    SH[!is.na(SH)]<-1
    
    # Mask population by farm size
    pop_sh<-pop*SH
    
    # Calculate population per land area
    pop_sh_density<-pop_sh/cellsize_ha
       
    names(pop_sh_density)<- paste0(names(Values)[i],"-",names(pop_sh_density),"-pop_density")
    names(pop_sh)<- paste0(names(Values)[i],"-",names(pop_sh),"-pop_total")
    
    X<-lapply(1:terra::nlyr(pop_sh_density),FUN=function(j){
        suppressWarnings(
            terra::writeRaster(pop_sh_density[[j]],
                               file=paste0(DataDirInt_cell,"/",names(pop_sh_density)[j],".tif"),
                               overwrite=T))
        suppressWarnings(
            terra::writeRaster(pop_sh[[j]],
                               file=paste0(DataDirInt_total,"/",names(pop_sh)[j],".tif"),
                               overwrite=T))   
    })
    
    list(pop_sh_density=pop_sh_density,pop_sh=pop_sh)
    
    })

names(PopList)<-names(Values)

terra::plot(PopList$h2$pop_density)

