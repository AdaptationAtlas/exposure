# Subset mapspam VoP data to cumulative farm size and caculate VoP per: 1) cell (total VoP); 2) per harvested area of crop(s) = economic yield per ha of crop; and 3) per cell area (less water bodies) = VoP per ha of terrestrial land in cell.

require(terra)

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
cellsize_da<-terra::disagg(cellsize_ha,fact=10)
water_rast<-terra::rasterize(waterbodies,cellsize_da)

# Work out proportion of cell that is not water and multiply cellsize_ha by this value
water_rast<-(100-terra::aggregate(water_rast,fact=10,fun=sum,na.rm=T))/100
water_rast[is.na(water_rast)]<-1
cellsize_ha<-cellsize_ha*water_rast

# Set directory for atlas IFPRI mapspam data by farm size
IFPRIDir<-paste0(DataDir,"/atlas_mapspam/intermediate/GeoTIFF")

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

# Create a list of crop groupings
crop_groups<-c("CERE","OILC","PULS","ROOT","TOTAL")

# Load and mask vop & harvest area per crop group, divide vop by harvested area and sum the resulting stack
Vop_Files<-list.files(IFPRIDir,".tif",full.names=T)
Vop_Files<-Vop_Files[grep("cumulative",Vop_Files)]
Vop_Farmsizes<-paste0("h",2:7)

crop_vop<-lapply(crop_groups,FUN=function(CROP){
    print(CROP)
    
    # Cross-reference mapspam files
    Vop_Files_2<-Vop_Files[grepl(CROP,Vop_Files)]
    
    X<-lapply(Vop_Farmsizes,FUN=function(FS){
        Vop_Files_3<-Vop_Files_2[grepl(paste0("_",FS,"_"),Vop_Files_2)]
      
        crop_ha<-terra::rast(grep("spam_ssp_h_",Vop_Files_3,value=T))
        crop_vop<-terra::rast(grep("spam_ssp_v_",Vop_Files_3,value=T))
        
        crop_vop<-terra::mask(terra::crop(crop_vop,adm1_africa),adm1_africa)*water_mask
        crop_ha<-terra::mask(terra::crop(crop_ha,adm1_africa),adm1_africa)*water_mask
      
        crop_vop_ha<-crop_vop/crop_ha
        crop_vop_cell<-crop_vop/cellsize_ha
        
        names(crop_vop)<-paste0(CROP,"-",FS,"-IND_total")
        names(crop_vop_ha)<-paste0(CROP,"-",FS,"-IND_ha")
        names(crop_vop_cell)<-paste0(CROP,"-",FS,"-IND_cell")
        
        suppressWarnings(terra::writeRaster(crop_vop,file=paste0(IFPRIDirInt_total,"/",FS,"-",CROP,"-vop.tif"), overwrite=T))           
        suppressWarnings(terra::writeRaster(crop_vop_ha,file=paste0(IFPRIDirInt_ep,"/",FS,"-",CROP,"-vop_per_harv_ha.tif"),overwrite=T))
        
        suppressWarnings(terra::writeRaster(crop_vop_cell,file=paste0(IFPRIDirInt_cell,"/",FS,"-",CROP,"-vop_per_cell_ha.tif"),overwrite=T))
        
        list(crop_vop=crop_vop, crop_vop_ha=crop_vop_ha,crop_vop_cell=crop_vop_cell)
        })    
   X
})

names(crop_vop)<-crop_groups

terra::plot(terra::rast(lapply(crop_vop[["TOTAL"]],"[[","crop_vop_ha")))
