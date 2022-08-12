# This method uses data subset to farm size from IFPRI

# Intersect crop VoP with smallholder size classes
require(data.table)
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# Set directory for atlas IFPRI mapspam data by farm size
IFPRIDir<-paste0(DataDir,"/atlas_mapspam/intermediate/GeoTIFF")

IFPRIDirInt<-paste0(DataDir,"/atlas_mapspam/intermediate/VoP_USD_ha")
if(!dir.exists(IFPRIDirInt)){
    dir.create(IFPRIDirInt,recursive=T)
    }

# Create a list of crop groupings
crop_groups<-c("CERE","OILC","PULS","ROOT","TOTAL")

# Load and mask vop & harvest area per crop group, divide vop by harvested area and sum the resulting stack
Vop_Files<-list.files(IFPRIDir,".tif",full.names=T)
Vop_Files<-Vop_Files[grep("cumulative",Vop_Files)]
Vop_Farmsizes<-paste0("h",2:7)

crop_vop_ha<-lapply(crop_groups,FUN=function(CROP){
    print(CROP)
    
    # Cross-reference mapspam files
    Vop_Files_2<-Vop_Files[grepl(CROP,Vop_Files)]
    
    X<-terra::rast(lapply(Vop_Farmsizes,FUN=function(FS){
        Vop_Files_3<-Vop_Files_2[grepl(paste0("_",FS,"_"),Vop_Files_2)]
      
        crop_ha<-terra::rast(grep("spam_ssp_h_",Vop_Files_3,value=T))
        crop_vop<-terra::rast(grep("spam_ssp_v_",Vop_Files_3,value=T))
        
        crop_vop_ha<-crop_vop/crop_ha
        
        crop_vop_ha<-terra::crop(crop_vop_ha,sh_africa)
        
        names(crop_vop_ha)<-paste0(CROP,"-",FS,"-USD_ha")
        
        suppressWarnings(terra::writeRaster(crop_vop_ha,file=paste0(IFPRIDirInt,"/spam_ssp_hv-",FS,"-cumulative-",CROP,"-A.tif"),overwrite=T))
        
        crop_vop_ha
        }))
    
    names(X)<-Vop_Farmsizes
    X
})

names(crop_vop_ha)<-crop_groups

terra::plot(crop_vop_ha[[1]])
