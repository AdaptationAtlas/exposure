DataDir<-"/home/jovyan/common_data"

SaveDir<-paste0(DataDir,"/atlas_boundaries/raw")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

IntDir<-paste0(DataDir,"/atlas_boundaries/intermediate")
if(!dir.exists(IntDir)){
    dir.create(IntDir)
    }


Countries<-c('AGO','BDI','BEN','BFA','BWA','CAF','CIV','CMR','COD','COG','DJI','ERI','ETH','GAB','GHA','GIN','GMB','GNB','GNQ','KEN','LBR','LSO','MDG','MLI','MOZ','MRT','MWI','NAM','NER','NGA','RWA','SEN','SLE','SOM','SDN','SSD','SWZ','TCD','TGO','TZA','UGA','ZAF','ZMB','ZWE')

options(timeout=480)

admin1<-lapply(1:length(Countries),FUN=function(i){
    
    URL<-paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_",Countries[i],"_shp.zip")
    destfile<-paste0(SaveDir,"/",Countries[i],".zip")
    
    # Display progress
    cat('\r                                                ')
    cat('\r',paste0("Downloading file: ",URL))
    flush.console()
    
    if(!file.exists(destfile)){
            download.file(URL, destfile)
        }  
    
    unzip(destfile,exdir=SaveDir)
    
    X<-suppressWarnings(terra::vect(grep(paste0(Countries[i],"_1.shp"),list.files(SaveDir,full.names=T),value=T)))
    
    Rm<-list.files(SaveDir,Countries[i],full.names=T)
    Rm<-Rm[!grepl(".zip",Rm)]
    unlink(Rm,recursive=T)
    X
})

admin1<-do.call(rbind,admin1)

# Fix Ghana issue with HASC_1 codes
admin1$HASC_1[admin1$GID_0=="GHA"]<-gsub("-",".",admin1$ISO_1[admin1$GID_0=="GHA"])

# Fix Guinea issue with HASC_1 codes
admin1$HASC_1[admin1$GID_0=="GIN"]<-c('GN.BO','GN.CO','GN.FA','GN.KA','GN.KI','GN.LA','GN.MA','GN.NZ')

# Fix Madagascar issue with HASC_1 codes
admin1$HASC_1[admin1$GID_0=="MDG"]<-c('MG.AV','MG.AS','MG.FI','MG.MA','MG.TM','MG.TL')

terra::writeVector(admin1,file=paste0(IntDir,"/gadm41_ssa_1.shp"),overwrite=T)
