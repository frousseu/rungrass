

library(jsonlite)
library(readxl)
library(taxize)
library(rgbif)
library(foreach)
library(doParallel)
library(sf)
library(jsonlite)
library(scales)
library(terra)
library(tidyterra)
library(RCurl)
library(future)
library(future.apply)
library(data.table)
library(magick)
library(httr)

source("https://raw.githubusercontent.com/frousseu/FRutils/master/R/colo.scale.R")

### borbonica and inpn logo
#borbonicalogo<-image_read("https://www.borbonica.re/img/carousel/carte-run.png") |> 
#  image_trim() |>
#  image_scale("x30") |>
#  image_quantize(50,dither=FALSE)
#image_write(borbonicalogo,"C:/Users/God/Documents/rungrass/images/borbonicalogo.png")
#inpnlogo<-image_read("https://openobs.mnhn.fr/openobs-hub/img/logo_openobs.png") |> 
#  image_trim() |>
#  image_scale("x30") |>
#  image_quantize(50,dither=FALSE)
#image_write(inpnlogo,"C:/Users/God/Documents/rungrass/images/inpnlogo.png")


# https://www.inaturalist.org/pages/api+recommended+practices
# x<-GET("https://www.inaturalist.org/users/api_token")
# 24 h
# api_token<-""



#src<-"https://res.cloudinary.com/dphvzalf9/image/upload/"
src<-"https://hebergix.net/cdn/fr/"
#src<-"images/"

#x<-fromJSON("https://api.inaturalist.org/v1/observations/90513306")
#x$results$observation_photos[[1]]$photo$attribution

# finds all possible sp names based on sp in d
allspnames<-function(sp){
  m<-match(sp,spnames$sp)
  if(is.na(m)){
    g<-unique(unlist(lapply(c("sp","flore","index","other"),function(i){
      grep(sp,spnames[,i,drop=TRUE])  
    })))
    if(any(g)){
      if(length(g)>1){
        print(g)
        stop(paste("Multiple matches for",sp))
      }else{
        m<-g
      }
    }else{
      stop(paste("No matches for",sp)) 
    }
  }
  sps<-c(spnames$sp[m],spnames$flore[m],spnames$index[m],strsplit(spnames$other[m],", ")[[1]])
  unique(sps[!is.na(sps)]) 
}

#allspnames("Avenella flexuosa")



### Data ########################################

d<-as.data.frame(read_excel("C:/Users/God/Documents/rungrass/grasses.xlsx"))
#dcsv<-read.csv("https://raw.githubusercontent.com/frousseu/reunion_graminoids/main/grasses.csv",sep=";")
dcsv<-read.csv("C:/Users/God/Documents/rungrass/grasses.csv",sep=";",na.strings=c("NA",""))

d$photo<-gsub("/medium.|/small.|/large.","/original.",d$photo)

d<-merge(d,dcsv[,c("sp","photo","attribution","powo","gbif","inatid","inat")],all.x=TRUE) # only get attributions
d<-d[order(d$sp,d$rank),]

d<-unique(d)
spnames<-unique(d[,c("sp","flore","index","other")])

#write.table(d[,1:9],"C:/Users/God/Documents/reunion_graminoids/grasses.csv",row.names=FALSE,sep=";",na="")

d$idphoto<-sapply(strsplit(sapply(strsplit(d$photo,"/original."),function(i){if(length(i)==1){NA}else{i[1]}}),"/"),tail,1)
d$idobs<-ifelse(!is.na(d$idphoto),sapply(strsplit(d$obs,"/"),tail,1),NA)
d$observer<-NA
  
### iNat credits ############################
#w<-1:nrow(d) # get them all to verify if any attributions have changed
w<-which(!is.na(d$idphoto) & is.na(d$attribution))#[1]
w<-which(!is.na(d$idphoto) & (is.na(d$attribution) | d$attribution=="no rights reserved"))
for(i in w){ # looping is better cause sometimes it times-out
  if(is.na(d$idobs[i])){
    a<-d$credit[i]
  }else{  
    x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations/",d$idobs[i]))
    m<-match(d$idphoto[i],x$results$observation_photos[[1]]$photo$id)
    a<-x$results$observation_photos[[1]]$photo$attribution[m]
    observer<-if(is.na(x$results$user$name)){x$results$user$login}else{x$results$user$name}
  }
  d$attribution[i]<-a
  d$observer[i]<-observer
  Sys.sleep(0.1) # not to make too many requests, but not sure it is relevant
  cat("\r",paste(match(i,w),length(w),sep=" / "))
}

d$attribution[which(is.na(d$attribution))]<-d$credit[which(is.na(d$attribution))]
d$attribution<-ifelse(d$attribution=="no rights reserved",paste0("(c) ",d$observer,", ",d$attribution," (CC0)"),d$attribution)

### POWO links #################################
d$powo[d$sp=="Poa borbonica"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:416623-1"
d$powo[d$sp=="Cyperus dubius"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:304357-1"
d$powo[d$sp=="Paspalum conjugatum"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:30011315-2"
d$powo[d$sp=="Urochloa distachya"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:426250-1"
d$powo[d$sp=="Aristida congesta barbicollis"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:389261-1"
d$powo[d$sp=="Aristida congesta congesta"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:389336-1"
d$powo[d$sp=="Eragrostis tenella insularis"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:413272-4"
k<-d$family!="Excluded" & is.na(d$powo)

sp<-unique(d$sp[k])
if(length(sp)){
  powo<-get_pow(sp,ask=TRUE,accepted=TRUE,rank_filter="species")
  powourl<-data.frame(sp=sp,powo=attributes(powo)$uri)
  d$powo[k]<-powourl$powo[match(d$sp[k],powourl$sp)]
}
#sp<-unique(d$sp[d$family!="Excluded" & is.na(d$powo)]) # manually replace link

### GBIF links ##################################
k<-d$family!="Excluded" & is.na(d$gbif)
sp<-unique(d$sp[k])
if(length(sp)){
  registerDoParallel(detectCores())
  keys<-foreach(i=sp,.packages=c("rgbif")) %dopar% {
    #sptab<-rev(sort(table(as.data.frame(occ_search(scientificName=i,limit=200)$data)$scientificName)))
    #spfull<-names(sptab)[1]
    #key<-as.data.frame(name_suggest(q=spfull)$data)$key[1]
    key<-as.data.frame(name_backbone(name=i, rank='species', kingdom='plants'))$usageKey[1]
    file.path("https://www.gbif.org/fr/species",key)
  }
  gbifurl<-data.frame(sp=sp,gbif=unlist(keys))
  d$gbif[k]<-gbifurl$gbif[match(d$sp[k],gbifurl$sp)]
}

# manually correct certain links not going to the right taxon
d$gbif[d$sp=="Cyperus involucratus"]<-paste0("https://www.gbif.org/fr/species/",2714166)

### iNat links ##################################

k<-d$family!="Excluded" & is.na(d$inat) # removing the inat part allows complete updating in case of taxonomy changes
if(any(k)){
  sp<-unique(d$sp[k])
  ids<-rep(NA,length(sp))
  for(i in sp){
  #ids<-sapply(sp,function(i){
    print(i)
    x<-fromJSON(paste0("https://api.inaturalist.org/v1/taxa?q=\"",gsub(" ","%20",i),"\""))$results$id[1]
    if(!is.null(x)){
      ids[match(i,sp)]<-x
    }
  #})
  }
  ###merge(,data.frame(sp=sp

  d$inatid[k]<-ids[match(d$sp[k],sp)]
  d$inat<-ifelse(is.na(d$inatid),NA,paste0("https://www.inaturalist.org/observations?subview=grid&place_id=8834&taxon_id=",d$inatid))
}

#d$inatid<-sapply(d$inatid,function(i){if(is.null(i)){NA}else{i}})

### MNHN, Tropicos links ##########################################

d$mnhn<-paste0("https://science.mnhn.fr/institution/mnhn/collection/p/item/list?scientificName=",tolower(gsub(" ","+",d$sp)))
d$tropicos<-paste0("https://tropicos.org/name/Search?name=",tolower(gsub(" ","%20",d$sp)))

#library(magick)
#im<-image_read("C:/Users/God/Downloads/tropicos_trans.png")
#im<-image_crop(im,gravity="west","80")
#im<-image_trim(im)
#im
#image_write(im,"C:/Users/God/Downloads/tropicos.png")

### FNA links ##########################################

d$fna<-paste0("http://floranorthamerica.org/",gsub(" ","_",d$sp))
links<-unique(d$fna)
plan(multisession,workers=8)
ex<-future_lapply(links,url.exists)
plan(sequential)
d$fna<-ifelse(unlist(ex)[match(d$fna,links)],d$fna,NA)


### INPN links ##########################################

d$inpn<-ifelse(is.na(d$taxref) | d$taxref=="",NA,paste0("https://openobs.mnhn.fr/redirect/inpn/taxa/",d$taxref,"?departmentInseeId=974&view=map"))

### Statuses ############################################
d$stat<-substr(d$status,1,2)


### OCCS #######################

inpn_update<-FALSE
gbif_update<-FALSE
inat_update<-FALSE

if(FALSE){
  
  run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
  run<-st_buffer(st_geometry(run),50)
  
  library(basemaps)
  sat<-basemap_raster(st_bbox(st_transform(run,3857)),map_res=1, map_service = "esri", map_type = "world_imagery")
  sat<-rast(sat)
  #plotRGB(sat,maxcell=5000000)  
  
  
  
  ### INPN occs ########################  
  
  # manual download from openobs  
  if(inpn_update){
    inpnfiles<-list.files("C:/Users/God/Documents/rungrass",full=TRUE,pattern="records")
    inpnfiles<-grep(".csv",inpnfiles,value=TRUE)
    inpn<-rbindlist(lapply(inpnfiles,fread,select=1:117,encoding="UTF-8"))
    inpn<-inpn[espece!="",]
    inpn[,sp:=espece,]
    changes<-list( # the rest is transformed below with the tax code
      c("Cenchrus setiger","Cenchrus setigerus"),
      c("Ceratochloa cathartica","Bromus catharticus"), 
      c("Dactyloctenium ctenioides","Dactyloctenium ctenoides"),
      c("Panicum hubbardii","Acroceras hubbardii"),
      c("Panicum juniperinum","Panicum lycopodioides"),
      c("Panicum pseudowoeltzkowii","Panicum voeltzkowii"),
      c("Sporobolus indicus","Sporobolus indicus"),
      c("Schedonorus arundinaceus","Lolium arundinaceum"),
      c("Urochloa distachyos","Urochloa distachya"),
      c("Setaria flavida","Paspalidium flavidum"),
      c("Setaria geminata","Paspalidium geminatum"),
      c("Asterochaete borbonica","Carpha borbonica"),
      c("Asterochaete nitens","Carpha nitens"),
      c("Cyperus stolonifer","Cyperus stoloniferus")
    ) 
    changes<-as.data.table(do.call("rbind",changes))
    setnames(changes,c("old","new"))
    inpn[changes, sp := new, on = .(sp = old)]
    
    ### build status from inpn occs to paste in excel file
    inpn<-inpn[,status:=statutBiogeoEspeceTaxref]
    status<-unique(inpn[,.(espece,status)][order(espece),])
    status<-status[status!="",]
    changes<-list( # the rest is transformed below with the tax code
      c("Introduit","Exotique"),
      c("Introduit envahissant","Exotique"),
      c("Présent (indigène ou indéterminé)","Indigène"),
      c("Introduit non établi (dont cultivé / domestique)","Exotique"),
      c("Endémique","Endémique"),
      c("Cryptogène","Cryptogène"),
      c("Subendémique","Indigène")
    ) 
    changes<-as.data.table(do.call("rbind",changes))
    setnames(changes,c("old","new"))
    status[changes, status := new, on = .(status = old)]
    #dtemp<-as.data.frame(read_excel("C:/Users/God/Documents/rungrass/grasses.xlsx"))
    #write(paste(status$status[match(dtemp$sp,status$espece)],collapse="\n"),file="C:/Users/God/Downloads/status.txt") # copy and paste in excel file 
    
    
    #rev(sort(table(inpn$espece)))[1:20]
    inpn<-inpn[!is.na(longitude),]
    inpn<-inpn[-grep("iNaturalist.org",descriptionJeuDonnees),]
    inpn<-inpn[-grep("GBIF",descriptionJeuDonnees),]

    #table(inpn$sp[!inpn$sp%in%d$sp])
  
    inpn[,source:="inpn"]
    inpn[,lon:=longitude]
    inpn[,lat:=latitude]
    inpn[,date:=dateObservation]
    inpn[,observer:=observateur]
    inpn[,dataset:=libelleJeuDonnees]
  
    inpn<-st_as_sf(inpn,coords=c("lon","lat"),crs=4326)
    st_geometry(inpn)<-"geometry"
    inpn<-st_transform(inpn,st_crs(run))
    st_write(inpn,"C:/Users/God/Documents/rungrass/inpn.gpkg",append=FALSE)
  }else{
    inpn<-st_read("C:/Users/God/Documents/rungrass/inpn.gpkg")
  }
  
  
  #unique(setDT(st_drop_geometry(inpn))[,.(sp,statutBiogeoEspeceTaxref)][order(sp),])
 
  
  #### GBIF occs #######################
  
  # maybe use all suggested species names (e.g. E. tenella not fully covered)
  # remove iNat with datasetName
  if(gbif_update){
    k<-d$family!="Excluded"
    sp<-unique(d$sp[k])#[1:2]
    key<-sapply(strsplit(d$gbif[match(sp,d$sp)],"/"),tail,1)
    m<-match(sp,d$sp)
    other<-d$other[m]
    flore<-d$flore[m]
    index<-d$index[m]
    #i<-which(sp=="Aristida setacea")
    occs<-foreach(i=seq_along(sp),.packages=c("rgbif")) %do% {
      if(!is.na(other[i])){
        #sps<-c(sp[i],flore[i],index[i],strsplit(other[i],", ")[[1]])
        #sps<-unique(sps[!is.na(sps)])
        sps<-allspnames(sp[i])
        keys<-sapply(sps,function(j){
          ### chex here how to find all occs better
          res<-as.data.frame(name_backbone(name=j, rank="species", kingdom='plants'))
          if(res$rank%in%c("SPECIES","SUBSPECIES")){
            res$usageKey[1]
          }else{
            NULL
          }
        })
        sps<-sps[!sapply(keys,is.null)]  
        keys<-unlist(keys,use.names=FALSE)
      }else{
        sps<-sp[i]
        keys<-key[i]
      }
      l<-lapply(seq_along(sps),function(k){
        spoccs<-as.data.table(occ_search(taxonKey=keys[k],limit=2000,hasCoordinate=TRUE,country="RE")$data)
        if(nrow(spoccs)==0){
          NULL
        }else{
          spoccs$sp<-sp[i]
          spoccs
        }
      })
      Sys.sleep(0.2) # not to make too many requests, but not sure it is relevant
      cat("\r",paste(i,length(sp),sep=" / "))
      l
    }
    gbif<-unlist(occs, recursive=FALSE)
    gbif<-rbindlist(gbif,fill=TRUE)
    gbif[,date:=as.Date(paste(year,month,day,sep="-"),"%Y-%m-%d")]
    
    ### removes TAXREF checklist dataset https://doi.org/10.15468/frrkp9
    #gbif<-gbif[gbif$datasetKey!="6ed43f52-25d1-4d56-a821-63a8564b81f6",]
    #gbif<-gbif[gbif$datasetKey!="91aa3d5b-6f77-4135-a823-cef438a60dfa",]
    g<-intersect(grep("TAXREF",gbif$nameAccordingTo),grep("Checklist",gbif$nameAccordingTo))
    if(any(g)){
      gbif<-gbif[-g,]
    }
    ### removes iNat RG obs
    g<-grep("iNaturalist",gbif$datasetName)
    if(any(g)){
      gbif<-gbif[-g,]
    }
    
    gbif[,source:="gbif"]
    gbif[,observer:=recordedBy]
    gbif[,lon:=decimalLongitude]
    gbif[,lat:=decimalLatitude]
    gbif[,dataset:=datasetName]
    delete<-names(gbif)[ which(sapply(gbif,class)=="list")]
    gbif[,(delete):=NULL]
    gbif<-st_as_sf(gbif,coords=c("lon","lat"),crs=4326)
    gbif<-st_transform(gbif,st_crs(run))
    st_geometry(gbif)<-"geometry"
    st_write(gbif,"C:/Users/God/Documents/rungrass/gbif.gpkg",append=FALSE)
  }else{
    gbif<-st_read("C:/Users/God/Documents/rungrass/gbif.gpkg")
  }

  ### iNat occs ##############################
    
  # include reviewed_by me 
  # https://api.inaturalist.org/v1/docs/#!/Observations/get_observations

  if(inat_update){
    api<-"https://api.inaturalist.org/v1/observations?geo=true&verifiable=true&place_id=8834&taxon_id=47434%2C47161%2C52642&hrank=species&lrank=subspecies&order=desc&order_by=created_at&page=1&per_page=200"
    x<-fromJSON(api)
    pages<-ceiling(x$total_results/200)
  
    inatjson<-foreach(i=1:pages,.packages=c("jsonlite")) %do% {
      page<-paste0("page=",i)
      x<-fromJSON(gsub("page=1",page,api))
      inat<-data.frame(
        sp=x$results$taxon$name,
        user=x$results$user$login,
        location=x$results$location,
        grade=x$results$quality_grade,
        date=x$results$observed_on
      )
      row.names(inat)<-((i-1)*200+1):(((i-1)*200+1)+nrow(inat)-1)
      cat("\r",paste(i,pages,sep=" / "))
      inat
    }  
    inat<-do.call("rbind",inatjson)
    inat<-setDT(inat)
    inat<-inat[grade=="research" | user=="frousseu",]
    inat[,lon:=as.numeric(sapply(strsplit(location,","),"[",2))]
    inat[,lat:=as.numeric(sapply(strsplit(location,","),"[",1))]
    inat[,source:="inat"]
    inat[,observer:=user]
    inat[,dataset:="iNaturalist"]
    inat<-st_as_sf(inat,coords=c("lon","lat"),crs=4326)
    inat<-st_transform(inat,st_crs(run))
    st_write(inat,"C:/Users/God/Documents/rungrass/inat.gpkg",append=FALSE)
  }else{
    inat<-st_read("C:/Users/God/Documents/rungrass/inat.gpkg")
  }
  
  keep<-c("sp","source","observer","date","dataset")
  occs<-rbind(inpn[,keep],gbif[,keep],inat[,keep])
  dupnames<-c("sp","date","observer")

  notdups<-!duplicated(as.data.frame(occs[,dupnames])) | occs$source=="inat"
  occs<-occs[notdups,]
  
  
  #st_write(occs,"C:/Users/God/Documents/rungrass/occs.gpkg",append=FALSE)
  #occsold<-st_read("C:/Users/God/Documents/rungrass/occs.gpkg")
  #st_geometry(occsold)<-"geometry"
  
  #occs1<-as.data.frame(occs)
  #occs1$geometry<-as.character(occs1$geometry)
  #occs2<-as.data.frame(occsold)
  #occs2$geometry<-as.character(occs2$geometry)
  #updates<-unique(anti_join(occs1,occs2)$sp)
  #if(length(updates)==0){stop("No species to update")}
  
  ### Maps ####################

# https://geoservices.ign.fr/bdalti  
    
  ### Shaded terrain
  run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
  run<-st_buffer(st_buffer(run,100),-100)
  lf<-list.files("C:/Users/God/Downloads/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/3_SUPPLEMENTS_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",full.names=TRUE,pattern=".shp")
  source<-st_read(lf[1])
  lf<-list.files("C:/Users/God/Downloads/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/1_DONNEES_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",full.names=TRUE)
  l<-lapply(lf,rast)
  r<-do.call("merge",l)
  crs(r)<-"+proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # proj of source
  #r<-focal(r,7,mean) # smooth pixels and look
  r[r<0]<-NA
  r<-trim(r)
  r<-crop(r,st_transform(st_buffer(run,dist=3000),crs(r)))
  r<-mask(r,vect(st_transform(run,crs(r))))
  #slope <- terrain(r, "slope", unit="radians")
  #aspect <- terrain(r, "aspect", unit="radians")
  #hill <- shade(slope, aspect, 40, 270)
  hill<-shade(terrain(r,"slope",unit="radians"),terrain(r,"aspect",unit="radians"),40,270)
  r2<-focal(r,7,mean) # smooth pixels and look
  hill2<-focal(hill,7,mean) # smooth pixels and look
  r2<-aggregate(r2,10)
  hill2<-aggregate(hill2,10)
  #hill<-crop(hill,st_transform(st_buffer(run,dist=3000),crs(r)))
  #hill<-mask(hill,vect(st_transform(run,crs(r))))
  #alt<-mask(r,vect(st_transform(run,crs(r))))
  #plot(hill,col=grey(seq(0.05,1,length.out=100)),legend=FALSE,mar=c(0,0,0,0),axes=FALSE)
  #plot(alt, col=adjustcolor(colo.scale(1:100,c("grey80","lightgoldenrod","lightgoldenrod2","green4","darkgreen","brown4","grey20")),0.25), add=TRUE)
  
  
  pcol<-list(
    #inat=adjustcolor("forestgreen",0.75),
    #gbif=adjustcolor("gold",0.75),
    inat=adjustcolor("chartreuse3",0.75),
    gbif=adjustcolor("gold",0.75),
    inpn=adjustcolor("deepskyblue3",0.75),
    #colgrad=grey(seq(0.3,1,length.out=100))
    #colgrad=colo.scale(200,c("grey90","lightgoldenrod","seagreen3","forestgreen","darkgreen","saddlebrown","sienna4","brown")),
    colgrad=colo.scale(200,c("lightgoldenrod","seagreen3","forestgreen","darkgreen","darkgreen","saddlebrown","sienna4","brown"))
  )
  
  inatlogo<-image_read("https://static.inaturalist.org/sites/1-favicon.png?1573071870") |> image_scale("x40")
  gbiflogo<-image_read("https://images.ctfassets.net/uo17ejk9rkwj/5NcJCYj87sT16tJJlmEuWZ/85058d511b3906fbbb199be27b2d1367/GBIF-2015-mark.svg") |> image_scale("x40") |> image_fill("#ffffff00","+39+1",fuzz=40)
  inpnlogo<-image_read("https://openobs.mnhn.fr/openobs-hub/img/logo_openobs.png") |> image_trim() |> image_scale("x40")
  #inpnlogo<-image_transparent(inpnlogo,"white")
  #rev(sort(table(image_raster(inpnlogo)[,3])))[1:10]
  
  
  ### locs
  mult<-abs(diff(st_bbox(run)[c(1,3)])/diff(st_bbox(run)[c(2,4)]))
  mult<-abs(diff(ext(r2)[c(1,2)])/diff(ext(r2)[c(3,4)]))
  k<-d$family!="Excluded"
  sp<-unique(d$sp[k])#[1:10]
  sp<-sp[which(!sp%in%inpn$espece)]
  #sp<-sample(sp,20)
  #sp<-updates
  occs2<-st_transform(occs,3857)
  foreach(i=seq_along(sp),.packages=c("rgbif")) %do% {
    # find all names mathcing a species
    #m<-match(sp[i],d$sp)
    #sps<-c(d$sp[m],d$flore[m],d$index[m],strsplit(d$other[m],", ")[[1]])
    #sps<-unique(sps[!is.na(sps)])
    
    sps<-allspnames(sp[i])
    
    #x<-occs2[which(occs2$sp%in%sp[i]),]
    x<-occs2[occs2$sp%in%sps,]
    
    ### small
    png(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),".png"),height=500,width=500*mult,units="px")
    par(mar=c(0,0,0,0),oma=c(0,0,0,0),bg="#111111")
    #plot(st_geometry(run),col=alpha("#FFF8DC",0.95),border=NA)
    #plot(hill2,col=grey(0:100/100),legend=FALSE,mar=c(0,0,0,0),axes=FALSE)
    #plot(r2,col=adjustcolor(pcol$colgrad,0.40),legend=FALSE,mar=c(0,0,0,0),axes=FALSE,add=TRUE)
    #par(bg="grey15")
    #plot(hill,col=grey(0:100/100),axes=FALSE, legend=FALSE, mar=c(0,0,0,0),maxcell=1000000)
    #plot(r,col=adjustcolor(colo.scale(1:200,pcol$colgrad),0.30),axes=FALSE,legend=FALSE,add=TRUE,maxcell=1000000)
    plotRGB(mask(sat,vect(st_transform(st_buffer(run,1000),st_crs(sat)))),axes=FALSE,colNA="#111111")
    
    
    xgbif<-x[x$source=="gbif",]
    if(nrow(xgbif)>0){
      plot(st_geometry(xgbif),pch=21,bg=pcol$gbif,col=adjustcolor("black",0.7),lwd=1,cex=5,xpd=TRUE,add=TRUE)
    }
    xinpn<-x[x$source=="inpn",]
    if(nrow(xinpn)>0){
      plot(st_geometry(xinpn),pch=21,bg=pcol$inpn,col=adjustcolor("black",0.7),lwd=1,cex=5,xpd=TRUE,add=TRUE)
    }
    xinat<-x[x$source=="inat",]
    if(nrow(xinat)>0){
      plot(st_geometry(xinat),pch=21,bg=pcol$inat,col=adjustcolor("black",0.7),lwd=1,cex=5,xpd=TRUE,add=TRUE)
      #plot(st_geometry(xinat),pch=16,col="black",cex=0.55,xpd=TRUE,add=TRUE)
    }
    mtext(side=3,adj=0.99,line=-5,text=d$stat[match(sp[i],d$sp)],col=grey(0.85),cex=8,font=1,xpd=TRUE)
    dev.off()
    im<-image_read(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),".png"))
    im<-image_scale(im,"125")
    im<-image_fill(im,"#ffffff00","+1+1",fuzz=1)
    im<-image_fill(im,"#ffffff00","+124+1",fuzz=1)
    im<-image_fill(im,"#ffffff00","+90+1",fuzz=1)
    im<-image_fill(im,"#ffffff00","+1+100",fuzz=1)
    im<-image_fill(im,"#ffffff00","+124+100",fuzz=1)
    im<-image_modulate(im,brightness=125)
    im<-image_quantize(im,500,dither=FALSE)
    image_write(im,paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),".png"))
    #file.show(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),".png"))
    
    ### large
    png(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),"_large.png"),width=5,height=4.5,units="in",res=300)
    par(bg="grey15")
    #plot(hill,col=grey(0:100/100),axes=FALSE, legend=FALSE, mar=c(0,0,0,0),maxcell=1000000)
    #plot(r,col=adjustcolor(colo.scale(1:200,pcol$colgrad),0.30),axes=FALSE,legend=FALSE,add=TRUE,maxcell=1000000)
    plotRGB(sat,axes=FALSE)
    if(nrow(xgbif)>0){
      plot(st_geometry(xgbif),cex=1.0,lwd=1.3,pch=21,col="black",bg=pcol$gbif,add=TRUE)
    }
    if(nrow(xinpn)>0){
      plot(st_geometry(xinpn),cex=1.0,lwd=1.3,pch=21,col="black",bg=pcol$inpn,add=TRUE)
    }
    if(nrow(xinat)>0){
      plot(st_geometry(xinat),cex=1.0,lwd=1.3,pch=21,col="black",bg=pcol$inat,add=TRUE)
      #plot(st_geometry(xxinat),cex=0.25,pch=16,col="black",add=TRUE)
    }
    legend("topright",inset=c(0.09,0),legend=c("iNat","INPN","GBIF"),text.col="#fff8dc",cex=1,pt.lwd=1.3,pch=21,col="black",pt.bg=c(pcol$inat,pcol$inpn,pcol$gbif),bty="n")
    legend("topright",inset=c(0.09,0),legend=c("iNat","INPN","GBIF"),text.col="#fff8dc",cex=1,pt.lwd=1.3,pch=21,col="black",pt.bg=c(pcol$inat,pcol$inpn,pcol$gbif),bty="n")
    #legend("topright",inset=c(0.05,0),legend=c("iNat","INPN","GBIF"),text.col="#fff8dc",pt.cex=0.25,pt.lwd=1.3,pch=16,col=c("black","#FFFFFF00","black"),pt.bg="#FFFFFF00",bty="n")
    dev.off()
    #file.show("C:/Users/God/Downloads/large_map.png") 
    #im<-image_read(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),"_large.png"))
    #im<-image_quantize(im,2000,dither=FALSE)
    #image_write(im,paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),"_large.png"))
    

    
    im<-image_read(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),"_large.png"))
    im<-image_composite(im,inatlogo,gravity="northwest",offset="+1360+42")
    im<-image_composite(im,inpnlogo,gravity="northwest",offset="+1360+102")
    im<-image_composite(im,gbiflogo,gravity="northwest",offset="+1360+161")
    image_write(im,paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),"_large.png"))
    #file.show(paste0(file.path("C:/Users/God/Documents/rungrass/images",gsub(" ","_",sp[i])),"_large.png"))
    cat("\r",paste(i,length(sp),sep=" / "))
  }
  
  
  #ims<-list.files("C:/Users/God/Documents/rungrass/images",full=TRUE,pattern="_large.png")
  #im<-image_read(ims[10])
  ##im<-image_fill(im,"blue","+30+30",fuzz=5)
  #im<-image_quantize(im,500,dither=FALSE)
  #plot(im)
  #image_write(image_read(ims[10]),"C:/Users/God/Documents/rungrass/images/test1.png")
  #image_write(im,"C:/Users/God/Documents/rungrass/images/test2.png")
  #file.show("C:/Users/God/Documents/rungrass/images/test2.png")
  
}

### Push pics to GitHub ########################
#shell("
#cd /Users/God/Documents/rungrass
#git add .
#git commit \"images\"
#git push origin website
#")


### Write data csv ##################

write.table(d,"C:/Users/God/Documents/rungrass/grasses.csv",row.names=FALSE,sep=";",na="")

### Data for website #################

d$cbnm<-ifelse(!is.na(d$codenom),paste0("https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_nom=",d$codenom)),ifelse(is.na(d$taxref),NA,paste0("https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_taxref=",d$taxref))))
d$borbonica<-ifelse(d$taxref!="",paste0("http://atlas.borbonica.re/espece/",d$taxref),NA)
d$flore<-ifelse(is.na(d$flore),"",d$flore)
d$index<-ifelse(is.na(d$index),"",d$index)
d$genus<-sapply(strsplit(d$sp," "),"[",1)


lsp<-unique(c(d$sp,unlist(lapply(strsplit(d$sp," "),function(i){c(i[1],paste0(substr(i[1],1,1),". ",i[2]))}))))
for(i in 1:length(lsp)){
  d$id<-gsub(lsp[i],paste0("<span style='font-style: italic;'>",lsp[i],"</span>"),d$id)  
}


d$id<-ifelse(is.na(d$id),NA,d$id)

#d<-d[order(factor(d$family,levels=c("Poaceae","Cyperaceae","Juncaceae","Excluded")),d$sp,d$rank),]
d<-d[order(d$sp,d$rank),]

## Functions #################

genus<-function(i){
  #ge<-sort(unique(d$genus[d$family!="Excluded"]))  
  l<-split(d[d$family!="Excluded",c("genus","sp")],d$genus[d$family!="Excluded"])
  l<-lapply(l,function(x){
    x<-unique(x)
    ge<-x$genus[1]
    sp<-sapply(strsplit(x$sp," "),function(y){
      ans<-y[2:length(y)]
      if(length(ans)>1){
        paste(substr(ans[1],1,1),". ",ans[2],sep="")
      }else{
        ans[1]
      }
    })
    ge<-c(ge,gsub(" ","_",x$sp))
    ge1<-ge
    ge1[1]<-ge1[2]
    ge2<-c(ge[1],paste0("&nbsp;&nbsp;",sp))
    paste(paste0("<a class=\"atoc\" href=\"#",ge1,"\">",ge2,"</a>"),collapse="<br>")
  })
  paste(l,collapse="<br>")
  #paste(paste0("<a class=\"atoc\" href=\"#",ge,"\">",ge,"</a>"),collapse="<br>")
}


css<-function(i){
cat(paste0("
<!DOCTYPE html>
  <html>
  <head>
  <!-- <link rel='preload' href='https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@100&display=swap' rel='stylesheet'> -->
    <link href='https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@200' rel='stylesheet'>
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>
      RUNGRASS Poacées, cypéracées et juncacées de la Réunion
  </title>
  <link rel='icon' type='image/png' href='images/rungrasslogosmall.png?v=2'/>
  <link rel='shortcut icon' type='image/png' href='images/rungrasslogosmall.png?v=2'/>
  <meta name=\"keywords\" content=\"Réunion, poacées, grass, grasses, poaceae, sedge, sedges, cypéracées, cyperaceae, cyperus, rush, rushes, juncacées, juncaceae, botanique, flore, herbes, herb, herbe, graminées, carex\">
  <style>

:root {
  --green: #43cd80; /* #43cd80; */
  --white: #fff8dc; /* #fff8dc; */
  --black: #111111; /* #111111; */
  --gray: #fff8dc77; /* #fff8dc77; */
}
* {
  background-color: var(--black);
}
body .main-container {
  /* max-width: 1950px !important; */
  /* width: 1950px !important; */
  /* margin-left: 5vh; */
}
body {
  /* max-width: 1950px !important; */
  margin-left: 4vw;
  margin-right: 4vw;
  /* zoom: 1; */
}

@media only screen and (max-width: 850px) {
  body {
    margin-left: 1vw;
    margin-right: 1vw;
    /*-webkit-transform: scale(1);*/
    /* transform: scale(1,1); */
  }
}


* {
  box-sizing: border-box;
}
p, li, ul {
  padding: 0px;
  margin: 1vh;
  font-family: 'Roboto Mono';
  font-weight: 200;
  color: var(--white);
}
p.desc {
  font-size: 2.25vmin;
}



h1 {
  color: var(--green);
  font-size: 11vmin;
  padding-left:0vh;
  padding-top:0vh;
  padding-bottom:0vh;
  font-family:'Roboto Mono'; 
  font-weight: 1600;
  margin-top: 0vh;
  margin-bottom: 0vh;
  text-align: top;
}
h2 {
  color: var(--white);
  font-size: 4.25vmin;
  padding-left:0vh;
  padding-top:0vh;
  padding-bottom:0vh;
  font-family:'Roboto Mono'; 
  font-weight: 50;
  margin-top: 0vh;
  margin-bottom: 0vh;
  text-align: bottom;
}

.button {
  background-color: var(--black);
  border: none;
  color: var(--white);
  padding: 0vh 0vh;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 8vmin;
  font-weight: 800;
  font-family:'Roboto Mono'; 
  cursor: pointer;
}
.button:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}
.idbutton {
  background: none;
  border: none;
  color: var(--white);
  padding-top: 6vmin;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 4vmin;
  font-weight: 1200;
  font-family:'Roboto Mono'; 
  cursor: pointer;
  /* height: 1.75vmin; */
}
.idbutton:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}
a {
  text-decoration: none; /* no underline */
  color: var(--green);
  font-weight: 600;
}
.a2 {
  text-decoration: none; /* no underline */
  color: var(--green);
  font-size: 2.25vmin;
}
.a3 {
  color: var(--white);
  font-size: 2.25vmin;
  font-weight: 400;
}
.atoc {
  text-decoration: none; /* no underline */
  color: var(--green); /* #228B22; */
  font-weight: 600;
  font-size: 2.25vmin;
  padding: 0vh;
  margin: 0vh;
  z-index: 0;
}
.atoc:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}

.About {
  display: none;
  margin-left: 10vw;
  margin-right: 10vw;
}
@media only screen and (max-width: 850px) {
  .About {
    margin-left: 2vw;
    margin-right: 2vw;
  }
}

.ID {
  display: none;
}
.flore {
  color: var(--gray);
  font-size: 2vmin;
  font-family:'Roboto Mono'; 
  font-weight: 100;
  display: inline-block;
  align-self: flex-end;
}
.column {
  float: left;
  width: 33%;
  padding: 8px;
  background: red;
}
.species {
  width: 100%;
  padding: 0px; 
  margin-top: 3vh;
  background: var(--black); /* forestgreen; */
  /* background: #39AC39; */
  border-radius: 0px;
  /* position: relative;  for flushing links to the bottom on the right side instead of float: right; */
}
/* .species:hover { */
/*  opacity: 0.70; */
/*  filter: alpha(opacity=100); */
/* } */
.row::after {
  content: \"\";
  clear: both;
  display: table;
  background: blue;
}
.imgsp {
  max-height:25vh;
  height:25vh;
  width: 10.40vw; /* 14.26% */
  object-fit:cover;
  padding: 1px; /* 2px */
  margin: 0px; /* 2px */
  background: #000000; /* var(--white); */
  background-origin: content-box;
  cursor: pointer;
  border-radius: 7vh;
}
.imgsp:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}

@media only screen and (max-width: 850px) {
  .imgsp {
    height: 15vh;
    width: 30vw;
    border-radius: 9vw;
  }
}


.imgmap {
  height: 11vh; 
  padding-top: 1vh;
  cursor: pointer;
}
.imgmap:hover {
  opacity: 0.60;
  filter: alpha(opacity=100);
}
.imglink:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}

.p2, .p3 {
  color: var(--white);
  font-size: 4vmin; /* 30px */
  padding-top: 6vmin;
  font-family:'Roboto Mono'; 
  font-weight: 800;
}
.p3 {
  padding-top: 8vmin;
  padding-left: 2vmin;
  padding-right: 8vmin;
  z-index: 99;
}


.scroller {
  scrollbar-width: thin;
}


::-webkit-scrollbar {
  width: 2vh;
}
::-webkit-scrollbar-track {
  background: #222; /* var(--gray); */
}
::-webkit-scrollbar-thumb {
  background: #444;
  border-radius: 1vh; /* 1 */
}
::-webkit-scrollbar-thumb:hover {
  /* background: #444; */
  opacity: 0.50;
  filter: alpha(opacity=100);
}
::-webkit-scrollbar-track {
  border-radius: 2vh; /* 2 */
  margin-top: 6vmin;
  margin-bottom: 6vmin;
}


@media only screen and (max-width: 850px) {
  ::-webkit-scrollbar {
    width: 1vh;
  }
  ::-webkit-scrollbar-thumb {
    background: #444;
    border-radius: 0.5vh;
  }
  ::-webkit-scrollbar-track {
    border-radius: 1vh;
  }
}




.bottomhr {
  height: 2vh;
  border-width:0;
  border-radius: 1vh;
  color: black;
  background-color: #222; /* var(--gray); */
}

.tophr {
  height: 1px;
  color: #000000; /* var(--gray); */
  border: 1px;
  background-color: var(--gray);
}

#imgsp {
  border-radius: 1vh;
  cursor: pointer;
  transition: 0.3s;
}

/* The Modal (background) */
.modal, .idmodal, .mapmodal {
 display: none; /* Hidden by default */
 position: fixed; /* Stay in place */
 z-index: 100; /* Sit on top */
 padding-top: 4vh; /* Location of the box */
 left: 0;
 top: 0;
 width: 100vw; /* Full width */
 height: 100vh; /* Full height */
 overflow: auto; /* Enable scroll if needed */
 background-color: var(--black); /* Fallback color */
 background-color: rgba(0,0,0,0.9); /* Black w/ opacity */
 overscroll-behavior: contain;
}


/* Modal Content (image) */
.modal-content, .idmodal-content {
 margin: auto;
 display: block;
 width: 70vw;
 max-width: 70vw;
 height: auto;
 max-height: auto;
}
@media only screen and (max-width: 850px) {
  .modal-content, .idmodal-content {
    width: 98vw;
    max-width: 98vw;
    height: auto;
    max-height: auto;
  }
}


/* Modal Content (image) */
.mapmodal-content {
margin: auto;
display: block;
width: auto;
max-width: auto;
height: 95vmin;
max-height: 95vmin;
background-color: var(--black);
}


/* Caption of Modal Image */
#caption {
margin: auto;
display: block;
width: 70vw;
max-width: 70vw;
text-align: center;
color: #ccc;
padding: 2vmin 0;
height: 5vmin;
font-family: 'Roboto Mono';
}
@media only screen and (max-width: 850px) {
  #caption {
    width: 98vw;
    max-width: 98vw;
  }
}

/* Link of Modal Image */
#link {
margin: auto;
display: block;
width: 70vw;
max-width: 70vw;
text-align: center;
color: #ccc;
padding: 2vmin 0;
margin-bottom: 6vh;
height: 7vmin;
font-family: 'Roboto Mono';
}
@media only screen and (max-width: 850px) {
  #link {
    width: 98vw;
    max-width: 98vw;
  }
}

#idtips {
margin: auto;
display: block;
width: 50%;
max-width: 50%;
text-align: center;
color: #ccc;
padding: 2vmin 0;
font-family: 'Roboto Mono';
}

div.sticky {
  position: -webkit-sticky;
  position: sticky;
  top: 6vmin; /* 0; */
  padding: 0vh;
    width: 100%;
  padding: 0vh;
  background: var(--black);
  border-radius: 0vh;
  z-index: 99;
}

div.left {
  position: -webkit-sticky;
  position: sticky;
  top: 6vh;
  width: 16%; 
  height: 98vh; 
  float: left; 
  display: inline-block; 
  padding-right: 0.5%; 
  overflow-y: scroll;
}

div.right {
  width: 82%; 
  float: left; 
  display: inline-block; 
  padding-left: 1.5%;
}

@media only screen and (max-width: 850px) {
  div.left {
    width: 35%;
  }
  div.right {
    width: 63%;
  }
}

/* Add Animation */
.modal-content, .idmodal-content, .mapmodal-content, #caption, #link {  
-webkit-animation-name: zoom;
-webkit-animation-duration: 0.3s;
animation-name: zoom;
animation-duration: 0.3s;
}

@-webkit-keyframes zoom {
from {-webkit-transform:scale(0)} 
to {-webkit-transform:scale(1)}
}

@keyframes zoom {
from {transform:scale(0)} 
to {transform:scale(1)}
}

/* The Close Button */
.close, .idclose {
position: absolute;
top: 1vh;
right: 2vh;
color: #f1f1f1;
font-size: 5vh;
font-weight: bold;
transition: 0.3s;
}

.close:hover,
.close:focus, .idclose:hover, .idclose:focus {
color: #bbb;
text-decoration: none;
cursor: pointer;
}

/* 100% Image Width on Smaller Screens */
@media only screen and (max-width: 850px){
.modal-content, .idmodal-content, .mapmodal-content  {
width: 100%;
}
}


#testing1, #testing2, #testing3, #testing4 {
  transition: 0.3s;
}


#header {
  position: -webkit-sticky;
  position: sticky;
  top: 0;
  padding-bottom: 0vh;
  display: flex;                  /* establish flex container */
  flex-direction: row;            /* default value; can be omitted */
  flex-wrap: wrap;              /* default value; can be omitted */
  justify-content: space-between; /* switched from default (flex-start, see below) */
  transition: 0.3s;
  z-index: 99;
  /* border-bottom: 1px solid #000; /* var(--gray); */ */
  /* background-color: #000; */
}
#header > div {
  /* width: 100px; */
  /* height: 8vh; */
  /* border: 2px dashed red; */
}

.headersp {
  display: flex;                  /* establish flex container */
  flex-direction: row;            /* default value; can be omitted */
  flex-wrap: wrap;              /* default value; can be omitted */
  justify-content: space-between; /* switched from default (flex-start, see below) */
  align-content: bottom;
  align-items: bottom;
  padding-top: 6vh;
  /* background-color: #111; */
}

.headersp > div {
  /* width: 100px; */
  /* height: 6vh; */
  /* border: 2px dashed red; */
}

.inner{
  background: var(--black);
  display: flex;
  height: 100%;
}


table{
  width: 100%;
  border: 0px solid #FFF;
  text-align: left;
  vertical-align: center;
  margin-top: auto;
  margin-bottom: auto;
  border-spacing: 2em;
}
th.logo{
  width: 12%;
  text-align: center;
}
th.text{
  width: 85%; 
  text-align: left;
}




</style>
</head>
<a id=\"top\"></a>  
<body>
<!-- <img style=\"height: 4vmin;\" src=\"https://res.cloudinary.com/dphvzalf9/image/upload/rungrasslogo.png\"> -->
<div id=\"header\">
  <div>
    <h1 id=\"testing1\"><a href=\"#top\"><img id=\"testing2\" style=\"height: 9vmin; padding: 0vh; margin: 0vh; border: 0px solid red;\" src=\"https://res.cloudinary.com/dphvzalf9/image/upload/rungrasslogotext.png\"></a></h1>
  </div>
  <div>
    <h2 id=\"testing3\">Guide&nbspphotographique&nbspdes&nbsppoacées,<br>cypéracées&nbspet&nbspjuncacées&nbspde&nbspla&nbspRéunion</h2>
  </div>
  <div>
    <a href=\"#pres\"><button class=\"button\" id=\"testing4\" onclick=\"scrollFunction(); myFunction();\">&#10068</button></a>
  </div>
</div>
  
<!-- <hr class=\"tophr\"> -->
  
<div class=\"About\" id=\"About\">  
<br>
<a id=\"pres\"></a>
<h1>&#32</h1>
<br>
<div>
  <button class=\"button\" style=\"display: inline-block; float: right;\" onclick=\"myFunction()\">x</button>
</div>
<br>
<br>
<br>
<br>
<br>

<img style=\"width: 60vw; padding: 0vh; margin: 0vh; border: 0px solid red;\" src=\"https://res.cloudinary.com/dphvzalf9/image/upload/rungrasslogotext.png\">
<br>
<br>
<br>
<br>
<br>
<p class=\"desc\">Cette page est un guide photographique des poacées (graminées), cypéracées et juncacées de la Réunion. La liste des espèces présentées est basée sur la liste des espèces reconnues comme étant présentes à la Réunion selon l'<a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore\" target=\"_blank\">Index taxonomique de la flore vasculaire de La Réunion</a> du <a href=\"http://www.cbnm.org/\" target=\"_blank\">Conservatoire National Botanique Mascarin (CBN - CPIE Mascarin)</a> et sur de nouvelles observations réalisées depuis la dernière parution de l'index. Plusieurs espèces n'ont pas été retenues, car leurs mentions résultent possiblement d'erreurs d'identification, d'étiquetages ou autres. La liste des espèces qui n'ont pas été retenues est présentée à la toute fin des sections sur chaque espèce. </p><br>
  
<p class=\"desc\">La plupart des photos proviennent d'observations déposées sur la plateforme <a href=\"https://www.inaturalist.org/\" target=\"_blank\">iNaturalist</a> ou de spécimens d'herbiers déposés au <a href=\"https://science.mnhn.fr/institution/mnhn/item/search\" target=\"_blank\">Muséum National d'Histoire Naturelle</a>. La majorité des photos proviennent de mes observations effectuées à la Réunion, mais plusieurs photos proviennent également d'observations déposées sur iNaturalist qui n'ont pas nécessairement été effectuées à la Réunion. Dans la plupart des cas, les photos ne sont pas libres de droits et leur utilisation doit respecter la license qui lui est associée. Un lien au bas de chaque photo permet de consulter l'observation et la license associée.</p><br>

<p class=\"desc\">Pour plusieurs espèces, notamment pour quelques espèces rares ou plus difficiles à identifier, seules des photos de spécimens d'herbier sont disponibles. Si vous possédez des photos pour ces espèces et si vous souhaitez contribuer à ce site, merci de me contacter ou de déposer vos photos sous forme d'observations sur <a href=\"https://www.inaturalist.org/\" target=\"_blank\">iNaturalist</a>. Pour la plupart des espèces, l'identification n'a pas été validée par des experts et je n'ai encore jamais observé plusieurs espèces présentées sur ce site. Il convient donc de rester très prudent lors de l'utilisation des images présentées ici à des fins d'identification. Il est fort probable que des erreurs sur l'identification s'y glissent. Dans bien des cas, certaines espèces ne seront pas identifiables par comparaison à partir des photos présentées ici et il faudra se référer à des clés d'identification comme celle de la <a href=\"https://www.editions.ird.fr/produit/471/9782709924535/flore-des-mascareignes-la-reunion-maurice-rodrigues\" target=\"_blank\">Flore des Mascareignes</a> pour pouvoir identifier les spécimens. Il faut également toujours considérer la possibilité qu'il s'agisse d'une nouvelle espèce qui n'a pas encore été observée à la Réunion et qui n'est donc pas rapportée sur ce site. Il existe assurément plusieurs dizaines d'espèces présentes sur l'île qui n'ont pas encore été rapportées. Finalement, merci de me faire signe si vous trouvez une nouvelle espèce, des erreurs sur le site ou pour toutes questions, commentaires ou suggestions (francoisrousseu at hotmail com ou <a href=\"https://www.inaturalist.org/people/frousseu\" target=\"_blank\">frousseu</a> sur iNaturalist). Il est également possible de contribuer par GitHub où le site et le code sont hébergés <a href=\"https://github.com/frousseu/rungrass\" target=\"_blank\">https://github.com/frousseu/rungrass</a>.</p><br><br>

<br>
<h2>Identification&nbsp&nbsp&#9660</h2><br><br>

<p class=\"desc\">
Pour quelques espèces, les traits distinctifs et l'aspect général de la plante sont décrits et des trucs sont donnés pour distinguer des espèces semblables pouvant être confondues avec l'espèce en question.  Ces descriptions sont données à titre indicatif seulement et on pour but de faciliter l'identification et la reconnaissance des différentes espèces sur le terrain et non de fournir une description exhaustive. Les descriptions sont majoritairement basées sur mes observations personnelles et elles sont parfois fortement inspirées des descriptions et des clés d'identification fournies dans la Flore des Mascareignes ainsi que d'autres ouvrages comme <a href=\"https://ausgrass2.myspecies.info\" target=\"_blank\">AusGrass2</a>, <a href=\"https://floranorthamerica.org\" target=\"_blank\">FNA</a>, <a href=\"https://powo.science.kew.org\" target=\"_blank\">POWO</a>, le guide <a href=\"http://opus.sanbi.org/handle/20.500.12143/5602\" target=\"_blank\"><i>Identification Guide to southern African Grass</i></a> (Fish, Mashau, Moeaha et Nembudani, 2015) ou Graminées des pâturages et des cultures à Madagascar (Bosser, 1969). Pour certaines espèces, mon expérience est trop limitée (et souvent inexistante!) pour fournir une description.
</p><br><br>

<br>
<h2>Occurrences<img style=\"height: 10vmin; padding: 0px;\" src=\"https://res.cloudinary.com/dphvzalf9/image/upload/Cyperus_javanicus.png\"></h2><br><br>

<p class=\"desc\">
L'objectif premier des cartes de distribution présentées ici est de donner une idée approximative de l'abondance et de la répartition des espèces à travers l'île. En aucun cas, ces occurrences doivent être interprétées comme étant exhaustives, précises ou représentatives de la répartition exacte des espèces. Lors de l'interprétation de ces cartes, il faut garder en tête que:
<br><br>
&nbsp&nbsp&nbsp- les localisations peuvent être très imprécises<br>
&nbsp&nbsp&nbsp- certaines localisations sont rapportées à une grille de faible résolution<br>
&nbsp&nbsp&nbsp- les identifications peuvent être erronées<br>
&nbsp&nbsp&nbsp- les mentions peuvent être historiques<br>
&nbsp&nbsp&nbsp- certaines espèces plus difficiles à identifier, à trouver ou d'intérêt moindre peuvent être sous-rapportées<br>
&nbsp&nbsp&nbsp- l'effort variable des observateurs ou d'échantillonnage d'un endroit à l'autre de l'île est susceptible d'affecter la répartition apparente<br>
&nbsp&nbsp&nbsp- pour diverses raisons, les mentions de certaines espèces peuvent ne pas avoir été intégrées aux base de donnnées utilisées pour récolter les occurrences<br>
&nbsp&nbsp&nbsp- des problèmes taxonomiques peuvent être à l'origine d'absences ou de surplus d'occurrences<br>
&nbsp&nbsp&nbsp- etc.<br>
<br>
Malgré ces multiples précautions à prendre, ces cartes demeurent généralement utiles pour se faire une idée rapide de la répartition des différentes espèces. Par exemple, Holcus lanatus et Nastus borbonicus sont deux espèces plutôt abondantes dans les hauts. Scleria sieberi est davantage retrouvée dans les forêts humides de l'est de l'île, alors que Heteropogon contortus est davantage retrouvé dans les milieux secs de l'ouest. En général, plus il y a d'occurrences, plus l'espèce est facile à observer. Plusieurs espèces n'ont aucune mention ce qui peut indiquer qu'elles sont rares, localisées, difficiles à identifier, négligées, historiquement présentes sur l'île ou tout simplement qu'aucune observation n'a été intégrée dans les bases de données utilisées. 
<br><br>
Les données d'occurrences illustrées sur ces cartes proviennent de GBIF, OpenObs et des observations sur iNaturalist de niveau recherche ou identifiées par moi.  Plusieurs observations sur OpenObs proviennent de GBIF. Dans ces cas, les observations sur OpenObs marquées comme provenant de GBIF sont illustrées comme provenant de GBIF. Plusieurs observations sur iNaturalist se retrouvent à la fois dans GBIF et dans OpenObs. Dans ces cas, toutes les observations qui proviennent originellement d'iNaturalist sont illustrées comme venant de cette plateforme. À noter qu'un certaine portion des observations effectuées avec <a target=\"_blank\" href=\"https://plantnet.org/\">PlantNet</a> se<a target=\"_blank\" href=\"https://plantnet.org/2020/08/06/vos-donnees-plntnet-integrees-gbif/\"> retrouveront également sur GBIF</a> et seront donc représentées ici. <a target=\"_blank\" href=\"https://www.gbif.org/dataset/14d5676a-2c54-4f94-9023-1e8dcd822aa0\">Les observations dont l'identification est uniquement basée sur l'algorithme de reconnaissance automatisée</a> ont toutefois été éliminées.
<!-- <br><br>
Pour beaucoup d'espèces, plusieurs noms ont été ou sont couramment utilisés ce qui peut complexifier les requêtes cherchant à récolter l'ensemble des mentions pour une espèce donnée. Voir la section identification pour chaque espèce pour la liste des noms utilisés pour récolter les occurrences de l'espèce. -->
<br><br>
Finalement, le statut des différentes espèces à la Réunion est indiqué sur chaque carte:<br><br>
&nbsp&nbsp&nbsp- <b>En</b>: Endémique de la Réunion ou des Mascareignes<br>
&nbsp&nbsp&nbsp- <b>In</b>: Indigène<br>
&nbsp&nbsp&nbsp- <b>Cr</b>: Cryptogène<br>
&nbsp&nbsp&nbsp- <b>Ex</b>: Exotique<br>
</p><br><br>

<br>
<h2>Liens externes</h2><br><br>

<table style=\"width:100%\">
  <tr>
    <th class=\"logo\"><a target=\"_blank\" href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"https://mascarine.cbnm.org/templates/favourite/favicon.ico\">
</a></th>
    <th><p class=\"desc\">
<a target=\"_blank\" href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore\">L'Index taxonomique de la flore vasculaire de La Réunion</a> produit par le CBN-CPIE Mascarin contient plusieurs informations sur les espèces présentées ici et a permis détablir la liste des espèces présentes sur l'île. Il contient notamment le statut de chaque espèce sur l'île (endémique, indigène, exotique, envahissante, cryptogène, etc.), les noms vernaculaires et les noms locaux, etc. La section PLUS D'INFOS est à consulter pour l'historique et le niveau de connaissance de chaque espèce sur l'ile.
  </tr>
    <tr>
    <th class=\"logo\"><a target=\"_blank\" href=\"http://atlas.borbonica.re\">
  <img style=\"height: 8vmin; padding: 0vmin;\" src=\"https://www.borbonica.re/img/carousel/carte-run.png\">
</a></th>
    <th><p class=\"desc\">
<a href=\"http://atlas.borbonica.re\" target=\"_blank\">Borbonica</a> est le portail d'accès aux données sur la faune et la flore du SINP à La Réunion (Système d'Information de l'iNventaire du Patrimoine naturel de La Réunion (SINP 974)) . Il est administré par la DEAL et le Parc national de La Réunion. L'atlas présente notamment des synthèses par espèce décrivant les occurrences, les habitats, la phénologie, la synonymie, etc. Les liens pour chaque espèce renvoient vers ces fiches lorsque celles-ci sont disponibles. Souvent, les occurrences retrouvées sur Borbonica sont plus complètes que les occurrences illustrées ici, car les données présentées par Borbonica sont moins ouvertement accessibles que les données disponibles sur iNaturalist ou GBIF.
</p></th>
  </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://openobs.mnhn.fr\">
  <img style=\"height: 5vmin; padding: 0px;\" src=\"images/inpnlogo.png\">
</a></th>
      <th><p class=\"desc\">
<a href=\"https://openobs.mnhn.fr\" target=\"_blank\">OpenObs</a> est le portail français d'accès
aux données d'observation sur les espèces.
</p></th>
    </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://www.inaturalist.org\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"https://static.inaturalist.org/sites/1-favicon.png?1573071870\"> 
</a></th>
      <th><p class=\"desc\">
<a href=\"https://www.inaturalist.org\">iNaturalist</a> est une plateforme et une application permettant d'enregistrer et de partager des observations du vivant qui seront soumises à la communauté pour l'identification. Il s'agit probablement de la plus importante plateforme de science participative pour l'enregistrement d'observations opportunistes du vivant. Le lien présenté pour chaque espèce renvoie vers l'ensemble des observations de l'espèce enregistrées sur cette plateforme pour la Réunion. À noter que toutes les observations faites et déposées sur iNaturalist seront éventuellement intégrées à ce site. Dans bien des cas, les espèces n'ont pas encore d'observations sur iNaturalist.
</p></th>
    </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://www.gbif.org/fr/\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"https://images.ctfassets.net/uo17ejk9rkwj/5NcJCYj87sT16tJJlmEuWZ/85058d511b3906fbbb199be27b2d1367/GBIF-2015-mark.svg\"> 
</a></th>
      <th><p class=\"desc\">
<a href=\"https://www.gbif.org/fr/\" target=\"_blank\">GBIF</a> (<i>Global Biodiversity Information Facility</i>) est un système mondial d’information sur la biodiversité. Les observations déposées sur iNaturalist ayant une license CC et atteignant le niveau recherche sont régulièrement versées sur GBIF. Les liens pour chaque espèce renvoient vers la page dédiée à chaque taxon. Dans bien des cas, une même espèce peut avoir plusieurs noms et ces noms peuvent être associés à différentes occurrences ou différentes photos. En général, le lien donné ici renvoie vers le nom le plus couramment utilisé pour l'espèce.
</p></th>
    </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://science.mnhn.fr\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"images/mnhn.png\"> 
</a></th>
      <th><p class=\"desc\">
<a href=\"https://science.mnhn.fr\">Le Muséum National d'Histoire Naturelle (MNHN)</a> contient plusieurs spécimens d'herbier digitalisés et est très utile pour étudier les spécimens types et les espèces plus rarement rencontrées ou rapportées dans les bases de donnnées de science participative.
</p></th>
    </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://tropicos.org\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"images/tropicos.png\"> 
</a></th>
      <th><p class=\"desc\">
<a href=\"https://tropicos.org\">Tropicos</a> est un système de données botaniques en ligne du <i>Missouri Botanical Garden</i>. Il contient de l'information sur la taxonomie, des spécimens d'herbier, etc.
</p></th>
    </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://powo.science.kew.org/\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"https://powo.science.kew.org/img/powo-favicon.ico\">
</a></th>
      <th><p class=\"desc\">
<a href=\"https://powo.science.kew.org/\" target=\"_blank\">POWO</a> (<i>Plants of the World Online</i>) est un programme du <i>Royal Botanical Garden, Kew</i> cherchant à rendre disponible l'ensemble des données numériques disponibles sur la flore mondiale. On y retrouve notamment des descriptions, des photos, des renseignements taxonomiques et des cartes de répartition pour la majorité de la flore vasculaire. Les liens pour chaque espèce renvoient vers les pages pour chaque espèce.
</p></th>
    </tr>
    <tr>
      <th class=\"logo\"><a target=\"_blank\" href=\"https://floranorthamerica.org\">
  <img style=\"height: 8vmin; padding: 0px;\" src=\"images/fna.jpg\"> 
</a></th>
      <th><p class=\"desc\">
<a href=\"https://floranorthamerica.org\"><i>Flora of North America</i></a> est une flore en ligne très utile pour l'identification des espèces lorsque celles-ci ont été rapportées en Amérique du Nord. On trouve notamment des descriptions, des illustrations et des clés d'identification.
</p></th>
    </tr>
</table>


<br>

<br>
<h2>Autres ressources</h2><br><br>

<p class=\"desc\">
L'application <a href=\"https://mascarine.cbnm.org/\" target=\"_blank\">Masacarine-Cadetiana</a> développée par le CBN-CPIE Mascarin est une interface permettant de faire des requêtes spatiales et d'étudier les occurrences de la flore réunionnaise.
</p><br><br>

<p class=\"desc\">
<a href=\"https://www.mi-aime-a-ou.com/flore_ile_reunion.php\" target=\"_blank\">Mi-aime-a-ou</a> est un site d'intérêt général  sur la Réunion et comporte notamment une impressionante quantité d'information sur la flore réunionnaise.
</p><br><br>

<p class=\"desc\">
<a href=\"https://ausgrass2.myspecies.info/\" target=\"_blank\">AusGrass2</a>
Site australien sur les poacées très utile pour l'identification lorsque les espèces sont aussi présentes en territoire australien.
</p><br><br>

<!-- <hr class=\"tophr\"> -->

</div>



<div style=\"display:inline-block; width:100%;\">
  <div class=\"left\" style=\"\">
    <div class=\"sticky\">
      <span class=\"p3\">Espèces</span>
    </div>
      <p style = \"color: black; font-size: 0vh; padding-top: 6vh; padding-bottom: 6vh; margin-left: 1vh;\">",genus(),"</p>
  </div>
  <div class=\"right\" style=\"\">

<!-- <div class=\"species\" style=\"margin-top: 0px;\">
<p class=\"p2\">iNaturalist&nbsp;&nbsp<span class=\"flore\">Flore des Mascareignes&nbsp;&nbsp</span><span class=\"flore\">Index du CBN-CPIE Mascarin</span><span class=\"flore\" style=\"float:right;\">Famille</span>
</p>
</div> -->
"))

}  
  

species_links<-function(x,i){
  res<-paste0(
    ifelse(is.na(x$cbnm[i]),"",
    paste0("<a target=\"_blank\" href=\"",x$cbnm[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"https://mascarine.cbnm.org/templates/favourite/favicon.ico\">
     </a>")),
    ifelse(is.na(x$borbonica[i]),"",
    paste0("<a target=\"_blank\" href=\"",x$borbonica[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"images/borbonicalogo.png\">
     </a>")),
    ifelse(is.na(x$inpn[i]),"",
           paste0("<a target=\"_blank\" href=\"",x$inpn[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"images/inpnlogo.png\">
     </a>")),
     "<a target=\"_blank\" href=\"",x$inat[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"https://static.inaturalist.org/sites/1-favicon.png?1573071870\"> 
     </a>
     <a target=\"_blank\" href=\"",x$gbif[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"https://images.ctfassets.net/uo17ejk9rkwj/5NcJCYj87sT16tJJlmEuWZ/85058d511b3906fbbb199be27b2d1367/GBIF-2015-mark.svg\"> 
     </a>
     <a target=\"_blank\" href=\"",x$mnhn[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"images/mnhn.png\">
     </a>
     <a target=\"_blank\" href=\"",x$tropicos[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"images/tropicos.png\">
     </a>
     <a target=\"_blank\" href=\"",x$powo[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"https://powo.science.kew.org/img/powo-favicon.ico\">
     </a>",
     ifelse(is.na(x$fna[i]),"",
     paste0("<a target=\"_blank\" href=\"",x$fna[i],"\">
       <img class=\"imglink\" style=\"height: 3vmin; padding: 0vh;\" src=\"images/fna.jpg\">
     </a>")),"
     </a>&nbsp;"
  )
  res
}

uncertain<-function(x){
  if(x=="Urochloa plantaginea"){return("Urochloa (plantaginea ?)")}  
  if(x=="Polypogon fugax"){return("Polypogon (fugax ?)")} 
  if(x=="Urochloa eminii"){return("Urochloa (eminii ?)")} 
  if(x=="Eragrostis capensis"){return("Eragrostis (capensis ?)")} 
  if(x=="Lolium multiflorum"){return("Lolium (multiflorum ?)")} 
  if(x=="Cyperus flavescens"){return("Cyperus (flavescens ?)")} 
  if(x=="Cynodon aethiopicus"){return("Cynodon (aethiopicus ?)")} 
  if(x=="Paspalum mandiocanum"){return("Paspalum (mandiocanum ?)")} 
  if(x=="Adenochloa hymeniochila"){return("(Adenochloa hymeniochila ?)")} 
  if(x=="Sporobolus diandrus"){return("Sporobolus (diandrus ?)")} 
  if(x=="Glyceria declinata"){return("Glyceria (declinata ?)")} 
  if(x=="Poa trivialis"){return("Poa (trivialis ?)")}
  if(x=="Panicum miliaceum"){return("Panicum (miliaceum ?)")}
  if(x=="Aristida congesta barbicollis"){return("Aristida (congesta barbicollis ?)")}
  if(x=="Avenella flexuosa"){return("Avenella (flexuosa ?)")}
  if(x=="Urochloa brizantha"){return("Urochloa (brizantha ?)")}
  if(x=="Aristida congesta congesta"){return("Aristida (congesta congesta ?)")}
  if(x=="Aristida ramosa"){return("Aristida (ramosa ?)")}
  if(x=="Chloris virgata"){return("Chloris (virgata ?)")}
  if(x=="Hordeum murinum"){return("Hordeum (murinum ?)")}
  if(x=="Urochloa mosambicensis"){return("Urochloa (mosambicensis ?)")}
  x
}


species_header<-function(x,i){
  cat(paste0(
    "<div id=\"",gsub(" ","_",x$sp[i]),"\" class=\"headersp\">
       <div class=\"inner\">
           <span class=\"p2\">",uncertain(x$sp[i]),"</span>"
           ,ifelse(is.na(x$id[i]),"",paste0("&nbsp&nbsp&nbsp<button class=\"idbutton\" onclick=\"showID('",paste0(x$sp[i],"ID"),"')\" data-id=\"",x$id[i],"\">&#9660</button>&nbsp")),"
       </div>
       <div class=\"inner\">
       <span class=\"flore\">",paste0(gsub(" ","&nbsp",x$flore[i]),"&#32&#32&#32",gsub(" ","&nbsp",x$index[i]),"&#32&#32"),species_links(x,i),x$family[i],"<img class=\"imgmap\" src=\"",paste0(src,paste0(gsub(" ","_",x$sp[i]),".png")),"\" data-src=\"",paste0(src,paste0(gsub(" ","_",x$sp[i])),"_large.png"),"\" loading=\"lazy\"></span>
       </div>
     </div>",ifelse(is.na(x$id[i]),"",paste0("<div class=\"ID\" id=\"",paste0(x$sp[i],"ID"),"\"><p class=\"desc\"><br>",x$id[i],"<br><br></p></div>")),"
     
 "))
}
#species_header(d,i)


species_photo<-function(x,i){
  if(!is.na(x$large[i])){
    large<-x$large[i]
  }else{
    large<-x$photo[i]
  }
  cat(paste0(
    "<img class=\"imgsp\" src=\"",gsub("/original.","/small.",x$photo[i]),"\" data-src=\"",large,"\" title=\"",paste(x$attribution[i]),"\" alt=\"",paste(x$obs[i]),"\" loading=\"lazy\">"
  ))
}

species_excluded<-function(i,j){
  #paste(sapply(seq_along(i),function(i){
  excluded<-paste0(paste0("<a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_taxref=",j),"\" target=\"_blank\">",i,"</a>"),collapse=", ")
  cat(paste("<br><br><p style = \"font-size: 2vh;\">Liste des espèces qui n'ont pas été retenues et lien vers l'index du CBN - CPIE Mascarin:"),excluded,"</p><br><br>")
}


### Species list #################

d2<-d[d$family!="Excluded",]
l<-split(d2,factor(d2$sp,levels=unique(d2$sp)))


### HTML #########################

con <- file("C:/Users/God/Documents/rungrass/index.html", open = "wt", encoding = "UTF-8")
sink(con)
css()
invisible(
  lapply(l[1:length(l)],function(i){
    species_header(i,1)
    if(!is.na(i$photo[1])){
      invisible(
        sapply(1:nrow(i),function(j){species_photo(i,j)})
      )
    }
  })
)

species_excluded(d$sp[d$family=="Excluded"],d$taxref[d$family=="Excluded"])



### Script ####################

cat("
<div id=\"myModal\" class=\"modal\">
  <span class=\"close\">&times;</span>
  <img class=\"modal-content\" id=\"img01\">
  <div id=\"caption\"></div>
  <div id=\"link\"></div>
</div>     
")


cat("
<script>

    var modalsp = document.getElementById('myModal');
    var images = document.getElementsByClassName('imgsp');
    var modalImg = document.getElementById(\"img01\");
    var captionText = document.getElementById(\"caption\");
    var linkText = document.getElementById(\"link\");
    
    modalsp.addEventListener('click',function(){
      this.style.display=\"none\";
      <!-- this.dataset.src = \"\"; -->
    })
    
    // Go through all of the images with our custom class
    for (var i = 0; i < images.length; i++) {
    var img = images[i];
    // and attach our click listener for this image.
    img.onclick = function(evt) {
    console.log(evt);
    modalImg.src = this.src;
    modalsp.style.display = \"block\";
    // https://stackoverflow.com/questions/15320052/what-are-all-the-differences-between-src-and-data-src-attributes
    modalImg.src = this.dataset.src;
    captionText.innerHTML = '<a class=\"a3\">' + this.title + '</a>';
    linkText.innerHTML = '<a class=\"a2\" href=\"' + this.alt + '\" target=\"_blank\">' + this.alt + '</a>' ;
    }
    }
    
    var span = document.getElementsByClassName(\"close\")[0];
    
    span.onclick = function() {
      modalsp.style.display = \"none\";
    }
    
    function myFunction() {
      var x = document.getElementById(\"About\");
      if (x.style.display == \"block\") {
        x.style.display = \"none\";
      } else {
        x.style.display = \"block\";
      }
    }
    
    </script>    
")




### Script ####################

cat("
<div id=\"idModal\" class=\"idmodal\">
  <span class=\"idclose\">&times;</span>
  <div id=\"idtips\"></div>
</div>     
")


cat("
<script>

    var idmodal = document.getElementById('idModal');
    var idspan = document.getElementsByClassName(\"idclose\")[0];
    var idText = document.getElementById(\"idtips\");
    var buttons = document.getElementsByClassName('idbutton');
    
    for (var i = 0; i < buttons.length; i++) {
      var butt = buttons[i];
      // and attach our click listener for this image.
      buttssssssssss.onclick = function(evt) {
        console.log(evt);
        idmodal.style.display = \"block\";
        idText.innerHTML = this.dataset.id;
      }
    }
    
    idspan.onclick = function() {
      idmodal.style.display = \"none\";
    }
    
    idmodal.addEventListener('click',function(){
      this.style.display=\"none\";
    })
    
    function idFunction() {

    }
    
    </script>    
")




### Script ####################

cat("
<script>

    var buttons = document.getElementsByClassName('idbutton');
    var divs = document.getElementsByClassName('ID');
    
    for (var i = 0; i < buttons.length; i++) {
      var butt = buttons[i];
      var div = divs[i];
      // and attach our click listener for this image.
      buttsssssssss.onclick = function(evt) {
        console.log(evt);
        if (div.style.display == \"block\") {
          div.style.display = \"none\";
        } else {
          div.style.display = \"block\";
        }
      }
    }
    
    function showID(sp) {
      var x = document.getElementById(sp);
      if (x.style.display == \"block\") {
        x.style.display = \"none\";
      } else {
        x.style.display = \"block\";
      }
    }
    
</script>    

")


### test for map the following

cat("
<div id=\"mapModal\" class=\"mapmodal\">
  <span class=\"close\">&times;</span>
  <img class=\"mapmodal-content\" id=\"map01\">
</div>     
")

cat("
<script>

    var modalcarte = document.getElementById('mapModal');
    var maps = document.getElementsByClassName('imgmap');
    var modalMap = document.getElementById(\"map01\");
    
    modalcarte.addEventListener('click',function(){
    this.style.display=\"none\";
    })
    
    for (var i = 0; i < maps.length; i++) {
    var map = maps[i];
    // and attach our click listener for this image.
    map.onclick = function(evt) {
    console.log(evt);
    modalMap.src = '';
    modalMap.src = this.dataset.src;
    modalcarte.style.display = \"block\";
    // https://stackoverflow.com/questions/15320052/what-are-all-the-differences-between-src-and-data-src-attributes
    <!-- modalMap.src = this.dataset.src; -->
    }
    }
    
    var span = document.getElementsByClassName(\"close\")[0];
    
    span.onclick = function() {
      modalcarte.style.display = \"none\";
    }
    
    function myFunction() {
      var x = document.getElementById(\"About\");
      if (x.style.display == \"block\") {
        x.style.display = \"none\";
      } else {
        x.style.display = \"block\";
      }
    }
    
    
    window.onscroll = function() {scrollFunction()};

    function scrollFunction() {
      if (document.body.scrollTop > 2 || document.documentElement.scrollTop > 2) {
        document.getElementById(\"testing1\").style.fontSize = \"4.25vmin\";
        document.getElementById(\"testing2\").style.height = \"4vmin\";
        document.getElementById(\"testing3\").style.fontSize = \"2vmin\";
        document.getElementById(\"testing4\").style.fontSize = \"3.75vmin\";
        document.getElementById(\"header\").style.paddingBottom = \"1vmin\";
      } else {
        document.getElementById(\"testing1\").style.fontSize = \"11vmin\";
        document.getElementById(\"testing2\").style.height = \"9vmin\";
        document.getElementById(\"testing3\").style.fontSize = \"4.25vmin\";
        document.getElementById(\"testing4\").style.fontSize = \"8vmin\";
        document.getElementById(\"header\").style.paddingBottom = \"0vmin\";
      }
    }
    
    
    
    
    </script>    
")








cat("
  </div>
</div>
 </body>
 </html>
")
sink()
close(con)

file.show("C:/Users/God/Documents/rungrass/index.html")




if(FALSE){
  
  data("hypsometric_tints_db")
  cols<-hypsometric_tints_db %>% filter(pal == "utah_1")#"wiki-schwarzwald-cont")
  cols<-hypsometric_tints_db %>% filter(pal == "utah_1")#"wiki-schwarzwald-cont")
  cols<-cols$hex[cols$limit>=0]
  cols<-c("gold","lightgoldenrod","darkgreen","forestgreen","seagreen","saddlebrown","sienna4","brown")
  cols<-c("grey90","lightgoldenrod","seagreen3","forestgreen","darkgreen","saddlebrown","sienna4","brown")
  hill<-shade(terrain(r,"slope",unit="radians"),terrain(r,"aspect",unit="radians"),40,270)
  plot(r,col=adjustcolor(colo.scale(1:200,),0.75),mar=c(0,0,0,0))
  
  png("C:/Users/God/Downloads/large_map.png",width=8,height=7,units="in",res=500)
  par(bg="grey15")
  plot(hill,col=grey(0:100/100), legend=FALSE, mar=c(0,0,0,0),maxcell=1000000)
  plot(r,col=adjustcolor(colo.scale(1:200,cols),0.30),add=TRUE,maxcell=1000000)
  plot(st_geometry(st_sample(run,10)),cex=1.25,lwd=1.5,pch=21,col="grey20",bg=adjustcolor("gold",0.75),add=TRUE)
  plot(st_geometry(st_sample(run,10)),cex=1.25,lwd=1.5,pch=21,col="grey20",bg=adjustcolor("forestgreen",0.75),add=TRUE)
  dev.off()
  file.show("C:/Users/God/Downloads/large_map.png") 
   
  library(basemaps)
  basemap_magick(st_bbox(run), map_service = "mapbox", map_type = "satellite")
  
  r<-basemap_raster(st_bbox(st_transform(run,3857)),map_res=1, map_service = "esri", map_type = "world_imagery")
  r<-rast(r)
  plotRGB(r,maxcell=5000000)
  plot(st_geometry(st_transform(gbif,st_crs(r))),cex=1.25,lwd=1.5,pch=21,col="grey20",bg=pcol$inat,add=TRUE)
  
  
  png(paste0(file.path("C:/Users/God/Downloads/images_large.png")),width=8,height=7,units="in",res=500)
  par(bg="grey15")
  plotRGB(r,maxcell=5000000)
  #plot(st_geometry(st_sample(run,10)),cex=1.25,lwd=1.5,pch=21,col="grey20",bg=pcol$gbif,add=TRUE)
  #plot(st_geometry(st_sample(run,10)),cex=1.25,lwd=1.5,pch=21,col="grey20",bg=pcol$inat,add=TRUE)
    plot(st_sample(st_geometry(st_transform(inat,st_crs(r))),20),cex=1.25,lwd=1.5,pch=21,col="grey20",bg=pcol$gbif,add=TRUE)


    plot(st_sample(st_geometry(st_transform(gbif,st_crs(r))),10),cex=1.25,lwd=1.5,pch=21,col="grey20",bg="orange",add=TRUE)

  dev.off()

  
}


if(FALSE){
  
inat$date<-as.Date(inat$date) 
inat$month<-gsub("\\.","",format(inat$date,"%b"))
inat$month<-factor(inat$month,levels=gsub("\\.","",format(seq.Date(as.Date("2008-01-01"),as.Date("2008-12-31"),by="month"),"%b")))
x<-aggregate(sp~month,data=inat[inat$sp=="Hyparrhenia rufa",],FUN=length,drop=FALSE)
  
   
  
}


