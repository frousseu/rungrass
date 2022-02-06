

library(jsonlite)
library(readxl)
library(taxize)
library(rgbif)
library(foreach)
library(doParallel)
library(sf)
library(jsonlite)

#x<-fromJSON("https://api.inaturalist.org/v1/observations/90513306")
#x$results$observation_photos[[1]]$photo$attribution

### Data ########################################

d<-as.data.frame(read_excel("C:/Users/God/Documents/rungrass/grasses.xlsx"))
#dcsv<-read.csv("https://raw.githubusercontent.com/frousseu/reunion_graminoids/main/grasses.csv",sep=";")
dcsv<-read.csv("C:/Users/God/Documents/rungrass/grasses.csv",sep=";",na.strings=c("NA",""))

d$photo<-gsub("/medium.|/small.|/large.","/original.",d$photo)

d<-merge(d,dcsv[,c("sp","photo","attribution","powo","gbif")],all.x=TRUE) # only get attributions
d<-d[order(d$sp,d$rank),]

d<-unique(d)

#write.table(d[,1:9],"C:/Users/God/Documents/reunion_graminoids/grasses.csv",row.names=FALSE,sep=";",na="")

d$idphoto<-sapply(strsplit(sapply(strsplit(d$photo,"/original."),function(i){if(length(i)==1){NA}else{i[1]}}),"/"),tail,1)
d$idobs<-ifelse(!is.na(d$idphoto),sapply(strsplit(d$obs,"/"),tail,1),NA)
  
### iNat credits ############################
#w<-1:nrow(d) # get them all to verify if any attributions have changed
w<-which(!is.na(d$idphoto) & is.na(d$attribution))
for(i in w){ # looping is better cause sometimes it times-out
  if(is.na(d$idobs[i])){
    a<-d$credit[i]
  }else{  
    x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations/",d$idobs[i]))
    m<-match(d$idphoto[i],x$results$observation_photos[[1]]$photo$id)
    a<-x$results$observation_photos[[1]]$photo$attribution[m]
  }
  d$attribution[i]<-a
  Sys.sleep(0.1) # not to make too many requests, but not sure it is relevant
  cat("\r",paste(match(i,w),length(w),sep=" / "))
}

d$attribution[which(is.na(d$attribution))]<-d$credit[which(is.na(d$attribution))]

### POWO links #################################
k<-d$family!="Excluded" & is.na(d$powo)
sp<-unique(d$sp[k])
if(length(sp)){
  powo<-get_pow(sp,ask=FALSE,accepted=TRUE,rank_filter="species")
  powourl<-data.frame(sp=sp,powo=attributes(powo)$uri)
  d$powo[k]<-powourl$powo[match(d$sp[k],powourl$sp)]
}
#sp<-unique(d$sp[d$family!="Excluded" & is.na(d$powo)]) # manually replace link
#d$powo[d$sp=="Poa borbonica"]<-"https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:416623-1"

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


### OCCS #######################

if(FALSE){
  
#### GBIF occs ##################
  
# maybe use all suggested species names (e.g. E. tenella not fully covered)
# remove iNat with datasetName

  run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
  run<-st_buffer(st_geometry(run),50)
  k<-d$family!="Excluded"
  sp<-unique(d$sp[k])
  key<-sapply(strsplit(d$gbif[match(sp,d$sp)],"/"),tail,1)
  other<-d$other[match(sp,d$sp)]
  #i<-which(sp=="Aristida setacea")
  occs<-foreach(i=seq_along(sp),.packages=c("rgbif")) %do% {
    if(!is.na(other[i])){
      sps<-c(sp[i],strsplit(other[i],", ")[[1]])
      keys<-sapply(sps,function(j){
        as.data.frame(name_backbone(name=j, rank='species', kingdom='plants'))$usageKey[1]
      })
    }else{
      sps<-sp[i]
      keys<-key[i]
    }
    l<-lapply(seq_along(sps),function(k){
      spoccs<-as.data.frame(occ_search(taxonKey=keys[k],limit=2000,hasCoordinate=TRUE,country="RE")$data)
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
  gbif<-gbif[!sapply(gbif,is.null)]
  gbif<-lapply(gbif,function(i){
    if(!any(names(i)=="datasetName")){
      i$datasetName<-NA
      i
    }else{
      i
    }
  })
  gbif<-lapply(gbif,function(i){i[,c("sp","decimalLongitude","decimalLatitude","datasetName")]})
  gbif<-do.call("rbind",gbif)
  gbif<-gbif[-grep("iNaturalist",gbif$datasetName),]
  gbif<-st_as_sf(gbif,coords=c("decimalLongitude","decimalLatitude"),crs=4326)
  gbif<-st_transform(gbif,st_crs(run))
  plot(st_geometry(run))
  plot(st_geometry(gbif),add=TRUE,pch=1)

### iNat occs ##############################
  
# include reviewed_by me 
# https://api.inaturalist.org/v1/docs/#!/Observations/get_observations

  api<-"https://api.inaturalist.org/v1/observations?geo=true&verifiable=true&place_id=8834&taxon_id=47434%2C47161%2C52642&hrank=species&lrank=subspecies&order=desc&order_by=created_at&page=1"
  x<-fromJSON(api)
  pages<-ceiling(x$total_results/30)

  inatjson<-foreach(i=1:pages,.packages=c("jsonlite")) %do% {
    page<-paste0("page=",i)
    x<-fromJSON(gsub("page=1",page,api))
    inat<-data.frame(
      sp=x$results$taxon$name,
      user=x$results$user$login,
      location=x$results$location,
      grade=x$results$quality_grade
    )
    row.names(inat)<-((i-1)*30+1):(((i-1)*30+1)+nrow(inat)-1)
    cat("\r",paste(i,pages,sep=" / "))
    inat
  }  
  inat<-do.call("rbind",inatjson)
  inat$lon<-as.numeric(sapply(strsplit(inat$location,","),"[",2))
  inat$lat<-as.numeric(sapply(strsplit(inat$location,","),"[",1))
  inat<-st_as_sf(inat,coords=c("lon","lat"),crs=4326)
  inat<-st_transform(inat,st_crs(run))
  #plot(st_geometry(run))
  #plot(st_geometry(inat),add=TRUE,pch=1)
  inat<-inat[inat$grade=="research" | inat$user=="frousseu",]
  
  occs<-rbind(gbif[,"sp"],inat[,"sp"])
  #plot(st_geometry(run))
  #plot(st_geometry(occs),add=TRUE,pch=1)
  
  
### Maps ####################
  
  run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
  run<-st_buffer(st_geometry(run),50)
  mult<-abs(diff(st_bbox(run)[c(1,3)])/diff(st_bbox(run)[c(2,4)]))
  k<-d$family!="Excluded"
  sp<-unique(d$sp[k])
  foreach(i=seq_along(sp),.packages=c("rgbif")) %do% {
    x<-occs[which(occs$sp==sp[i]),]
    png(paste0(file.path("C:/Users/God/Downloads",gsub(" ","_",sp[i])),".png"),height=40,width=40*mult,units="px")
    par(mar=c(0,0,0,0),oma=c(0,0,0,0),bg="#111111")
    plot(st_geometry(run),col=alpha("#FFF8DC",0.95),border=NA)
    if(nrow(x)>0){
      plot(st_geometry(x),pch=16,col=alpha("#5CBE35",0.6),cex=1.2,add=TRUE)
    }
    dev.off()
    cat("\r",paste(i,length(sp),sep=" / "))
  }
  
}

### Write data csv ##################

write.table(d,"C:/Users/God/Documents/rungrass/grasses.csv",row.names=FALSE,sep=";",na="")

### Data for website #################

d$cbnm<-paste0("https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_taxref=",d$taxref))
d$borbonica<-paste0("http://atlas.borbonica.re/espece/",d$taxref)
d$flore<-ifelse(is.na(d$flore),"-",d$flore)
d$index<-ifelse(is.na(d$index),"-",d$index)
d$genus<-sapply(strsplit(d$sp," "),"[",1)


#d<-d[order(as.integer(is.na(d$photo)),-as.integer(factor(d$family)),d$sp,d$rank),]

d<-d[order(factor(d$family,levels=c("Poaceae","Cyperaceae","Juncaceae","Excluded")),d$sp,d$rank),]

## Functions #################

genus<-function(i){
  #ge<-sort(unique(d$genus[d$family!="Excluded"]))  
  l<-split(d[d$family!="Excluded",c("genus","sp")],d$genus[d$family!="Excluded"])
  l<-lapply(l,function(x){
    x<-unique(x)
    ge<-x$genus[1]
    sp<-sapply(strsplit(x$sp," "),"[",2)
    ge<-c(ge,x$sp)
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
  <link href='https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@100&display=swap' rel='stylesheet'>
  <title>
      RUNGRASS Poacées, cypéracées et juncacées de la Réunion
  </title>
  <link rel='icon' type='image/png' href='rungrass3.png?v=2'/>
  <link rel='shortcut icon' type='image/png' href='rungrass3.png?v=2'/>
  <meta name=\"keywords\" content=\"Réunion, poacées, grass, grasses, poaceae, sedge, sedges, cypéracées, cyperaceae, cyperus, rush, rushes, juncacées, juncaceae, botanique, flore, herbes, herb, herbe, graminées, carex\">
  <style>

:root {
  --green: #5cbe35; /* #7AB914; */
  --white: #fff8dc; /* #F2F3F4; */
  --black: #111111;
  --gray: #fff8dc77;
}
* {
  background-color: var(--black);
}
body .main-container {
  max-width: 1950px !important;
  width: 1950px !important;
}
body {
  max-width: 1950px !important;
}
* {
  box-sizing: border-box;
}
p {
  padding: 0px;
  margin: 4px;
  font-family: 'Roboto Mono';
  font-weight: 200;
  color: var(--white);
}
h1 {
  color: var(--green);
  font-size:90px;
  padding-left:10px;
  padding-top:0px;
  padding-bottom:0px;
  font-family:'Helvetica'; 
  font-weight: 100;
  margin-top: 0;
  margin-bottom: 0;
}
h2 {
  color:var(--white);
  font-size:40px;
  padding-left:10px;
  padding-top:0px;
  padding-bottom:0px;
  font-family:'Helvetica'; 
  font-weight: 50;
  margin-top: 0;
  margin-bottom: 30px;
}
a {
  text-decoration: none; /* no underline */
  color: var(--green);
  font-weight: 600;
}
.a2 {
  text-decoration: none; /* no underline */
  color: var(--green);
}
.atoc {
  text-decoration: none; /* no underline */
  color: var(--green); /* #228B22; */
  font-weight: 600;
  font-size: 16px;
}
.atoc:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}
.flore {
  color: var(--gray);
  font-size:20px;
  font-family:'Helvetica' 
  font-weight: 100;
  /*font-style: italic;*/
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
  margin-top: 20px;
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
.img2 {
  height:200px;
  width:14.26%;
  object-fit:cover;
  padding: 1px; /* 2px */
  background: #000000; /* #EEEEEE */
  background-origin: content-box;
  cursor: pointer;
  border-radius: 50px;
}
.img2:hover {
  opacity: 0.70;
  filter: alpha(opacity=100);
}
.p2 {
  color: var(--white);
  font-size:30px;
  font-family:'Helvetica' 
  font-weight: 100;
}
.scroller {
  scrollbar-width: thin;
}
#img2 {
  border-radius: 5px;
  cursor: pointer;
  transition: 0.3s;
}

/* The Modal (background) */
.modal {
display: none; /* Hidden by default */
position: fixed; /* Stay in place */
z-index: 1; /* Sit on top */
padding-top: 20px; /* Location of the box */
left: 0;
top: 0;
width: 100%; /* Full width */
height: 100%; /* Full height */
overflow: auto; /* Enable scroll if needed */
background-color: rgb(0,0,0); /* Fallback color */
background-color: rgba(0,0,0,0.9); /* Black w/ opacity */
}

/* Modal Content (image) */
.modal-content {
margin: auto;
display: block;
width: 70%;
max-width: 70%;
}

/* Caption of Modal Image */
#caption {
margin: auto;
display: block;
width: 70%;
max-width: 70%;
text-align: center;
color: #ccc;
padding: 10px 0;
height: 25px;
font-family: 'Roboto Mono';
}

/* Link of Modal Image */
#link {
margin: auto;
display: block;
width: 70%;
max-width: 70%;
text-align: center;
color: #ccc;
padding: 10px 0;
margin-bottom: 50px;
height: 42px;
font-family: 'Roboto Mono';
}

div.sticky {
  position: -webkit-sticky;
  position: sticky;
  top: 0;
  padding: 0px;
    width: 100%;
  padding: 0px;
  background: var(--black);
  border-radius: 0px;
}

div.sticky2 {
  position: -webkit-sticky;
  position: sticky;
  top: 0;
  padding: 0px;
    width: 100%;
  padding: 0px;
}

/* Add Animation */
.modal-content, #caption, #link {  
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
.close {
position: absolute;
top: 15px;
right: 35px;
color: #f1f1f1;
font-size: 40px;
font-weight: bold;
transition: 0.3s;
}

.close:hover,
.close:focus {
color: #bbb;
text-decoration: none;
cursor: pointer;
}

/* 100% Image Width on Smaller Screens */
@media only screen and (max-width: 700px){
.modal-content {
width: 100%;
}
}
</style>
</head>

<body>
  
<h1>rungrass<img style=\"height; 53px; width: 53px;\" src=\"rungrass3.png\"></h1>
<h2>Guide photographique des poacées, cypéracées et juncacées de la Réunion</h2>
  
<p style = \"font-size:17px;\">Cette page est un guide photographique des poacées (graminées), cypéracées et juncacées de la Réunion. La liste des espèces présentées est basée sur la liste des espèces reconnues comme étant présentes à la Réunion selon <a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore\" target=\"_blank\">l'Index taxonomique de la flore vasculaire de La Réunion</a> du <a href=\"http://www.cbnm.org/\" target=\"_blank\">Conservatoire National Botanique Mascarin (CBN - CPIE Mascarin)</a>. Cliquez sur le nom d'une espèce pour accéder à sa fiche sur l'index. Plusieurs espèces n'ont pas été retenues, car leurs mentions résultent possiblement d'erreurs d'identification, d'étiquetages ou autres. La liste des espèces qui n'ont pas été retenues est présentée à la toute fin. </p><br>
  
<p style = \"font-size:17px;\">La plupart des photos proviennent d'observations déposées sur <a href=\"https://www.inaturalist.org/\" target=\"_blank\">iNaturalist</a> ou de spécimens d'herbiers déposés au <a href=\"https://science.mnhn.fr/institution/mnhn/item/search\" target=\"_blank\">Muséum National d'Histoire Naturelle</a>. La plupart des photos présentées sont toutes sous une license <a href=\"https://creativecommons.org/about/cclicenses/\" target=\"_blank\">Creative Commons (CC)</a> permettant leur utilisation à des fins non-commerciales, mais vérifiez la license et l'auteur de chaque photo en y passant votre curseur ou en cliquant sur la photo et en consultant l'adresse URL au bas de chaque agrandissement. </p><br>

<p style = \"font-size:17px;\">Pour plusieurs espèces, notamment pour quelques espèces endémiques, rares ou difficiles à identifier, seules des photos de spécimens d'herbier sont disponibles. Si vous possédez des photos pour ces espèces et si vous souhaitez contribuer à ce site, merci de déposer vos photos sous forme d'observations sur <a href=\"https://www.inaturalist.org/\" target=\"_blank\">iNaturalist</a> et de me contacter. Finalement, merci de me faire signe si vous trouvez des erreurs sur le site ou pour toutes questions, commentaires ou suggestions. L'identification pour la plupart des photos n'a pas été validée par des experts et je suis moi-même en apprentissage de ces espèces. Je n'ai encore jamais observé plusieurs espèces présentées sur ce site. Il convient donc de rester très prudent lors de l'utilisation des images présentées ici à des fins d'identification. Dans bien des cas, certaines espèces ne seront pas identifiables par comparaison à partir des photos présentées ici et il faudra se référer à des clés d'identification comme celle de la <a href=\"https://www.editions.ird.fr/produit/471/9782709924535/flore-des-mascareignes-la-reunion-maurice-rodrigues\" target=\"_blank\">Flore des Mascareignes</a> pour pouvoir identifier les spécimens. Pour me contacter: francoisrousseu at hotmail com</p><br>


<div style=\"display:inline-block; width:100%;\">
  <div class=\"sticky2\" style=\"width:14%; height: 100vh; float: left; display: inline-block; padding-right: 0.5%; overflow-y:scroll;\">
  <div class=\"sticky\">
  <p class=\"p2\">Espèces</p>
  </div>
  <p style = \"color:black;font-size:17px;\">",genus(),"
  </p>
  </div>
  <div style=\"width:85%; float: left; display: inline-block; padding-left: 0.5%;\">

<div class=\"species\" style=\"margin-top: 0px;\">
<p class=\"p2\">iNaturalist&nbsp;&nbsp<span class=\"flore\">Flore des Mascareignes&nbsp;&nbsp</span><span class=\"flore\">Index du CBN - CPIE Mascarin</span><span class=\"flore\" style=\"float:right;\">Famille</span>
</p>
</div>
"))

}  
  

species_links<-function(x,i){
  paste0(
    "<a target=\"_blank\" href=\"",x$cbnm[i],"\">
       <img style=\"height: 18px; padding: 0px;\" src=\"https://mascarine.cbnm.org/templates/favourite/favicon.ico\">
     </a>
     <a target=\"_blank\" href=\"",x$borbonica[i],"\">
       <img style=\"height: 21px; padding: 0px;\" src=\"http://atlas.borbonica.re/static/custom/images/favicon.ico\">
     </a>
     <a target=\"_blank\" href=\"",x$powo[i],"\">
       <img style=\"height: 17px; padding: 0px;\" src=\"https://powo.science.kew.org/img/powo-favicon.ico\">
     </a>
     <a target=\"_blank\" href=\"",x$gbif[i],"\">
       <img style=\"height: 18px; padding: 0px;\" src=\"https://images.ctfassets.net/uo17ejk9rkwj/5NcJCYj87sT16tJJlmEuWZ/85058d511b3906fbbb199be27b2d1367/GBIF-2015-mark.svg\"> 
     </a>
     </a>&nbsp;&nbsp;"
  )
}


species_header<-function(x,i){
  cat(paste0(
 "<div id=\"",x$sp[i],"\" class=\"species\">
    <p class=\"p2\"><span class=\"p2\">
      ",x$sp[i],"&nbsp<img style=\"height: 35px; padding: 0px;\" src=\"",paste0(gsub(" ","_",x$sp[i]),".png"),"\"></span>&nbsp;&nbsp<span class=\"flore\">",x$flore[i],"</span>","&nbsp;&nbsp<span class=\"flore\">",x$index[i],"</span>","<span class=\"flore\" style=\"float: right; margin-top: 17px;\">",species_links(x,i),x$family[i],"</span>
    </p>
   </div>  
 "))
}

species_photo<-function(x,i){
  if(!is.na(x$large[i])){
    large<-x$large[i]
  }else{
    large<-x$photo[i]
  }
  cat(paste0(
    "<img class=\"img2\" src=\"",gsub("/original.","/small.",x$photo[i]),"\" data-src=\"",large,"\" title=\"",paste(x$attribution[i]),"\" alt=\"",paste(x$obs[i]),"\">"
  ))
}

species_excluded<-function(i,j){
  #paste(sapply(seq_along(i),function(i){
  excluded<-paste0(paste0("<a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_taxref=",j),"\" target=\"_blank\">",i,"</a>"),collapse=", ")
  cat(paste("<br><br><p style = \"font-size:17px;\">Liste des espèces qui n'ont pas été retenues et lien vers l'index du CBN - CPIE Mascarin:"),excluded,"</p><br><br>")
}


### Species list #################

d2<-d[d$family!="Excluded",]
l<-split(d2,factor(d2$sp,levels=unique(d2$sp)))


### HTML #########################

con <- file("C:/Users/God/Downloads/index.html", open = "wt", encoding = "UTF-8")
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

cat("
<div id=\"myModal\" class=\"modal\">
  <span class=\"close\">&times;</span>
  <img class=\"modal-content\" id=\"img01\">
  <div id=\"caption\"></div>
  <div id=\"link\"></div>
</div>     
")

### Script ####################

cat("
<script>

    // create references to the modal...
    var modal = document.getElementById('myModal');
    // to all images -- note I'm using a class!
    var images = document.getElementsByClassName('img2');
    // the image in the modal
    var modalImg = document.getElementById(\"img01\");
    // and the caption in the modal
    var captionText = document.getElementById(\"caption\");
    // and the link in the modal
    var linkText = document.getElementById(\"link\");
    
    modal.addEventListener('click',function(){
    this.style.display=\"none\";
    })
    
    // Go through all of the images with our custom class
    for (var i = 0; i < images.length; i++) {
    var img = images[i];
    // and attach our click listener for this image.
    img.onclick = function(evt) {
    console.log(evt);
    modal.style.display = \"block\";
    // https://stackoverflow.com/questions/15320052/what-are-all-the-differences-between-src-and-data-src-attributes
    modalImg.src = this.dataset.src;
    captionText.innerHTML = this.title;
    linkText.innerHTML = '<a class=\"a2\" href=\"' + this.alt + '\" target=\"_blank\">' + this.alt + '</a>' ;
    }
    }
    
    var span = document.getElementsByClassName(\"close\")[0];
    
    span.onclick = function() {
    modal.style.display = \"none\";
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

file.show("C:/Users/God/Downloads/index.html")

