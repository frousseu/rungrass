
library(supercells)
library(terra)
library(sf)
library(rmapshaper)
library(FRutils)
library(webshot2)
library(magick)

#########################################
### Extyract spikes #####################
#########################################

##img<-image_read("https://inaturalist-open-data.s3.amazonaws.com/photos/178794881/original.jpeg")
##img<-image_crop(im,"x1200+0+750",gravity="south")
##img<-image_read("https://inaturalist-open-data.s3.amazonaws.com/photos/48714761/original.jpg")



#img<-image_read("https://inaturalist-open-data.s3.amazonaws.com/photos/256906882/original.jpg")
#im<-image_quantize(img,4,dither=FALSE)
#plot(im)
#ims<-image_split(im)
#plot(ims[2])
#par(mfrow=n2mfrow(length(ims),asp=1.55),mar=c(1,1,1,1))
#lapply(1:length(ims),function(i){
#  plot(ims[i])
#})
#im<-image_crop(ims[1],"200x950",gravity="northwest")
#im<-image_rotate(im,23)
#im<-image_chop(im,"180x450")
#table(image_raster(im)[,3])
#plot(im)
#image_write(im,"C:/Users/God/Downloads/newlogo1.png") # edit spike here



img<-image_read("https://inaturalist-open-data.s3.amazonaws.com/photos/256909900/original.jpg") |> image_crop("200x1100+100+100",gravity="northwest")
im<-image_quantize(img,3,dither=FALSE)
plot(im)
ims<-image_split(im)
plot(ims[2])
par(mfrow=n2mfrow(length(ims),asp=1.55),mar=c(1,1,1,1))
lapply(1:length(ims),function(i){
  plot(ims[i])
})
#im<-image_crop(ims[1],"200x950",gravity="northwest")
im<-image_rotate(ims[2],8)
#im<-image_chop(im,"80x450")
table(image_raster(im)[,2])
plot(im)
#image_write(im,"C:/Users/God/Downloads/newlogo2.png") # edit spike here


#####################################
### Assemble spikes #################
#####################################

#col<-image_read("C:/Users/God/Downloads/cencaf.jpg") |> image_raster()
#head(rev(sort(table(col[,3])))) #adjustcolor("#4d1d2bff",0.8)

col<-colo.scale(1:100,c("red4","purple4"))[20]
#col<-"gold"
spike<-image_trim(image_read("C:/Users/God/Downloads/newlogo2.png"))
spike<-image_quantize(spike,2) |> image_split()
spike<-image_trim(spike[1])
spike<-image_fill(spike,col,"+36+250",fuzz=2)
spike<-image_crop(spike,"100x800")
plot(spike)
spikes<-lapply(c(-60,-35,0,35,60),function(i){
  if(i>0){
    res<-image_flop(spike)
  }else{
    res<-spike
  }
  res<-image_rotate(res,i)
  res<-image_trim(res)
  h<-image_info(res)$height
  w<-image_info(res)$width
  left<-paste0("+",1,"+",floor(h/2))
  right<-paste0("+",w-1,"+",floor(h/2))
  res<-image_fill(res,"#ffffff00",left,fuzz=10)
  res<-image_fill(res,"#ffffff00",right,fuzz=10)
  #res<-image_flip(res)
  res
})
spikes<-do.call("c",spikes)
#tuft<-image_append(spikes) |> image_flip()
#spikes<-image_coalesce(spikes)
central<-image_border(spikes[3],"#ffffff00","1000x200")
#central<-image_border(central,"blue","2x2")
w<-image_info(central)$width/2
tuft<-image_composite(central,spikes[2],operator="over",gravity="northwest",offset=paste0("+",w-510,"+350"))
tuft<-image_composite(tuft,spikes[1],operator="over",gravity="northwest",offset=paste0("+",w-760,"+625"))
tuft<-image_composite(tuft,spikes[4],operator="over",gravity="northwest",offset=paste0("+",w-20,"+350"))
tuft<-image_composite(tuft,spikes[5],operator="over",gravity="northwest",offset=paste0("+",w-75,"+700"))
tuft<-image_fill(tuft,"white","+1+1",fuzz=20)
#tuft<-image_scale(tuft,"400")
plot(tuft)





##############################################
### Create logo ##############################
##############################################


flag<-image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/8/8e/Proposed_flag_of_R%C3%A9union_%28VAR%29.svg/1200px-Proposed_flag_of_R%C3%A9union_%28VAR%29.svg.png")
flag<-image_quantize(flag,3,dither=FALSE)
#ims<-image_split(flag)
ras<-image_raster(flag)
table(ras[,3])
cols<-t(col2rgb(ras[,3]))
r<-rast(cbind(ras[,1:2],cols))
r<-flip(r)
#RGB(r)<-1:3
r<-r[[2]]

run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
run<-st_buffer(st_geometry(run),50)
run<-ms_simplify(run,keep=0.01)

xs<-st_coordinates(st_centroid(run))[,1]-600
ys<-st_coordinates(st_centroid(run))[,2]-400

run<-run-c(xs,ys)
centroid<-st_centroid(run)
run<-(run-centroid)*0.008+centroid

# https://stackoverflow.com/questions/73387889/how-to-automate-plotting-3-different-regular-polygons-recursively
dodecagon <- function(x = 600, y = 400, r = 300) {
  theta <- seq(pi/12, 24 * pi/12, pi/6)
  x<-data.frame(x = x + r * cos(theta), y = y + r * sin(theta))
  st_as_sf(x,coords=c("x","y"),crs=st_crs(run)) |>
    st_union() |>
    st_convex_hull()
}
dodecagon()

#b2<-dodecagon(r=335)+c(+5,+1)
#b1<-dodecagon(r=300)+c(+5,+1)
#b<-st_difference(b2,b1)

b2<-st_buffer(st_centroid(run)+c(+3,+1),360,nQuadSegs=30) #335
b1<-st_buffer(st_centroid(run)+c(+3,+1),300,nQuadSegs=30)
b<-st_difference(b2,b1)

r<-crop(r,vect(run))
r<-mask(r,vect(run))
volcano<-as.polygons(r) |> st_as_sf()
volcano<-volcano[volcano$green==0,]
volcano<-st_cast(volcano,"POLYGON")
volcano<-volcano[which.max(st_area(volcano)),]
rest<-st_difference(run,volcano)



cols<-c("#43cd80","#fff8dc")
#cols<-c("#43cd80","#fcfc16ff")
png("C:/Users/God/Downloads/rungrasslogo.png",width=5,height=5,units="in",res=300)
par(bg="#111111")
plot(st_geometry(b),col=cols[1],border=NA,axes=FALSE)
#plot(r,mar=c(0,0,0,0),col=cols,legend=FALSE,axes=FALSE,add=TRUE)
plot(rest,col=cols[2],add=TRUE,border=FALSE)
plot(volcano,col=cols[1],add=TRUE,border=FALSE)
dev.off()
logo<-image_read("C:/Users/God/Downloads/rungrasslogo.png") |> image_trim()
grass<-image_fill(tuft,"#ffffff00","+1+1") |> image_trim()
grass<-image_fill(grass,"#ffffff00","+1100+1")
grass<-image_scale(grass,"600")
cov<-image_quantize(logo,3) |> image_split()
logo<-image_composite(logo,grass,offset="+116+125")
logo<-image_composite(logo,cov[1])
logo<-image_composite(logo,cov[2])
logo<-image_scale(logo,"900")
h<-image_info(logo)$height
w<-image_info(logo)$width
logo<-image_fill(logo,"#ffffff00","+1+1")
logo<-image_fill(logo,"#ffffff00",paste0("+",h-1,"+",w-1))
logo<-image_fill(logo,"#ffffff00",paste0("+",1,"+",w-1))
logo<-image_fill(logo,"#ffffff00",paste0("+",h-1,"+",1))
image_write(logo,"C:/Users/God/Downloads/rungrasslogo.png")
image_write(image_quantize(image_scale(logo,"500"),100),"C:/Users/God/Downloads/rungrasslogo500.png")
file.show("C:/Users/God/Downloads/rungrasslogo.png")

logo<-image_read("C:/Users/God/Downloads/rungrasslogo.png")
#logo<-image_motion_blur(logo,radius=5)
logo<-image_noise(logo)
#logo<-image_emboss(logo)
plot(logo)


#############################################################
### logo with text ##########################################
#############################################################

textlogo<-function(){
cat("
<!DOCTYPE html>
  <html>
  <head>
  <!-- <link rel='preload' href='https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@100&display=swap' rel='stylesheet'> -->
    <link href='https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@200' rel='stylesheet'>
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">

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

p {
  padding: 0px;
  margin: 0vh;
  font-family: 'Roboto Mono';
  font-weight: 600;
  font-size: 25vh;
  color: var(--green);
}

</style>
</head>
<body>

")

cat("
  <p>RUNGRASS</p>
  </body>
  </html>
")
}

con <- file("C:/Users/God/Documents/rungrass/rungrasslogo.html", open = "wt", encoding = "UTF-8")
sink(con)
textlogo()
close(con)
#file.show("C:/Users/God/Documents/rungrass/rungrasslogo.html")

webshot("C:/Users/God/Documents/rungrass/rungrasslogo.html","C:/Users/God/Documents/rungrass/rungrasslogotext.png",zoom=2)

logotext<-image_read("C:/Users/God/Documents/rungrass/rungrasslogotext.png") |> image_trim()
#logotext<-image_border(logotext,"#111111","500")
logo<-image_read("C:/Users/God/Downloads/rungrasslogo.png")
logo<-image_scale(logo,paste0(image_info(logotext)$height))
logo<-image_background(logo,"#111111")
logo<-image_border(logo,"#111111","20")
logotext<-image_append(c(logotext,logo)) |> image_trim()
image_write(logotext,"C:/Users/God/Documents/rungrass/rungrasslogotext.png",depth=16)
file.show("C:/Users/God/Documents/rungrass/rungrasslogotext.png")
#logotrans<-image_background(logotext,"none")
#plot(logotrans)

#plot(image_distort(spikes[3],"arc",coordinates=seq(1,80,length.out=12)))
#plot(image_shear(spikes[3],"500x50"))

#cols<-image_raster(ims[1])[,3]
#cols<-t(col2rgb(cols))
#pic<-image_raster(ims[1])
#r<-rast(cbind(pic[,1:2],cols))
#RGB(r)<-1:3
#r<-flip(r)
#x<-supercells(r,k=2000,compactness=0.0000001,clean=FALSE)
#plot(r)
#plot(st_geometry(x),add=TRUE)




lf<-list.files("C:/Users/God/Downloads",pattern="large",full=TRUE)

im<-image_read(lf[1])
im<-image_rotate(im,90)
im1<-image_scale(im,"500")
#im<-image_quantize(im,2,colorspace="gray",dither=FALSE)
#im<-image_threshold(im,type="black",th="50%")
im<-image_lat(im)
im<-image_scale(im,"500")
par(mfrow=c(1,2))
plot(im1)
plot(im)

