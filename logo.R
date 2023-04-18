
library(supercells)
library(terra)
library(sf)
library(rmapshaper)
library(FRutils)
library(webshot2)
library(magick)
library(smoothr)


#########################################
### Extract spike #######################
#########################################

lf<-list.files("C:/Users/God/Downloads/cencaf",full=TRUE)
lapply(lf,function(i){
  img<-image_read(i)# |> image_crop("200x1100+100+100",gravity="northwest")
  im<-image_quantize(img,2,colorspace="gray",dither=FALSE)
  plot(im,main=i)
})
i<-lf[16] # "C:/Users/God/Downloads/cencaf/20230401_181233.jpg"
img<-image_read(i)# |> image_crop("200x1100+100+100",gravity="northwest")
#img<-image_rotate(img,90)
im<-image_quantize(img,2,colorspace="gray",dither=FALSE)
im<-image_negate(im)
plot(im,main=i)
#image_write(im,"C:/Users/God/Downloads/spikes1.png") # edit spike here
# need to clear blothes here with paint to leave only the right spike
spikes1<-image_read("C:/Users/God/Documents/rungrass/spikes.png") # edit spike here


#####################################
### Assemble spikes #################
#####################################

#col<-image_read("C:/Users/God/Downloads/cencaf.jpg") |> image_raster()
#head(rev(sort(table(col[,3])))) #adjustcolor("#4d1d2bff",0.8)

col<-colo.scale(1:100,c("red4","purple4"))[30]
spike<-image_trim(image_read("C:/Users/God/Documents/rungrass/spikes.png"))
spike<-image_quantize(spike,2) |> image_split()
spike<-image_trim(spike[1])
spike<-image_crop(spike,"900x5000+900+0")
spike<-image_trim(spike)
spike<-image_fill(spike,col,"+445+1600",fuzz=2)
spike<-image_rotate(spike,-10)
plot(spike)

spikes<-lapply(c(-60,-35,0,35,60),function(i){
  if(i<0){
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
central<-image_border(spikes[3],"#ffffff00","5000x500")
w<-image_info(central)$width/2
tuft<-image_composite(central,spikes[2],operator="over",gravity="northwest",offset=paste0("+",w-2210,"+950"))
tuft<-image_composite(tuft,spikes[1],operator="over",gravity="northwest",offset=paste0("+",w-3260,"+1900"))
tuft<-image_composite(tuft,spikes[4],operator="over",gravity="northwest",offset=paste0("+",w+0,"+900"))
tuft<-image_composite(tuft,spikes[5],operator="over",gravity="northwest",offset=paste0("+",w+0,"+2000"))
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
r<-r[[2]]

run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
run<-st_buffer(st_geometry(run),50)
run<-ms_simplify(run,keep=0.01)
run<-smooth(run,method="ksmooth",smoothness=2)

xs<-st_coordinates(st_centroid(run))[,1]-600
ys<-st_coordinates(st_centroid(run))[,2]-400

run<-run-c(xs,ys)
centroid<-st_centroid(run)
run<-(run-centroid)*0.008+centroid
run<-run+c(10,40)


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

b2<-st_buffer(run,dist=13,nQuadSegs=30) #335
b1<-st_buffer(run,dist=5,nQuadSegs=30)
b<-st_difference(b2,b1)
plot(run)
plot(b,col="green",add=TRUE)


r<-crop(r,vect(run))
r<-mask(r,vect(run))
volcano<-as.polygons(r) |> st_as_sf()
volcano<-volcano[volcano$green==0,]
volcano<-st_cast(volcano,"POLYGON")
volcano<-volcano[which.max(st_area(volcano)),]
rest<-st_difference(run,volcano)
plot(r)
plot(run,add=TRUE)
plot(volcano,add=TRUE)


cols<-c("#43cd80","#fff8dc")
#cols<-c("#43cd80","#fcfc16ff")
png("C:/Users/God/Documents/rungrass/rungrasslogo.png",width=5,height=5,units="in",res=300)
par(bg="#111111")
plot(st_geometry(b),col=cols[1],border=NA,axes=FALSE)
plot(st_geometry(run),col=cols[1],border=NA,axes=FALSE,add=TRUE)
plot(rest,col=cols[2],add=TRUE,border=FALSE)
plot(volcano,col=cols[1],add=TRUE,border=FALSE)
dev.off()
logo<-image_read("C:/Users/God/Documents/rungrass/rungrasslogo.png") |> image_trim()
grass<-image_fill(tuft,"#ffffff00","+1+1") |> image_trim()
grass<-image_fill(grass,"#ffffff00","+1100+1")
grass<-image_scale(grass,"1200")
grass<-image_trim(grass)
cov<-image_quantize(logo,3) |> image_split()
#logo2<-image_composite(logo,grass,gravity="northeast",offset="+0+0",operator="atop")
logo<-image_composite(logo,image_crop(grass,"2000x2000+125+60",gravity="northwest"),gravity="northwest",offset="+0+0",operator="over")
#plot(logo)
logo<-image_composite(logo,cov[1])
logo<-image_composite(logo,cov[2])
logo<-image_scale(logo,"900") #900
h<-image_info(logo)$height
w<-image_info(logo)$width
fuzz<-20
off<-5
logo<-image_fill(logo,"#ffffff00",paste0("+",off,"+",off),fuzz=fuzz)
logo<-image_fill(logo,"#ffffff00",paste0("+",w-off,"+",h-off),fuzz=fuzz)
logo<-image_fill(logo,"#ffffff00",paste0("+",off,"+",h-off),fuzz=fuzz)
logo<-image_fill(logo,"#ffffff00",paste0("+",w-off,"+",off),fuzz=fuzz)
image_write(logo,"C:/Users/God/Documents/rungrass/rungrasslogo.png")
image_write(image_quantize(image_scale(logo,"500"),100),"C:/Users/God//Documents/rungrass/rungrasslogo500.png")
file.show("C:/Users/God/Documents/rungrass/rungrasslogo.png")

#logo<-image_read("C:/Users/God/Documents/rungrass/rungrasslogo.png")
#logo<-image_motion_blur(logo,radius=5)
#logo<-image_noise(logo)
#logo<-image_emboss(logo)
#plot(logo)


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
  <p><span style=\"color: #fff8dc\">RUN</span>GRASS</p>
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
logo<-image_read("C:/Users/God/Documents/rungrass/rungrasslogo.png")
logo<-image_scale(logo,paste0("x",image_info(logotext)$height))
logo<-image_background(logo,"#111111")
logo<-image_border(logo,"#111111","20")
logotext<-image_append(c(logo,logotext)) |> image_trim()
logotext<-image_border(logotext,"#111111","10x10")
image_write(logotext,"C:/Users/God/Documents/rungrass/rungrasslogotext.png",depth=16)
logotext<-image_read("C:/Users/God/Documents/rungrass/rungrasslogotext.png")
logotext<-image_fill(logotext,"#11111100","+1+1",fuzz=10)
logotext<-image_fill(logotext,"#11111100","+400+100",fuzz=10)
logotext<-image_fill(logotext,"#11111100","+1300+120",fuzz=10)
logotext<-image_fill(logotext,"#11111100","+1550+120",fuzz=10)
#logotext<-image_annotate(logotext,"#",location="+1300+100")
#logotext<-image_annotate(logotext,"#",location="+1550+100")
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

#lf<-list.files("C:/Users/God/Downloads",pattern="large",full=TRUE)

#im<-image_read(lf[1])
#im<-image_rotate(im,90)
#im1<-image_scale(im,"500")
##im<-image_quantize(im,2,colorspace="gray",dither=FALSE)
##im<-image_threshold(im,type="black",th="50%")
#im<-image_lat(im)
#im<-image_scale(im,"500")
#par(mfrow=c(1,2))
#plot(im1)
#plot(im)

