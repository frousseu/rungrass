library(readxl)

d<-as.data.frame(read_excel("C:/Users/God/Downloads/ITR_2020-1_Diffusion_Externe/ITR_2020-1_Diffusion_Externe/ITR-2020-1-MAJ-20201204_Diff-Externe.xlsx",skip=5))#range="A6:K1116"))
d<-d[,1:match("FAMILLE",names(d))]

rev(sort(table(d$FAMILLE)))

d<-d[d$FAMILLE%in%c("Poaceae","Cyperaceae","Juncaceae"),]

d<-d[is.na(d$"NOM ACCEPTÉ (ou de rattachement)"),]
d<-d[d$RANG>20,]
d<-d[order(factor(d$FAMILLE,levels=c("Poaceae","Cyperaceae","Juncaceae","Excluded")),d$"NOM BOTANIQUE"),]
d$sp<-sapply(strsplit(d$"NOM BOTANIQUE"," "),function(i){paste(i[1:2],collapse=" ")})

d$sp[duplicated(d$sp)]

d<-d[!duplicated(d$sp),]

x<-as.data.frame(read_excel("C:/Users/God/Documents/reunion_graminoids/grasses.xlsx"))
m<-apply(cbind(match(x$sp,d$sp),match(x$flore,d$sp)),1,function(i){sort(i)[1]})
x$sp[is.na(m)]
write(paste(d$"CODE TAXREF"[m],collapse="\n"),"C:/Users/God/Downloads/crap.txt")

### esp?ce non accept?es
rejected<-sort(d$sp[!d$sp%in%c(x$sp,x$flore)])
write(paste(rejected,collapse="\n"),"C:/Users/God/Downloads/crap.txt")

taxref<-d$"CODE TAXREF"[sapply(rejected,function(i){grep(i,d$"NOM BOTANIQUE")})]
write(paste(taxref,collapse="\n"),"C:/Users/God/Downloads/crap.txt")

excluded<-paste(sapply(seq_along(rejected),function(i){
  paste0("<a href=\"https://mascarine.cbnm.org/index.php/flore/index-de-la-flore/nom?",paste0("code_taxref=",taxref[i]),"\" target=\"_blank\">",rejected[i],"</a>")
}),collapse=", ")
cat(excluded)


###########################
### Get status

x<-as.data.frame(read_excel("C:/Users/God/Downloads/ITR_2020-1_Diffusion_Externe/ITR_2020-1_Diffusion_Externe/ITR-2020-1-MAJ-20201204_Diff-Externe.xlsx",skip=5))#range="A6:K1116"))
x<-x[,c("CODE TAXREF","ENDÉMICITÉ","STATUT GÉNÉRAL RÉUNION","STATUT SPONTANÉ RÉUNION")]
d<-as.data.frame(read_excel("C:/Users/God/Documents/rungrass/grasses.xlsx"))

m<-match(d$taxref,x$`CODE TAXREF`)
cbind(d,x[m)

d<-merge(d,x,by.x="taxref",by.y="CODE TAXREF",all.x=TRUE)




