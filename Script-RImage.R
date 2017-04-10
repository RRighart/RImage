# IMAGE PROCESSING OF HANDWRITTEN DIGITS
  
# ANALYSES AND R PROGRAMMING CODE

# Loading handwritten digits from GitHub.

myurl <- "https://raw.githubusercontent.com/RRighart/RImage/master/arraydat.Rds" 
z <- tempfile()
download.file(myurl,z,mode="wb")
dat <- readRDS(z)
file.remove(z)
class(dat)
dim(dat)

# R code examples for three dimensional array

v1<-c(0,1,2)
m1<-matrix(c(0:8), nrow=3)
a1<-array(c(0:17), dim=c(3,3,2))
class(v1)
class(m1)
class(a1)
length(v1)
dim(m1)
dim(a1)
a1[,,1]
a1[,,2]

# Visualize a digit from the image array

library(gplots)
library(RColorBrewer)
dim(dat[,,10])
ht<-dat[,,10]
htval <- formatC(ht, format="f", digits=2)
my_palette <- colorRampPalette(c("black", "lightgray"))(n = 50)

par(mar=c(7,4,4,2)+0.1)
heatmap.2(ht, Rowv=FALSE, Colv=FALSE, dendrogram="none", main=NULL, key=FALSE, xlab=NULL, cexRow=0.5, cexCol=0.5, ylab=NULL, col=my_palette, cellnote=htval, trace="none", notecol="red", lhei=c(1,9), lwid=c(0.5,5), notecex=0.5, colsep=1:ncol(ht), rowsep=1:nrow(ht), sepcolor="black", keysize = 1.5, margins=c(6, 6))


# Plot a serie of unprocessed images

par(mfrow=c(2,5),
        oma = c(3,3,3,3),
        mar = c(0,0.1,0,0.1))
for(i in 12:21){
image(t(apply(dat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1) 
}

# Negative images

neg <- function(M,i){
apply(M, 3, max)[i]-M[,,i]
}

mmat<-array(0,dim=dim(dat))
for(i in 1:dim(dat)[3]){
mmat[,,i]<-neg(dat,i)
}


par(mfrow=c(2,5),
        oma = c(3,3,3,3),
        mar = c(0,0.1,0,0.1))
for(i in 12:21){
image(t(apply(mmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1)  
}

# Preprocessing steps

range01 <- function(M){(M-min(M))/(max(M)-min(M))}
scmat<-array(0,dim=dim(mmat))
for(i in 1:dim(mmat)[3]){
scmat[,,i]<-range01(mmat[,,i])
}

# threshold
thresh <- function(M){ifelse(M<0.2, 0, M)}
thmat<-thresh(scmat)

# centralize
bmat<-array(0,dim=dim(thmat))
for(i in 1:dim(thmat)[3]){
temp<-thmat[,,i]
w<-temp[apply(temp,1,mean)>0,apply(temp,2,mean)>0]
if(is.null(dim(w))) next
if(dim(w)[1]<4) next
if(dim(w)[2]<4) next
if(dim(w)[1]>26) next
if(dim(w)[2]>26) next
bim<-matrix(rep(0,28*28),nrow=28)
ly=floor(((dim(bim)[1]-dim(w)[1])/2)+0.5)
uy=ly+dim(w)[1]-1
lx=floor(((dim(bim)[2]-dim(w)[2])/2)+0.5)
ux=lx+dim(w)[2]-1
bim[c(ly:uy),c(lx:ux)]<-w
bmat[,,i]<-bim
}

# Frame to 24x24
sfr<-bmat[c(3:26), c(3:26), ]


# Display the preprocessed images

par(mfrow=c(2,5),
        oma = c(3,3,3,3),
        mar = c(0,0.1,0,0.1))
for(i in 12:21){
image(t(apply(sfr[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1)  
}

# QUIZ QUESTIONS AND ANSWERS

# Quiz Question I

# If one wants to check minimum and maximum value in every digit, then the `apply` function would work. 
# In the following code, apply is used over the third dimension and the resulting minimum and maximum values for every digit are written to a vector:

minm<-apply(mmat, 3, min)
maxm<-apply(mmat, 3, max)
head(minm, 10)
head(maxm, 10)

# One could also calculate a single minimum and maximum value across all digits: 

summary(mmat)

# Quiz Question II
# To appreciate the variance of 10 different handwritten digits of the digit „0“, plot them yourself in a 2x5 frame.

# Unfortunately, no labels were given for these data, so we can not subset the data for the given label "0".
# What we need to do is to identify manually the index for each digit and finally only select those that we want to display.

par(mfrow=c(10,10),
        oma = c(3,3,3,3),
        mar = c(0,0.1,0,0.1))
for(i in 1:100){
image(t(apply(mmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1)
mtext(i, cex=0.6, col="red", side=3, line=-1)
}

# Now we know the indices that belong to the digit "0", we can use them to display only those.

par(mfrow=c(2,5),
        oma = c(3,3,3,3),
        mar = c(0,0.1,0,0.1))
for(i in c(2,12,22,32,44,54,64,74,86,96)){
image(t(apply(mmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1)
}


# As this is laborious, it is always preferable to have the labels whenever possible. 
# For those interested, the labels can be found at my GitHub page in the repository "Digits": https://rrighart.github.io/Digits/

# CONTACT
# If you have any questions, please do not hesitate to contact me: rrighart@googlemail.com



  
  