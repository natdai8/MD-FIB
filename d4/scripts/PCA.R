#Load data
dd<-read.csv("C:/Users/Gerard/Downloads/d17.csv", stringsAsFactors=TRUE)

colnames(dd)<-c("hotel", "can", "lt", "arr_m", "arr_wn", "arr_dm", "s_wdn",
"s_wn", "adults", "children", "babies", "meal", "country", "d_wl", "ct",
"adr", "res_s")

attach(dd)

#Numerical variables
numeriques<-c(3,7,8,9,10,11,14,16)
numeriques

dcon<-dd[,numeriques]
sapply(dcon,class)

# PRINCIPAL COMPONENT ANALYSIS OF dcon
pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)


# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum


# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 6

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq))

# PLOT OF INDIVIDUALS
#select your axis
eje1<-1
eje2<-3

plot(Psi[,eje1],Psi[,eje2], type="n")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#Projection of variables
Phi = cor(dcon,Psi)

#select your axis
X<-Phi[,eje1]
Y<-Phi[,eje2]


#numerical variables
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)
names(dd)

#Qualitative variables
dcat<-c(1,2,4,6,12,15,17)
colors<-rainbow(length(dcat))

fm = round(max(abs(Psi[,1]))) 

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-1,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="lightgray")
axis(side=3, pos= 0, labels = F, col="lightgray")
axis(side=2, pos= 0, labels = F, col="lightgray")
axis(side=4, pos= 0, labels = F, col="lightgray")

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)


#numnericals and categoricals
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-1,1), xlab="Accomodation Demand and expenses", ylab="Days in waiting list")
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="lightgray")
axis(side=3, pos= 0, labels = F, col="lightgray")
axis(side=2, pos= 0, labels = F, col="lightgray")
axis(side=4, pos= 0, labels = F, col="lightgray")

arrows(ze, ze, X, Y, length = 0.07,col="cyan")
text(X,Y,labels=etiq,col="cyan", cex=0.7)

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)


#Graph of the countries
dcat<-c(12, 13)
colors<-c("red", "darkgreen")

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-1,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="lightgray")
axis(side=3, pos= 0, labels = F, col="lightgray")
axis(side=2, pos= 0, labels = F, col="lightgray")
axis(side=4, pos= 0, labels = F, col="lightgray")

arrows(ze, ze, X, Y, length = 0.07,col="cyan")
text(X,Y,labels=etiq,col="cyan", cex=0.7)

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)




