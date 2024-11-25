dd <- read.csv("dd_filled_quali.csv",header=T);
                  
attach(dd)
library(class)

uncompleteVars<-c(28)

fullVariables<-c(8,9,10,11,12,18,19,22,29,30)
aux<-dd[,fullVariables]

for (k in uncompleteVars){
  aux1 <- aux[!is.na(dd[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(dd[,k]),]
  dim(aux2)

  RefValues<- dd[!is.na(dd[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   

  dd[is.na(dd[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-dd[,fullVariables]
}

install.packages("StatMatch")
library(cluster)
require(StatMatch)

#assume missings represented with NA
uncompleteVar<-function(vector){any(is.na(vector))}

Mode <- function(x) 
{
  x<-as.factor(x)
  maxV<-which.max(table(x))
  return(levels(x)[maxV])
}



MiMMi <- function(data, priork=-1)
{
  #Identify columns without missings
  colsMiss<-which(sapply(data, uncompleteVar))
  if(length(colsMiss)==0){
    print("Non missing values found")
    out<-dd
    }else{
    K<-dim(data)[2]
    colsNoMiss<-setdiff(c(1:K),as.vector(colsMiss))
  
    #cluster with complete data
    dissimMatrix <- daisy(data[,colsNoMiss], metric = "gower", stand=TRUE)
    distMatrix<-dissimMatrix^2
  
    hcdata<-hclust(distMatrix, method = "ward.D2")
    plot(hcdata)
    nk<-2
    if(priork==-1){
      print("WARNING: See the dendrogramm and ZOOM if required")
      print("and enter a high number of clusters")
      nk<-readline("(must be a positive integer). k: ")
      nk<-as.integer(nk)
    }else{nk<-priork}
  
    partition<-cutree(hcdata, nk)

    CompleteData<-data
    #nomes cal per tenir tra?a de com s'ha fet la substituci?
    newCol<-K+1
    CompleteData[,newCol]<-partition
    names(CompleteData)[newCol]<-"ClassAux"
  
    setOfClasses<-as.numeric(levels(as.factor(partition)))
    imputationTable<-data.frame(row.names=setOfClasses)
    p<-1
  
    for(k in colsMiss)
    {
       #Files amb valors utils
       rowsWithFullValues<-!is.na(CompleteData[,k])
    
       #calcular valors d'imputacio
       if(is.numeric(CompleteData[,k]))
       {
          imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=mean)
       }else{
          imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=Mode)
       }
    
       #Impute
    
       for(c in setOfClasses)
       {
          CompleteData[is.na(CompleteData[,k]) & partition==c,k]<-imputingValues[c,2]
       }
    
       #Imputation Table
       imputationTable[,p]<-imputingValues[,2]
       names(imputationTable)[p]<-names(data)[k]
       p<-p+1
    }
    
    rownames(imputationTable)<-paste0("c", 1:nk)
    out<-new.env()
    out$imputedData<-CompleteData
    out$imputation<-imputationTable
  }
  return(out)
}

#run MIMMI
dimpute<-MiMMi(dd)

#table of imputation values used
dimpute$imputation

#imputed dataset
dimpute$imputedData

write.csv(dd, "C:/Users/Gerard/Downloads/dd_full_values.csv", row.names = FALSE)

