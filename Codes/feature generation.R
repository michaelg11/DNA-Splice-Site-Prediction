## importing and creating sequence and response
install.packages("R.matlab")
library(R.matlab)
path<-"% enter folder path here %"
pathname<-file.path(path,"elegans_size=25000_subsample=0.mat")
eleg_sub<-readMat(pathname)
resp<-eleg_sub$LT
resp<-t(resp)
dna<-eleg_sub$XT
dna<-as.matrix(dna)
eleg_data<-data.frame(cbind(dna,resp),stringsAsFactors = F)
colnames(eleg_data)<-c("seq","resp")
View(eleg_data)

## feature creation

# compositional features
source("https://bioconductor.org/biocLite.R")
biocLite("Biostrings")
library(Biostrings)

newdata=matrix(0,nrow = 25000,ncol = 64)


sequence_kmers <- function(sequence, k){
  require(gtools)
  t<-data.frame(permutations(4,k,v=c("A","C","G","T"),repeats.allowed = T))
  p<-paste(t[,1:k],sep="")
  k_mers <- lapply(sequence,function(x){
    seq_loop_size <- length(DNAString(x))-k+1
    kmers <- sapply(1:seq_loop_size, function(z){
      y <- z + k -1
      kmer <- substr(x=x, start=z, stop=y)
      return(kmer)
    })
    return(kmers)
  })
  uniq <- unique(unlist(k_mers))
  ind <- t(sapply(k_mers, function(x){
    tabulate(match(x, p), length(p))
  }))
  colnames(ind) <- p
  return(ind)
}

k_mer<-function(data,k){
  newdata=matrix(0,nrow = nrow(data),ncol=4^k)
  for (i in 1:nrow(data)){
  s<-data[i,1]
  ind<-sequence_kmers(s,k)
  for (j in 1:ncol(newdata)){
    newdata[i,j]=ind[j]
  }
  }
  return(newdata)
}  

one_mer<-k_mer(eleg_data,1)
colnames(one_mer)<-p
colnames(newdata)<-p


coding<-matrix(0,nrow = 25000,ncol = 64)
non_coding<-matrix(0,nrow=25000,ncol=64)


for (i in 1:25000){
  s<-eleg_data[i,1]
  s_coding<-substr(s,62,142)
  s_noncoding<-substr(s,1,61)  
  ind_coding<-sequence_kmers(s_coding,3)
  ind_noncoding<-sequence_kmers(s_noncoding,3)
  for (j in 1:64){
    coding[i,j]=ind_coding[j]
    non_coding[i,j]=ind_noncoding[j]
  }
}
colnames(coding)<-paste(p,"up",sep="_")
colnames(non_coding)<-paste(p,"down",sep="_")

# positional features

dna<-list("A","C","G","T")
feat_creat<-function(data,amino){
  feat<-matrix(0,nrow = nrow(data),ncol=564)
  for (i in 1:nrow(data)){
    a<-unlist(strsplit(as.character(data[i,1]),""))
    for (j in 1:length(a)){
      for (k in 1:4){
        if (a[j]==amino[k]){
          feat[i,(j-1)*4+k]=1
        }
      }
    }
  }
  return(feat)
}

orth_enc<-feat_creat(eleg_data,dna)

feat_matrix<-cbind(newdata,coding,non_coding,orth_enc,one_mer,eleg_data[,2])
save(feat_matrix,file="feat_matrix.Rd")
