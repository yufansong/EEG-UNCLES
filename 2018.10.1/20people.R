all_people<- function()
for (index in 3:4)
{
X= list()
for (person_number in 1:20)
{
person_number=as.character(person_number)
path_source = './FFT/onedata'
path_result = './processed_data/10.29/onedata_deal'
if (index == 1)
  Path=paste(path_source,person_number,'2_2.csv',collapse = NULL,sep='_')
if (index == 2)
  Path=paste(path_source,person_number,'1_1.csv',collapse = NULL,sep='_')
if (index == 3)
  Path=paste(path_source,person_number,'1_2.csv',collapse = NULL,sep='_')
if (index == 4)
  Path=paste(path_source,person_number,'2_1.csv',collapse = NULL,sep='_')
if (index == 5)
  Path=paste(path_source,person_number,'2_all.csv',collapse = NULL,sep='_')
if (index == 6)
  Path=paste(path_source,person_number,'1_all.csv',collapse = NULL,sep='_')

onedata_1=read.csv(Path,head=F)
X[[person_number]]=as.matrix(onedata_1)
}
#paste("all shape:",length(X),"\n")
#for (dim in 1:20)
#  {paste(dim,": ",length(X[dim]),"\n")}
unclesResult <- uncles(X)#UNCLES::uncles(X)
#unclesResult <- UNCLES::uncles(X,methods = list(kmeansKA, list(HC, method = "ward.D2")))

mnResult <- UNCLES::mnplots(unclesResult, doplot =T)

n_row <- vector(mode="numeric",length=0)
for (i in 1:nrow(na.omit(mnResult$C))){
  
  if ( mnResult$C[i,1] <= 44 ){
    
    n_row[i] <- ((mnResult$C[i,1] -1)%/% 4 + 1)
  }
  
  else if ( mnResult$C[i,1] <= 132 ){
    n_row[i] <- (( mnResult$C[i,1] - 45 ) %/% 8 + 12)
  }
  else if ( mnResult$C[i,1] <= 264 ){
    n_row[i] <- (( mnResult$C[i,1] - 133 ) %/% 12 + 23)
  }
  else {
    n_row[i] <- (( mnResult$C[i,1] - 265 ) %/% 16 + 34)
  }
}
for (i in 1:length(n_row)){
  if (i==1){
    R <- unclesResult$B[[n_row[i]]][,mnResult$C[i,4]]
  }
  else{
    R <- rbind(R,unclesResult$B[[n_row[i]]][,mnResult$C[i,4]])
  }
}
for (i in 1:nrow(R)){
  for (j in 1:ncol(R)){
    if (R[i,j]==TRUE){
      R[i,j]<-1
    }
    else{
      R[i,j]<-0
    }
  }
}
if (index==1)
  filename=paste(path_result,'2_2.csv',collapse = NULL,sep='_')
if (index==2)
  filename=paste(path_result,'1_1.csv',collapse = NULL,sep='_')
if (index==3)
  filename=paste(path_result,'1_2.csv',collapse = NULL,sep='_')
if (index==4)
  filename=paste(path_result,'2_1.csv',collapse = NULL,sep='_')
if (index==5)
  filename=paste(path_result,'2_all.csv',collapse = NULL,sep='_')
if (index==6)
  filename=paste(path_result,'1_all.csv',collapse = NULL,sep='_')
write.csv(R, filename)
}