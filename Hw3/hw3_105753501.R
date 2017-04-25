library('ROCR')


contingency<-function(pre,ref,target){
  if(pre==target){
    if(ref==target){con<-"TP"}
    else{con<-"FP"}
  }
  else{
    if(ref==target){con<-"FN"}
    else{con<-"TN"}
  }
  con
}


postive_or_not<-function(pre,ref){
  if(pre==ref){
    con<-"P"
  }
  else{
    con<-"F"
  }
  con
}




# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  stop("USAGE: Rscript hw2_105753036.R --target male|female --files file1 file2 ... filen --out out.csv", call.=FALSE)
}

# parse parameters
i <- 1 
while(i < length(args)) {
  if(args[i] == "--target") {
    query_m <- args[i+1]
    i <- i+1
  } else if(args[i] == "--files") {
    j <- grep("-", c(args[(i+1):length(args)], "-"))[1]
    files <- args[(i+1):(i+j-1)]
    i <- i+j-1
  } else if(args[i] == "--out") {
    out_f <- args[i+1]
    i <- i+1
  } else {
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i <- i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))





names<-c()
sensitivities<-c()
specificities<-c()
fonescores<-c()
aucs<-c()
filename<-c()

# read files

for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  
  
  names<-c(names,name)
  data<-apply(d[,c("prediction","reference")],1,function(x) contingency(x[1],x[2],query_m))
  confusionmatrix<-table(data)
  pred<-prediction(d$pred.score,d$reference)
  auc.tmp<-performance(pred,"auc")
  
  
   sensitivity<-round(confusionmatrix[4]/(confusionmatrix[1]+confusionmatrix[4]),digit=2)
   specificity<-round(confusionmatrix[3]/(confusionmatrix[3]+confusionmatrix[2]),digit=2)
   fonescore<-round(2*confusionmatrix[4]/(2*confusionmatrix[4]+confusionmatrix[2]+confusionmatrix[1]),digit=2)
   auc<-round(as.numeric(auc.tmp@y.values),digit=2)
   
   
   sensitivities<-c(sensitivities,sensitivity)
   specificities<-c(specificities,specificity)
   fonescores<-c(fonescores,fonescore)
   aucs<-c(aucs,auc)
   filename<-c(filename,file)
}

#write output file
outdata<-data.frame(method=names,sensitivity=sensitivities,specificity=specificities,
                    F1=fonescores,AUC=aucs,stringsAsFactors = F)

hight<-apply(outdata[-1],2,which.max)
maxn<-function(n) function(x) order(x,decreasing=TRUE)[n] #find the n-th function
second<-apply(outdata[-1],2,maxn(2))
hightrow<-c("hightest",names[hight])


#fisher test


bestfile<-read.csv(filename[hight[3]],header=T,sep=",")
secondfile<-read.table(filename[second[3]],header=T,sep=",")


tab<-table(bestfile$prediction,secondfile$prediction)
if(fisher.test(tab)$p.value<0.05){
  hightrow[4]<-paste0(names[hight[3]],"*")
}

#output file

outdata<-rbind(outdata,hightrow)
write.csv(outdata,file=out_f,row.names = F, quote = F)