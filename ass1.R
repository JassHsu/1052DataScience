
arge = commandArgs(trailingOnly=TRUE)
if(Length(args)==0) 
{
  stop("USAGE: Rscript hw1_105753501.R -files input -out output",call.=FALSE)
}


i<-1

while(i<length(args))
{
if(args[i]=="-files")
{  
  i_f<-args[i+1]
  i<-i+1
}
else if(args[i]=="out")
{
  o_f<-args[i+1]
  i<-i+1
} 
i<-i+1
}

input <- sub('\\.csv$','',i_f)

data<-read.csv(i_f,header=TRUE,sep=",")


max_weight <- round(max(data[,2]),digit=2)
max_height <- round(max(data[,3]),digit=2)

result=data.frame(set=i_f,weight=max_weight,height=max_height)
write.csv.(result,file=o_f,row.names=FLASE)
