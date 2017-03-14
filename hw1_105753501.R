args = commandArgs(trailingOnly=TRUE)

if (length(args)==0)
{
  stop("USAGE: Rscript hw1_105753501.R -files test.1.csv -out result.csv",call.=FALSE)
}

i <- 1

while(i<length(args))
{
  if(args[i]=="-files")
  {
    datafile <- args[i+1]
    i<- i+1
  }
  else if(args[i]=="-out")
  {
    resultfile <- args[i+1]
    i <- i+1
  }
  i <- i+1
}


data <- read.csv(datafile,header=TRUE,sep=',')

maxweight <- round(max(data[,2]),digits=2)
maxheight <- round(max(data[,3]),digits=2)

result = data.frame(set=datafile,weight=maxweight,height=maxheight)
write.csv(result,file=resultfile,row.names=FALSE)