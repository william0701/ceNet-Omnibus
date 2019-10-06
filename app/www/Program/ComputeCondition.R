library(parallel)
library(jsonlite)
print(getwd())
args=commandArgs(T)
datapath=args[1]
codepath=args[2]
type=args[3]
core=as.numeric(args[4])
logpath=args[5]
task=args[6]
load(datapath)

starttime=as.numeric(Sys.time())

cat(paste(toJSON(data.frame(task=task,time=starttime,total=length(geneset1),stringsAsFactors = F)),"\n",sep=""),file = logpath)
run=function(g1,type,codepath)
{
  source(codepath)
  result=t(as.matrix(unlist(lapply(X = as.list(geneset2),FUN = get(type),g1=g1))))
  cat(".",file = logpath,append = T)
  colnames(result)=geneset2
  rownames(result)=g1
  return(result)
}
cluster=makeCluster(core)
clusterExport(cluster,varlist = ls())
result=parLapply(cl = cluster,X = as.list(geneset1),fun = run,type='LA',codepath=codepath)
result=do.call(what = rbind,args = result)
stopCluster(cluster)
cat("Finish!\n",file = logpath)