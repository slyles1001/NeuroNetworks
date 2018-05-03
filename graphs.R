setwd("C:/Users/Seth/Desktop/Internet Algorithms/NeuroNetworks")
# tst = read.csv("HCP_PTN1200/node_timeseries/3T_HCP1200_MSMAll_d50_ts2/100206.txt", 
#                sep = " ", header = F)
# ar_tst = ar(tst)
# summary(ar_tst)
# dim(ar_tst$ar)
# tst_parcel = read.csv("workbench/bin_windows64/parcels.txt", header=F)
# tst_data = read.csv("workbench/bin_windows64/100206_txt.txt", sep = '\t', header=F)
# max(tst_parcel)
# summary(tst_data)
k = 10000; n= 50
library(igraph)
library(ggplot2)

plot_ergm_degs = function(k, n){
  stor = array(dim=c(k,60))
  dist = abs(floor(rnorm(k, 19, 9))) # make array of means with sd similar
  
  
  for (i in 1:k){
    g = erdos.renyi.game(n, dist[i], type="gnm")
    gd = degree_distribution(g)
    gd = gd * n
    if(length(gd)> 60) print("oops")
    length(gd) = 60
    gd[is.na(gd)] = 0
    stor[i,] = gd
  }
  
  avg = colMeans(stor)
  
  avgdf = data.frame(num = 0:8, average = avg[avg > 0])
  ggplot(avgdf, aes(x=num, y = average)) + geom_bar(stat="identity") + 
    ggtitle("Average Deg GNM model, 50 Nodes\n 10000 trials, mean = 19, SD = 9")
}

