rm(list=ls())
require(ggplot2)
require(jsonlite)
library(tools)

fields = list ()
fields = c("l", "realizations","ρ", "delta", "energy","mag","binder","l2","mcs","qq")
names(fields) = c( "L"
                 , "Realizations"
                 , "Disorder"
                 , "Δ"
                 , "Energy"
                 , "Magnetization"
                 , "Binder Cumulant - U"
                 , "l_2"
                 , "Mean zero-cluster size (without the largest) - MCS"
                 , "Q"
                 )
#observables = c("energy","mag","binder","l2","mcs","qq")
observables = c("mag","binder","l2","mcs","qq")

#filename = "../results/bimodal/p07/bimRStats.json"
#filename = "../results/p05/wideRStats.json"
#filename = "../results/d2/all/d2RStats.json"
#filename = "../results/right/rightRStats.json"
#filename = "../results/3d/p05/3dRStats.json"
#filename = "../results/3d/p2/3dp2RStats.json"
#filename = "../results/3d/p2/narrow/3dnRStats.json"
filename = "../results/3d/p2/p2RStats.json"
#filename = "../results/3d/p2/d3/3dd3RStats.json"

config = list ()

plotGS = function (filename, field){
  dataname = basename(file_path_sans_ext(filename))
  datadir = dirname(filename)
  configfile = paste(c(datadir,"/",dataname, ".plot"),collapse='')
  print(configfile)
  config = fromJSON(configfile)
  print(config)
  if(config$groupBy == "l"){
    xaxis = "delta"
    xaxisname = names(fields)[fields == xaxis]
    yaxisname = names(fields)[fields == field]
    print(c("plotting",xaxisname,"vs",yaxisname))
    gss = fromJSON(filename)
    meanColumn = paste(c(field, "_mean"), collapse='')
    seColumn = paste(c(field, "_se"), collapse='')
    lowColumn = paste(c(field,"_low"),collapse='')
    highColumn = paste(c(field,"_high"),collapse='')
    gss[lowColumn] = gss[meanColumn] - 1.96 * gss[seColumn]
    gss[highColumn] = gss[meanColumn] + 1.96 * gss[seColumn]
    theplot = ggplot(gss,aes(x=gss[xaxis][,1],y=gss[meanColumn][,1],group=l)) +
    #scale_x_continuous(limits = c(1.96, 2.16)) +
    #scale_y_continuous(limits = c(0.2, 0.8)) +
    geom_point() +
    geom_errorbar(aes(colour=factor(l),ymin=gss[lowColumn][,1], ymax=gss[highColumn][,1])) +
    ggtitle(config$title) +
    labs(x=xaxisname,y=yaxisname)
    print(theplot)
    exportname = paste(c(datadir,"/",dataname, "_", xaxis,"_vs_", field, ".png"),collapse='')
    ggsave(filename=exportname,plot=theplot)
  }
  if(config$groupBy == "delta"){
    xaxis = "l"
    xaxisname = names(fields)[fields == xaxis]
    yaxisname = names(fields)[fields == field]
    print(c("plotting",xaxisname,"vs",yaxisname))
    gss = fromJSON(filename)
    meanColumn = paste(c(field, "_mean"), collapse='')
    seColumn = paste(c(field, "_se"), collapse='')
    lowColumn = paste(c(field,"_low"),collapse='')
    highColumn = paste(c(field,"_high"),collapse='')
    gss[lowColumn] = gss[meanColumn] - 1.96 * gss[seColumn]
    gss[highColumn] = gss[meanColumn] + 1.96 * gss[seColumn]
    theplot = ggplot(gss,aes(x=gss[xaxis][,1],y=gss[meanColumn][,1])) +
    geom_point() +
    geom_errorbar(aes(ymin=gss[lowColumn][,1], ymax=gss[highColumn][,1])) +
    ggtitle(config$title) +
    labs(x=xaxisname,y=yaxisname)
    print(theplot)
    exportname = paste(c(datadir,"/",dataname, "_", xaxis,"_vs_", field, ".png"),collapse='')
    ggsave(filename=exportname,plot=theplot)
  }
}

toCSV = function (filename) {
  gss = fromJSON(filename)
  datadir = dirname(filename)
  dataname = basename(file_path_sans_ext(filename))
  csvfile = paste(c(datadir,"/",dataname, ".csv"),collapse='')
  write.csv(gss, file = csvfile)
}

makeplots = function (filename) {
  lapply(observables, function(field){plotGS(filename,field)})
}
