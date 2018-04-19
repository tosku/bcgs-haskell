rm(list=ls())
require(ggplot2)
require(jsonlite)
library(tools)

fields = list ()
fields = c("L", "realizations","ρ", "delta", "energy","mag","binder","l2","mcs","qq")
names(fields) = c( "L"
                 , "Realizations"
                 , "Disorder"
                 , "Δ"
                 , "Energy"
                 , "Magnetization"
                 , "Binder Cumulant - U"
                 , "l2"
                 , "Mean zero-cluster size (without the largest) - MCS"
                 , "Q"
                 )

filename = "../results/bimodal/p07/bimRStats.json"

config = list ()

plotGS = function (filename){
  dataname = basename(file_path_sans_ext(filename))
  datadir = dirname(filename)
  configfile = paste(c(datadir,"/",dataname, ".plot"),collapse='')
  print(configfile)
  config = fromJSON(configfile)
  print(config)
  xaxis = config$xaxis
  field = config$yaxis
  xaxisname = names(fields)[fields == xaxis]
  yaxisname = names(fields)[fields == field]
  gss = fromJSON(filename)
  order(print(gss[config$groupBy]))
  meanColumn = paste(c(field, "_mean"), collapse='')
  seColumn = paste(c(field, "_se"), collapse='')
  lowColumn = paste(c(field,"_low"),collapse='')
  highColumn = paste(c(field,"_high"),collapse='')
  gss[lowColumn] = gss[meanColumn] - 1.96 * gss[seColumn]
  gss[highColumn] = gss[meanColumn] + 1.96 * gss[seColumn]
  theplot = ggplot(gss,aes(x=gss[xaxis],y=gss[meanColumn])) +
  geom_point(aes(color=factor(l))) +
  geom_errorbar(aes(colour=factor(l),ymin=gss[lowColumn], ymax=gss[highColumn])) +
  ggtitle(config$title) +
  labs(x=xaxisname,y=yaxisname)
  print(theplot)
  exportname = paste(c(datadir,"/",dataname, "_", xaxis,"_vs_", field, ".png"),collapse='')
  ggsave(filename=exportname,plot=theplot)
}

toCSV = function (filename) {
  gss = fromJSON(filename)
  datadir = dirname(filename)
  dataname = basename(file_path_sans_ext(filename))
  csvfile = paste(c(datadir,"/",dataname, ".csv"),collapse='')
  write.csv(gss, file = csvfile)
}

plotGS(filename)
