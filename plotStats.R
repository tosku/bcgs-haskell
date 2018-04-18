rm(list=ls())
require(ggplot2)
require(jsonlite)

config = list ()

config$fields = c("energy","mag","binder","l2","mcs","qq")
names(config$fields) = c("Energy"
                         , "Magnetization"
                         , "Binder Cumulant - U"
                         , "l2", "Mean zero-cluster size (without the largest) - MCS"
                         , "Q")

dataset = "../results/bimodal/p07/bimRStats"
filename = toString(paste(c(dataset,".json"),collapse=''))
gss = fromJSON(filename)
plot
#gss = fromJSON("RStats.json")
#gss = fromJSON("scanRStats.json")
#gss = fromJSON("bigRStats.json")
#gss = fromJSON("binRStats.json")
#field = "energy"
#field = "mag"
#field = "binder"
#field = "l2"
field = "mcs"
#field = "qq"
meanColumn = paste(c(field, "_mean"), collapse='')
seColumn = paste(c(field, "_se"), collapse='')
lowColumn = paste(c(field,"_low"),collapse='')
highColumn = paste(c(field,"_high"),collapse='')
gss[lowColumn] = gss[meanColumn] - 1.96 * gss[seColumn]
gss[highColumn] = gss[meanColumn] + 1.96 * gss[seColumn]
reals = gss$realizations[1]
print (reals)
#theplot = ggplot(gss,aes(x=delta,y=gss[meanColumn],group=ρ,color=ρ)) +
#geom_point(aes(colour=factor(ρ),shape=factor(ρ))) +
#geom_errorbar(aes(color=factor(ρ),ymin=gss[lowColumn], ymax=gss[highColumn]),width=.001) +
#ggtitle(paste(c("Bimodal"," Δ=2"," realizations=",reals),collapse='')) +
theplot = ggplot(gss,aes(x=delta,y=gss[meanColumn],group=l,color=l)) +
geom_point(aes(colour=factor(l))) +
geom_errorbar(aes(colour=factor(l),ymin=gss[lowColumn], ymax=gss[highColumn])) +
ggtitle(paste(c("Unimodal"," D=2"),collapse='')) +
#ylim(-0.4,0.4) +
labs(x="Δ",y=field, legend="L")
#labs(x="Δ",y="Mean zero-cluster size (without the largest)", legend="L")

print(theplot)
exportname = paste(c(filename, "_", field, ".png"),collapse='')
ggsave(filename=exportname,plot=theplot)
