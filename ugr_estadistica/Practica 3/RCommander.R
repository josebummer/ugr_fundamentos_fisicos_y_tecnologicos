
respuestas <- 
  readXL("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls",
   rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
resp_varon <- subset(respuestas, subset=Sexo=="Varón")
Boxplot(Peso~Red, data=resp_varon, id.method="y")
library(tcltk, pos=14)
library(aplpack, pos=14)
with(resp_varon, stem.leaf(Peso, na.rm=TRUE))
cerezos <- read.table("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/cerezos.txt", header=TRUE, sep="", 
  na.strings="NA", dec=".", strip.white=TRUE)
library(relimp, pos=16)
showData(cerezos, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
library(abind, pos=17)
library(e1071, pos=18)
numSummary(cerezos[,"altura"], statistics=c("mean", "sd", "quantiles", "cv"), quantiles=c(0.5))
numSummary(cerezos[,"altura"], statistics=c("mean", "sd", "quantiles", "cv"), quantiles=c(.5))
numSummary(cerezos[,"altura"], statistics=c("quantiles"), quantiles=c(.5))
numSummary(cerezos[,"altura"], statistics=c("mean", "sd", "quantiles", "cv"), quantiles=c(.5))
numSummary(cerezos[,"altura"], groups=cerezos$variedad, statistics=c("mean", "sd", "quantiles", "cv"), quantiles=c(.5))
showData(cerezos, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
local({
  .Table <- with(cerezos, table(variedad))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
with(cerezos, Hist(diametro, scale="frequency", breaks="Sturges", col="darkgray"))
numSummary(cerezos[,"diametro"], statistics=c("skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")
with(cerezos, Hist(diametro, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ volumen, data=cerezos, id.method="y")
save("cerezos", file="C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 3/cerezos.RData")

