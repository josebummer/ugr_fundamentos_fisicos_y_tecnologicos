
Respuestas <- 
  readXL("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls",
   rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
library(relimp, pos=14)
showData(Respuestas, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
library(abind, pos=15)
local({
  .Table <- xtabs(~Sexo+Red, data=Respuestas)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nTotal percentages:\n")
  print(totPercents(.Table))
})
local({
  .Table <- xtabs(~Sexo+Red, data=Respuestas)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
})
local({
  .Table <- xtabs(~Red+Sexo, data=Respuestas)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
})
local({
  .Table <- xtabs(~Red+Sexo, data=Respuestas)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
})
local({
  .Table <- xtabs(~Red+Sexo, data=Respuestas)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
with(Respuestas, Barplot(Red, by=Sexo, style="divided", legend.pos="topright", xlab="Red", ylab="Frequency"))
with(Respuestas, Barplot(Sexo, by=Red, style="divided", legend.pos="topright", xlab="Sexo", ylab="Frequency"))
with(Respuestas, tapply(Edad, list(Red), mean, na.rm=TRUE))
with(Respuestas, plotMeans(Edad, Red, error.bars="se"))
with(Respuestas, plotMeans(Edad, Red, error.bars="none"))
cor(Respuestas[,c("Altura","Edad","Peso")], use="complete")
scatterplotMatrix(~Altura+Edad+Peso, reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), id.n=0, diagonal = 'density', 
  data=Respuestas)
scatterplotMatrix(~Altura+Edad+Peso, reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), id.n=0, diagonal = 'none', 
  data=Respuestas)
scatterplot(Peso~Altura, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=Respuestas)
.Table <- matrix(c(20,10,4,16,12,2), 2, 3, byrow=TRUE)
rownames(.Table) <- c('SI', 'NO')
colnames(.Table) <- c('Primarios', 'Medios', 'Superiores')
.Table  # Counts
rowPercents(.Table) # Row Percentages
remove(.Table)
.Table <- matrix(c(20,10,4,16,12,2), 2, 3, byrow=TRUE)
rownames(.Table) <- c('SI', 'NO')
colnames(.Table) <- c('Primarios', 'Medios', 'Superiores')
.Table  # Counts
colPercents(.Table) # Column Percentages
remove(.Table)
.Table <- matrix(c(20,10,4,16,12,2), 2, 3, byrow=TRUE)
rownames(.Table) <- c('SI', 'NO')
colnames(.Table) <- c('Primarios', 'Medios', 'Superiores')
.Table  # Counts
.Test <- chisq.test(.Table, correct=FALSE)
.Test
remove(.Test)
remove(.Table)
tcon <- read.table("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 4/tcont.txt", header=TRUE, sep="", 
  na.strings="NA", dec=".", strip.white=TRUE)
showData(tcon, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
local({
  .Table <- xtabs(~cancer+sexo, data=tcon)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nTotal percentages:\n")
  print(totPercents(.Table))
})
local({
  .Table <- xtabs(~cancer+sexo, data=tcon)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
})
local({
  .Table <- xtabs(~cancer+sexo, data=tcon)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
})
local({
  .Table <- xtabs(~cancer+sexo, data=tcon)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
with(tcon, Barplot(cancer, xlab="cancer", ylab="Frequency"))
cerezos <- read.table("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/cerezos.txt", header=TRUE, sep="", 
  na.strings="NA", dec=".", strip.white=TRUE)
showData(cerezos, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
cor(cerezos[,c("altura","diametro","volumen")], use="complete")
scatterplotMatrix(~altura+diametro+volumen, reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), id.n=0, diagonal = 'none', 
  data=cerezos)
scatterplot(volumen~diametro, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=cerezos)

