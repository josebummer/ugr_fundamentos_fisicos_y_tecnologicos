
Dataset <- 
  read.table("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.txt",
   header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
library(relimp, pos=14)
load("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls")
Dataset <- readXL("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls", rownames=FALSE, header=TRUE, 
  na="", sheet="Respuestas", stringsAsFactors=TRUE)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
with(Dataset, (t.test(Altura, alternative='two.sided', mu=185, conf.level=.95)))
with(Dataset, (t.test(Peso, alternative='greater', mu=72, conf.level=.95)))
with(Dataset, (t.test(Peso, alternative='greater', mu=72, conf.level=.95)))
with(Dataset, (t.test(Peso, alternative='less', mu=72, conf.level=.95)))
with(Dataset, (t.test(Peso, alternative='greater', mu=72, conf.level=.95)))
with(Dataset, (t.test(Peso, alternative='less', mu=72, conf.level=.95)))
with(Dataset, (t.test(Altura, alternative='two.sided', mu=185, conf.level=.95)))
with(Dataset, (t.test(Peso, alternative='less', mu=72, conf.level=.95)))
local({
  .Table <- xtabs(~ Sexo , data= Dataset )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='less', p=.5, conf.level=.90, correct=FALSE)
})
with(Dataset, tapply(Altura, Sexo,  var, na.rm=TRUE))
var.test(Altura ~ Sexo, alternative='two.sided', conf.level=.90, data=Dataset)
t.test(Altura~Sexo, alternative='two.sided', conf.level=.90, var.equal=TRUE, data=Dataset)

