
load("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls")
Dataset <- 
  readXL("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls",
   rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
library(relimp, pos=14)
showData(Dataset, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
t.test(Altura~Sexo, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)
with(Dataset, tapply(Altura, Sexo,  var, na.rm=TRUE))
var.test(Altura ~ Sexo, alternative='two.sided', conf.level=.95, data=Dataset)
t.test(Altura~Sexo, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Dataset)
library(abind, pos=15)
local({  .Table <- xtabs(~Sexo+I_1opcion, data=Dataset)
  cat("\nPercentage table:\n")
  print(rowPercents(.Table))
  prop.test(.Table, alternative='two.sided', conf.level=.90, correct=FALSE)
})

