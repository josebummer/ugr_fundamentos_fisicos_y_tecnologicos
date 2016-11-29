
Respuestas <- 
  readXL("C:/Users/Jose/Google Drive/Universidad/SEGUNDO CUATRIMESTRE/Estadistica/Practicas/Practica 2/respuestas.xls",
   rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
library(relimp, pos=14)
showData(Respuestas, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
scatterplot(Peso~Altura, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots='xy', span=0.5, 
  ellipse=FALSE, levels=c(.5, .9), data=Respuestas)
RegLineal <- lm(Peso~Altura, data=Respuestas)
summary(RegLineal)
peso_estimado <- -66.0046 + 0.7915 * 173
peso_estimado
RegLinealHombres <- lm(Peso~Altura, data=Respuestas, subset=Sexo == "Varón")
summary(RegLinealHombres)
RegLinealMultiples <- lm(Peso ~ Altura +Sexo, data=Respuestas)
summary(RegLinealMultiples)
RegLinealMultiple2 <- lm(Peso ~ Altura + Sexo +n_herm, data=Respuestas)
summary(RegLinealMultiple2)

