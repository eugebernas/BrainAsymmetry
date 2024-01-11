library(sjPlot)

data=read.csv("baseAD.csv")

data$AD = as.factor(data$AD)
data$SES = as.factor(data$SES)
data$Educ = as.factor(data$Educ)
data$M.F = as.factor(data$M.F)

colnames(data)[2]="genero"

library(lavaan)
library (cSEM)
library (listviewer)
library(car)

# instalaciones previas para poder instalar la libreria matrixpls
#install.packages("devtools") 
library(devtools)
#install.packages("xfun")
library(xfun)
#install_github("mronkko/matrixpls")
#install.packages("lavaan", repos="http://www.da.ugent.be", type="source")
#install.packages("simsem", repos="http://rweb.quant.ku.edu/kran", type="source")
#library(lavaan)
library(matrixpls)
#Modelo Estructural


data <- na.omit(data)

datasano=data[data$AD==0,]


#Modelo de Medida y Estructural

depmedio2 <- '
#Modelo de Medida (<~ formativos)
SIMETRIA=~coronoaxial+axial88+axial78+axial108+axial68+axial118+coronal104+coronal94+coronal84+coronal114+coronal124

Edad<~ Age
NSE<~ SES
sexo<~ genero


SIMETRIA~Edad+NSE+sexo

'

#Parmetros del algoritmo PLS====
pls2 <- csem(
  .data = datasano,
  .model = depmedio2,
  .disattenuate = FALSE,
  .iter_max= 300,
  .tolerance= 1e-07,
  .R= 200,
  .resample_method = "bootstrap")

#Descripci?n de ajuste general
verify(pls2)

csem_plot(pls2)

#Cargas, pesos y path, IC
summarize(pls2)
assess(pls2)
calculateGoF(pls2)






