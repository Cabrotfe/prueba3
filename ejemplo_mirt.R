

# Apertura de paquetes ----------------------------------------------------


pacman::p_load(tidyverse, mirt)


# Creación de datos -------------------------------------------------------


datos= simdata(a=runif(10,0.8,1.2), d=rnorm(10,0,1), itemtype = "2PL", N =1000)
datos = data.frame(datos)


# Modelamiento y gráficos -------------------------------------------------

empirical_plot(datos, which.items = 1:10, smooth = F)


# Modelo ------------------------------------------------------------------



model=mirt(itemtype = "Rasch", model = 1, SE = T,data=datos)


coef(model, IRTpars = T, simplify=T)
coef(model, IRTpars = T, simplify=F)


plot(model, type = "trace")


# ajustes -----------------------------------------------------------------


M2(model)
itemfit(model)
itemfit(model, empirical.plot = 9, empirical.CI = .95)



