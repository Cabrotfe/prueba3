

# Apertura de paquetes ----------------------------------------------------


pacman::p_load(tidyverse, mirt, stringr)


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
personfit(model)



# Ordenamiento de ítems por dificultad ------------------------------------

dif=coef(model, IRTpars=T, simplify =T)$items[,2] ## dificultades

it_orden=function(data, coefs){
  dificultades=data.frame(cbind(items=names(data),dif=coefs))
  dificultades = dificultades %>% mutate(dif = as.numeric(as.character(dif))) %>% arrange(dif)
  datos_orden=datos[,as.character(dificultades$items)]
  return(datos_orden)
}


hola=it_orden(datos, coefs = dif)


hola %>% colSums()


