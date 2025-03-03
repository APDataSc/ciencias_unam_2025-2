#**************************************************************************************#
#**************************************************************************************#
#
#                                      Demografía
#                               Funciones de crecimiento
#                               Licenciatura en Actuaría
#                                 VI semestre, 2025-2 
#                    Capítulo 2. Probabilidades y tasas específicas
#
#         Creado por:               Andrés Peña M.   
#         Fecha de creación:        27-02-2025
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   03-03-2025
#         Institución:              Facultad de Ciencias - UNAM
#         Contacto:                 agpena@colmex.mx
#
#**************************************************************************************#
#**************************************************************************************#

# 0. Preámbulo ----

## a. Limpiar la memoria 
rm(list = ls())
graphics.off( )

## b. Instalación de paquetes  
# Listado de paquetes 
.packages <-  c( "data.table", "lubridate", "latex2exp")
# Instalacion de paquetes no instalados
.inst <- .packages %in% installed.packages()
if( length( .packages[ !.inst ] ) > 0 ){
  install.packages( .packages[ !.inst ], dependencies = TRUE )
}

## c. Carga de paquetes 
lapply( .packages, require, character.only = TRUE )

## d. Semilla
set.seed(123)

## e. Datos 
data <- fread("https://raw.githubusercontent.com/APDataSc/ciencias_unam_2025-2/refs/heads/main/excel/Cap2/Mx_ARG_GUA.csv")


"------------------------------------------------------------------------------"

# 1. Ejemplo de funciones ----

x <- 1:10
x <- 1:50

# Equiponderado
w <- rep(1/10, 10)

# Ponderaciones desiguales
xi <- abs(rnorm(10))
w <- xi/sum(xi) 
barplot(w)

# Función de media ponderada
wei_ave <- function(x, w=rep(1/length(x), length(x))){

average <- sum(x*w)/sum(w)
return(average)  

}

wei_ave(x)
wei_ave(x, w)


## 1.1 Ejemplo de TBM ----

# Tasa bruta de mortalidad = D/N
(TBM <- data[ , .(D=sum(D), N=sum(N)), .(location)] %>% 
      .[ , d:=D/N*1000])
TBM

# Estructura por edad (C_x) y Tasas específicas de mortalidad (m_x)
data[ , `:=`(C=N/sum(N), m=D/N), .(location)]


# Gráfica de estructura por edad
plot(data[location=="Guatemala", x], 
     data[location=="Guatemala", C], col = "red", type = "l", 
     xlab = "Edad (x)", ylab = TeX("$_{n} C_{x}$"))
lines(data[location=="Argentina", x], 
      data[location=="Argentina", C], col = "blue", type = "l")

# Gráfica de tasas específicas de mortalidad
plot(data[location=="Argentina", x], 
     data[location=="Argentina", log(m)], col = "blue", type = "l",
     xlab = "Edad (x)", ylab = TeX("$ln(_{n} m_{x}$)"))
lines(data[location=="Guatemala", x], 
      data[location=="Guatemala", log(m)], col = "red", type = "l")

# Tasa bruta de mortalidad como promedio ponderado por edades específicas
(TBM_C <- data[ , .(d_c=wei_ave(m, C)*1000), .(location)])


# Tasa bruta de mortalidad estandarizada a la estructura de Guatemala
gua_C <- data[location=="Guatemala", C] 
data[ , C_s := c(gua_C, gua_C)]

(TBM_Cs <- data[ , .(d_cs=wei_ave(m, C_s)*1000), .(location)])


# Unión de los tres tipos de tasa calculados
left_join(TBM, TBM_C, by="location") %>% 
  left_join(TBM_Cs, by="location")



# 2. Crecimiento lineal ----

lin <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- (N_T/N_0-1)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * (1+r*h)  

  return(N_h)
  
}


cre_lin <- lin( N_0=112336538, N_T=126014024, 
                t_0="2010-06-25", t_T="2020-03-15",     
                  t=seq(2021.5, 2100.5, 1)
)


# Gráfica
data.frame(Año = c(decimal_date(as.Date("2010-06-25")), 
                   decimal_date(as.Date("2020-03-15")), seq(2021.5, 2100.5, 1)),
           N_t   = c(112336538, 126014024, cre_lin)) %>% 
  ggplot(aes(Año, N_t)) + 
  geom_line() + 
  geom_point()



# 3. Crecimiento geométrico ----

geo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- ((N_T/N_0)^(1/dt))-1
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * (1+r)^h  
  
  return(N_h)
  
}


cre_geo <- geo( N_0=112336538, N_T=126014024, 
                t_0="2010-06-25", t_T="2020-03-15",     
                t=seq(2021.5, 2100.5, 1)
)

# Gráfica
data.frame(Año = c(2010+5/12+25/365, 2020+2/12+15/365, seq(2021.5, 2100.5, 1)),
           N_t   = c(112336538, 126014024, cre_geo)) %>% 
  ggplot(aes(Año, N_t)) + 
  geom_line() + 
  geom_point()


# 4. Crecimiento exponencial ----

expo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- log(N_T/N_0)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * exp(r*h)  
  
  return(N_h)
  
} 


cre_exp <- expo( N_0=112336538, N_T=126014024, 
                 t_0="2010-06-25", t_T="2020-03-15",     
                 t=seq(2021.5, 2100.5, 1)
)

# Gráfica
data.frame(Año = c(2010+5/12+25/365, 2020+2/12+15/365, seq(2021.5, 2100.5, 1)),
           N_t   = c(112336538, 126014024, cre_exp)) %>% 
  ggplot(aes(Año, N_t)) + 
  geom_line() + 
  geom_point()



# 5. Crecimiento consolidado ----

crecim <- function(N_0, N_T, t_0, t_T, t, type = "lin"){
  
  # si se quiere el modelo exponencial
  if(tipo == "exp"){
    Nh <- expo(N_0, N_T, t_0, t_T, t) # una función dentro de otra!!!
  }
  
  # si se quiere el modelo lineal
  if(tipo == "lin"){
    Nh <- lin(N_0, N_T, t_0, t_T, t) # una función dentro de otra!!!
  }
  
  # si se quiere el modelo geométrico
  if(tipo == "geo"){
    Nh <- geo(N_0, N_T, t_0, t_T, t) # una función dentro de otra!!!
  }
  
  return(Nh)
}


"--------------------------------------FIN-------------------------------------"