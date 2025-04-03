#**************************************************************************************#
#**************************************************************************************#
#
#                                      Demografía
#                          Función de Tabla de Vida Quinquenal
#                               Licenciatura en Actuaría
#                                 VI semestre, 2025-2 
#                           Capítulo 4. Decrementos múltiples
#
#         Creado por:               Andrés Peña M.  
#         Fecha de creación:        04-03-2025
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   04-03-2025
#         Institución:              Facultad de Ciencias - UNAM
#         Contacto:                 agpena@colmex.mx
#
#**************************************************************************************#
#**************************************************************************************#

# rm(list = ls())
# graphics.off()

# library(data.table)


# x <- sort(c(1, seq(0, 85, 5)))
# data <- setDT(read.table("clipboard", header = T, sep = "\t"))
# data[ , mx := nDx/nNx ]
# mx <- data[ , mx ]



# 1. Tabla de vida abreviada ----

lt_mx_abr <- function(x, mx, sex="f", IMR = NA){
  
  m <- length(x)  
  n <- c(diff(x), NA)  
  
  ax <- n/2
  qx <- rep(0, m)
  
  
  # Pag. 4 notas de clase - cuadro
  
  ## Coale y Demeny edades 0 a 1
  
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[1] <- 0.330 }else{
      ax[1] <- 0.045+2.684*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[1] <- 0.350 }else{
      ax[1] <- 0.053+2.800*mx[1]
    }  
  }
  
  ## Coale y Demeny edades 1 a 4
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[2] <- 1.352 }else{
      ax[2] <- 1.651-2.816*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[2] <- 1.361 }else{
      ax[2] <- 1.522-1.518*mx[1]
    }  
  }
  
  
  # Probabilidad de muerte
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[m] <- 1
  
  
  # Modificación de IMR con fuente externa
  if(!is.na(IMR)){
    
    qx[1] <- IMR
    
  }
  
  
  # Probabilidad de sobrevivir
  px <- 1-qx
  
  
  # Sobrevivientes
  lx <- 100000 * cumprod(c(1, px[-m]))
  
  
  # Defunciones
  dx <- c(-diff(lx), lx[m])
  
  
  # Años persona vividos
  Lx <- n* c(lx[-1], 0) + ax*dx
  Lx[m] <- lx[m]/mx[m] 
  
  
  # Años persona vividos acumulados
  Tx <- rev(cumsum(rev(Lx)))
  
  
  # Esperanza de vida
  ex <- Tx/lx   
  
  
  # Modificación de m_x con IMR de fuente externa
  if(!is.na(IMR)){
    
    mx[1] <- dx[1]/Lx[1]
    
  }
  
  
  # Tabla de mortalidad
  return(data.table(x, n, mx, ax, qx, px, 
                    lx=floor(lx), dx=floor(dx), Lx=floor(Lx), 
                    Tx=floor(Tx), ex))
}

# lt_mx_abr(x, mx, IMR = 0.0440)
# lt_mx_abr(x, mx)


# 2. Crecimiento lineal ----

lin <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- (N_T/N_0-1)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * (1+r*h)  
  
  return(N_h)
  
}


# 3. Crecimiento geométrico ----

geo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- ((N_T/N_0)^(1/dt))-1
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * (1+r)^h  
  
  return(N_h)
  
}


# 4. Crecimiento exponencial ----

expo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- log(N_T/N_0)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * exp(r*h)  
  
  return(N_h)
  
} 


# 4. Crecimiento logístico ----

logistic <- function(U = 200000000, L = 40000000, 
                     anio_ini=1950.5, anio_fin=2100.5,
                     anio=decimal_date(as.Date(c("2010-06-25", "2020-03-15"))), 
                     I=c(112336538, 126014024)){
  U <- U
  L <- L 
  anio_ini <- anio_ini
  anio_fin <- anio_fin
  anio <- decimal_date(as.Date(anio))
  anios <- anio_fin
  I <- I 
  
  Z <- log((U-I)/(I-L))
  a <- lm(Z~anio)$coefficients[1]
  w <- lm(Z~anio)$coefficients[2]
  
  Z_t <- a+w*anios
  I_t <- (U+L*exp(Z_t))/(1+exp(Z_t))
  
  logistic <- setDT(as.data.frame(cbind(anios, I_t)))
  logistic[anios>anio[length(anio)], .(I_t)] %>% pull() 
  
}



# 5. Crecimiento consolidado ----

crecim <- function(N_0, N_T, t_0, t_T, t, type = "lin"){
  
  # si se quiere el modelo exponencial
  if(type == "exp"){
    Nh <- expo(N_0, N_T, t_0, t_T, t) # una función dentro de otra!!!
  }
  
  # si se quiere el modelo lineal
  if(type == "lin"){
    Nh <- lin(N_0, N_T, t_0, t_T, t) # una función dentro de otra!!!
  }
  
  # si se quiere el modelo geométrico
  if(type == "geo"){
    Nh <- geo(N_0, N_T, t_0, t_T, t) # una función dentro de otra!!!
  }
  
  # si se quiere el modelo geométrico
  if(type == "log"){
    Nh <- logistic(U = 200000000, L = 40000000,
                   anio_ini = 1950, anio_fin = t,
                   anio = c(t_0, t_T),
                   I = c(N_0, N_T)
    ) # una función dentro de otra!!!
  }
  
  return(Nh)
}