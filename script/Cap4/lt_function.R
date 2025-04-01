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
