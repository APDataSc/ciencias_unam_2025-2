#**************************************************************************************#
#**************************************************************************************#
#
#                                      Demografía
#                               Manejo de R en Demografía
#                               Licenciatura en Actuaría
#                                 VI semestre, 2025-2 
#                        Capítulo 1. Conceptos básicos y medidas
#                                       Semana 1
#
#         Creado por:               Andrés Peña M.  
#         Fecha de creación:        30-01-2025
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   31-01-2025
#         Institución:              Facultad de Ciencias - UNAM
#         Contacto:                 agpena@colmex.mx
#
#**************************************************************************************#
#**************************************************************************************#

# Preámbulo ----

## 1. Limpiar la memoria ----
rm(list = ls())

## 2. Instalación de paquetes ---- 
## install.packages("data.table", dependencies = T)

## 3. Carga de paquetes ----
library(data.table)


# Manipulación de datos ----

## Vectores ----
vec <- c(6, 1, 3, 6, 10, 5)
vec[5]
vec[c(2, 4)]

## Manipulación de edades ----
x <- 0:100
x5 <- x - x %% 5  
sex <- c(rep(1,51), rep(2,50))

## Creación de data.frame ----
df <- data.frame(x, x5, sex)

## Factores en R
df$sex <- factor(df$sex, levels = 1:2,
                 labels = c("Hombre", 
                            "Mujer"))
table(df$sex)

## Recodificación de la edad ----
df$edadr <- cut(x, breaks = c(0, 15, 60, 100),
                include.lowest = T, right = F, 
                labels = c("Me15", "15y59", 
                           "60ymas")   
                )

## Filtro ----
df[df$sex=="Mujer" & df$x<60, "edadr"]


# Tablas de datos ---- 
pop <- fread("https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/pry23/00_Pob_Mitad_1950_2070.csv")
dim(pop)
names(pop)

names(pop) <- tolower(names(pop)) # Poner en minúsculas nombres de variables


## Tabulado con data.table ----
pop_nac <- pop[entidad=="República Mexicana", 
               .(pop=sum(poblacion)), 
               .(ano=año, sex=sexo)]

## Tabulado con dplyr ----
pop_nac_d <- pop %>% 
    group_by(año, sexo) %>%
    filter(entidad=="República Mexicana") %>% 
    summarise(pop=sum(poblacion))


## Gráficas ----
plot(pop_nac$ano, pop_nac$pop)

ggplot(data=pop_nac, aes(x=ano, y=pop, group=sex, 
                         color=sex)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_minimal() +
  geom_vline(xintercept = 2024, linetype = 'dashed', 
             color = 'red', 
             size = 0.54, alpha = 0.70)

#*****************************************FIN******************************************#

# Nacional
pop_nac <- pop[entidad=="República Mexicana", 
               .(pop=sum(poblacion)), 
               .(año, edad)]

tab_rd <- pop_nac[ , age:=ifelse(edad %in% 15:59, "ft", "dep") ] %>% 
          .[ , .(pop = sum(pop)), .(año, age) ] %>%
          dcast(año ~ age) %>%
          .[ , RDT := dep / ft * 100 ]


ggplot(data=tab_rd, aes(x=año, y=RDT)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_minimal() +
  geom_vline(xintercept = 2025, linetype = 'dashed', 
             color = 'red', 
             size = 0.54, alpha = 0.70)

# Por entidad federativa
pop_nac <- pop[entidad!="República Mexicana", 
               .(pop=sum(poblacion)), 
               .(entidad, año, edad)]

tab_rd <- pop_nac[ , age:=ifelse(edad %in% 15:59, "ft", "dep") ] %>% 
  .[ , .(pop = sum(pop)), .(entidad, año, age) ] %>%
  dcast(entidad + año ~ age) %>%
  .[ , RDT := dep / ft * 100 ]


ggplot(data=tab_rd, aes(x=año, y=RDT)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_minimal() +
  geom_vline(xintercept = 2025, linetype = 'dashed', 
             color = 'red', 
             size = 0.54, alpha = 0.70) +
  facet_wrap(~entidad)
