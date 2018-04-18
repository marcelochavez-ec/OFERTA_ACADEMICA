#--------------------------------------------------------------------------------------------------------------------------

# SCRIPTING HOMOLOGACIÓN Y ESTRUCTURACIÓN DE LA OFERTA ACADÉMICA HISTÓRICA DEL PROCESO SER BACHILLER:

# PAQUETES:
library(RPostgreSQL)
library(dplyr)
library(plyr)
library(reshape2)
library(xlsx)
#library(plyr)

# CONEXIÓN A POSTGRESQL:

senescyt_bi<-dbConnect("PostgreSQL",dbname="senescyt_bi",
                       host="10.0.99.47",port=5432,user="postgres",
                       password="M0n1t0r")

prueba<-dbConnect("PostgreSQL",dbname="prueba",
                   host="10.0.99.47",port=5432,user="postgres",
                   password="M0n1t0r")


load("c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/asignaciones_1.RData")
asignaciones_1<-as.data.frame(lapply(asignaciones_1,function(x) if(is.character(x))
                iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=F)


load("c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/asignaciones_2.RData")
asignaciones_2<-as.data.frame(lapply(asignaciones_2,function(x) if(is.character(x))
                iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=F)



# SECCIÓN 1: CÁLCULO DE LA OFERTA DEL PER 2 AL PER 12:

oferta_p2_p12<-asignaciones_1 %>%
                filter(ccp_estado=="A") %>%
                select(per_id,
                       provincia_campus,
                       canton_campus,
                       parroquia_campus,
                       ies_nombre_instit,
                       ies_tipo_ies,
                       ies_tipo_financiamiento,
                       area_nombre,
                       subarea,
                       car_nombre_carrea,
                       ccp_cupos_reales) %>%
                distinct() %>%
                group_by(per_id,
                         provincia_campus,
                         canton_campus,
                         parroquia_campus,
                         ies_nombre_instit,
                         ies_tipo_ies,
                         ies_tipo_financiamiento,
                         area_nombre,
                         subarea,
                         car_nombre_carrea) %>%
                summarise_each(funs(sum),ccp_cupos_reales) %>%
                plyr::rename(c("car_nombre_carrea"="car_nombre_carrera",
                               "ccp_cupos_reales"="total_cupos")) %>% 
                filter(total_cupos!="NA")



# SECCIÓN 2: CÁLCULO DE LA OFERTA DEL PER 13 AL PER 14:
oferta_p13_p14<-asignaciones_2 %>% 
                select(per_id,
                       provincia_campus,
                       canton_campus,
                       parroquia_campus,
                       ies_nombre_instit,
                       ies_tipo_ies,
                       ies_tipo_financiamiento,
                       area,
                       subarea,
                       car_nombre_carrera,
                       ccp_cupos_nivelacion,
                       ccp_cupos_primer_semestre,
                       ccp_cupos_exoneracion) %>% 
                mutate(total_cupos=rowSums(.[11:13])) %>% 
                distinct() %>% 
                select(per_id,
                       provincia_campus,
                       canton_campus,
                       parroquia_campus,
                       ies_nombre_instit,
                       ies_tipo_ies,
                       ies_tipo_financiamiento,
                       area,
                       subarea,
                       car_nombre_carrera,
                       total_cupos) %>% 
                  group_by(per_id,
                           provincia_campus,
                           canton_campus,
                           parroquia_campus,
                           ies_nombre_instit,
                           ies_tipo_ies,
                           ies_tipo_financiamiento,
                           area,
                           subarea,
                           car_nombre_carrera) %>% 
                  summarise_each(funs(sum),total_cupos) %>% 
                  plyr::rename(c("area"="area_nombre"))

# SECCIÓN 3: CÁLCULO DE LA OFERTA DEL PER 15:

oferta_p15 <- dbGetQuery(prueba,"select * from oferta_p15")
oferta_p15<-as.data.frame(lapply(oferta_p15,function(x) if(is.character(x))
            iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=F)

# SECCIÓN 4: UNIÓN OFERTA:

oferta_p2_p15<-rbind(oferta_p2_p12,oferta_p13_p14,oferta_p15)


oferta_p2_p15$area_nombre<-recode(oferta_p2_p15$area_nombre,
                                  "ADMINISTRACION"="ADMINISTRACIÓN",
                                  "ADMINISTRACIÓN DE EMPRESAS Y DERECHO"="ADMINISTRACIÓN",
                                  "AGRICULTURA"="AGRICULTURA, SILVICULTURA, PESCA Y VETERINARIA",
                                  "AGRICULTURA, SILVICULTURA Y PESCA"="AGRICULTURA, SILVICULTURA, PESCA Y VETERINARIA",
                                  "AGRICULTURA, SILVICULTURA, PESCA Y VETERINARIA"="AGRICULTURA, SILVICULTURA, PESCA Y VETERINARIA",
                                  "BIENESTAR (CINE 2013)"="SALUD Y BIENESTAR",
                                  "CIENCIAS"="CIENCIAS NATURALES, MATEMÁTICAS Y ESTADÍSTICA",
                                  "CIENCIAS NATURALES, MATEMATICAS Y ESTADISTICA"="CIENCIAS NATURALES, MATEMÁTICAS Y ESTADÍSTICA",
                                  "CIENCIAS SOCIALES, EDUCACION COMERCIAL Y DERECHO"="CIENCIAS SOCIALES, PERIODISMO, INFORMACIÓN Y DERECHO",
                                  "CIENCIAS SOCIALES, EDUCACIÓN COMERCIAL Y DERECHO"="CIENCIAS SOCIALES, PERIODISMO, INFORMACIÓN Y DERECHO",
                                  "CIENCIAS SOCIALES, PERIODISMO E INFORMACIÓN"="CIENCIAS SOCIALES, PERIODISMO, INFORMACIÓN Y DERECHO",
                                  "CIENCIAS SOCIALES, PERIODISMO, INFORMACION Y DERECHO"="CIENCIAS SOCIALES, PERIODISMO, INFORMACIÓN Y DERECHO",
                                  "EDUCACION"="EDUCACIÓN",
                                  "HUMANIDADES Y ARTES"="ARTES Y HUMANIDADES",
                                  "INGENIERIA, INDUSTRIA Y CONSTRUCCION"="INGENIERÍA, INDUSTRIA Y CONSTRUCCIÓN",
                                  "SALUD Y SERVICIOS SOCIALES"="SALUD Y BIENESTAR",
                                  "TECNOLOGÍAS DE LA INFORMACIÓN Y COMUNICACIÓN (TIC)"="TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)",
                                  "TECNOLOGIAS DE LA INFORMACION Y LA COMUNICACION (TIC)"="TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)")

oferta_p2_p15$subarea<-recode(oferta_p2_p15$subarea,
                              "AGRICULTURA, SILVICULTURA Y PESCA"="AGRICULTURA",
                              "ARQUITECTURA Y CONSTRUCCION"="ARQUITECTURA Y CONSTRUCCIÓN",
                              "CIENCIAS BIOLOGICAS Y AFINES"="CIENCIAS BIOLÓGICAS Y AFINES",
                              "CIENCIAS DE LA VIDA"="CIENCIAS BIOLÓGICAS Y AFINES",
                              "CIENCIAS FISICAS"="CIENCIAS FÍSICAS",
                              "EDUCACION"="EDUCACIÓN",
                              "EDUCACION COMERCIAL Y ADMINISTRACION"="EDUCACIÓN COMERCIAL Y ADMINISTRACIÓN",
                              "EDUCACIÓN COMERCIAL Y ADMINISTRACION"="EDUCACIÓN COMERCIAL Y ADMINISTRACIÓN",
                              "FORMACION DE PERSONAL DOCENTE Y CIENCIAS DE LA EDUCACION"="EDUCACIÓN",
                              "INDUSTRIA Y PRODUCCION"="INDUSTRIA Y PRODUCCIÓN",
                              "INFORMATICA"="TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)",
                              "INFORMÁTICA"="TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)",
                              "INGENIERIA Y PROFESIONES AFINES"="INGENIERÍA Y PROFESIONES AFINES",
                              "MATEMATICAS Y ESTADISTICA"="MATEMÁTICAS Y ESTADÍSTICA",
                              "MEDICINA"="SALUD",
                              "MEDICINA."="SALUD",
                              "PERIODISMO E INFORMACION"="PERIODISMO E INFORMACIÓN",
                              "PROTECCION DEL MEDIO AMBIENTE"="MEDIO AMBIENTE",
                              "SALUD"="SALUD",
                              "SALUD."="SALUD",
                              "SERVICIOS DE PROTECCION"="SERVICIOS DE PROTECCIÓN",
                              "SERVICIOS SOCIALES"="SERVICIOS PERSONALES",
                              "TECNOLOGÍAS DE LA INFORMACIÓN Y COMUNICACIÓN (TIC)"="TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)",
                              "TECNOLOGIAS DE LA INFORMACION Y LA COMUNICACION (TIC)"="TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN (TIC)")

oferta_p2_p15$periodos<-recode(oferta_p2_p15$per_id,
                               "2"="1er. Semestre 2012",
                               "3"="2do. Semestre 2012",
                               "4"="1er. Semestre 2013",
                               "5"="2do. Semestre 2013",
                               "6"="1er. Semestre 2014",
                               "7"="2do. Semestre 2014",
                               "8"="1er. Semestre 2015",
                               "9"="2do. Semestre 2015",
                               "10"="1er. Semestre 2016",
                               "12"="2do. Semestre 2016",
                               "13"="1er. Semestre 2017",
                               "14"="2do. Semestre 2017",
                               "15"="1er. Semestre 2018")

oferta_p2_p15$periodos<-factor(oferta_p2_p15$periodos,
                               levels=c(
                                 "1er. Semestre 2012",
                                 "2do. Semestre 2012",
                                 "1er. Semestre 2013",
                                 "2do. Semestre 2013",
                                 "1er. Semestre 2014",
                                 "2do. Semestre 2014",
                                 "1er. Semestre 2015",
                                 "2do. Semestre 2015",
                                 "1er. Semestre 2016",
                                 "2do. Semestre 2016",
                                 "1er. Semestre 2017",
                                 "2do. Semestre 2017",
                                 "1er. Semestre 2018"))

oferta_p2_p15$ies_tipo_ies<-recode(oferta_p2_p15$ies_tipo_ies,
                                   "I"="INSTITUTO",
                                   "U"="UNIVERSIDAD")

# area<-data.frame(table(oferta_p2_p15$area_nombre))
# subarea<-data.frame(table(oferta_p2_p15$subarea))

# oferta_p2_p15<-as.data.frame(lapply(oferta_p2_p15,function(x) if(is.character(x))
#                iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=F)

dbWriteTable(senescyt_bi,"oferta_p2_p15",oferta_p2_p15,overwrite=T,row.names=F)


