#'---
#'title: Aula - 6 - Manejo de Florestas Plantadas
#'subtitle: Curvas de sítio método da diferença algébrica (Bailey e Clutter Polimórfico)
#'author: Gabriel F. Pereira
#'---

#' ### Importando arquivo e adequando base para função do pacote cmrinvflor
ifc <- read.csv2( "aulas/aula_03/ifc.csv" )

ifc <- ifc[, c( "parcela", "idade", "hdom" ) ]

names( ifc ) <- c( "idamostra", "idade1", "hdom1" )

modelo <- "b0 * ( ( hdom1 / b0 ) ^ ( ( idade1 / idade2 ) ^ b2 ) )"

parms <- list( b0 = 51.5835150, b2 = 0.7754248 )

#' gráfico
cls <- class_sitio_dif_alg( amostras = ifc, 
                            expr = modelo, 
                            parms = parms, 
                            iref = 72, 
                            graf_curvas = TRUE )

