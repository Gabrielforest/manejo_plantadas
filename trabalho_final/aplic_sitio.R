# Curvas de sítio pelo método da diferença algébrica

source( "trabalho_final/ajuste_sitio.R" )

library( cmrinvflor )

ifc <- read.csv2( "trabalho_final/ifc.csv" )
head( ifc )

ifc <- ifc[ ,c( "parcela","idade", "hdom" ) ]
names( ifc ) <- c( "idamostra","idade1","hdom1" )

# Bailey e Clutter Polimórfico - melhor
bcpoli <- "b0*((hdom1/b0)^((idade1/idade2)^b2))"
parms <- list( b0 = cfbcpoli[ 1 ], b2 = cfbcpoli[ 2 ] )

# 4 classes
clsbcpoli <- class_sitio_dif_alg( amostras = ifc,
                                  expr = bcpoli,
                                  parms = parms,
                                  iref = 72,
                                  graf_curvas = T )

clsbcpoli_estabilidade <- clsbcpoli$estabilidade
clsbcpoli_estabilidade$modelo <- "Bailey-Clutter polimórfico"

# Bailey e Clutter Anamórfico
bcana <- "hdom1/(exp(b1*((idade2^b2)-(idade1^b2))))"
parms <- list( b1 = cfbcana[ 1 ], b2 = cfbcana[ 2 ] )

clsbcana <- class_sitio_dif_alg( amostras = ifc,
                                 expr = bcana,
                                 parms = parms,
                                 iref = 72,
                                 graf_curvas = T )

clsbcana_estabilidade <- clsbcana$estabilidade
clsbcana_estabilidade$modelo <- "Bailey-Clutter anamórfico"

# Chapman e Richards Polimórfico
crpoli <- "b0*((hdom1/b0)^((log(1-exp(b1*idade2)))/(log(1-exp(b1*idade1)))))"
parms <- list( b0 = cfcrpoli[ 1 ], b1 = cfcrpoli[ 2 ] )

clscrpoli <- class_sitio_dif_alg( amostras = ifc,
                                  expr = crpoli,
                                  parms = parms,
                                  iref = 72,
                                  graf_curvas = T )

write.csv2( clscrpoli$amostras, "trabalho_final/crpoli_sitios.csv" )
clscrpoli_estabilidade <- clscrpoli$estabilidade
clscrpoli_estabilidade$modelo <- "Chapman-Richards polimórfico"

# Chapman e Richards Anamórfico
crana <- "((hdom1*(1-exp(b1*(idade2))))/(1-exp(b1*(idade1))))^b2"
parms <- list( b1 = cfcrana[ 1 ], b2 = cfcrana[ 2 ] )

clscrana <- class_sitio_dif_alg( amostras = ifc,
                                 expr = crana,
                                 parms = parms,
                                 iref = 72,
                                 graf_curvas = T )

clscrana_estabilidade <- clscrana$estabilidade
clscrana_estabilidade$modelo <- "Chapman-Richards anamórfico"

# Schumacher Polimórfico
schpoli <- "b0*((hdom1/b0)^((idade1)/(idade2)))"
parms <- list( b0 = cfschpoli[ 1 ] )

clsschpoli <- class_sitio_dif_alg( amostras = ifc,
                                   expr = schpoli,
                                   parms = parms,
                                   iref = 72,
                                   graf_curvas = T )

View( clsschpoli$amostras )

clsschpoli_estabilidade <- clsschpoli$estabilidade
clsschpoli_estabilidade$modelo <- "Schumacher polimórfico"

# Schumacher Anamórfico
schana <- "hdom1*(exp(b1*((1/idade2)-(1/idade1))))"
parms <- list( b1 = cfschana[ 1 ] )

clsschana <- class_sitio_dif_alg( amostras = ifc,
                                  expr = schana,
                                  parms = parms,
                                  iref = 72,
                                  graf_curvas = T )

clsschana_estabilidade <- clsschana$estabilidade
clsschana_estabilidade$modelo <- "Schumacher anamórfico"


tabela_estabilidade <- rbind( clsbcana_estabilidade, clscrana_estabilidade,
                              clsschpoli_estabilidade, clsschana_estabilidade )

tabela_estabilidade_melhores_modelos <- rbind( clsbcpoli_estabilidade, clscrpoli_estabilidade )




