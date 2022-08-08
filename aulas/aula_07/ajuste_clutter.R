#'---
#'title: Aula - 7 - Manejo de Florestas Plantadas
#'subtitle: Ajuste (Bailey e Clutter Polimórfico) base com sítios
#'author: Gabriel F. Pereira
#'---

#' Lendo dados

ifc <- read.csv2( "aulas/aula_07/dados.csv" )
ifc <- setNames( ifc, c( "parcelas", "idade", "hdom", "ab", "vtcc", "s" ) )
ifc_pareado <- cmrinvflor::parear_seqmed( ifc )
names( ifc_pareado )

ifc_pareado$s <- ifc_pareado$s2

names( ifc_pareado ) <- ifelse( names( ifc_pareado ) == "s2", "s", names( ifc_pareado ) )

aj_clutter <- lm( log( vtcc2 ) ~ I( 1 / s ) + I( 1 / idade2 ) +
                    I( ( idade1 / idade2 ) * log( ab1 ) ) +
                    I( 1 - idade1 / idade2 ) + 
                    I( ( 1 - idade1 /idade2 ) * s ),
                   data = ifc_pareado )

sumario <- summary( aj_clutter )

dy <- 1 / ifc_pareado$vtcc2
medgeo <- exp( mean( log( dy ) ) )

ind_furnival <- 1 / medgeo * sumario$sigma
ind_furnival_perc <- ind_furnival / mean( ifc_pareado$vtcc2 ) * 100

#' fator de correção de discrepância logarítmica de Meyer,
#' objetiva a correção das estimativas dos modelos que
#' sofreram uma transformação matemática na variável dependente, sendo que apenas o cálculo do antilog em
#' modelos logarítmicos não é suficiente para a minimização desses erros, sendo isso chamado de discrepância
#' logarítmica

fc <- exp( 0.5 * sumario$sigma^2 )
vtcc2_estimado <- exp( predict( aj_clutter ) ) * fc

bs <- unname( coef( aj_clutter ) )
