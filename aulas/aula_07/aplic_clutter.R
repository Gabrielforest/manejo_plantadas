#'---
#'title: Aula - 7 - Manejo de Florestas Plantadas
#'subtitle: Aplicando (Bailey e Clutter Polimórfico) com no ajuste
#'author: Gabriel F. Pereira
#'---

source( file = "C:/Users/OppenSocial/Desktop/manejo_plantadas/aulas/aula_07/ajuste_clutter.R" )

#' parâmetros da equação 

b0 <- bs[ 1 ]
b1 <- bs[ 2 ]
b2 <- bs[ 3 ]
b3 <- bs[ 4 ]
b4 <- bs[ 5 ] 
b5 <- bs[ 6 ]

#' Variáveis independentes (dados da última medição)
#' Exemplo parcela 19

# última medição da parcela 19 nós chamamos de idade 1
idade1 <- 43.5
ab1 <- 15.27
s <- 27.5

#' estimar a curva de produção (senóide)
eqvol <- expression( exp( b0 + b1 / s + b2 / x + b3 * ( idade1 / x ) * log( ab1 ) +
                            b4 * ( 1 - idade1 / x ) + b5 * ( 1 - idade1 / x ) * s ) 
                     )
#' gerando incremento

#D( eqvol, "x" )
der_eqvol <-  expression( exp(b0 + b1/s + b2/x + b3 * (idade1/x) * log(ab1) + 
                                b4 * (1 - idade1/x) +
                                b5 * (1 - idade1/x) * s) * (b4 * (idade1/x^2) - 
                                                              (b3 * (idade1/x^2) * log(ab1) + b2/x^2) + b5 * (idade1/x^2) * 
                                                              s)
)

vtcc <- function( x ) eval( eqvol )
vtcc( 60 )
vtcc( seq( from = 12, to = 120, by = 12 ) )

#' incremento corrente médio
icm <- function( x ) eval( der_eqvol )
icm( seq( from = 12, to = 120, by = 12 ) )

#' incremento corrente anual
ica5 <- icm( 60 )* 12

#' incremento médio mensal
imn <- function( x ) vtcc( x ) / x
imn( 60 )

par( mfrow = c( 2, 1 ),
     pch = 20,
     mai = rep( 4, 0.1 ),
     bg = "#171717",
     col.axis = "white",
     col.lab = "white",
     col.main = "white",
     fg = "white" )
curve( vtcc( x ), 12, 120, xlab = "idade (mensal)", ylab = "vtcc (m³/ha)" )
curve( icm( x )* 12, 12, 120, xlab = "idade (meses)", ylab = "icm (m³/ha.ano)", col = "red" )
curve( imn( x )* 12, 12, 120, xlab = "idade (meses)", ylab = "ima (m³/ha.ano)", add = TRUE, col = "green" )
legend( "topright", c( "ica", "ima" ), text.col = c( "red", "green" ), box.lty = 0,
        col = c( "red", "green" ), lty = c( 1, 1 ) )

#' Definição da idade ótima de corte silvicultural

# Opção 1

op <- optimise( f = imn, interval = c( 12, 120 ), maximum = TRUE )
iocs <- op$maximum / 12

#' Opção 2
fo <- function( x ) abs( icm( x ) - imn( x ) )
op <- optimise( f = fo, interval = c( 12, 120 ), maximum = FALSE )
iocs <- op$minimum / 12

#' Opção 3
eqimm <- expression( exp( b0 + b1 / s + b2 / x + b3 * ( idade1 / x ) * log( ab1 ) +
                            b4 * ( 1 - idade1 / x ) + b5 * ( 1 - idade1 / x ) * s )  / x
)
D( eqimm, "x" )
der_eqimm <- expression( exp(b0 + 
                               b1/s + 
                               b2/x + 
                               b3 * (idade1/x) * log(ab1) + 
                               b4 * (1 - idade1/x) +
                               b5 * (1 - idade1/x) * s) * (b4 * (idade1/x^2) - (b3 * (idade1/x^2) * log(ab1) + b2/x^2) +
                                                             b5 * (idade1/x^2) * s)/x - exp(b0 + b1/s + b2/x + 
                                                                                              b3 * (idade1/x) * log(ab1) + b4 * (1 - idade1/x) + b5 * (1 - idade1/x) * s)/x^2
)

iocs <- ( b5 * idade1 * s + ( b4 - log( ab1 ) * b3 ) * idade1 - b2 ) / 12

#' Adicionando idade ótima de corte no gráfico
par( mfrow = c( 2, 1 ),
     pch = 20,
     bg = "#171717",
     col.axis = "white",
     col.lab = "white",
     col.main = "white",
     fg = "white",
     bty = "l" )
curve( vtcc( x ), 12, 120, xlab = "idade (mensal)", ylab = "vtcc (m³/ha)" )
curve( icm( x )* 12, 12, 120, xlab = "idade (meses)", ylab = "icm (m³/ha.ano)", col = "red" )
curve( imn( x )* 12, 12, 120, xlab = "idade (meses)", ylab = "ima (m³/ha.ano)", add = TRUE, col = "green" )
legend( "topright", c( "ica", "ima" ), text.col = c( "red", "green" ), box.lty = 0,
        col = c( "red", "green" ), lty = c( 1, 1 ) )
par( xpd = NA )
abline( v = iocs * 12, col = "white", lty = 2 )
text( locator( 1 ), paste0( round( iocs * 12, 2 ), " meses"), cex = 0.8, col = "white" )
#text( iocs, imn( iocs )* 12, paste( round( iocs * 12, 2 ), " anos" ), cex = 0.8, col = "white" )