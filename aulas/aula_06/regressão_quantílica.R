#'---
#'title: Aula - 6 - Manejo de Florestas Plantadas
#'subtitle: Regressão quantílica para classificação de sítio
#'author: Gabriel F. Pereira
#'---

ifc <- read.csv2( "ifc.csv" )
names( ifc )

#' #### Idade == x, hdom == y
ifc$x <- ifc$idade
ifc$y <- ifc$hdom

#' #### Idade de predição para visualização das curvas
xpred <- 0:100

library( quantreg )

# bailey_cuttler <- function( x, b0, b1, b2 ){
#   exp( b0 + b1 * ( 1 / x ) * b2 )
# }

chaprich <- function( x, b0, b1, b2 ) {
  b0 * ( 1 - exp( b1 * x ) ) ^ b2
}

b0 <- 33.78
b1 <- -0.02825
b2 <- 1.338

#' #### Regressão não linear clássica
initial <- list( b0 = b0, b1 = b1, b2 = b2 )
res_nls <- nls( y ~ chaprich( x, b0, b1, b2 ), data = ifc, start = initial )

#' #### Gráfico com curva média (linha central)
{
  par( mfrow = c( 1, 1 ),
       pch = 20,
       bg = "#171717",
       col.axis = "white",
       col.lab = "white",
       col.main = "white",
       fg = "white" )
  with( ifc, plot( x, y, xlim = c( 0, 100 ), ylim = c( 0, 40 ) ) )
  
  lines( xpred, predict( res_nls, newdata = list( x = xpred ) ), lty = 2, col = "red" )
}

#' #### Curvas nos gráficos (quantílica e clássica) 
qs <- c( 0.1, 0.25, 0.5, 0.75, 0.9 )

{
  par( mfrow = c( 1, 1 ),
       pch = 20,
       bg = "#171717",
       col.axis = "white",
       col.lab = "white",
       col.main = "white",
       fg = "white" )
  with( ifc, plot( x, y, 
                   xlim = c( 0, 100 ), ylim = c( 0, 40 ),        
                   xlab = "idade", ylab = "altura dominante" ) )
  for ( sqs in qs ) {
    res <- nlrq( y ~ chaprich( x, b0, b1, b2 ), 
                 data = ifc, 
                 start = initial, 
                 tau = sqs )
    lines( xpred, predict( res, newdata = list( x = xpred ) ), lty = 2, col = "green" )
    print( summary( res ) )
  }
}

#' #### comparação média e mediana:
{
  par( mfrow = c( 1, 1 ),
       pch = 20,
       bg = "#171717",
       col.axis = "white",
       col.lab = "white",
       col.main = "white",
       fg = "white" )
  with( ifc, plot( x, y,
                   main = "Média x Mediana",
                   xlab = "idade",
                   ylab = "altura dominante",
                   pch = 20,
                   col = "green",
                   xlim = c( 0,100 ),
                   ylim = c( 0, 40 )
  )
  )
  classica <- nls( y ~ chaprich( x, b0, b1, b2 ), data = ifc, start = initial )
  lines( xpred, predict( classica, newdata = list( x = xpred ) ), lty = 2, col = "pink" )
  
  quantilica <- nlrq( y ~ chaprich( x, b0, b1, b2 ), data = ifc, start = initial, tau = 0.5 )
  lines( xpred, predict( quantilica, newdata = list( x = xpred ) ), lty = 1, col = "red" )
  
  legend( x = "topleft", legen = c( "clássica", "quantílica" ), lty = c( 2, 1 ), col = c( "pink", "red" ) , bty = "n" )
}
