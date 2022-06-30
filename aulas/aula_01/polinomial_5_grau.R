library(cmrinvflor)

cub <- read.csv2( "cubagem.csv" )
names( cub )

par( mfrow = c( 1, 2 ) )
# posição absoluta
with( cub, plot( hi, dicc, pch = 20) ) 

# posição relativa 
with( cub, plot( hi/ht, dicc, pch = 20) ) 

par( mfrow = c( 1, 1 ) ) 

ajlin <- lm( I( dicc/dap ) 
             ~ I( ( hi/ht )^1 ) 
             + I( ( hi/ht )^2 ) 
             + I( ( hi/ht )^3 ) 
             + I(( hi/ht )^4 ) + I(( hi/ht )^5), data = cub )

summary( ajlin )

bs <- coef( ajlin )
ajnlin <- nls("dicc ~ dap * 
              ( b0 + b1 * (hi/ht) 
              + b2 * ( (hi/ht) ^ 2) 
              + b3 *( (hi/ht) ^ 3) 
              + b4 *( (hi/ht) ^ 4) 
              + b5 * ( (hi/ht) ^ 5 ) 
              )",
              cub,
              start = list( b0 = 1.7, b1 = -4, b2 = 19, b3 = -43, b4 = 41, b5= -14) )

format( as.numeric( coef( ajnlin ) ), decimal.mark = ",")


