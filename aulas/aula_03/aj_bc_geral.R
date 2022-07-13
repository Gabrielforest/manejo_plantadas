# aj bc geral

dados <- read.csv2( "aulas/aula_03/ifc.csv" )

tail(dados)

tracemem(dados)

par( mfrow = c( 2, 2 ),
     mai = c( rep( 0.4, 4 ) ),
     pch = 20,
     bg = "#171717",
     col.axis = "white",
     col.lab = "white",
     col.main = "white",
     fg = "white" )

plot( dados$idade, dados$hdom, 
      xlab = "idade (meses)",
      ylab = "hdom (m)", 
      col = "green" )

plot( dados$idade, dados$vtcc,
      xlab = "idade (meses)",
      ylab = "vtcc (m³/ha)", 
      col = "blue" )

plot( dados$hdom, dados$vtcc, 
      xlab = "hdom (m)",
      ylab = "vtcc (m³/ha)", 
      col = "red" )

plot( dados$ab, dados$vtcc, 
      xlab = "ab (m²/ha)",
      ylab = "vtcc (m³/ha)", 
      col = "pink" )

ajuste <- nls( formula = "log(hdom) ~ b0 + b1 * ( 1 / idade )^ b2", 
               data = dados, 
               start = list( b0 = 4, b1 = -11, b2 = 0.6 ) )

format( coef( ajuste ), dec = ",")

