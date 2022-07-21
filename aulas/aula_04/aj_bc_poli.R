
# importando dados pareados
dados <- na.omit( read.csv2( "aulas/aula_04/ifc_pareado.csv" ) )
 
# pareando dados
library( cmrinvflor )
dados_npar <- parear_seqmed( read.csv2( "aulas/aula_04/ifc.csv" ) )

par( mfrow = c( 2, 2 ),
     mai = c( rep( 0.4, 4 ) ),
     pch = 20,
     bg = "#171717",
     col.axis = "white",
     col.lab = "white",
     col.main = "white",
     fg = "white" )

with( dados, plot( hdom1, hdom2 ) )
with( dados, plot( hdom2, idade2 ) )

bc_poli <- "hdom2 ~ b0 * ( ( hdom1 / b0 ) ^ ( ( idade1 / idade2 )^b2 ) )"

ajuste <- nls( formula = bc_poli,
               data = dados,
               start = list( b0 = 50, b2 = 0.5 ) 
              )

bs <- coef( ajuste )

for ( i in 1:2 ){
  print( noquote( format( bs[ i ], decimal.mark = "," ) ) )
}

# erro, gráfico resíduos tem que igual em inventário no trab final
# ver normalidade, etc. Coisa que não fazemos em sala


