### Ajustes dos modelos de classificação de sítios e análise estatistica dos ajustes
library( cmrinvflor )

dados_npar <- read.csv2( "trabalho_final/ifc.csv" )
names( dados_npar )
dados_npar <- dados_npar[ c( "parcela", "idade", "hdom", "ab", "vtsc", "vtcc" ) ]

dados <- cmrinvflor::parear_seqmed( dados_npar )
head( dados )

par( mfrow = c( 1, 2 ) )
with( dados, plot( hdom1, hdom2, pch = 20 ) )
with( dados, plot( idade2, hdom2, pch = 20 ) )

# Ajustes Bayley Clutter --------------------------------------------------

# Ajuste Bayley Clutter polimórfico

bc_poli <- "hdom2~b0*((hdom1/b0)^((idade1/idade2)^b2))"

ajbcpoli <- nls(
  formula = bc_poli,
  data = dados,
  start = list(
    b0 = 50, 
    b2 = 0.5
  )
)

#Erro padrão residual
syxbcpoli <- summary( ajbcpoli )$sigma
syxperbcpoli <- syxbcpoli / mean( dados$hdom2 ) * 100

#coeficientes bc poli
cfbcpoli <- coef( ajbcpoli )

#Ajuste Bayley Clutter anamórfico

bc_ana <- "hdom2~hdom1/(exp(b1*((idade2^b2)-(idade1^b2))))"

ajbcana <- nls(
  formula = bc_ana,
  data = dados,
  start = list(
    b1 = 7.9378414, 
    b2 = -0.4497666
  )
)

#Erro padrão residual
syxbcana <- summary( ajbcana )$sigma
syxperbcana <- syxbcana / mean( dados$hdom2 ) * 100

#coeficientes bc poli
cfbcana <- coef( ajbcana )

# Ajustes Chapman e Richards --------------------------------------------------

# Ajuste Chapman e Richards polimórfico

cr_poli <- "hdom2~b0*((hdom1/b0)^((log(1-exp(b1*idade2)))/(log(1-exp(b1*idade1)))))"

ajcrpoli <- nls(
  formula = cr_poli,
  data = dados,
  start = list(
    b0 = 35.2288428, 
    b1 = -0.0221952
  )
)

#Erro padrão residual
syxcrpoli <- summary( ajcrpoli )$sigma
syxpercrpoli <- syxcrpoli / mean( dados$hdom2 ) * 100

#coeficientes cr poli
cfcrpoli <- coef( ajcrpoli )

#Ajuste Chapman e Richards anamórfico

cr_ana <- "hdom2~((hdom1*(1-exp(b1*(idade2))))/(1-exp(b1*(idade1))))^b2"

ajcrana <- nls(
  formula = cr_ana,
  data = dados,
  start = list(
    b1 = -0.01949213, 
    b2 = 1.05428713
  )
)

#Erro padrão residual
syxcrana <- summary( ajcrana )$sigma
syxpercrana <- syxcrana / mean( dados$hdom2 ) * 100

#coeficientes cr poli
cfcrana <- coef( ajcrana )

# Ajustes Schumacher --------------------------------------------------

# Ajuste Schumacher polimórfico

sch_poli <- "hdom2~b0*((hdom1/b0)^((idade1)/(idade2)))"

ajschpoli <- nls(
  formula = sch_poli,
  data = dados,
  start = list(
    b0 = 39.1903
  )
)

#Erro padrão residual
syxschpoli <- summary( ajschpoli )$sigma
syxperschpoli <- syxschpoli / mean( dados$hdom2 ) * 100

#coeficientes sch poli
cfschpoli <- coef( ajschpoli )

#Ajuste Schumacher anamórfico

sch_ana <- "hdom2~hdom1*(exp(b1*((1/idade2)-(1/idade1))))"

ajschana <- nls(
  formula = sch_ana,
  data = dados,
  start = list(
    b1 = 25.59895
  )
)

#Erro padrão residual
syxschana <- summary( ajschana )$sigma
syxperschana <- syxschana / mean( dados$hdom2 ) * 100

#coeficientes sch ana
cfschana <- coef( ajschana )


# Gráficos ----------------------------------------------------------------

#Calculando os resíduos

#Bayley clutter
dados$resbcpoli <- residuals( ajbcpoli ) #polimórfico
dados$resbcana <- residuals( ajbcana ) #anamórfico

#Chapman e Richards
dados$rescrpoli <- residuals( ajcrpoli ) #polimórfico
dados$rescrana <- residuals( ajcrana ) #anamórfico

#Schumacher
dados$resschpoli <- residuals( ajschpoli ) #polimórfico
dados$resschana <- residuals( ajschana ) #anamórfico

library( fBasics ) 
#gráficos com casca

par( mfrow = c( 3, 2 ) ) 
qqnormPlot( ( dados$resbcpoli ), title = FALSE, main = "Bayley Clutter Polimórfico" )
qqnormPlot( ( dados$resbcana ), title = FALSE, main = "Bayley Clutter Anamórfico" )
qqnormPlot( ( dados$rescrpoli ), title = FALSE, main = "Chapman e Richards Polimórfico" )
qqnormPlot( ( dados$rescrana ), title = FALSE, main = "Chapman e Richards Anamórfico" )
qqnormPlot( ( dados$resschpoli ), title = FALSE, main = "Schumacher Polimórfico" )
qqnormPlot( ( dados$resschana ), title = FALSE, main = "Schumacher Anamórfico" )

par( mfrow = c( 3, 2 ) )
with( dados, plot( idade1, resbcpoli, pch = 20,
              main = "Bayley Clutter Polimórfico",
              xlab = "meses",
              ylab = "Resíduos (m)",
              col = "red",
              ylim = c( -6.5, 6.5 ) ) )
abline( h = 0)

with(dados,plot(idade1,resbcana,pch= 20,
                main="Bayley Clutter Anamórfico",
                xlab="meses",
                ylab="Resíduos (m)",
                col="red",
                ylim = c( -6.5, 6.5 ) ) )
abline( h = 0)

with(dados,plot(idade1,rescrpoli,pch = 20,
                main="Chapman e Richards Polimórfico",
                xlab="meses",
                ylab="Resíduos (m)",
                col="red",
                ylim = c( -6.5, 6.5 ) ) )
abline( h = 0)

with(dados,plot(idade1,rescrana,pch = 20,
                main="Chapman e Richards Anamórfico",
                xlab="meses",
                ylab="Resíduos (m)",
                col="red",
                ylim = c( -6.5, 6.5 ) ) )
abline( h = 0)

with(dados,plot(idade1,resschpoli,pch = 20,
                main="Schumacher Polimórfico",
                xlab="meses",
                ylab="Resíduos (m)",
                col="red",
                ylim = c( -6.5, 6.5 ) ) )
abline( h = 0)

with(dados,plot(idade1,resschana,pch = 20,
                main="Schumacher Anamórfico",
                xlab="meses",
                ylab="Resíduos (m)",
                col="red",
                ylim = c( -6.5, 6.5 ) ) )
abline( h = 0)
