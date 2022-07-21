#'---
#'title: Aula - 05 - Manejo de Florestas Plantadas
#'subtitle: Ajuste do modelo de Kozak 2004
#'author: Lucas ( com minhas modificações de aula; Gabriel F. Pereira )
#'---

#' # Abrindo a base de dados
#' #### Arquivo `5grau.rda` possui resultados aplicados ao polinômio de quinto grau para comparação futura...

arv <- read.csv2( "cubfinal.csv" )
load( file = "5grau.rda" )

#' ### Criando uma função com o modelo proposto.
fkozak <- function( hi, dap, ht, b0, b1, b2, b3, b4, b5, b6, b7, b8 ) {
  return(b0*(dap**b1)*(ht**b2)*(((1-((hi/ht)^(1/3)))/(1-((1.3/ht)^(1/3))))**(b3*(hi/ht)**4+b4*(1/exp(dap/ht))+
        (b5*(((1-((hi/ht)^(1/3)))/(1-((1.3/ht)^(1/3))))**0.1))+(b6*(1/dap))+(b7*(ht**(1-((hi/ht)**(1/3)))))+
        (b8*((1-((hi/ht)^(1/3)))/(1-((1.3/ht)^(1/3))))))))
}


#' #### Aqui é feito o ajuste do modelo. Os valores dos betas apresentados são parâmetros
#' #### iniciais sob os quais o R deve trabalhar, esses valores podem ser encontrados na 
#' #### literatura. Nem sempre um valor obtido na literatura será aplicável à base de dados
#' #### que você possui, portanto, é sempre importante ter uma boa base literária.
kozak <- nls( dicc ~ fkozak(hi, dap, ht, b0, b1, b2, b3, b4, b5, b6, b7, b8 ), data = arv,
              start = list( b0 = 1.152499,
                            b1 = 0.985718,
                            b2 = -0.032988,
                            b3 = 0.211738,
                            b4 = -0.599772,
                            b5 = 0.691037,
                            b6 = 1.181447,
                            b7 = 0.077640,
                            b8 = -0.19 ) )


#' #### O objeto "ps" a seguir conterá os valores dos coeficientes (alfas e betas) obtidos no ajuste
ps <- as.list( coef( kozak ) )
summary( kozak )            


#' ### Estatísticas do modelo
n <- nrow( arv )
dicc_calc <- predict( kozak )
res_calc <- arv$dicc - dicc_calc

pdiccresunid <- function( hi, res, modelo, xlab = "hi(m)", ylim = range( -4, 4 ) ) {
  plot( hi, res, pch = 20, xlab = xlab, ylab = "Resíduo (cm)", 
        main = modelo, ylim = ylim, col = "black")
  abline( h = 0 )
  tvalor <- qt( 0.995, length( res ) -1 )
  abline( h = tvalor, lty = 2, col = "black" )
  abline( h = -tvalor, lty = 2, col = "black" )
}

windows( width = 1366, height = 768 )
pdiccresunid( arv$hi, res_calc, "Kozak (2004)")

graphics.off()


#' ### Integral de Kozak
fgkozak <- function( hi, dap, ht, b0, b1, b2, b3, b4, b5, b6, b7, b8 ) {
  return(pi/40000*(b0*(dap**b1)*(ht**b2)*(((1-((hi/ht)^(1/3)))/(1-((1.3/ht)^(1/3))))**(b3*(hi/ht)**4+
                   b4*(1/exp(dap/ht))+(b5*(((1-((hi/ht)^(1/3)))/(1-((1.3/ht)^(1/3))))**0.1))+(b6*(1/dap))+
                  (b7*(ht**(1-((hi/ht)**(1/3)))))+(b8*((1-((hi/ht)^(1/3)))/(1-((1.3/ht)^(1/3))))))))^2)
}
vkozak <- function( hi, dap, ht, b0, b1, b2, b3, b4, b5, b6, b7, b8 ){ 
  return(integrate(fgkozak,lower=0,upper=hi,dap=dap,ht=ht,b0=b0,b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6,b7=b7,b8=b8)$value) 
}
arv$vol_calc <- mapply( vkozak, hi = as.list(arv$hi),dap = as.list(arv$dap),
                        ht = as.list(arv$ht),
                        b0 = ps$b0,
                        b1 = ps$b1,
                        b2 = ps$b2,
                        b3 = ps$b3,
                        b4 = ps$b4, b5 = ps$b5,b6=ps$b6,b7=ps$b7,b8=ps$b8) 


#' #### resíduo = volume real - estimado
res.m3 <- arv$vcc - arv$vol_calc
#' #### criando uma função presunid (plot resíduo na unidade) onde obteremos 
#' #### o gráfico da distribuição dos resíduos para o cálculo de volume da equação

presunid <- function( hi, res, modelo, xlab="hi(m)", ylim = range( -0.05, 0.05 ) ){
  plot( hi, res, pch = 20, xlab = xlab,ylab="Resíduo (m³)", 
        main = modelo, ylim = ylim, col = "black" );
  abline( h = 0 );
}

# windows( width = 1366, height = 768 )
presunid( arv$hi,res.m3,"Kozak (2004)" )


#' ### Como devemos selecionar o melhor modelo?


#' ### Medidas de precisão:
sumario <- summary( kozak )
syx <- sumario$sigma
(syxp <- syx/mean(arv$dicc) * 100 )
syxquintograuporc
#' #### Erro padrão residual = 5.315% (Kozak)
#' #### Erro padrão residual: 6.555% (Quinto Grau)
#' #### Logo o modelo de Kozak foi melhor...


(RMSE <- with(arv,sqrt(sum((vcc-vol_calc)^2)/n)))
(syx_perc_vol <- with(arv,(RMSE/mean(vcc))*100))
syx_perc_quintograu.m3
#' #### erro_vol_perc = 4.65% (Kozak)
#' #### Para comparar: 6.21% (Quinto Grau)
#' #### Kozak melhor nessa métrica estatística também...

#' ### Analisando graficamente os modelos
#' #### O modelo de Kozak não tem heterogeneidade de variância pois o volume foi calculado de forma acumulada, 
#' #### mas o de quinto grau "abre" mais que o de Kozak: 
#' 
windows( width = 1366, height = 768 )
par( oma = c(3,3,0,0), mar = c(3,3,2,2), mfrow = c(2,2) )
pdiccresunid( arv$hi, res_calc, "Kozak (2004)") 
pdiccresunid( arv$hi, res.quintograu, "Schöepfer (1966)" )
presunid( arv$hi, res.m3, "Kozak (2004)" )
presunid( arv$hi, res.quintograu.m3, "Schöepfer (1966)" )
