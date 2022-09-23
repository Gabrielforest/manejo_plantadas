### Ajustes dos modelos de classificação de sítios e análise estatistica dos ajustes
library( cmrinvflor )
library( ggplot2 )
library( magick )
library( cowplot )
library( gridExtra )
library( qqplotr )

dados_npar <- read.csv2( "trabalho_final/ifc.csv" )
names( dados_npar )
dados_npar <- dados_npar[ c( "parcela", "idade", "hdom", "ab", "vtsc", "vtcc" ) ]

dados <- cmrinvflor::parear_seqmed( dados_npar )
head( dados )

img <- image_read( "trabalho_final/chestreelogo.PNG" )

theme_chesTree <- theme_bw( ) +
  theme( panel.grid.major = element_line( color = "gray70", linetype = "dashed" ),
         panel.grid.minor = element_blank( ),
         axis.title = element_text( size = rel( 1.25 ) ),
         axis.text = element_text( size = rel( 1.1 ) ),
         legend.position = c( 0, 1 ),
         legend.justification = c( -0.05, 1.02 ),
         legend.title = element_blank( ),
         legend.text = element_text( size = rel( 0.8 ) ) )

p <- ggplot( dados, aes( x = idade2, y = hdom2, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5) +
  #labs( title = "Testando título" ) +
  #geom_hline( yintercept = 0, linetype = "dashed" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( name = "Altura Dominante (cm)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

ggdraw( ) +
  draw_plot( p, x = 0, y = 0.15, width = 1, height = 0.85 ) +
  draw_image( img, x = 0.65, y = 0.02, width = 0.35, height = 0.2 )  

# A curva no gráfico acima é conhecida como LOESS 
# (suavização de dispersão estimada localmente),
# e ela nos mostra o melhor ajuste sem assumir
# que os dados devem se ajustar a alguma forma de distribuição.
# Além disso, foi apresentado o erro padrão em torno da curva em cinza.

# par( mfrow = c( 1, 1 ) )
# with( dados, plot( hdom1, hdom2, pch = 20 ) )

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

#gráficos com casca

theme_chesTree <- theme_bw( ) +
  theme( panel.grid.major = element_line( color = "gray70", linetype = "dashed" ),
         panel.grid.minor = element_blank( ),
         axis.title = element_text( size = rel( 1.25 ) ),
         axis.text = element_text( size = rel( 0.6 ) ),
         legend.position = c( 0, 1 ),
         legend.justification = c( -0.05, 1.02 ),
         legend.title = element_blank( ),
         legend.text = element_text( size = rel( 1.1 ) ) )

bcnorm_poli <- ggplot( data = dados, mapping = aes( sample = resbcpoli ) ) +
  geom_qq_band( bandType = "ks", mapping = aes( fill = "KS" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "ts", mapping = aes( fill = "TS" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "pointwise", mapping = aes( fill = "Pointwise" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "boot", mapping = aes( fill = "Bootstrap" ), alpha = 0.5 ) +
  stat_qq_line( ) +
  stat_qq_point( ) +
  labs( title = "Bayley-Clutter Polimórfico",
        x = "Quantis Normais", 
        y = "Quantis de Amostra" ) +
  scale_fill_discrete( "Faixas de Confiança" ) +
  theme_chesTree

bcnorm_ana <- ggplot( data = dados, mapping = aes( sample = resbcana ) ) +
  geom_qq_band( bandType = "boot", mapping = aes( fill = "Bootstrap" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "pointwise", mapping = aes( fill = "Normal" ), alpha = 0.5 ) +
  stat_qq_line( ) +
  stat_qq_point( ) +
  labs( title = "Bayley-Clutter Anamórfico",
        x = "Quantis Normais", 
        y = "Quantis de Amostra" ) +
  scale_fill_discrete( "Faixas de Confiança" )

crnorm_poli <- ggplot( data = dados, mapping = aes( sample = rescrpoli ) ) +
  geom_qq_band( bandType = "boot", mapping = aes( fill = "Bootstrap" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "pointwise", mapping = aes( fill = "Normal" ), alpha = 0.5 ) +
  stat_qq_line( ) +
  stat_qq_point( ) +
  labs( title = "Chapman-Richards Polimórfico",
        x = "Quantis Normais", 
        y = "Quantis de Amostra" ) +
  scale_fill_discrete( "Faixas de Confiança" )

crnorm_ana <- ggplot( data = dados, mapping = aes( sample = rescrana ) ) +
  geom_qq_band( bandType = "boot", mapping = aes( fill = "Bootstrap" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "pointwise", mapping = aes( fill = "Normal" ), alpha = 0.5 ) +
  stat_qq_line( ) +
  stat_qq_point( ) +
  labs( title = "Chapman-Richards Anamórfico",
        x = "Quantis Normais", 
        y = "Quantis de Amostra" ) +
  scale_fill_discrete( "Faixas de Confiança" )

scnorm_poli <- ggplot( data = dados, mapping = aes( sample = resschpoli ) ) +
  geom_qq_band( bandType = "boot", mapping = aes( fill = "Bootstrap" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "pointwise", mapping = aes( fill = "Normal" ), alpha = 0.5 ) +
  stat_qq_line( ) +
  stat_qq_point( ) +
  labs( title = "Shcumacher Polimórfico",
        x = "Quantis Normais", 
        y = "Quantis de Amostra" ) +
  scale_fill_discrete( "Faixas de Confiança" )

scnorm_ana <- ggplot( data = dados, mapping = aes( sample = resschana ) ) +
  geom_qq_band( bandType = "boot", mapping = aes( fill = "Bootstrap" ), alpha = 0.5 ) +
  geom_qq_band( bandType = "pointwise", mapping = aes( fill = "Normal" ), alpha = 0.5 ) +
  stat_qq_line( ) +
  stat_qq_point( ) +
  labs( title = "Schumacher Anamórfico",
        x = "Quantis Normais", 
        y = "Quantis de Amostra" ) +
  scale_fill_discrete( "Faixas de Confiança" )

# "pointwise" constrói bandas de confiança simultâneas com base na distribuição normal;
# "boot" cria bandas de confiança pontuais com base em um boostrap paramétrico;
# "ks" constrói bandas de confiança simultâneas com base em uma inversão do teste de Kolmogorov-Smirnov;
# "ts" constrói bandas de confiança sensíveis à cauda, como proposto por Aldor-Noiman et al. (2013).

arranged_norm <- grid.arrange( bcnorm_ana, 
                               crnorm_poli, crnorm_ana,
                               scnorm_poli, scnorm_ana,
                               ncol = 2, nrow = 3 )

# anexos
ggdraw( ) +
  draw_plot( arranged_norm, x = 0, y = 0.15, width = 1, height = 0.85 ) +
  draw_image( img, x = 0.55, y = 0.02, width = 0.35, height = 0.5 )  


### Resíduos

res_plot_bcpoli <- ggplot( dados, aes( x = idade1, y = resbcpoli, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5 ) +
  labs( title = "Bayley-Clutter Polimórfico" ) +
  geom_hline( yintercept = 0, col = "black" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( limits = c( -8, 8 ), breaks = c( -8:8  ), name = "Resíduos (m)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

# anexos
res_plot_bcana <- ggplot( dados, aes( x = idade1, y = resbcana, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5 ) +
  labs( title = "Bayley-Clutter Anamórfico" ) +
  geom_hline( yintercept = 0, col = "black" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( limits = c( -8, 8 ), breaks = c( -8:8  ), name = "Resíduos (m)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

res_plot_crpoli <- ggplot( dados, aes( x = idade1, y = rescrpoli, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5 ) +
  labs( title = "Chapman-Richards Polimórfico" ) +
  geom_hline( yintercept = 0, col = "black" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( limits = c( -8, 8 ), breaks = c( -8:8  ), name = "Resíduos (m)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

arranged_res_norm <- grid.arrange( crnorm_poli, res_plot_crpoli, ncol = 2 )

ggdraw( ) +
  draw_plot( arranged_res_norm, x = 0, y = 0.15, width = 1, height = 0.85 ) +
  draw_image( img, x = 0.35, y = 0.02, width = 0.35, height = 0.2 )  


res_plot_crana <- ggplot( dados, aes( x = idade1, y = rescrana, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5 ) +
  labs( title = "Chapman-Richards Anamórfico" ) +
  geom_hline( yintercept = 0, col = "black" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( limits = c( -8, 8 ), breaks = c( -8:8  ), name = "Resíduos (m)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

res_plot_schpoli <- ggplot( dados, aes( x = idade1, y = resschpoli, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5 ) +
  labs( title = "Schumacher Polimórfico" ) +
  geom_hline( yintercept = 0, col = "black" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( limits = c( -8, 8 ), breaks = c( -8:8  ), name = "Resíduos (m)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

res_plot_schana <- ggplot( dados, aes( x = idade1, y = resschana, color = "lightgreen" ) ) +
  geom_point( size = 1.5 , alpha = 0.5 ) +
  labs( title = "Schumacher Anamórfico" ) +
  geom_hline( yintercept = 0, col = "black" ) +
  geom_smooth( se = TRUE, method = "loess", formula = "y ~ x" ) +
  scale_y_continuous( limits = c( -8, 8 ), breaks = c( -8:8  ), name = "Resíduos (m)" ) +
  scale_x_continuous( name = "Idade (meses)" ) +
  scale_color_manual( values = c( "#50B23D","#0072B2" ), guide = "none" ) +
  scale_fill_manual( values = c( "#E69F00","#0072B2" ), guide = "none" ) +
  theme_chesTree

arranged_res <- grid.arrange( res_plot_bcpoli, res_plot_bcana, 
                              res_plot_crana,
                              res_plot_schpoli, res_plot_schana,
                              ncol = 2, nrow = 3 )
# anexos
ggdraw( ) +
  draw_plot( arranged_res, x = 0, y = 0.15, width = 1, height = 0.85 ) +
  draw_image( img, x = 0.58, y = 0.02, width = 0.35, height = 0.5 )  

margin <- theme( plot.margin = unit( c( 2, 2, 2, 2 ), "cm" ) )
