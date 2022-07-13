library( openxlsx )
library( cmrinvflor )

fustes <- read.xlsx( "exercícios/aplic_5grau_r_exercício_01.xlsx", sheet = "fustes" )
coefs <- read.xlsx( "exercícios/aplic_5grau_r_exercício_01.xlsx", sheet = "coefs" )
produtos <- read.xlsx( "exercícios/aplic_5grau_r_exercício_01.xlsx", sheet = "produtos" )

subseq <- 1

nonprod <- as.matrix( produtos[, 1] )
vprod <- as.matrix( produtos[, 2:5] )

procafill <- as.data.frame(
  multprodarvbt5grau( fustes, coefs, vprod, nonprod, subseq )
)

lapply( procafill[, c(2:4,15:18) ], FUN = sum )

wb <- loadWorkbook( "exercícios/aplic_5grau_r_exercício_01.xlsx" )
addWorksheet( wb, "resultado" )
writeData( wb, "resultado", procafill )
saveWorkbook( wb, "exercícios/aplic_5grau_r_exercício_01.xlsx", overwrite = TRUE )
