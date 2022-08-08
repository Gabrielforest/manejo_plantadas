library( openxlsx )
library( cmrinvflor )

fustes <- read.xlsx( "aulas/aula_02/aplic_5grau_r.xlsx", sheet = "fustes" )
coefs <- read.xlsx( "aulas/aula_02/aplic_5grau_r.xlsx", sheet = "coefs" )
produtos <- read.xlsx( "aulas/aula_02/aplic_5grau_r.xlsx", sheet = "produtos" )

subseq <- 1

nomprod <- as.matrix( produtos[, 1] )
vprod <- as.matrix( produtos[, 2:5] )

procafill <- as.data.frame(
  multprodarvbt5grau( fustes, coefs, vprod, nonprod, subseq )
)

wb <- loadWorkbook( "aulas/aula_02/aplic_5grau_r.xlsx" )
addWorksheet( wb, "resultado" )
writeData( wb, "resultado", procafill )
saveWorkbook( wb, "aplic_5grau_r.xlsx", overwrite = TRUE )