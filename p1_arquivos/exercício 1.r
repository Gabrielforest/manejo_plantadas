
#Exerc�cio 1 - Prova de Manejo de Florestas Plantadas



#Importando arquivos de dados do excel
arvores=read.csv2('arvores.csv');
coeficientes=read.csv2('coeficientes.csv');
produtos=read.csv2('produtos1.csv');
library(cmrinvflor);
subseq=1
   
#informa��es para processamento

nomprod=as.matrix(produtos[ ,1]);
vprod=as.matrix(produtos[ ,2:5]);

#Fun��o para obten��o do n�mero de toras
procafil=multprodarvbt5grau(arvores,coeficientes,vprod,nomprod,subseq); procafil;

#### 6 toras por �rvore####
##Total = 36 toras ####

##para metro estereo multiplico o total por 1.6
