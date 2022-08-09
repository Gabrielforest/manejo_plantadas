
#Exercício 1 - Prova de Manejo de Florestas Plantadas



#Importando arquivos de dados do excel
arvores=read.csv2('arvores.csv');
coeficientes=read.csv2('coeficientes.csv');
produtos=read.csv2('produtos1.csv');
library(cmrinvflor);
subseq=1
   
#informações para processamento

nomprod=as.matrix(produtos[ ,1]);
vprod=as.matrix(produtos[ ,2:5]);

#Função para obtenção do número de toras
procafil=multprodarvbt5grau(arvores,coeficientes,vprod,nomprod,subseq); procafil;

#### 6 toras por árvore####
##Total = 36 toras ####

##para metro estereo multiplico o total por 1.6
