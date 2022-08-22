library(cmrinvflor);

prc<-read.csv2('aulas/aula_08/fluxo_caixa.csv');
View(prc);

#names(prc)<-c("projeto","idade","custo","receita","producao")

ii<-!duplicated(prc$projeto,prc$idade) & prc$idade!=0
cat<-prc[ii,]; cat$custo<-0; cat$receita<-0; cat$producao<-0;

tj<-0.10
vt=5000;
cat$custo<-vt*tj;
ae<-avecon(rbind(prc,cat),tj=tj,vt=vt,calc_cmpr = T);
#tj:Taxa de juros - TMA - Taxa de oportunidade
#vt:Valor da terra
#calc_cmpr: Calcular o custo médio de produção? T ou F

View(ae);

x11();
tj<-seq(0.06,0.12,0.02);
cat$custo<-vt*tj[1];
ae<-avecon(rbind(prc,cat),tj=tj[1],vt=vt,calc_cmpr = T);
plot(ae$projeto,ae$vplinf,ylim=c(0,30000),
     xlab='rotação',ylab='VPLinf',
     col=1,type='l');
text(x=ae$projeto,y=ae$vplinf,round(ae$tir*100,2),cex=0.8);

for(i in 2:length(tj)){
  cat$custo<-vt*tj[i];
  ae<-avecon(rbind(prc,cat),tj=tj[i],vt=vt,calc_cmpr = T);
  lines(ae$projeto,ae$vplinf,col=i);
  text(x=ae$projeto,y=ae$vplinf,round(ae$tir*100,2),cex=0.8);
}
legend('bottomright',legend=paste(tj*100,'%'),
       text.col=1:length(tj),
       horiz=T,bty='n')

