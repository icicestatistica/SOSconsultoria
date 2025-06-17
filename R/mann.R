mann <- function(x,y,nomex,nomey,niveis='auto',dig=2,respcol=T,excluirtotal=F,cor="cyan4", idioma="PT",ordinal=F, niveis_ord='auto',virgula=F){

 x=unlist(x)
 y=unlist(y)
if(respcol==T) ref=nomex else ref=nomey  
  
d=data.frame("x"=x,"y"=y)
if(niveis[1]=='auto') niveis = names(table(d$x))
if(ordinal==T) {if(niveis_ord[1]=='auto') niveis_ord = names(table(d$y))
                d$y = factor(d$y, levels=niveis_ord)}
 
y1=as.numeric(d$y[d$x==niveis[1]])
y2=as.numeric(d$y[d$x==niveis[2]])
  
if(ordinal==F) a=wilcox.test(as.numeric(d$y) ~ d$x) else a=wilcox.test(as.numeric(factor(d$y, levels=niveis_ord)) ~ d$x)
 
if(length(na.omit(y1))>3 & length(na.omit(y1))<5000 & length(table(na.omit(y1)))>1) str1=shapiro.test(y1) else str1="Não"
if(length(na.omit(y2))>3 & length(na.omit(y2))<5000 & length(table(na.omit(y2)))>1) str2=shapiro.test(y2) else str2="Não"

if (ordinal==F) r=rcompanion::wilcoxonR(as.numeric(d$y), g = d$x, ci = F) else r=rcompanion::wilcoxonR(as.numeric(factor(d$y, levels=niveis_ord)), g = d$x, ci = F)

if(r<0) dif="menor que" else dif="maior que"

if (abs(r)<0.1) mag=paste0(" Através da estatística r (",round(r,dig),"), verificamos a magnitude da diferença detectada menor que 0.1, o que foi classificado em Fritz et al., 2011 como desprezível, mesmo que seja estatisticamente significativa.") else
  if(abs(r)<0.3) mag=paste0(" Através da estatística r (",round(r,dig),"), verificamos a magnitude da diferença detectada entre 0.1 e 0.3, o que foi classificado em Fritz et al., 2011 como um efeito pequeno.") else
    if(abs(r)<0.5) mag=paste0(" Através da estatística r (",round(r,dig),"), verificamos a magnitude da diferença detectada entre 0.3 e 0.5, o que foi classificado em Fritz et al., 2011 como um efeito médio.") else
      mag=paste0(" Através da estatística r (",round(r,dig),"), verificamos a magnitude da diferença detectada maior que 0.5, o que foi classificado em Fritz et al., 2011 como um efeito grande.")

if(ordinal==T) sup=paste0(c(" O motivo da escolha do teste de Mann Whitney é a natureza ordinal da variável '",nomey,"'."),sep="",collapse="") else  {
if(str1[1]=="Não" || str2[1]=="Não") sup=c(" A suposição de normalidade das amostras não pode ser verificada pelo teste de Shapiro-Wilk, uma vez que o tamanho de pelo menos uma das amostras foi menor que 3 ou maior que 5000. Por este motivo, recomendamos a utilização do teste não paramétrico de Mann-Whitney ao invés do tradicional teste-t") else{
if(str1$p.value>=0.05 & str2$p.value>=0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor maior que 0.05 para as duas amostras, não rejeitamos a normalidade das distribuições (",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),", ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),"), por este motivo o teste-t também pode ser utilizado, com muitas vantagens do ponto de vista de interpretação do resultado.") else
if(str1$p.value>=0.05 & str2$p.value<0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor menor que 0.05 rejeitamos a normalidade da distribuição de ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),", mas não de ",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),", o que justifica a realização do teste Mann-Whitney ao invés do tradicional teste-t.")
if(str1$p.value<0.05 & str2$p.value>=0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor menor que 0.05 rejeitamos a normalidade da distribuição de ",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),", mas não de ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),", o que justifica a realização do teste Mann-Whitney ao invés do tradicional teste-t.")
if(str1$p.value<0.05 & str2$p.value<0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor menor que 0.05 rejeitamos a normalidade da distribuição dos dois grupos (",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),",  ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),"), o que justifica a realização do teste Mann-Whitney ao invés do tradicional teste-t.")}}

if(a$p.value < 0.05) {texto=c(" * **",ref,":** Realizando o teste de Mann Whitney (W=",as.numeric(c(a$statistic)),", p=",pvalor(a$p.value),"), rejeitamos a hipótese de igualdade de distribuições de ",nomey," entre os grupos. ")
if(ordinal==F) texto = c(texto,
"O grupo ",niveis[1]," (mediana=",round(median(y1, na.rm=T),dig)," e intervalo interquartil = [",
round(quantile(y1,0.25,na.rm=T),dig),",",round(quantile(y1,0.75,na.rm=T),dig),"]) tem ",nomey," ",dif," que o grupo ",niveis[2]," (mediana=",round(median(y2, na.rm=T),dig)," e intervalo interquartil = [",
round(quantile(y2,0.25,na.rm=T),dig),",",round(quantile(y2,0.75,na.rm=T),dig),"]).",mag,sup,"\n")} else
  texto= c(" * **",ref,":** Não encontramos evidência estatística através do teste de Mann Whitney para rejeitar diferenças entre as distribuições de ",nomey," dos grupos (W=",as.numeric(c(a$statistic)),", p=",pvalor(a$p.value),").",sup,"\n")

p=paste0(pvalor(a$p.value),"d (r=",round(r,dig),")")
tot=dim(na.omit(d))[1]
 
if(ordinal==F) res=desc_bi_cont(d$y,d$x,F,respcol,F,dig) else 
   {if(respcol==F) res=desc_bi_cat(linha=d$y,col=d$x,respcol=F) else res=desc_bi_cat(linha=d$x,col=d$y,respcol=T)}
if(excluirtotal==T) res=res[-1,]
textograf <- paste0("Mann Whitney (W=",as.numeric(c(a$statistic)),", p",ifelse(a$p.value<0.001,"<0.001",paste0("=",round(a$p.value,3),collapse="")),")",collapse="")
  
res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2]-1)),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))
labc = ifelse(length(table(d$y))>4,F,T)
 if (ordinal == F) grafico = grafico_comp_box(d$y,nomey,d$x,nomex,cor=cor,textograf,dig, idioma=idioma,virgula=virgula) else 
   grafico = grafico_catcat(d$x, nomex, d$y, nomey, cor=cor, textograf, idioma=idioma, labels=ifelse(length(table(d$x))>4,F,T),virgula=virgula)

resumo=ifelse(a$p.value<0.05,paste0("Quanto a '",nomex,"', '",niveis[1],"' apresentou '",nomey,"' ",dif," '",niveis[2], paste0("' (Mann Whitney W=",as.numeric(c(a$statistic)),", p",ifelse(a$p.value<0.001,"<0.001",paste0("=",round(a$p.value,3),collapse="")),")",collapse="")),paste0("Quanto a '",nomex,"', não há diferença entre '",nomey, "' de '",niveis[1],"' e '",niveis[2], paste0("' (Mann Whitney W=",as.numeric(c(a$statistic)),", p",ifelse(a$p.value<0.001,"<0.001",paste0("=",round(a$p.value,3),collapse="")),")",collapse="")))
testes=data.frame(Nome1 = nomey, Nome2 = nomex, tipo = "mw", sig_ou_não = ifelse(a$p.value<0.05,T,F), resumo = resumo,sup=NA)

return(list("testes"=testes,
            "result"=res,
            "texto"=list("tex"=paste(texto,collapse="")),
            "grafico"=grafico))}
