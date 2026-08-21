catcat <- function(x,y,nomex,nomey,niveisx=F,niveisy=F,dig=2,respcol=T,excluirtotal=T,cor="cyan4",idioma="PT",labels=T,virgula=F){
  
  ref=nomex
  if(respcol==T) linhacol=1 else linhacol=2
  
  if(niveisx[1]==F) niveisx=names(table(x))
  if(niveisy[1]==F) niveisy=names(table(y))
  
  x=factor(unlist(x),levels=niveisx)
  y=factor(unlist(y),levels=niveisy)
  
  help=data.frame(x,y)
  names(help)=c("x","y")
  signif=F
  tex=NULL; contor=NULL
  
  if(dim(table(help))[1]==1 | dim(table(help))[2]==1) {testes=NULL; tabela=NULL ; p="N/A" ; result=NULL ; grafico=NULL ; texto=c(" * **",ref,":** Não é possível realizar testes de comparações com variáveis com apenas uma categoria.","\n")} else {
    
    tabela <- table(help$x, help$y)
    quiqua2 <- chisq.test(tabela)
    
    if(dim(tabela)[1]==2 & dim(tabela)[2]==2) doispordois=TRUE else doispordois=FALSE
    
    if(sum(quiqua2$expected<5)/(nrow(tabela)*ncol(tabela))>0.2 | sum(quiqua2$expected<1)>0)
    {pvalorc=fisher.test(help$x, help$y,simulate.p.value = T)$p.value
    resumotestes=ifelse(pvalorc<0.05, paste0("Encontramos associação estatisticamente significativa entre ",nomex," e ",nomey," (Exato de Fisher, ",pvalor(pvalorc),")"),paste0("Não encontramos associação estatisticamente significativa entre ",nomex," e ",nomey," (Exato de Fisher, ",pvalor(pvalorc),")"))
    method="fisher" ; pvalorgraf=ifelse(pvalorc < 0.001, "<0.001", round(pvalorc,3)); metodograf=ifelse(idioma=="PT","Exato de Fisher ","Fisher's Exact Test")
    textograf=substitute(paste(metodograf," (p=", pvalorgraf,")"), list(metodograf=metodograf,pvalorgraf=pvalorgraf))
    p=paste0(pvalor(pvalorc),"b")
    
    
    if(fisher.test(help$x, help$y,simulate.p.value = T)$p.value>0.05) {result=NULL
    texto=c(" * **",ref,":** Não encontramos evidências para rejeitar a ausência de associação entre variáveis pelo do teste Exato de Fisher. \n")} else {
      signif=T
      library(rcompanion)
      result=pairwiseNominalIndependence(tabela,fisher=T,chisq=F, gtest=F,digits=3,simulate.p.value = T)
      texto=c(" * **",ref,":** A associação entre as variáveis foi testada através do teste Exato de Fisher, que encontrou evidências para rejeitar a hipótese de ausência de associação. As categorias que apresentaram diferenças estatisticamente significativas foram.","\n")
    }}   else{
      metodograf=ifelse(idioma=="PT","Qui-quadrado ","Chi-square ")
      pvalorc=quiqua2$p.value
      resumotestes=ifelse(quiqua2$p.value<0.05,paste0("Encontramos associação estatisticamente significativa entre ",nomex," e ",nomey," (",paste("$\\chi^2$",collapse=NULL),"(",quiqua2$parameter,") = ", round(quiqua2$statistic,dig)," p=", pvalor(quiqua2$p.value),")",collapse="",sep=""),paste0("Não encontramos associação estatisticamente significativa entre ",nomex," e ",nomey," (",paste("$\\chi^2$",collapse=NULL),"(",quiqua2$parameter,") = ", round(quiqua2$statistic,dig)," p=", pvalor(quiqua2$p.value),")",collapse="",sep=""))
      para=round(quiqua2$parameter,dig) ; stat=round(quiqua2$statistic,dig) ; pvalorgraf=ifelse(quiqua2$p.value < 0.001, "<0.001", round(quiqua2$p.value,3))
      textograf=substitute(paste(metodograf," (",chi^2,"(",para,") = ",stat ," p=", pvalorgraf,")"), list(metodograf=metodograf,para=para,stat=stat,pvalorgraf=pvalorgraf))
      
      method="wald"
      p=paste0(pvalor(quiqua2$p.value),"a (v=",round(rcompanion::cramerV(help$x,help$y),dig),")")
      
      if(quiqua2$p.value>0.05) {result=NULL
      texto=c(" * **",ref,":** A associação foi investigada por um Teste Qui-quadrado de independência, que não encontrou indícios de associação (",paste("$\\chi^2$",collapse=NULL),"(",quiqua2$parameter,") = ", round(quiqua2$statistic,dig)," p=", pvalor(quiqua2$p.value),").","\n")} else {
        
        signif=T
        ef=rcompanion::cramerV(help$x,help$y)
        par <- min(nrow(tabela)-1,ncol(tabela)-1)
        
        if(par==1){
          if(ef<0.1) efeito="que pode ser considerado desprezível apesar de ser estatisticamente significativo. Cabe verificar do ponto de vista prático o impacto desta associação." else
            if(ef<0.3) efeito="que pode ser considerado um efeito pequeno." else 
              if(ef<0.5) efeito="que pode ser considerado um efeito médio." else
                efeito="que pode ser considerado um efeito grande."} else
                {if(par==2){
                  if(ef<0.07) efeito="que pode ser considerado desprezível apesar de ser estatisticamente significativo. Cabe verificar do ponto de vista prático o impacto desta associação." else
                    if(ef<0.21) efeito="que pode ser considerado um efeito pequeno." else 
                      if(ef<0.35) efeito="que pode ser considerado um efeito médio." else
                        efeito="que pode ser considerado um efeito grande."} else
                        {if(ef<0.06) efeito="que pode ser considerado desprezível apesar de ser estatisticamente significativo. Cabe verificar do ponto de vista prático o impacto desta associação." else
                          if(ef<0.17) efeito="que pode ser considerado um efeito pequeno." else 
                            if(ef<0.29) efeito="que pode ser considerado um efeito médio." else
                              efeito="que pode ser considerado um efeito grande."}}
        
        
        texto=c(" * **",ref,":** A associação foi investigada por um Teste Qui-quadrado de independência. Os resultados indicaram que as variáveis são associadas (",paste("$\\chi^2$",collapse=NULL),"(",quiqua2$parameter,") = ", round(quiqua2$statistic,dig)," p=", pvalor(quiqua2$p.value), "). O tamanho do efeito foi calculado pelo V de Cramer (",par,")=", round(ef,dig),"),",efeito,"\n")
        
        n=nrow(tabela)
        corte=qnorm(0.05/(nrow(tabela)*ncol(tabela))/2)
        
        sig=matrix("",nrow(tabela),ncol(tabela))
        for (i in 1:nrow(tabela))
          for (j in 1:ncol(tabela))
            if(abs(quiqua2$stdres[i,j]) > abs(corte)) sig[i,j]="*"
        
        mat = matrix(paste0(round(quiqua2$stdres,dig),sig),ncol=ncol(tabela))
        
        nomes=NULL
        for (i in 1:n)
          nomes <- c(nomes,rownames(tabela)[i],rep("",2))
        
        tab<-NULL
        for (linha in 1:n){
          tab <- rbind(tab,rbind(quiqua2$observed[linha,],round(quiqua2$expected[linha,],dig),mat[linha,]))}
        
        result=data.frame("Categoria"=nomes,"Estatística"=rep(c("Observado","Esperado","Resíduos ajustados"),times=n),tab)
      }}
    if(doispordois==T & signif==T){
      
      if (linhacol==1){
        
        OR=round((tabela[1,1]/tabela[1,2]) / (tabela[2,1]/tabela[2,2]),2)
        
        tex=c("Calculamos a OR (odds ratio ou razão de chances),  que compara a chance do desfecho '",names(table(help$y))[1],"' na variável ",nomey," no grupo ",names(table(help$x))[1]," da variável ",nomex," (",tabela[1,1],"/",tabela[1,2]," = ",round(tabela[1,1]/tabela[1,2],2) ,") e a chance do mesmo desfecho no grupo ",names(table(help$x))[2]," da variável ",nomex," (",tabela[2,1],"/",tabela[2,2]," = ",round(tabela[2,1]/tabela[2,2],dig),"). Assim, a OR=",OR," IC95%=(",round(epitools::oddsratio(tabela,method=method)$measure[2,][2],dig),",",round(epitools::oddsratio(tabela,method=method)$measure[2,][3],dig),")")
        
        if(OR>1) contor= c(" indica ",OR," vezes mais chance do desfecho ",names(table(help$y))[1]," na variável ",nomey," no grupo ",names(table(help$x))[1]," da variável ",nomex," que no grupo ",names(table(help$x))[2],". OBS: Não podemos dizer que uma coisa CAUSE a outra, apenas que estão associadas. \n") else contor= c(" indica que a chance do desfecho ",names(table(help$y))[1]," na variável ",nomey," no grupo ",names(table(help$x))[1]," da ",nomex," é igual a ",OR," vezes a chance do mesmo desfecho no grupo ",names(table(help$x))[2],", representando uma diminuição de ",100*(1-OR),"%. OBS: Não podemos dizer que uma coisa CAUSE a outra, apenas que estão associadas.","\n")} else
          
        {OR=round((tabela[1,1]/tabela[2,1]) / (tabela[1,2]/tabela[2,2]),dig)
        
        tex=c("Calculamos a OR (odds ratio ou razão de chances),  que compara a chance do desfecho '",names(table(help$x))[1],"' na variável ",nomex," no grupo ",names(table(help$y))[1]," da variável ",nomey," (",tabela[1,1],"/",tabela[2,1]," = ",round(tabela[1,1]/tabela[2,1],2) ,") e a chance do mesmo desfecho no grupo ",names(table(help$y))[2]," da variável ",nomey," (",tabela[1,2],"/",tabela[2,2]," = ",round(tabela[1,2]/tabela[2,2],dig),"). Assim, a OR=",OR," IC95%=(",round(epitools::oddsratio(tabela,method=method)$measure[2,][2],dig),",",round(epitools::oddsratio(tabela,method=method)$measure[2,][3],dig),")")
        
        if(OR>1) contor= c(" indica ",OR," vezes mais chance do desfecho ",names(table(help$x))[1]," na variável ",nomex," no grupo ",names(table(help$y))[1]," da variável ",nomey," que no grupo ",names(table(help$y))[2],". OBS: Não podemos dizer que uma coisa CAUSE a outra, apenas que estão associadas. \n") else contor= c(" indica que a chance do desfecho ",names(table(help$x))[1]," na variável ",nomex," no grupo ",names(table(help$y))[1]," da ",nomey," é igual a ",OR," vezes a chance do mesmo desfecho no grupo ",names(table(help$y))[2],", representando uma diminuição de ",100*(1-OR),"%. OBS: Não podemos dizer que uma coisa CAUSE a outra, apenas que estão associadas.","\n")}}
    
    if(respcol==F) {indep=help$y ;  nomeindep=nomey ; dep=help$x ; nomedep=nomex} else {indep=help$x ;  nomeindep=nomex ; dep=help$y ; nomedep=nomey}
    
    grafico = grafico_catcat(indep,nomeindep,dep,nomedep,cor=cor,textograf,idioma,labels,virgula)
  
  testes = data.frame(Nome1 = nomey, Nome2 = nomex, tipo = paste0("cc_",ifelse(method=="fisher","e","q")), sig_ou_não = ifelse(pvalorc < 0.05, T, F), resumo = resumotestes, sup = NA)
   
  }
  
  if(is.null(result)==T) texto = paste(c(texto,tex,contor),collapse="") else texto = list(paste(c(texto,tex,contor),collapse=""),result)
  
  res=desc_bi_cat(help$x,F,help$y,F,F,dig,respcol)
  tot=dim(na.omit(help))[1]
  
  if(excluirtotal==T) res=res[-1,]
  
  res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2]-1)),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))
  
  return(list("testes"=testes,
              "result"=res,
              "grafico"=grafico,
              "texto"=texto))}
