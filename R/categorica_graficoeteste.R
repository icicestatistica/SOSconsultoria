desc_uni_categorica <- function(variavel,nome,niveis='auto',nas=F,label=F,ordenar=T,acumula=T,teste=F,grafico=T,cor="cyan4",digitos=2,virgula=F){
  variavel=unlist(variavel)
  if (niveis[1]=='auto') niveis = names(table(variavel))
  variavel <- factor(variavel, levels = niveis)
  
  tablevar = table(variavel)
  if(ordenar==T) tablevar = table(factor(variavel,levels=names(tablevar)[order(tablevar,decreasing=T)]))
  prop = 100*round(prop.table(tablevar),3)
  
  res=c()
  for (i in 1:(length(tablevar)-1)) res = c(res, paste(names(tablevar)[i]," (",prop[i],"%)",sep=""))
descri = paste(paste(res,collapse=", ")," e ",paste(names(tablevar)[length(tablevar)]," (",prop[length(tablevar)],"%).",sep=""))

  interpretacao = paste(" + Em relação à variável **'",nome,"'**, tivemos os grupos ",descri,sep="")
  interp_resumo = paste(nome," se dividiu nos grupos ",descri,sep="")
  
  if (nas==FALSE) {d<-data.frame(t(rbind(round(tablevar,0),paste0(round(100*prop.table(tablevar),digitos),"%"))))} else
  {d<-t(rbind(round(tablevar,0),paste0(round(100*tablevar/length(variavel),digitos),"%")))
   d <- rbind(d, "N/A"=c(sum(is.na(variavel)),paste0(round(100*sum(is.na(variavel))/length(variavel),digitos),"%")))}
  if (ordenar==TRUE) {d <- d[order(as.numeric(d[,1]),decreasing = T),]}
  if (label==TRUE) {d <- data.frame(d, "Freq."=paste0(d[,1]," (",d[,2],")"))}
  if (acumula==TRUE) {d <- data.frame(d,"Freq. Relativa Acumulada"= paste0(cumsum(d[,1])," (", round(100*cumsum(d[,1])/(cumsum(d[,1])[nrow(d)]),digitos),"%)"))}
  d <- data.frame(row.names(d),d)
  colnames(d) <- c("Característica","Frequência","Freq. Relativa","Freq.","Freq. Acumulada")[c(T,T,T,label,acumula)]
  row.names(d)=NULL
  if(teste==F){testectexto=NULL;testectabela=NULL} else {
    testec=quiqua_aderencia(variavel,nome,niveis,ordenar,digitos)
    if(length(testec)==1) {testectexto=testec$texto ; testectabela=NULL} else  {testectexto=testec$texto ; testectabela=testec$tabela}}
  if(grafico==T) {
      if(sum(nchar(niveis)) < 80) graficoc=grafico_categorica(variavel,nome,niveis,cor,ordenar,virgula) else graficoc = grafico_categorica_vert(variavel,nome,niveis,cor,ordenar,virgula)} else graficoc=NULL
  
    testes = data.frame(Nome1 = "", Nome2 = nome, tipo = ifelse(ordenar==T,"factor","ordinal"), 
        sig_ou_não = '-', resumo = interp_resumo, sup = NA)
    
    resultados=list("testes"=testes,"result"=d,"texto"=testectexto,"interp"=interpretacao,"tabela"=testectabela,"grafico"=graficoc)
  return(resultados)}

grafico_categorica = function(var,nome,niveis = "auto",cor = "cyan4",ordenar = T,virgula = F) {
    require(dplyr)
    require(forcats)

    var = unlist(var)

    if (niveis[1] == "auto")
        niveis = names(table(var))

    var = factor(var, levels = niveis)
    niveisnovo = vetor_comsep_c(niveis, 11)
    levels(var) = niveisnovo

    tab <- data.frame(
        table(var),
        perc = paste0(
            table(var),
            paste0(" (", 100 * round(prop.table(table(var)), 3), "%)")
        ),
        prop = paste0(
            table(var),
            paste0(
                "\n  (",
                100 * round(prop.table(table(var)), 3),
                "%)"
            )
        )
    )

    if (ordenar == T) {
        if (length(niveis) > 2) {
            result <- na.omit(tab) %>%
                mutate(
                    var = fct_reorder(var, desc(Freq))
                ) %>%
                ggplot() +
                geom_bar(
                    aes(x = var, y = Freq),
                    fill = cor,
                    stat = "identity"
                ) +
                ylim(0, max(table(var)) * 1.1) +
                theme_clean() +
                ylab("") +
                xlab("") +
                ggtitle(
                    paste0(
                        vetor_comsep_c(nome, 50),
                        " (n=",
                        length(na.omit(var)),
                        ")",
                        collapse = ""
                    )
                ) +
                geom_text(
                    aes(x = var, y = Freq),
                    label = ponto_para_virgula(tab$perc, virgula),
                    vjust = -0.5
                ) +
                theme(
                    plot.background = element_rect(colour = "white"),
                    axis.text.x = element_text(size = 12),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        } else {
            result = ggplot(tab, aes(x = "", y = Freq, fill = var)) +
                geom_bar(stat = "identity", width = 1) +
                coord_polar("y", start = 0) +
                theme_void(base_size = 12) +
                labs(
                    fill = "",
                    title = paste0(
                        vetor_comsep_c(nome, 50),
                        " (n=",
                        length(na.omit(var)),
                        ")"
                    )
                ) +
                theme(
                    plot.title = element_text(
                        hjust = 0.5,
                        size = ceiling(12 * 1.1),
                        face = "bold"
                    ),
                    plot.subtitle = element_text(
                        size = ceiling(12 * 1.05)
                    ),
                    plot.background = element_rect(colour = "white")
                ) +
                geom_text(
                    aes(
                        label = ponto_para_virgula(prop, virgula)
                    ),
                    color = "white",
                    position = position_stack(vjust = 0.5)
                ) +
                scale_fill_manual(
                    labels = vetor_comsep_c(niveis, 11),
                    values = lighten(
                        cor,
                        seq(
                            0,
                            0.3,
                            (0.3 / (length(tab$var) - 1))
                        )
                    )
                )
        }
    }

    if (ordenar == F) {
        result <- ggplot(tab) +
            geom_bar(
                aes(x = var, y = Freq),
                fill = cor,
                stat = "identity"
            ) +
            ylim(0, max(table(var)) * 1.1) +
            theme_clean() +
            ylab("") +
            xlab("") +
            ggtitle(
                paste0(
                    vetor_comsep_c(nome, 50),
                    " (n=",
                    length(na.omit(var)),
                    ")",
                    collapse = ""
                )
            ) +
            geom_text(
                aes(x = var, y = Freq),
                label = ponto_para_virgula(tab$perc, virgula),
                vjust = -0.5
            ) +
            theme(
                plot.background = element_rect(fill = NA, colour = NA),
                axis.text.x = element_text(size = 12),
                plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
            )
    }

    return(result)
}

grafico_categorica_vert = function (lesoes, nome, niveis = "auto", cor = "cyan4", ordenar = T, 
    virgula = F) 
{
    lesoes = unlist(lesoes)
    if (niveis[1] == "auto") 
        niveis = names(table(lesoes))
    df = table(lesoes) %>% data.frame()
    df$Freq_rel = prop.table(table(lesoes)) %>% data.frame() %>% 
        select("Freq") %>% unlist()
    df$lesoes = vetor_comsep_c(df$lesoes, 25)
    if (ordenar == T) 
        df = df %>% arrange(desc(-Freq)) %>% mutate(lesoes = factor(lesoes, 
            levels = unique(lesoes))) else df = df %>% mutate(lesoes = factor(lesoes, levels = vetor_comsep_c(niveis,25)))
    plot = df %>% ggplot(aes(y = lesoes, x = Freq_rel)) + geom_bar(stat = "identity", 
        fill = cor) + theme_icic("v") + geom_text(aes(label = ponto_para_virgula(paste0(Freq," (",100 * 
        round(Freq_rel, 2), "%)"), virgula),x=Freq_rel+max(df$Freq_rel)/10)) + 
        scale_x_continuous(labels = scales::percent_format(), 
            expand = expansion(mult = c(0, 0.05)), limits=c(0,max(df$Freq_rel)+0.13)) + labs(y = NULL, 
        x = "Proporção", title = vetor_comsep_c(paste(nome, 
            " (n=", length(na.omit(lesoes)), ")", sep = ""), 50))
    return(plot)
}

quiqua_aderencia <- function(vetor,nomecat,niveis='auto',ordenar=T,dig=2){
vetor = unlist(vetor)
if(niveis[1]=='auto') niveis = names(table(vetor))
vetor <- factor(vetor,levels=niveis)

a=NULL
texto2=c()
tabela <- table(vetor)
gl=length(tabela)-1
if(gl==0) {texto=paste0(" * **",nomecat,":** Não é possível realizar testes estatísticos em variáveis com apenas uma categoria de resposta. \n",sep="")} else {

quiqua <- chisq.test(tabela)

ic <- c()
for (i in 1:length(tabela)) {ic <- rbind(ic,paste("(",paste(round(100*prop.test(x=as.vector(tabela)[i],n=length(vetor),p=1/length(tabela))$conf.int[1:2],2),collapse="%, "),"%)",sep=""))}

ef=round(cramer_v(tabela, p = rep(1/length(tabela),length(tabela))),dig)

if(gl==1){
    if(ef<0.1) efeito="pode ser considerado desprezível." else
      if(ef<0.3) efeito="pode ser considerado um efeito pequeno." else 
        if(ef<0.5) efeito="pode ser considerado um efeito médio." else
          efeito="pode ser considerado um efeito grande."} else
          {if(gl==2){
    if(ef<0.07) efeito="pode ser considerado desprezível." else
      if(ef<0.21) efeito="pode ser considerado um efeito pequeno." else 
        if(ef<0.35) efeito="pode ser considerado um efeito médio." else
          efeito="pode ser considerado um efeito grande."} else
          {if(ef<0.06) efeito="pode ser considerado desprezível." else
      if(ef<0.17) efeito="pode ser considerado um efeito pequeno." else 
        if(ef<0.29) efeito="pode ser considerado um efeito médio." else
          efeito="pode ser considerado um efeito grande."}}

if(quiqua$p.value>0.05) texto = paste0("* **",nomecat,":** O teste qui-quadrado de aderência apontou que não devemos rejeitar a hipótese de igualdade entre as frequências de todas as categorias (",paste("$\\chi^2$",collapse=NULL,sep=""),"(",quiqua$parameter,") = ", round(quiqua$statistic,dig),", p=", pvalor(quiqua$p.value),", V de Cramer=",ef,"). Assim, não rejeitamos que ",paste(paste("a proporção de ",names(tabela),sep=""),collapse=" é igual ")," = 1/",length(tabela)," = **",round(100*1/length(tabela),dig),"%**. O efeito foi medido pela estatística V de Cramer (",ef,"), que ",efeito," \n") else
  texto = paste0(" * **",nomecat,":** Através do teste qui-quadrado de aderência, rejeitamos a hipótese de igualdade entre todas as frequências (",paste("$\\chi^2$",collapse=NULL),"(",quiqua$parameter,") = ", round(quiqua$statistic,dig),", p=", pvalor(quiqua$p.value),", V de Cramer=",ef,"). Isso significa que pelo menos uma frequência difere de 1/",length(tabela)," = **",round(100*1/length(tabela),dig),"%**.  O efeito foi medido pela estatística V de Cramer (",ef,"), que ",efeito, " Passamos a analisar os resíduos do teste qui-quadrado para encontrar quais frequências não são compatíveis com a frequência esperada (",round(100*1/length(tabela),dig),"%). Tomando como base uma significância de 5%, como temos ",length(tabela)," categorias, o ponto de corte $\\alpha$ utilizado será 0.05/",length(tabela),"=",round(0.05/length(tabela),4),", resultando num valor crítico (bilateral) de ",abs(round(qnorm((0.05/length(tabela))/2),dig)),". Portanto, resíduos padronizados ajustados fora da região (",round(qnorm((0.05/length(tabela))/2),dig),",",-round(qnorm((0.05/length(tabela))/2),dig),") serão considerados estatisticamente significativos. Complementando a análise, calculamos o p-valor de cada resíduo. Lembrando que agora, utilizaremos o novo $\\alpha$ (",round(0.05/length(tabela),4),"), ou seja, o resíduo será considerado estatisticamente significativo se o p-valor for menor do que esse valor.")

if (quiqua$p.value<0.05){
a=data.frame(names(tabela),paste0(round(100*prop.table(tabela),dig),"%"),"IC 95%"=ic,"`Resíduos padronizados ajustados`"=round(quiqua$stdres,dig),
                 "p-valor"=round(2*(1-pnorm(abs(quiqua$stdres))),3))[,-c(4,6)]
names(a)=c("Categoria","Frequência observada","IC 95%","Resíduos padronizados","p-valor")
if(ordenar==T) a=a[order(a$`Resíduos padronizados`,decreasing=T),]

a$`p-valor`[a$`p-valor`<0.001] <- "<0.001*"
a$`p-valor`[a$`p-valor`<(0.05/length(tabela))] <- paste0(a$`p-valor`[a$`p-valor`<(0.05/length(tabela))],"*")


maior=c();menor=c();nula=c()

for (i in 1:(dim(a)[1])) {if(str_sub(a$`p-valor`[i],-1)=="*" & a$`Resíduos padronizados`[i]>0) maior <- c(maior,a$Categoria[i]) else
  if(str_sub(a$`p-valor`[i],-1)=="*" & a$`Resíduos padronizados`[i]<0) menor <- c(menor,a$Categoria[i]) else nula <- c(nula,a$Categoria[i])
}

texto2=c(texto2," Através da análise de resíduos, concluimos que: \n")
if(length(maior)==1) texto2 <- c(texto2,c("  + A categoria ",printvetor(maior)," possui frequência **maior** do que era esperado sob hipótese de igualdade de proporções. \n"))
if(length(maior)>1) texto2 <- c(texto2,c("  + As categorias ",printvetor(maior)," possuem frequência **maior** do que era esperado sob hipótese de igualdade de proporções."),"\n")
if(length(menor)==1) texto2 <- c(texto2,c("  + A categoria ",printvetor(menor)," possui frequência **menor** do que era esperado sob hipótese de igualdade de proporções."),"\n")
if(length(menor)>1) texto2 <- c(texto2,c("  + As categorias ",printvetor(menor)," possuem frequência **menor** do que era esperado sob hipótese de igualdade de proporções."),"\n")  
if(length(nula)==1) texto2 <- c(texto2,c("  + A categoria ",printvetor(nula)," **não difere** estatisticamente da frequência esperada sob hipótese de igualdade de proporções."),"\n") else {
if(length(nula)==(dim(a)[1])) texto2 <- c(texto2,c("  + Apesar do teste ser significativo globalmente, nenhuma das categorias diferem estatisticamente da frequência esperada sob hipótese de igualdade de proporções."),"\n") else {
if(length(nula)>1) texto2 <- c(texto2,c("  + As categorias ",printvetor(nula)," **não diferem** estatisticamente da frequência esperada sob hipótese de igualdade de proporções."),"\n")}}
texto2 <- c(texto2,"\n Podemos verificar o valor dos resíduos na tabela a seguir: \n")
}}
resultado=list("texto"=paste(c(texto,texto2,"\n"),collapse=" "),"tabela"=a)

return(resultado)}
