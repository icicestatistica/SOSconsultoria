grafico_correl <- function(conty,nomey,cor="cyan4",contx,nomex,text="",idioma="PT",virgula=F){
dadosd <- data.frame(conty,contx)
if(idioma=="PT") titulo=c("Correlação entre \'","\' e \'") else titulo=c("Correlation between \'","\' and \'")
plot = ggplot(dadosd, aes(y=conty,x=contx)) + geom_point() + theme_clean() + geom_smooth(color=cor,method="gam", fullrange=T, span=1) + xlab(nomex) + ylab(nomey) +
  ggtitle(vetor_comsep_c(paste0(titulo[1],nomex,titulo[2],nomey,"\' (n=",dim(na.omit(dadosd))[1],")",collapse=""),40), subtitle=text) +
  theme(plot.background = element_rect(colour=NA, fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color=NA),
  plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
return(plot)}

grafico_comp_bar <- function (cont, nomecont, cat, nomecat,cor="cyan4",teste=NULL,dig=2, ordenar=T, idioma="PT", printn=T,virgula=F,stat.test=NULL) 
{
niveis = names(table(cat))
if(is.null(stat.test)) {aumento=0} else {aumento = dim(stat.test)[1]*0.1}
    dadosd = data.frame(cont = cont, cat = cat)
    n = table(dadosd$cat)
    dadosd$cat <- factor(dadosd$cat, levels=niveis)
    if(printn == T) {levels(dadosd$cat) = paste(niveis, "\nn=", n,sep = "");
      if (is.null(stat.test)==F) {stat.test$group1=factor(stat.test$group1, levels=niveis);
                                    stat.test$group2=factor(stat.test$group2, levels=niveis);
                                    levels(stat.test$group1)=paste(niveis, "\nn=", n, sep = "");
                                    levels(stat.test$group2)=paste(niveis, "\nn=", n, sep = "")}
    }
    df.summary <- na.omit(dadosd) %>% group_by(factor(cat)) %>% 
        dplyr::summarise(sd = sd(cont, na.rm = TRUE), mean = mean(cont, na.rm=TRUE), n=length(na.omit(cont)))
    names(df.summary) = c("cat", "sd", "mean","n")
    if (ordenar == T) {
        df.summary$cat <- factor(df.summary$cat, levels = df.summary$cat[order(df.summary$mean)])
    }
    liminf = ifelse(min(df.summary$mean - 1.96*df.summary$sd/sqrt(df.summary$n), na.rm=T)<0,min(df.summary$mean - 1.96*df.summary$sd/sqrt(df.summary$n),na.rm=T),0)
    limsup = ifelse(max(df.summary$mean + 1.96*df.summary$sd/sqrt(df.summary$n), na.rm=T)>0,max(df.summary$mean + 1.96*df.summary$sd/sqrt(df.summary$n),na.rm=T),0) * (1+aumento)
    titulo = ifelse(idioma == "PT", paste0("Comparação de médias de ", 
        nomecont, " por ", nomecat, " (n=", dim(na.omit(dadosd))[1], 
        ")", collapse = ""), paste0("Comparison of ", 
        nomecont, " means by ", nomecat, " (n=", 
        dim(na.omit(dadosd))[1], ")", collapse = ""))
    plot = ggplot() + theme_clean() + geom_bar(df.summary, mapping = aes(cat, 
        mean), stat = "identity", fill = cor, width = 0.8) + ggtitle(vetor_comsep_c(titulo, 40), subtitle = teste) + 
        geom_errorbar(df.summary, mapping = aes(x = cat, ymin = mean - 
            1.96*sd/sqrt(n), ymax = mean + 1.96*sd/sqrt(n)), width = 0.1, size = 1) + geom_label(df.summary, 
        mapping = aes(x = cat, y = mean, label = round(mean, 
            dig))) + ylab(nomecont) + xlab(nomecat) + ylim(liminf,limsup) + theme(plot.background = element_rect(colour = NA, 
        fill = "transparent"), panel.background = element_rect(fill = "transparent", 
        color = NA), plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
    if(is.null(stat.test)==F) plot = plot + ggpubr::stat_pvalue_manual(stat.test, y.position = max(df.summary$mean)*1.1, step.increase = 0.15,label = "signif")
    return(plot)
}
grafico_comp_box = function (cont, nomecont, cat, nomecat, cor = "cyan4", teste = NULL, 
    dig = 2, ordenar = T, idioma = "PT", dot = "auto", printn = T, 
    virgula = F, stat.test = NULL) 
{
    dadosd <- data.frame(cont = cont, cat = cat)
    dadosd <- na.omit(dadosd)
    if (is.null(stat.test)) {
        aumento = 0
    } else {
        aumento = dim(stat.test)[1] * 0.05
    }
    if (min(dadosd$cont) < 0) 
        limites = "ylim(min(dadosd$cont), max(dadosd$cont)*(1+aumento))" else limites = "ylim(0, max(dadosd$cont)*(1+aumento))"
    n = table(dadosd$cat)
    n = n[n > 0]
    niveis = names(n)
    dadosd$cat <- factor(dadosd$cat)
    if (printn == T) {
        levels(dadosd$cat) = paste(niveis, "\nn=", n, sep = "")
        if (is.null(stat.test) == F) {
            stat.test$group1 = factor(stat.test$group1, levels = niveis)
            stat.test$group2 = factor(stat.test$group2, levels = niveis)
            levels(stat.test$group1) = paste(niveis, "\nn=", 
                n, sep = "")
            levels(stat.test$group2) = paste(niveis, "\nn=", 
                n, sep = "")
        }
    }
    df.summary = dadosd %>% group_by(cat) %>% dplyr::summarise(med = median(cont, 
        na.rm = T), q3 = quantile(cont, 0.75, na.rm = T), media = mean(cont, 
        na.rm = T))
    if (ordenar == F) {
      lev = "vetor_comsep_c(levels(dadosd$cat),floor(80/length(n)))"; lev2=levels(dadosd$cat)} else {lev = "vetor_comsep_c(levels(reorder(dadosd$cat,dadosd$cont,FUN=median)),floor(80/length(n)))"; lev2=levels(reorder(dadosd$cat,dadosd$cont,FUN=median))}
    x_c = paste0("factor(vetor_comsep_c(cat,floor(80/length(n))), levels=",lev,")")
    df.summary$cat = factor(df.summary$cat, levels=lev2)
    levels(df.summary$cat)=eval(parse(text=lev))
    if (dot == "auto") 
        dot = ifelse(sum(n) > 200, F, T)
    titulo = ifelse(idioma == "PT", paste0("Comparação de distribuições de ", 
        nomecont, " por ", nomecat, " (n=", dim(na.omit(dadosd))[1], 
        ")", collapse = ""), paste0("Comparison of ", nomecont, 
        " distributions by ", nomecat, " (n=", dim(na.omit(dadosd))[1], 
        ")", collapse = ""))
    if (dot == F) {
        plot = ggplot(dadosd %>% filter(!is.na(cat)), mapping = aes(y = cont, 
            x = eval(parse(text = x_c)))) + geom_violin(fill = cor, 
            alpha = 0.2) + geom_boxplot(fill = cor, alpha = 0.7) + 
            geom_point(data = df.summary, aes(y = media, x = cat), 
                shape = 23, fill = "red", color = "red", size = 3) + 
            ylab(vetor_comsep_c(nomecont, 40)) + xlab(vetor_comsep_c(nomecat, 
            50)) + theme_clean() + ggtitle(vetor_comsep_c(titulo, 
            40), subtitle = teste) + eval(parse(text = limites)) + 
            theme(plot.background = element_rect(colour = NA, 
                fill = "transparent"), panel.background = element_rect(fill = "transparent", 
                color = NA), plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5))
    }
    else {
        plot = ggplot(dadosd %>% filter(!is.na(cat)), mapping = aes(y = cont, 
            x = eval(parse(text = x_c)))) + geom_boxplot(fill = cor, 
            outlier.alpha = 0, alpha = 0.7) + geom_dotplot(binaxis = "y", 
            stackdir = "center", alpha = 0.3, dotsize = 0.9) + 
            geom_point(data = df.summary, aes(y = media, x = cat), 
                shape = 23, fill = "red", color = "red", size = 3) + 
            ylab(vetor_comsep_c(nomecont, 40)) + xlab(vetor_comsep_c(nomecat, 
            50)) + theme_clean() + ggtitle(vetor_comsep_c(titulo, 
            40), subtitle = teste) + eval(parse(text = limites)) + 
            theme(plot.background = element_rect(colour = NA, 
                fill = "transparent"), panel.background = element_rect(fill = "transparent", 
                color = NA), plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5))
    }
    if (is.null(stat.test) == F) 
        plot = plot + ggpubr::stat_pvalue_manual(stat.test, y.position = max(dadosd$cont), 
            step.increase = 0.05, label = "signif")
    return(plot)
}



grafico_comp_box_pareado = function (id,cont, nomecont, cat, nomecat, moms, cor = "cyan4", teste = NULL, 
    dig = 2, idioma = "PT", dot = "auto", 
    virgula = F) 
{
    dadosd <- data.frame(cont = cont, cat = cat,id=id)
    dadosd$cat = factor(dadosd$cat, levels=moms)
    dadosd <- na.omit(dadosd)
    
    dados_w = dadosd %>% tidyr::pivot_wider(names_from = cat, values_from = cont)
    dados_w = na.omit(dados_w)
    
    dife = dados_w[,moms]
    
    if(length(table(dadosd$cat))==2) {dados_w$dif = dife[,2]-dife[,1]
                                      if(mean(unlist(dados_w$dif))>0)  {
                                      p_leg = (max(unlist(dados_w$dif))-min(unlist(dados_w$dif)))/20
                                      text_maior = paste0(sum(unlist(dados_w$dif)>0)," (",100*round(sum(unlist(dados_w$dif)>0)/length(unlist(dados_w$dif)),2),"%)")} else {
                                      p_leg = -((max(unlist(dados_w$dif))-min(unlist(dados_w$dif)))/20)
                                      text_maior = paste0(sum(unlist(dados_w$dif)<=0)," (",100*round(sum(unlist(dados_w$dif)<=0)/length(unlist(dados_w$dif)),2),"%)")  
                                      }
    }
    
    if (min(dadosd$cont) < 0) 
        limites = "ylim(min(dadosd$cont), max(dadosd$cont))" else limites = "ylim(0, max(dadosd$cont))"
    n = table(dadosd$cat)
    n = n[n > 0]
    niveis = names(n)
    dadosd$cat <- factor(dadosd$cat, levels=moms)

    df.summary = dadosd %>% group_by(cat) %>% dplyr::summarise(med = median(cont, 
        na.rm = T), q3 = quantile(cont, 0.75, na.rm = T), media = mean(cont, 
        na.rm = T))
        x_c = "factor(vetor_comsep_c(cat,floor(80/length(n))), levels=vetor_comsep_c(levels(dadosd$cat),floor(80/length(n))))"
    if (dot == "auto") 
        dot = ifelse(sum(n) > 100, F, T)
    titulo = ifelse(idioma == "PT", paste0("Diferença de ", 
        nomecont, " por ", nomecat, " (n=", dim(dados_w)[1], 
        ")", collapse = ""), paste0("Difference of ", nomecont, 
        " distributions by ", nomecat, " (n=", dim(dados_w)[1], 
        ")", collapse = ""))
    
       if(length(table(dadosd$cat))==2) {tit=teste;sub=NULL} else {tit=titulo;sub=teste}
    
    if (dot == F) {
        plot = ggplot(dadosd %>% filter(!is.na(cat)), mapping = aes(y = cont, 
            x = eval(parse(text = x_c)))) + geom_violin(fill = cor, 
            alpha = 0.2) + geom_boxplot(fill = cor, alpha = 0.7) + 
            geom_point(data = df.summary, aes(y = media, x = cat), 
                shape = 23, fill = "red", color = "red", size = 3) + 
            ylab(vetor_comsep_c(nomecont, 40)) + xlab(vetor_comsep_c(nomecat, 
            50)) + theme_clean() + ggtitle(vetor_comsep_c(tit, 
            40), subtitle = sub) + eval(parse(text = limites)) + 
            theme(plot.background = element_rect(colour = NA, 
                fill = "transparent"), panel.background = element_rect(fill = "transparent", 
                color = NA), plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5))
    } else {
        plot = ggplot(dadosd %>% filter(!is.na(cat)), mapping = aes(y = cont, 
            x = eval(parse(text = x_c)))) + geom_boxplot(fill = cor, 
            outlier.alpha = 0, alpha = 0.7) + geom_line(aes(group=id), color="darkgray") + geom_point(alpha = 0.3, size = 2)  +
            geom_point(data = df.summary, aes(y = media, x = cat), 
                shape = 23, fill = "red", color = "red", size = 3) + 
            ylab(vetor_comsep_c(nomecont, 40)) + xlab(vetor_comsep_c(nomecat, 
            50)) + theme_clean() + ggtitle(label=tit, subtitle=sub) + eval(parse(text = limites)) + 
            theme(plot.background = element_rect(colour = NA, 
                fill = "transparent"), panel.background = element_rect(fill = "transparent", 
                color = NA), plot.title = element_text(hjust = 0.5, face="plain"), 
                plot.subtitle = element_text(hjust = 0.5))
    }
    
    if(length(table(dadosd$cat))==2) {
      if(dot==T){
      grafdif = ggplot(dados_w, aes(y=unlist(dif),x=0)) +
            geom_boxplot(fill = cor, outlier.alpha = 0, alpha = 0.7)  +
            geom_point(aes(x=0,y=unlist(dif)),alpha = 0.3, size = 2) +
            theme_icic("h") +
            theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
            labs(y=paste0(moms[2],"-",moms[1])) +
            geom_abline(slope=0,intercept=,color="red") +
            geom_text(y=p_leg,x=0.20, label=text_maior, color="red") +
            geom_point(aes(y = mean(unlist(dados_w$dif),na.rm=T), x = 0), 
            shape = 23, fill = "red", color = "red", size = 3) +
            scale_y_continuous(limits=c(min(0,min(unlist(dados_w$dif),na.rm=T)),max(p_leg,max(unlist(dados_w$dif),na.rm=T))))} else
      
        grafdif = ggplot(dados_w, aes(y=unlist(dif),x=0)) +
            geom_violin(fill = cor, alpha = 0.2) +
            geom_boxplot(fill = cor, outlier.alpha = 0, alpha = 0.7)  +
            theme_icic("h") +
            theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
            labs(y=paste0(moms[2],"-",moms[1])) +
            geom_abline(slope=0,intercept=,color="red") +
            geom_text(y=p_leg,x=0.20, label=text_maior, color="red") +
            geom_point(aes(y = mean(unlist(dados_w$dif),na.rm=T), x = 0), shape = 23, fill = "red", color = "red", size = 3) +
            scale_y_continuous(limits=c(min(0,min(unlist(dados_w$dif),na.rm=T)),max(p_leg,max(unlist(dados_w$dif),na.rm=T))))
      
      plot = plot + grafdif + 
          plot_layout(widths = c(3, 1)) +
          plot_annotation(titulo,theme=theme(plot.title=element_text(hjust=0.5, face="bold",size=14)))
    }
    
    return(plot)
}


grafico_catcat <- function(x,nomex,y,nomey,cor="cyan4",texto="", idioma="PT", labels=T,virgula=F,printn = T){
  help = na.omit(data.frame(x, y))
  if(class(help$y)=="factor") help$y=factor(help$y,levels=names(table(help$y))[which(names(table(help$y)) %in% names(table(as.character(help$y))))])
  if(class(help$x)=="factor") help$x=factor(help$x,levels=names(table(help$x))[which(names(table(help$x)) %in% names(table(as.character(help$x))))])
  names(help) = c("x", "y")
  tabela <- table(help$x, help$y)
  d_completo <- data.frame(tabela, prop.table(tabela, 1))
  d_completo$lab = paste0(d_completo$Freq, " (", round(100 * 
                                                         d_completo$Freq.1, 1), "%)")
  d_completo$Var1 <- factor(d_completo$Var1)
  # --- FIX: casar o n com a categoria por NOME, não por posição ---
  n_label <- if (printn) paste0(" (n=", table(help$x), ")") else ""
  levels(d_completo$Var1) = vetor_comsep_c(paste0(levels(d_completo$Var1), n_label),
                                          floor(60/length(levels(d_completo$Var1))))
  # --- fim do fix ---
  titulo = ifelse(idioma == "PT", paste0("Associação entre '",nomex, "' e '", nomey, "' (n=", dim(na.omit(help))[1], 
                                         ")"), paste0("Association between '", nomex, "' and '", nomey, "' (n=", dim(na.omit(help))[1], ")"))
  nome_eixo_y = ifelse(idioma == "PT", "Frequência", "Frequency")
  if(length(cor)==1){
    if(length(table(y))<4) palette = colorRampPalette(colors=c(cor,lighten(cor,0.5))) else palette=colorRampPalette(colors=c(darken(cor,0.5),cor,"gray"))}
  else palette = colorRampPalette(colors=cor)
  if (labels == T){
    plot = ggplot(d_completo, aes(x = Var1, y = Freq.1, fill = Var2)) + 
      geom_bar(stat = "identity", position = position_stack(reverse = T)) + 
      theme_clean() + scale_y_continuous(labels = scales::percent) + 
      geom_text(label = ifelse(d_completo$lab == "0 (0%)","", d_completo$lab), position = position_stack(vjust = 0.5, reverse = T)) + 
      labs(title = vetor_comsep_c(titulo,40), subtitle = texto, y = nome_eixo_y, x = vetor_comsep(nomex,8), fill = "") +
      scale_fill_manual(labels = vetor_comsep(names(table(y)), 3), values = palette(length(table(y)))) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.background = element_rect(color = NA, fill = "transparent"), plot.background = element_rect(colour = NA, fill = "transparent"), panel.background = element_rect(fill = "transparent", color = NA))}
  else {
    plot = ggplot(d_completo, aes(x = Var1, y = Freq.1, 
                                  fill = Var2)) + geom_bar(stat = "identity", position = position_stack(reverse = T)) + 
      theme_clean() + scale_y_continuous(labels = scales::percent) + 
      labs(title = vetor_comsep_c(titulo, 40), subtitle = texto, 
           y = nome_eixo_y, x = vetor_comsep(nomex, 8), fill = "") + 
      scale_fill_manual(labels = vetor_comsep(names(table(y)), 3), values = palette(length(table(y)))) + theme(plot.title = element_text(hjust = 0.5), 
      plot.subtitle = element_text(hjust = 0.5), legend.background = element_rect(color = NA,fill = "transparent"), plot.background = element_rect(colour = NA, fill = "transparent"), panel.background = element_rect(fill = "transparent", color = NA))}
  return(plot)}

grafico_teste_t_3 <- function(cont,nomecont,cores,cat,nomecat,niveis){
  
  d=data.frame(cont=cont,cat=factor(cat,levels=niveis,ordered=T))
  grupo1=d$cont[d$cat==niveis[1]]
  grupo2=d$cont[d$cat==niveis[2]]
  aux=data.frame(cat=niveis,n=c(paste0("n=",length(grupo1)),paste0("n=",length(grupo2))),max=c(max(grupo1)+1.5*sd(grupo1),max(grupo2)+1.5*sd(grupo2)),
                 media=c(mean(grupo1),mean(grupo2)),
                 dp=c(sd(grupo1),sd(grupo2)),
                 lugar=c(mean(grupo1)+2.5*sd(grupo1),mean(grupo2)+2.5*sd(grupo2)),
                 cor=cores,altura=c(unlist(quantile(density(grupo1)$y,0.75)),unlist(quantile(density(grupo2)$y,0.75))))
  row.names(aux)=niveis
  aux$cat <- factor(aux$cat, levels=niveis)
  
  ggplot(d,aes(x=cont,fill=cat)) + facet_grid(cat~.) +
    geom_histogram(aes(y=..density..,fill=cat), alpha=0.7,bins=30) +
    scale_fill_manual(values=setNames(cores,nm=niveis)) + 
    geom_density(alpha=0.5,colour=NA) +
    xlab(nomecont) + 
    geom_text(data=aux,aes(x=lugar,y=altura,label=cat,colour=cat),size=6,fontface="bold") +
    geom_text(data=aux,aes(x=max,y=0,label=n,colour=cat),size=4,fontface="bold") +
    geom_errorbarh(data=aux,aes(xmax = media+dp, xmin = media-dp,y=0),inherit.aes = F,height=max(density(d$cont)$y)/5) +
    geom_label(data=aux,aes(x=media,y=0,label=paste("Média \n",round(media,2)),fill=cat),size=4,lineheight=.8) +
    scale_color_manual(values=darken(cores,0.5)) + 
    theme_clean() + theme(legend.position = "none", plot.background = element_rect(colour="white"),strip.text.y = element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank(),
                          axis.line.y = element_blank(),
                          plot.margin = unit(c(0,0,1,0), "cm"),
                          panel.grid.major.x=element_line(colour="gray"),
                          panel.grid.minor.x=element_line(colour="lightgray"),
                          panel.grid.major.y=element_blank())}
