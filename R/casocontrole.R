get_ORs = function (dados, ngrupo, refgrupo, cols, refs) 
{
  
  nomes = names(dados)[cols]
  classe = sapply(dados[,cols],class)
  ORs = data.frame()
  nome_grupo = names(dados)[ngrupo]
  for (i in 1:length(cols)) {
        df = dados[, c(cols[i], ngrupo)]
        df[, 2] = factor(unlist(df[, 2]))
        df[, 2] <- relevel(unlist(df[, 2]), ref = refgrupo)
        
        if(classe[i] %in% c("factor","character")){
            df[, 1] = factor(unlist(df[, 1]))
            df[, 1] = relevel(unlist(df[, 1]), ref = refs[i])}
        
        modelo <- glm(as.formula(paste0("`", nome_grupo, "` ~ .")), 
            family = binomial(link = "logit"), data = df)
        ORtab = exp(cbind(OR = coef(modelo), confint(modelo))) %>% 
            round(3)
        
        OR = ORtab %>% 
          apply(1, function(x) paste0(x[1], " (",x[2], ";", x[3], ")"))
        
        OR = cbind(OR,ORtab)
        
        if(classe[i] %in% c("factor","character")){
          OR[1,1] = "ref"
          row.names(OR)[1] = paste0(nomes[i], refs[i])
          row.names(OR) = str_remove_all(row.names(OR), fixed("`"))
          vari = str_sub(row.names(OR), end = nchar(nomes[i]))
          cate = str_sub(row.names(OR), start = nchar(nomes[i]) + 1)
        
          ORs = rbind(ORs, cbind("cara"=vari, "Característica" = cate, 
            OR))
        } else {ORs = rbind(ORs,cbind("cara"=row.names(OR)[2], Característica = "Min-Máx", 
            t(OR[2,])))
        names(ORs)=c("cara","Característica","OR","OR","2.5 %",   "97.5 %")}

  }
   return(ORs)}


casocontrole = function(dados,cols,refs,ngrupo,refgrupo,auxiliar,...){

### ORs
ORs = get_ORs(dados,ngrupo,refgrupo,c(cols),refs)
ORs$cara = str_remove_all(ORs$cara,fixed("`"))

### Tabela descritiva

nomes = names(dados)[cols]

a = get_analise(dados,cols,auxiliar,ngrupo)
res = a$result

det = str_detect(res$Característica,fixed("*"))
ini = c(1:length(det))[det]
fim=c(ini[-1]-1,length(det))

df = data.frame(nomes,ini,fim)
df$reps = df$fim-df$ini+1

res$cara = sapply(nomes,function(x) rep(x,times=df[df$nomes==x,]$reps)) %>% unlist() %>% unname()

## Juntando as duas

tabfim = left_join(res,ORs[,1:3],by=c("Característica","cara"))[,-6]

tabfim$OR[is.na(tabfim$OR)]=""


### Gráfico

graf=grafico_respcat(dados,cols,ngrupo,a$testes,...)

#### Gráfico ORs

ORgraf = data.frame(ORs)
ORgraf[ORgraf$Característica=="Min-Máx",]$Característica = ORgraf[ORgraf$Característica=="Min-Máx",]$cara
ORgraf[ORgraf$OR=="ref",]$OR.1=1
ORgraf[ORgraf$OR=="ref",]$X2.5..=NA
ORgraf[ORgraf$OR=="ref",]$X97.5..=NA

ORgraf %>% 
  mutate(cara=factor(cara,levels=unique(cara))) %>%
  group_by(cara) %>%   # Agrupa por facet
  arrange(Característica, OR.1) %>%   # Ordena dentro de cada facet
  mutate(Característica = factor(Característica, levels = unique(Característica))) %>%  # Define a ordem dos fatores
  ungroup() %>%
  ggplot(aes(y=Característica,x=as.numeric(OR.1))) +
  geom_point() +
  geom_errorbarh(aes(y=Característica,xmin=as.numeric(X2.5..),xmax=as.numeric(X97.5..))) +
  facet_grid(cara ~.,space="free_y",scales="free",switch = "y") +
  geom_vline(xintercept=1,color="red") +
  theme_icic("v") +
  scale_x_continuous(trans = "log10") +
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 0.5),
        strip.placement = "outside")


ordem = ORgraf %>% 
  filter(OR!="ref") %>% 
  group_by(cara) %>% 
  summarise("Média"=mean(as.numeric(OR.1))) %>% 
  arrange(Média) %>% 
  select(1) %>% 
  unlist()


grafOR = ORgraf %>% 
  mutate(cara = factor(cara, levels = ordem)) %>%
  ggplot(aes(y = Característica, x = as.numeric(OR.1))) +
  geom_point() +
  geom_errorbarh(aes(y = Característica, xmin = as.numeric(X2.5..), xmax = as.numeric(X97.5..))) +
  facet_grid(cara ~ ., space = "free_y", scales = "free", switch = "y") +
  geom_vline(xintercept = 1, color = "red",linetype=2) +
  geom_text(aes(x=0.1,label = ifelse(cara != Característica, Característica, "")), 
            hjust = 0, vjust = 0,stat="identity") +
  geom_text(aes(label = ifelse(sign((as.numeric(X2.5..)-1)*(as.numeric(X97.5..)-1))>0, "*", "")), 
            nudge_y = 0.3,color="red") +# Adiciona os rótulos apenas quando forem diferentes
  theme_icic("v") +
  labs(y=NULL) +
  scale_x_continuous(expand=c(0,0.1),
                     trans = "log10",limits=c(0.1,1.1*max(as.numeric(ORgraf$X97.5..),na.rm=T)),
                     name="OR (IC95%)",
                     n.breaks = 8) +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1,vjust=0.5),
    strip.placement = "outside",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

caption = paste0(a$caption,"; OR (IC95%): odds ratio com intervalo de 95% de confiança")

return(list("testes"=a$testes,"resumo"=a$resumo,"tabela"=tabfim,"caption"=caption,"grafico"=graf,"grafOR"=grafOR))}
