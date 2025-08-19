anova_mista = function(resp,nomeresp,entre,nomeentre,intra,nomeintra,id){
  
    df2 = data.frame(resp, entre, intra, id) %>% na.omit()
    
  df = df2 %>% 
  pivot_wider(names_from = intra,values_from = resp) %>% 
  na.omit() %>% 
  pivot_longer(cols=-c(1,2)) %>% 
  rename("intra"=3,"resp"=4)

mod.ANOVA <- ez::ezANOVA(data = df,
                     dv = .(resp),
                     wid = .(id),
                     within = .(intra),
                     between = .(entre),
                     detailed = TRUE,
                     type = 3)

### Anova global

tab_anova = mod.ANOVA$ANOVA
tab_anova_print = data.frame(tab_anova[,1:3],"F"=round(tab_anova$`F`,3),pvetor(tab_anova$p),round(tab_anova$ges,2))
names(tab_anova_print) = c("Efeito","GLn","GLd","F","p-valor","ges")
tab_anova_print$Efeito=c("Intercepto",nomeentre,nomeintra,paste0(nomeentre,":",nomeintra))

texto1 = paste0("Concluímos que ",
printvetor(paste(paste0("O efeito ",tab_anova_print$Efeito,ifelse(tab_anova$p<0.05," foi estatisticamente significativo"," não foi estatisticamente significativo"))[-1],
paste0("F(",tab_anova$DFn,",",tab_anova$DFd,")=",round(tab_anova$`F`,2),", p=",pvetor(tab_anova$p),", ges=",round(tab_anova$ges,2))[-1]),aspas=F),". \n")

### Tabela descritiva

dat = df  %>% 
  group_by(entre,intra) %>%
  dplyr::summarise("Média"=mean(resp, na.rm=T),"se"=sd(resp)/sqrt(length(resp)))

tab_medias = dat %>%
  mutate("label"=paste0(round(Média,2)," (",paste(round(Média-1.96*se,2),"-", round(Média+1.96*se,2), sep=""),")")) %>% 
  mutate(Média=NULL, se=NULL) %>% 
  tidyr::pivot_wider(names_from = c(entre), values_from = label) %>% as.data.frame()

### Gráfico

graf = dat %>% 
  mutate(entre=as.character(entre)) %>%
  ggplot() +
  geom_point(aes(y=Média, x=intra, color=entre, group=entre)) +
  geom_line(aes(y=Média, x=intra, color=entre, group=entre), size=1) +
  geom_errorbar(aes(x=intra,ymin=Média-1.96*se, ymax=Média+1.96*se, color=entre), width=0.2) +
  ggprism::scale_color_prism("candy_bright") + 
  guides(y = "prism_offset_minor") + 
  ggprism::theme_prism(base_size = 12) +
  labs(y=nomeresp,x=nomeintra) +
  scale_y_continuous(limits=c(0,max(dat$Média+1.96*dat$se)))

### Análises pareadas

signifs = tab_anova[-1,]$Effect[tab_anova[-1,]$`p<.05`=="*"]

if(length(signifs)==0) {texc=""} else {

texcint="";texcintra="";texcentre=""
comps=NULL;compsintra=NULL;compsentre=NULL

fat = "entre"
if("entre:intra" %in% signifs) {
  if("entre" %in% signifs)
    {comps = df %>%  group_by(intra) %>% emmeans_test(resp ~ entre, p.adjust.method = "bonf"); fat="intra"} else
      {comps = df %>%  group_by(entre) %>% emmeans_test(resp ~ intra, p.adjust.method = "bonf")}} 
  if("intra" %in% signifs) compsintra = df %>% emmeans_test(resp ~ intra, p.adjust.method = "bonf")
  if("entre" %in% signifs) compsentre = df %>% emmeans_test(resp ~ entre, p.adjust.method = "bonf")


cria_texto=function(comp){
  return(paste0(printvetor(paste(comp$group1, ifelse(comp$p.adj>0.05, " não diferiu de",paste0(" teve média",ifelse(comp$statistic>0," maior "," menor "),"que")) , comp$group2,paste0("(t(",comp$df,")=",round(comp$statistic,2),", p=",pvetor(comp$p.adj),")")),aspas=F),"."))}

### Para comps
if(is.null(comps)==F){
levs = names(table(comps[,fat]))
tex = c()
for (i in 1:length(levs)) tex = c(tex,cria_texto(comps[comps[,fat]==levs[i],]))
texcint = paste0("Seguindo para a comparação pareada, temos:",paste(paste0("\n    - Para ",levs),tex,sep=": ",collapse=""),collapse="")}

## Para compsintra e compsentre
if(is.null(compsintra)==F){texcintra = paste0("Seguindo para a comparação pareada por ",nomeintra,", temos: ",cria_texto(compsintra),collapse="")}
if(is.null(compsentre)==F){texcentre = paste0("Seguindo para a comparação pareada por ",nomeentre,", temos: ",cria_texto(compsentre),collapse="")}

texc = paste0(texcentre,texcintra,texcint)
}

res = list(paste0("\n#### ",nomeresp,"\n"),"Iniciamos com uma análise descritiva com média e intervalo de confiança para os valores, que podemos ver a seguir: \n","tab_medias" = as.data.frame(tab_medias),"O resultado da anova pode ser visto a seguir: \n","tab_anova"=tab_anova_print,"texto"=paste0(texto1,texc),"\n","graf"=graf,"\n")

return(res)}

get_anova_mista = function(dados,nid,nentre,nintra,colsresp){
  id=dados[,nid] %>% unlist()
  entre=dados[,nentre] %>% unlist()
  intra=dados[,nintra] %>% unlist()
  nomeentre=names(dados)[nentre]
  nomeintra=names(dados)[nintra]
  nomesresp=names(dados)[colsresp]
  
  graf = dados %>% 
    select(colsresp,nid,nentre,nintra) %>% 
    rename("id"=length(colsresp)+1,"entre"=length(colsresp)+2,"intra"=length(colsresp)+3) %>% 
    pivot_longer(cols=1:length(colsresp)) %>% 
    mutate(name=factor(name,levels=unique(name))) %>% 
    group_by(intra,entre,name) %>%
    dplyr::summarise("Média"=mean(value, na.rm=T),"se"=sd(value)/sqrt(length(value))) %>% 
    ggplot() +
    geom_point(aes(y=Média, x=intra, color=entre, group=entre)) +
    geom_line(aes(y=Média, x=intra, color=entre, group=entre), linewidth=1) +
    geom_errorbar(aes(x=intra,ymin=Média-1.96*se, ymax=Média+1.96*se, color=entre), width=0.2) +
    labs(x=nomeintra,y=NULL) +
    ggprism::scale_color_prism("candy_bright") + 
    guides(y = "prism_offset_minor") + 
    ggprism::theme_prism(base_size = 12) +
    facet_wrap(~name,ncol=2,scales="free")
  
  tab_medias = data.frame()
  tab_anova = data.frame()
  texto = c()
  grafs = list()

  for (i in colsresp) {
    resp = unlist(dados[,i])
    nomeresp = names(dados)[i]
    res = anova_mista(resp,nomeresp,entre,nomeentre,intra,nomeintra,id)
    tab_medias = rbind(tab_medias,cbind("Variável"=c(nomeresp,rep("",(dim(res$tab_medias)[1]-1))),res$tab_medias))
    tab_anova = rbind(tab_anova,cbind("Variável"=c(nomeresp,"","",""),res$tab_anova))
    texto = c(texto,paste0("- **",nomeresp,"**: ",res$texto))
    grafs = list.append(grafs,res$graf)
  }
  
resumir = tab_anova
resumir$Variável = preencher(resumir$Variável,"")

result = tab_medias %>%
         mutate(Variável = preencher(Variável,"")) %>%
         left_join(resumir %>% 
                   filter(Efeito != "Intercepto") %>% 
                   select(1,2,6) %>% 
                   pivot_wider(names_from = Efeito, values_from = `p-valor`) %>% 
                   rename("Variável" = 1))

pararesumo = result
colsalterar = (dim(pararesumo)[2]-2):dim(pararesumo)[2]

for (i in colsalterar) pararesumo[,i] = ifelse(str_sub(unlist(pararesumo[,i]),start=-1)=="*",T,F)

pr = pararesumo %>%
  select(1,colsalterar) %>%
  unique()
pr$Resultado = apply(pr[,-1],1,function(x) printvetor(names(pr[,-1])[x],aspas=F))

pr = pr[,c("Variável","Resultado")] %>% 
  group_by(Resultado) %>% 
  summarise(Variáveis = printvetor(Variável,aspas=F))

pr$Resultado = ifelse(pr$Resultado=="",paste0("Não foi significativo para ",nomeintra,", ",nomeentre," e nem para a interação entre elas."),paste0("Foi significativo para ",pr$Resultado))

textoresumo = paste0(paste0("- ",pr$Variáveis,": ",pr$Resultado),collapse="\n")

result[,1] = arruma_tabela_repetidos(unlist(result[,1]))
result[result$Variável=="",colsalterar]=""

  return(list("textoresumo"=textoresumo,"result" = result,"texto"=paste0(texto,"\n "),"graf"=graf,"\n Gráficos individuais: ","grafind"=grafs))
}
