grafico_respcat = function(dados,cols,nresp,testes,n_colunas=1,titulo="lado",cor="cyan4"){

  numresp = length(table(dados[,nresp]))
  
  if (length(cor) == 1) {
      if (numresp < 4) 
            palette = colorRampPalette(colors = c(cor, lighten(cor, 
                0.5)))
        else palette = colorRampPalette(colors = c(darken(cor, 
            0.5), cor, "gray"))
    }
    else palette = colorRampPalette(colors = cor)

nomeresp=names(dados)[nresp]
classes = sapply(dados[,cols],class)

nomescols = names(dados)[cols]
nomescols[testes$sig_ou_não==T] = paste0(nomescols[testes$sig_ou_não==T],"*")
names(dados)[cols]=nomescols

cols_cat = cols[classes %in% c("factor","character")]
cols_cont = cols[classes %in% c("numeric","integer")]

namescat = nomescols[classes %in% c("factor","character")]
namescont = nomescols[classes %in% c("numeric","integer")]

niv = lapply(dados[,cols_cat],function(x) names(table(x)))
names(niv)=namescat

### Gráficos pra categóricas

if(length(cols_cat>0)){
  
df = dados[,c(cols_cat,nresp)] %>% 
  cbind("Id"=1:(dim(dados)[1])) %>% 
  pivot_longer(cols=1:length(cols_cat)) %>% 
  rename("resp"=1) %>% 
  group_by(name,value,resp) %>% 
  summarise("Freq"=n()) %>%
  ungroup() %>% 
  group_by(name,value) %>% 
  summarise(Total=sum(Freq),Freq=Freq,resp=resp) %>% 
  mutate(label=paste0(Freq,"\n",round(100*Freq/Total,1),"%"))

graf_biv_cat = function(nome){

graf = df %>% 
  filter(name==nome) %>% 
  mutate(value=factor(value,levels=niv[nome]) %>%
  ggplot(aes(y=value,fill=resp,x=Freq)) +
  geom_bar(stat="identity",position="fill") +
  theme_icic() +
  labs(y=nome,x=NULL,fill=nomeresp,title=nome) +
  scale_x_continuous(labels=scales::percent) +
  geom_text(aes(label=label),position=position_fill(vjust=0.5),lineheight=0.9) +
  scale_fill_manual(values = palette(numresp))

return(graf)}

graficoscat = lapply(namescat,graf_biv_cat)
names(graficoscat)=namescat

} else graficoscat=NULL

## Gráficos para contínuas

if(length(cols_cont)>0){

graf_biv_cont = function(i){

graf = dados[,c(cols_cont[i],nresp)] %>% 
  rename("cont"=1,"resp"=2) %>% 
  ggplot(aes(x=cont,y=resp,fill=resp)) +
  geom_boxplot() +
  theme_icic("v") +
  labs(x=NULL,x="Valor",fill=nomeresp,y=namescont[i],title=namescont[i])  +
  scale_fill_manual(values = palette(numresp))
return(graf)}

graficoscont = lapply(1:length(cols_cont),graf_biv_cont)
names(graficoscont)=namescont

} else graficoscont=NULL

### Evita duplicações de legenda
if(length(cols_cont)>0 & length(cols_cat)>0) {
graficoscont = lapply(graficoscont,function(x) x+theme(legend.position = "none"))}

graficos = c(graficoscont,graficoscat)[nomescols]

if(titulo=="lado") {graficos = lapply(graficos,function(x) x + theme(plot.title=element_blank()))} else
  {graficos = lapply(graficos,function(x) x + theme(axis.title.y = element_blank(),plot.title=element_text(size=10)))}

gfinal = wrap_plots(graficos,ncol=n_colunas) + plot_layout(guides = "collect")
return(gfinal)}
