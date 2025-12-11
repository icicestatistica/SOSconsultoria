pvalor <- function(p){
  if(is.numeric(p)==F) res="" else
    if(is.na(p)==T) res="" else
      if(p>0.05) res = p=formatC(round(p,3), format='f', digits=3) else
        if(p>0.01) res=paste0(formatC(round(p,3), format='f', digits=3),"\\*") else
          if(p>=0.001) res=paste0(formatC(round(p,3), format='f', digits=3),"\\*\\*") else res = "<0.001\\*\\*\\*"
      return(res)}

pvetor <- function(vetor){
  pvalores <- c()
  for (i in 1:length(vetor))
    pvalores <- c(pvalores,pvalor(vetor[i]))
  return(pvalores)}

printvetor <- function(vetor, idioma="PT",aspas=T){
size=length(vetor)
if(size==0) print="" else {
if(aspas==T) {
  conec = ifelse(idioma=="PT","' e '","' and '")
  if(size==1) print=paste0("'",vetor[1],"'") else print=paste(paste("'",vetor[1:(size-1)],collapse="', ",sep=""),conec,vetor[size],"'",collapse=NULL,sep="")} else {
    conec = ifelse(idioma=="PT"," e "," and ")
    if(size==1) print=paste0(vetor[1]) else print=paste(paste(vetor[1:(size-1)],collapse=", ",sep=""),conec,vetor[size],collapse=NULL,sep="")}}
return(print)}

format_real <- function(valor) {
   res=format(round(valor,2),nsmall = 2, decimal.mark = ",", big.mark = ".")
return(res)}

caixadeselecao = function (vetor, sep = ",", opcoes = "auto") 
{
    vetor = unlist(vetor)
    if (opcoes[1] == "auto") 
        opcoes = unique(gdata::trim(unlist(stringr::str_split(vetor, sep))))
    opcoes = opcoes[is.na(opcoes) == F]
    matrix <- matrix(c(rep("", length(opcoes) * length(vetor))), 
        ncol = length(opcoes))
    for (i in 1:length(opcoes)) 
      for (j in 1:length(vetor))
        if (is.na(vetor[j]) == T) matrix[j, i] = NA else {
            if (opcoes[i] %in% gdata::trim(stringr::str_split(vetor[j],sep)[[1]]))  matrix[j, i] = "Sim"  else matrix[j, i] = "Não"}
    m <- data.frame(matrix)
    colnames(m) <- opcoes
    m <- m[, names(m) != "NA"]
    return(m)
}
   
nivcatsame = function(niveisoriginal){
   possi = unique(unlist(str_split(paste0(eval(parse(text=niveisoriginal)),sep=", ",collapse=""),", ")))
   possi = possi[which(possi!="")]
   niveiscerto = vec_to_string(possi)
   return(niveiscerto)}


arrumacaixadeselecao = function(cx,nomesini,nomesagrup){

names(cx) = nomesini
novascats = names(table(nomesagrup))

cx_novo = matrix(rep("",length(novascats)*dim(cx)[1]),ncol=length(novascats))

for (i in 1:length(novascats)){
  id = which(nomesagrup==novascats[i])
  df = data.frame(cx[,id])
  cx_novo[,i]=apply(df,1,function(x) ifelse(sum(x=="Sim",na.rm=T)>0,"Sim","Não"))
}
cx_novo = data.frame(cx_novo)
names(cx_novo)=novascats

vec_novo = apply(cx_novo,1,function(x) paste0(novascats[x=="Sim"],collapse=",")) %>% unlist() %>% unname()
vec_novo[vec_novo==""]=NA
return(list("cx_novo"=cx_novo,"vec_novo"=vec_novo))}

                 

vetor_comsep <- function(vec,corte){
mat = stringr::str_split(vec," ")
res=c()
for (i in 1:length(vec)){
  if (length(mat[[i]]) <= corte) res <- c(res, paste(mat[[i]],collapse=" ",sep="")) else {
    linhas=ceiling(length(mat[[i]])/corte)
    if(length(mat[[i]])%%corte==0) ultima = corte else ultima=length(mat[[i]])%%corte
    aux=c()
    if (linhas==2) {aux <- c(aux,paste(mat[[i]][1:corte],collapse=" ",sep=""))}
    if (linhas>2){
      for (j in 0:(linhas-2)) aux <- c(aux,paste(mat[[i]][j*corte+(1:corte)],collapse=" ",sep=""))}
    aux <- c(aux, paste(mat[[i]][((linhas-1)*corte)+(1:ultima)],collapse=" ",sep=""))
  res=c(res, paste(aux, collapse=" \n "))}}
return(res)}
   
vetor_comsep_c <- function(vec,corteinicial){
res = c()
    for (vetor in vec) {
        a = T
        espacos = c(stringr::str_locate_all(vetor, " ")[[1]][, 1], 
            nchar(vetor) + 1)
        inicio = 1
        corte = corteinicial
        fim = ifelse(espacos[1] > corte, espacos[1] - 1, espacos[max(which(espacos - 
            1 <= corte))] - 1)
        newvec <- NULL
        ena=FALSE
        while (a) {
            if(is.na(vetor)==T) {ena = T; a=F} else {
            newvec = c(newvec, stringr::str_sub(vetor, inicio, fim))
            inicio = fim + 2
            if (inicio > nchar(vetor)) 
                a = F
            corte = inicio + corteinicial
            fim = espacos[max(which(espacos - 1 <= corte))] - 
                1
            if (fim < inicio) {
                fim = espacos[max(which(espacos - 1 <= corte)) + 
                  1] - 1
            }
        }}
        if(ena) newvec=NA else newvec = paste0(newvec, collapse = "\n")
        res = c(res, newvec)
    }
return(res)}

#vetor_comsep_c(names(dados),50)

relatorio <- function(a,pularprimeiro=T,dig=2){
  require(knitr)
if (pularprimeiro==T) comeco=2 else comeco=1
for (i in comeco:length(a)){
   if(is.null(a[[i]])==T) tantofaz=0 else {
if(class(a[[i]])[1]=="data.frame") print(kable(a[[i]], row.names=F,digits=dig)) else
  if(class(a[[i]])[1]=="list") relatorio(a[[i]],F) else
      if(class(a[[i]])[1]=="character") cat(a[[i]], sep="\n") else
        print(a[[i]])}}}
   
cont_analises <- function(nome,a){
df = data.frame("Sessão"=rep(nome,dim(a$testes)[1]),a$testes)
return(df)}
   
transpordf <- function(tab){
nova=data.frame(t(tab))
nova <- cbind(names(tab),nova)
names(nova)=c(names(tab)[1],tab[,1])
nova <- nova[-1,]
row.names(nova)<-NULL
return(nova)}

vec_to_string <- function(vec){
   str = paste0("c(",paste(paste("'",vec,"'",sep=""),collapse=","),")",collapse="")
   return(str)}
      
turn_into_factor=function(vec,niveis_str){
  return(unname(factor(unlist(vec),levels=eval(parse(text=niveis_str)))))}

Transformar = function(vec,original,novo){
a=factor(vec,levels=original)
levels(a)=novo
return(a)}
     
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x))}
     
ponto_para_virgula = function (vetor, virgula = F) {
    if(virgula==T) res = str_replace_all(vetor, "\\.", 
        "\\,") else res=vetor
    return(res)}

arruma_tabela_repetidos = function(vec) {
  fica = unlist(lapply(1:length(unique(vec)),function(x) min(which(str_detect(vec, fixed(unique(vec)[x]))==T))))
  vec[-fica]=""
  return(vec)}


rodape_tabela = function(nome,testes){

ana = unique(testes$tipo)
ana = factor(ana,levels=c("numeric","ordinal","factor","catsame","t","mw","aov1","kw","cc_e","cc_q","correl_s","correl_p", "wilc","fried","mcnem","qcoch"))

indicador = table(ana)>0

cap = c("Q1-Q3 = Primeiro e terceiro quartis; DP=desvio padrão","","",
  "IC95% = Intervalo de 95% de confiança para proporção",
  "p-valores marcados pela letra 'c' indicam realização do Teste-t e tamanho de efeito d de cohen",
  "p-valores marcados pela letra 'd' indicam realização do teste de Mann-Whitney e tamanho de efeito r",
  "p-valores marcados pela letra 'e' indicam realização do teste ANOVA e tamanho de efeito eta",
  "p-valores marcados pela letra 'f' indicam realização do teste Kruskall-Wallis e tamanho de efeito eta",
  "p-valores marcados pela letra 'b' indicam realização do teste Exato de Fisher",
  "p-valores marcados pela letra 'a' indicam realização do teste Qui-quadrado, seguido do tamanho de efeito V de cramer",
  "",
  "",      
  "p-valores marcados pela letra 'j' indicam realização do teste Wilcoxon",
  "p-valores marcados pela letra 'i' indicam realização do teste de Friedman",
  "p-valores marcados pela letra 'k' indicam realização do teste de McNemar",
  "p-valores marcados pela letra 'l' indicam realização do teste Q de Cochran")

  capt = paste0(cap[indicador], collapse="; ")

if (sum(indicador[-c(1:4)])>0) capt = paste0(capt, "; \\* significante a 5%; \\*\\* significante a 1%; \\*\\*\\* significante a 0.1%")

capt = paste0(nome,"; ",capt,"\n")

return(capt)}


get_niveis = function(vec) {return(vec_to_string(names(table(vec))))}

###################
                       
preencher = function(vec,vazios=""){
  
if(is.na(vazios)) {vec[is.na(vec)]=""; vazios=""}

id_vaz = which(vec==vazios)
id_nvaz = which(vec!=vazios)

mudar = sapply(id_vaz, function(x) max(id_nvaz[id_nvaz<x]))

vec_novo = vec
vec_novo[id_vaz] = vec[mudar]

return(vec_novo)}

get_ggplots <- function(x) {
  res <- list()
  
  for (i in seq_along(x)) {
    item <- x[[i]]
    
    if (inherits(item, "ggplot")) {
      res <- c(res, list(item))
      
    } else if (is.list(item)) {
      # chama recursivamente
      res <- c(res, get_ggplots(item))
    }
  }
  
  res
}
