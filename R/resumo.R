resumo_inner = function(anali){
resumo = c()
if(dim(anali)[1]==1) resumo = paste0("\n - ",anali$resumo) else {
  todos <- c(anali$Nome1, anali$Nome2)
  tab <- table(todos)
  ref_nome <- names(tab)[which.max(tab)] 
  nomes_res <- ifelse(anali$Nome1 == ref_nome, anali$Nome2, anali$Nome1)
  ref <- ref_nome

if(sum(anali$sig_ou_não=="cat")>0) {
  resumo = c(resumo,paste0("\n - ",unique(anali[anali$sig_ou_não=="cat",]$resumo)))}  else {
if(sum(anali$sig_ou_não==T)>0) {
  resumo = c(resumo,paste0("\n - ",anali[anali$sig_ou_não==T,]$resumo))
}
if(sum(anali$sig_ou_não==T)==0) {resumo = c(resumo, paste0("\n - Nenhuma das variáveis estudadas (",paste0(printvetor(nomes_res[anali$sig_ou_não==F]), collapse="",sep=""),") tiveram associações ou correlações estatisticamente significativas com ",ref,". \n"))} else {
if(sum(anali$sig_ou_não==F)>0) {
  resumo = c(resumo, paste0("\n - As demais variáveis (",paste0(printvetor(nomes_res[anali$sig_ou_não==F]), collapse="",sep=""),") não tiveram associações ou correlações estatisticamente significativas. \n"))}
}}
if(sum(anali$sig_ou_não=="-")>0) {resumo = paste0("\n - ",anali$resumo)}}

return(resumo)}


resumo_geral = function(analises){

res=c()
sessoes = unique(analises$Sessão)
n_sess = length(sessoes)
for (i in 1:n_sess)
  res = c(res,paste0(paste0("\n\n**",sessoes[i],"**:\n"),paste0(resumo_inner(analises[analises$Sessão==sessoes[i],]), collapse="")))

return(paste0(res))}
