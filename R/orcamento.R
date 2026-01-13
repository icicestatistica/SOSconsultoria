add_watermark <- function(cor="darkblue",tamanho=12,texto=paste0('GRÁFICO CONFECCIONADO PARA ORÇAMENTO\nENVIADO EM ',Sys.Date(),'\n ICIC CONSULTORIA ESTATÍSTICA\n PRODUZIDO POR:\nISABELLE CRISTINA IDALGO CARNIELLI')) return(paste0("
      annotation_custom(
    grid::textGrob('",
      texto,"',
      gp = grid::gpar(
        col = '",cor,"',
        alpha = 0.8,
        fontsize = ",tamanho,",
        fontface = 'bold'
      ),
      rot = 0
    ),
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  )"))


# uso:
# + eval(parse(text=add_watermark()))

orcamento <- function(dataenviobancobruto="01/02/2023",
                      bancobruto=dados,
                      auxiliar=auxiliar,
                      analises=analises,
                      prazoprop=3,
                      prazo=7,
                      parcelas=2,
                      conhecimento=c('muito'=T,'pouco'=F,'nada'=F),
                      continuidade=c('dificilmente'=T,'provavel'=F,'certeza'=F),
                      perfil=c('tranquilo'=T,'perdido'=F,'perdido_enrolado'=F),
                      tempo = c('suporte'=T,'tempo_reuniao'=30,'outros'=0),
                      imposto=0.175,
                      nomepesquisador="nome",
                      cpfpesquisador="123.456.789-00"){
  
  mês <- c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")

  ##### DESCRITIVA DE UM OU MAIS BANCOS #####
  
  fun_descritiva_banco = function(bancobruto,auxiliar){
  descricaobanco <- c()
  for (i in 1:dim(auxiliar)[1]){
    if(auxiliar$tipo[i]=="factor"){
      descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável categórica com ",length(eval(parse(text=auxiliar$niveis[i])))," grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),").",sep=""))} else 
        if(auxiliar$tipo[i]=="ordinal"){
          descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável ordinal com ", length(eval(parse(text=auxiliar$niveis[i]))), " grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),").",sep=""))} else
            if(auxiliar$tipo[i]=="ID"){
              descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Coluna de identificação. Não será utilizada na análise.", sep=""))} else
                if(auxiliar$tipo[i]=="catsame"){
                  descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável do tipo caixas de respostas (o respondente pode marcar mais de uma opção). Para realizar a análise, é necessário separar essa coluna em ",length(eval(parse(text=auxiliar$niveis[i]))),", sendo uma coluna para cada resposta possível (",printvetor(eval(parse(text=auxiliar$niveis[i]))),").", sep=""))} else
                    if(auxiliar$tipo[i]=="numeric"){
                      descricaobanco <- c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável numérica.",sep=""))} else
                      {descricaobanco <- c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável textual ou não categorizada corretamente. Não será utilizada na análise.",sep=""))}}
  descricaobanco = paste0(descricaobanco, sep="",collapse="\n")
  
  descbase = paste("A base de dados foi enviada na data ",dataenviobancobruto,", com ",dim(bancobruto)[1]," linhas e ",dim(bancobruto)[2]," colunas.
As variáveis presentes no banco são:",sep="",collapse="")
  
  result = paste0(c(descbase,"\n",descricaobanco),sep="\n",collapse="\n")
  return(result)}

  descritivacompleta = c()
  if(is.null(dim(bancobruto))) {
    for (i in 1:length(bancobruto)) descritivacompleta = c(descritivacompleta,fun_descritiva_banco(bancobruto[[i]],auxiliar[[i]]))
  descritivacompleta = paste0(descritivacompleta, sep="\n")} else descritivacompleta = fun_descritiva_banco(bancobruto,auxiliar)
  
  ## descritivacompleta guarda a descrição de um ou mais bancos enviados ###
  
  
  gs4_deauth()
  preçotestes = read_sheet("https://docs.google.com/spreadsheets/d/1VAuR_iQHx-pazZvWJCR_2qHhXPNI6pB3IlGl4Qo4WSU/edit?usp=sharing")
  
  codigos = preçotestes$cod
  
  analises$tipo = factor(analises$tipo, levels=codigos)
  
  sessoes=unique(analises$Sessão)
  n_analises = length(sessoes)
  
  resumoanalises = analises %>% group_by(`Sessão`,tipo) %>% dplyr::summarise(nome2=printvetor(unique(Nome2),aspas=F),nome1=printvetor(unique(Nome1),aspas=F))


descricaoanalises <- c()

for (i in 1:n_analises){
resumo_sessaoi = resumoanalises %>% filter(Sessão==sessoes[i])
desc_i = c()

  ###### DESCRITIVAS
## numéricas
  if(c("numeric") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Para as variáveis numéricas, a saber, ",resumo_sessaoi[resumo_sessaoi$tipo=="numeric",]$nome2,", cálculo do Mínimo, Máximo, Quartis, Média, Mediana, Desvio Padrão e teste de Normalidade de Shapiro-Wilk."))
## ordinais e fatores
  if(sum(c("ordinal","factor") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Para as variáveis categóricas nominais e/ou categóricas ordinais, a saber, ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("ordinal","factor"),]$nome2,aspas=F),", cálculos de frequências absoluta (n) e relativa (%)."))

  #### BIVARIADOS INDEPENDENTES
  
  ## Teste-t e Mann-Whitney
  if(sum(c("t","mw") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Teste-t independente de comparação de duas médias, no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados nos dois grupos de ",unique(resumo_sessaoi[resumo_sessaoi$tipo %in% c("t","mw"),]$nome2)," e com o teste correspondente não paramétrico (Mann-whitney) caso contrário, bem como o cálculo do tamanho do efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("t","mw"),]$nome1,aspas=F),"."))
## Anova e Kruskall Wallis
if(sum(c("aov1","kw") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + One-way anova de comparação de mais de duas médias, no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados nos grupos de ",unique(resumo_sessaoi[resumo_sessaoi$tipo %in% c("aov1","kw"),]$nome2)," e com o teste correspondente não paramétrico (Kruskall-Wallis) caso contrário, bem como as devidas análises post-hoc (Tukey e Dunn, respectivamente) e cálculo de tamanho de efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("aov1","kw"),]$nome1,aspas=F),"."))
## correlação de spearman e pearson
if(sum(c("correl_p","correl_s") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Cálculo da correlação de pearson, no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados e correlação de spearman (não paramétrica), caso contrário, para testar a correlação entre a(s) variável(is) ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("correl_p","correl_s"),]$nome1," e ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("correl_p","correl_s"),]$nome2,"."))
## cc - exato de fisher ou qui-quadrado
if(sum(c("cc_e","cc_q") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Realização do teste Qui-quadrado, se a suposição de até 20% das caselas da tabela de contingência com valor esperado menor que 5 e nenhuma com valor esperado menor que 1 e do teste Exato de Fisher caso contrário, para verificação da independência/homogeneidade entre a(s) variável(is) ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("cc_e","cc_q"),]$nome1," e ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("cc_e","cc_q"),]$nome2,"."))

#### BIVARIADOS PAREADOS

## Teste-t pareado e wilcoxon
if(sum(c("t_par","wilc") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Teste-t pareado de comparação de duas médias pareadas (em dois momentos ou por dois avaliadores diferentes, por exemplo), no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados e o teste correspondente não paramétrico (Wilcoxon) caso contrário, bem como o cálculo do tamanho do efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("t_par","wilc"),]$nome1,aspas=F),"."))
## Anova de medidas repetidas e Friedman
if(sum(c("aovmr","fried") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Anova de medidas repetidas de comparação de mais de duas médias pareadas (em mais de dois momentos ou por mais de dois avaliadores diferentes, por exemplo), no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados e o teste correspondente não paramétrico (Friedman) caso contrário, bem como as devidas análises post-hoc (Teste-t pareado e Wilcoxon, respectivamentem, ambos com correção de bonferroni) e cálculo de tamanho de efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("aovmr","fried"),]$nome2,aspas=F),"."))
## McNemar: medidas categóricas repetidas em dois momentos
if(c("mcnem") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Teste de McNemar, utilizado para testar diferenças nas proporções de uma variável categórica medida de forma pareada (em dois momentos ou por dois avaliadores diferentes, por exemplo). Variáveis a serem testadas: ",resumo_sessaoi[resumo_sessaoi$tipo=="mcnem",]$nome1,"."))
##Q de Cochran: medidas categóricas repetidas em mais de dois momentos
if(c("qcoch") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Teste Q de Cochran, utilizado para testar diferenças nas proporções de uma variável categórica medida de forma pareada (em mais de dois momentos ou por mais de dois avaliadores diferentes, por exemplo). Variáveis a serem testadas: ",resumo_sessaoi[resumo_sessaoi$tipo=="qcoch",]$nome1,"."))
  
##### OUTROS
  
## catsame (caixa de resposta)
  if(c("catsame") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Para os itens contidos em perguntas do tipo \"caixa de respostas\", em que cada item pode ser respondido em uma das categorias apresentadas, a saber, ",resumo_sessaoi[resumo_sessaoi$tipo=="catsame",]$nome2,", cálculo da frequência, frequência relativa e, para as respostas dicotômicas, intervalo de confiança para uma das proporções, incluindo todas as comparações entre as proporções de respostas das variáveis."))
  
 ## Análise de sensibilidade
if(c("sensi") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Realização da análise de sensibilidade, com cálculo da acurácia, sensibilidade, especificidade, Valor Predito Positivo (VPP) e Valor Predito Negativo (VPN) dos testes ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo=="sensi",]$nome2, aspas=F),", tomando como padrão ouro o teste",resumo_sessaoi[resumo_sessaoi$tipo=="sensi",]$nome1,"."))

descricaoanalises = c(descricaoanalises, paste0("\n **",unique(resumo_sessaoi$Sessão),"** \n",paste0(desc_i, collapse="\n",sep="\n")))
}
  
  totaltab = table(factor(analises$tipo,levels=preçotestes$cod))
  valorbruto=sum(totaltab*preçotestes$preço)
  
  crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazo, "%d/%m/%Y"),".",sep="",collapse="")
  
  datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
  dataprop=format(Sys.Date(), "%d/%m/%Y")
  
  ## Prazo
  cortes_prazo = c(1,2,3,5,10)
  aumento_prazo = c(0.5,0.3,0.15,0.05,0)
  mult_prazo = aumento_prazo[max(which(prazo>=cortes_prazo))]
  
  ## Número de análises
  numero = sum(totaltab)
  cortes_numero = c(1,11,51,101)
  aumentos_numero = c(0,-0.01,-0.03,-0.05)
  mult_numero = aumentos_numero[max(which(numero>=cortes_numero))]
  
  ## Conhecimento
  aumentos_conhecimento = c(0,0.05,0.10)
  mult_conhecimento = sum(conhecimento*aumentos_conhecimento)
  
  ## Continuidade
  aumentos_continuidade=c(0,-0.05,-0.1)
  mult_continuidade = sum(continuidade*aumentos_continuidade)
  
  ## Perfil
  aumentos_perfil=c(0,0.05,0.15)
  mult_perfil = sum(perfil*aumentos_perfil)
  
  ## Extras
  aumentos_extra = c(150,300/60,1)
  soma_extras = sum(tempo*aumentos_extra)
  
  ## Taxas e impostos
  mult_imposto = 1/(1-imposto)
  
  
  preco_desconto = round(((valorbruto*(1+mult_prazo)*(1+mult_perfil)*(1+mult_conhecimento)*(1+mult_numero)*(1+mult_continuidade))+soma_extras)*mult_imposto,2)
  
  preco_parcelado = round(preco_desconto/0.9,2)
  
  crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazo, "%d/%m/%Y"),".",sep="",collapse="")
  
  datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
  dataprop=format(Sys.Date(), "%d/%m/%Y")
  dataparcelas = format(Sys.Date()+30*(1:parcelas-1), "%d/%m/%Y")
  
  
  # Investimento
  
  invest1 = paste("O investimento para consultoria é de \n
 * **R$ ",format_real(preco_parcelado),"**, pago em até 3x no cartão de crédito \n
OU \n
 * **R$ ",format_real(preco_desconto),"** (10% de desconto) à vista via pix, no ato do aceite da proposta. \n",sep="",collapse="")

  
  invest2 =  paste("Proposta válida até ",datavalido,".",sep="",collapse="")
  
  
  cat("
\\newpage

# ORÇAMENTO

## BASE DE DADOS
",descritivacompleta,"
Qualquer alteração no(s) banco(s) após o aceite da proposta poderá gerar retrabalho por parte da estatística e, portanto, poderá ser cobrado. Por este motivo, pedimos que tenha certeza sobre o envio do material.

## OBJETIVOS DA CONSULTORIA
O objetivo da presente consultoria é a entrega dos seguintes resultados:
", descricaoanalises,"

## CRONOGRAMA
",crono,"

## FORMA DE ENTREGA
A consultoria proposta inclui:

* Definição dos métodos de análise
* Aplicação da metodologia apropriada
* Relatório de análise estatística com análises, tabelas, interpretação dos resultados, detalhes da metodologia e bibliografia utilizadas;
* Construção de gráficos sob demanda por 6 meses a partir da entrega do relatório;
* Uma reunião de 30 minutos para esclarecimento de dúvidas metodológicas.

## INVESTIMENTO
",invest1,"
**Dados bancários:** \n
* Banco Cora SCD. - 403
* Agência: 0001
* Conta: 2833009-2
* Razão Social - ISABELLE CRISTINA IDALGO LTDA
* CNPJ - 41.986.330/0001-85

A chave pix é o número do CNPJ. Pedimos a gentileza de envio do comprovante de pagamento através do e-mail icicestatistica@gmail.com.
A nota fiscal será enviada até 3 dias após a conclusão do pagamento. \n", invest2,"

## OBSERVAÇÕES
* Não nos cabe a conclusão e argumentação dos resultados, uma vez que não detemos a “inteligência” de cada trabalho e sua respectiva área de pesquisa.
* O trabalho que desenvolvemos é único e exclusivamente para a realização da análise estatística, não é de nossa competência fazer a edição e formatação dos resultados conforme regimento de cada universidade e/ou revista.

\\newpage",
      paste0("Campinas, ",mday(Sys.Date())," de ",mês[month(Sys.Date())]," de ",year(Sys.Date()),sep=""),"

<br> <br>

------------------------------------------------------------------------ \n

###### **Estatística Responsável:** Isabelle Cristina Idalgo Carnielli \n

###### CONRE: 10734 \n

<br>

------------------------------------------------------------------------ \n
",
      paste0("###### **Pesquisador(a):** ",nomepesquisador," \n",sep=""),
      
      paste0("###### CPF: ",cpfpesquisador," \n",sep=""),"

\\newpage

",sep="\n", collapse="")}


orcamento_personalizado <- function(dataenviobancobruto=Sys.Date(),
                      dados=dados,
                      auxiliar=auxiliar,
                      analises=analises,
                      preçotestes=preçotestes,
                      dadosbanc=dadosbanc,
                      assinatura=assinatura,
                      prazoprop=3,
                      prazo=7,
                      parcelas=2,
                      conhecimento=c('muito'=T,'pouco'=F,'nada'=F),
                      continuidade=c('dificilmente'=T,'provavel'=F,'certeza'=F),
                      perfil=c('tranquilo'=T,'perdido'=F,'perdido_enrolado'=F),
                      tempo = c('suporte'=T,'tempo_reuniao'=30,'outros'=0),
                      imposto=0.175,
                      nomepesquisador="nome",
                      cpfpesquisador="000.000.000-00"){
  
  mês <- c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")

  ##### DESCRITIVA DE UM OU MAIS BANCOS #####
  
 fun_descritiva_banco = function(dados,auxiliar){
  descricaobanco <- c()
  for (i in 1:dim(auxiliar)[1]){
    if(auxiliar$tipo[i]=="factor"){
      descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável categórica com ",length(eval(parse(text=auxiliar$niveis[i])))," grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),").",sep=""))} else 
        if(auxiliar$tipo[i]=="ordinal"){
          descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável ordinal com ", length(eval(parse(text=auxiliar$niveis[i]))), " grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),").",sep=""))} else
            if(auxiliar$tipo[i]=="ID"){
              descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Coluna de identificação. Não será utilizada na análise.", sep=""))} else
                if(auxiliar$tipo[i]=="catsame"){
                  descricaobanco=c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável do tipo caixas de respostas (o respondente pode marcar mais de uma opção). Para realizar a análise, é necessário separar essa coluna em ",length(eval(parse(text=auxiliar$niveis[i]))),", sendo uma coluna para cada resposta possível (",printvetor(eval(parse(text=auxiliar$niveis[i]))),").", sep=""))} else
                    if(auxiliar$tipo[i]=="numeric"){
                      descricaobanco <- c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável numérica.",sep=""))} else
                      {descricaobanco <- c(descricaobanco,paste("+ **",auxiliar$nomes[i],":** Variável textual ou não categorizada corretamente. Não será utilizada na análise.",sep=""))}}
  descricaobanco = paste0(descricaobanco, sep="",collapse="\n")
  
  descbase = paste("A base de dados foi enviada na data ",dataenviobancobruto,", com ",dim(dados)[1]," linhas e ",dim(dados)[2]," colunas.
As variáveis presentes no banco são:",sep="",collapse="")
  
  result = paste0(c(descbase, descricaobanco),sep="\n",collapse="\n")
  return(result)}

  descritivacompleta = c()
  if(is.null(dim(dados))) {
    for (i in 1:length(dados)) descritivacompleta = c(descritivacompleta,paste0("\n### Banco de dados número ",i,"\n"), fun_descritiva_banco(dados[[i]],auxiliar[[i]]))
  descritivacompleta = paste0(descritivacompleta, sep="\n")} else descritivacompleta = fun_descritiva_banco(dados,auxiliar)
  
  ## descritivacompleta guarda a descrição de um ou mais bancos enviados ###
  
  codigos = preçotestes$cod
  
  analises$tipo = factor(analises$tipo, levels=codigos)
  
  sessoes=unique(analises$Sessão)
  n_analises = length(sessoes)
  

  resumoanalises = analises %>% group_by(Sessão,tipo) %>% summarise(nome2=printvetor(unique(Nome2),aspas=F),nome1=printvetor(unique(Nome1),aspas=F))

  descricaoanalises <- c()
  
  for (i in 1:n_analises){
    resumo_sessaoi = resumoanalises %>% filter(Sessão==sessoes[i])
    desc_i = c()
    
    ###### DESCRITIVAS
    ## numéricas
    if(c("numeric") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Para as variáveis numéricas, a saber, ",resumo_sessaoi[resumo_sessaoi$tipo=="numeric",]$nome2,", cálculo do Mínimo, Máximo, Quartis, Média, Mediana, Desvio Padrão e teste de Normalidade de Shapiro-Wilk."))
    ## ordinais e fatores
    if(sum(c("ordinal","factor") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Para as variáveis categóricas nominais e/ou categóricas ordinais, a saber, ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("ordinal","factor"),]$nome2,aspas=F),", cálculos de frequências absoluta (n) e relativa (%)."))
    
    #### BIVARIADOS INDEPENDENTES
    
    ## Teste-t e Mann-Whitney
    if(sum(c("t","mw") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Teste-t independente de comparação de duas médias, no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados nos dois grupos de ",unique(resumo_sessaoi[resumo_sessaoi$tipo %in% c("t","mw"),]$nome2)," e com o teste correspondente não paramétrico (Mann-whitney) caso contrário, bem como o cálculo do tamanho do efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("t","mw"),]$nome1,aspas=F),"."))
    ## Anova e Kruskall Wallis
    if(sum(c("aov1","kw") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + One-way anova de comparação de mais de duas médias, no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados nos grupos de ",unique(resumo_sessaoi[resumo_sessaoi$tipo %in% c("aov1","kw"),]$nome2)," e com o teste correspondente não paramétrico (Kruskall-Wallis) caso contrário, bem como as devidas análises post-hoc (Tukey e Dunn, respectivamente) e cálculo de tamanho de efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("aov1","kw"),]$nome1,aspas=F),"."))
    ## correlação de spearman e pearson
    if(sum(c("correl_p","correl_s") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Cálculo da correlação de pearson, no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados e correlação de spearman (não paramétrica), caso contrário, para testar a correlação entre a(s) variável(is) ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("correl_p","correl_s"),]$nome1," e ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("correl_p","correl_s"),]$nome2,"."))
    ## cc - exato de fisher ou qui-quadrado
    if(sum(c("cc_e","cc_q") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Realização do teste Qui-quadrado, se a suposição de até 20% das caselas da tabela de contingência com valor esperado menor que 5 e nenhuma com valor esperado menor que 1 e do teste Exato de Fisher caso contrário, para verificação da independência/homogeneidade entre a(s) variável(is) ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("cc_e","cc_q"),]$nome1," e ",resumo_sessaoi[resumo_sessaoi$tipo %in% c("cc_e","cc_q"),]$nome2,"."))
    
    #### BIVARIADOS PAREADOS
    
    ## Teste-t pareado e wilcoxon
    if(sum(c("t_par","wilc") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Teste-t pareado de comparação de duas médias pareadas (em dois momentos ou por dois avaliadores diferentes, por exemplo), no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados e o teste correspondente não paramétrico (Wilcoxon) caso contrário, bem como o cálculo do tamanho do efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("t_par","wilc"),]$nome1,aspas=F),"."))
    ## Anova de medidas repetidas e Friedman
    if(sum(c("aovmr","fried") %in% resumo_sessaoi$tipo)>0) desc_i=c(desc_i,paste0("\n + Anova de medidas repetidas de comparação de mais de duas médias pareadas (em mais de dois momentos ou por mais de dois avaliadores diferentes, por exemplo), no caso das variáveis numéricas cujo teste de Shapiro-Wilk não rejeitou a normalidade dos dados e o teste correspondente não paramétrico (Friedman) caso contrário, bem como as devidas análises post-hoc (Teste-t pareado e Wilcoxon, respectivamentem, ambos com correção de bonferroni) e cálculo de tamanho de efeito. Variáveis a serem testadas: ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo %in% c("aovmr","fried"),]$nome2,aspas=F),"."))
    ## McNemar: medidas categóricas repetidas em dois momentos
    if(c("mcnem") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Teste de McNemar, utilizado para testar diferenças nas proporções de uma variável categórica medida de forma pareada (em dois momentos ou por dois avaliadores diferentes, por exemplo). Variáveis a serem testadas: ",resumo_sessaoi[resumo_sessaoi$tipo=="mcnem",]$nome1,"."))
    ##Q de Cochran: medidas categóricas repetidas em mais de dois momentos
    if(c("qcoch") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Teste Q de Cochran, utilizado para testar diferenças nas proporções de uma variável categórica medida de forma pareada (em mais de dois momentos ou por mais de dois avaliadores diferentes, por exemplo). Variáveis a serem testadas: ",resumo_sessaoi[resumo_sessaoi$tipo=="qcoch",]$nome1,"."))
    
    ##### OUTROS
    
    ## catsame (caixa de resposta)
    if(c("catsame") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Para os itens contidos em perguntas do tipo \"caixa de respostas\", em que cada item pode ser respondido em uma das categorias apresentadas, a saber, ",resumo_sessaoi[resumo_sessaoi$tipo=="catsame",]$nome2,", cálculo da frequência, frequência relativa e, para as respostas dicotômicas, intervalo de confiança para uma das proporções, incluindo todas as comparações entre as proporções de respostas das variáveis."))
    
    ## Análise de sensibilidade
    if(c("sensi") %in% resumo_sessaoi$tipo) desc_i=c(desc_i,paste0("\n + Realização da análise de sensibilidade, com cálculo da acurácia, sensibilidade, especificidade, Valor Predito Positivo (VPP) e Valor Predito Negativo (VPN) dos testes ",printvetor(resumo_sessaoi[resumo_sessaoi$tipo=="sensi",]$nome2, aspas=F),", tomando como padrão ouro o teste",resumo_sessaoi[resumo_sessaoi$tipo=="sensi",]$nome1,"."))
    
    descricaoanalises = c(descricaoanalises, paste0("\n **",unique(resumo_sessaoi$Sessão),"** \n",paste0(desc_i, collapse="\n",sep="\n")))
  }
  
  ## Cálculos
  
  totaltab = table(factor(analises$tipo,levels=preçotestes$cod))
  valorbruto=sum(totaltab*preçotestes$preço)
  
  crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazo, "%d/%m/%Y"),".",sep="",collapse="")
  
  datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
  dataprop=format(Sys.Date(), "%d/%m/%Y")
  
  ## Prazo
  cortes_prazo = c(1,2,3,5,10)
  aumento_prazo = c(0.5,0.3,0.15,0.05,0)
  mult_prazo = aumento_prazo[max(which(prazo>=cortes_prazo))]
  
  ## Número de análises
  numero = sum(totaltab)
  cortes_numero = c(1,11,51,101)
  aumentos_numero = c(0,-0.01,-0.03,-0.05)
  mult_numero = aumentos_numero[max(which(numero>=cortes_numero))]
  
  ## Conhecimento
  aumentos_conhecimento = c(0,0.05,0.10)
  mult_conhecimento = sum(conhecimento*aumentos_conhecimento)
  
  ## Continuidade
  aumentos_continuidade=c(0,-0.05,-0.1)
  mult_continuidade = sum(continuidade*aumentos_continuidade)
  
  ## Perfil
  aumentos_perfil=c(0,0.05,0.15)
  mult_perfil = sum(perfil*aumentos_perfil)
  
  ## Extras
  aumentos_extra = c(150,300/60,1)
  soma_extras = sum(tempo*aumentos_extra)
  
  ## Taxas e impostos
  mult_imposto = 1/(1-imposto)
  
  
  preco_parcelado = round(((valorbruto*(1+mult_prazo)*(1+mult_perfil)*(1+mult_conhecimento)*(1+mult_numero)*(1+mult_continuidade))+soma_extras)*mult_imposto,2)
  
  preco_desconto = round(0.95*preco_parcelado,2)
  
  ### Parcelamentos
  
  preço_parcelas=rep(0,parcelas)
  preço_parcelas[-1]=round(preco_parcelado/parcelas,2)
  preço_parcelas[1]=preco_parcelado-sum(preço_parcelas[-1])
  
  dataparcelas = Sys.Date()+30*(1:parcelas-1)
  
  crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazo, "%d/%m/%Y"),".",sep="",collapse="")
  
  datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
  dataprop=format(Sys.Date(), "%d/%m/%Y")
  dataparcelas = format(Sys.Date()+30*(1:parcelas-1), "%d/%m/%Y")
  
  
  # Investimento
  
  invest1 = paste("O investimento para consultoria é de **R$ ",format_real(preco_parcelado),"**, pago em ",parcelas,"x, sendo: \n
", paste(paste(" * _R$ ",format_real(preço_parcelas),"_ no dia ", dataparcelas,"; \n",sep=""),collapse=""),"
OU \n
 * **R$ ",format_real(preco_desconto),"** (5% de desconto) à vista via pix, no ato do aceite da proposta. \n",sep="",collapse="")
  
  invest2 =  paste("Proposta válida até ",datavalido,".",sep="",collapse="")
  
  
  cat("
\\newpage

# ORÇAMENTO

## BASE DE DADOS
",descritivacompleta,"
Qualquer alteração no(s) banco(s) após o aceite da proposta poderá gerar retrabalho por parte da estatística e, portanto, poderá ser cobrado. Por este motivo, pedimos que tenha certeza sobre o envio do material.

## OBJETIVOS DA CONSULTORIA
O objetivo da presente consultoria é a entrega dos seguintes resultados:
", descricaoanalises,"

## CRONOGRAMA
",crono,"

## FORMA DE ENTREGA
A consultoria proposta inclui:

* Definição dos métodos de análise
* Aplicação da metodologia apropriada
* Relatório de análise estatística com análises, tabelas, interpretação dos resultados, detalhes da metodologia e bibliografia utilizadas;
* Construção de gráficos sob demanda por 6 meses a partir da entrega do relatório;
* Uma reunião de ",tempo[2]," minutos para esclarecimento de dúvidas metodológicas.

## INVESTIMENTO
",invest1,dadosbanc,invest2,"

## OBSERVAÇÕES
* Não nos cabe a conclusão e argumentação dos resultados, uma vez que não detemos a “inteligência” de cada trabalho e sua respectiva área de pesquisa.
* O trabalho que desenvolvemos é único e exclusivamente para a realização da análise estatística, não é de nossa competência fazer a edição e formatação dos resultados conforme regimento de cada universidade e/ou revista.

\\newpage",
      paste0("Campinas, ",mday(Sys.Date())," de ",mês[month(Sys.Date())]," de ",year(Sys.Date()),sep=""),"

<br> <br>

------------------------------------------------------------------------ \n

",assinatura,"

<br>

------------------------------------------------------------------------ \n
",
      paste0("###### **Pesquisador(a):** ",nomepesquisador," \n",sep=""),
      
      paste0("###### CPF: ",cpfpesquisador," \n",sep=""),"

\\newpage

# Bibliografia

",sep="\n", collapse="")}
