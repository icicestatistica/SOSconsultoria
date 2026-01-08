cat_same_levels = function (dad, nomes = "auto", niveis = "auto", nas = F, dig = 2, 
    cor = "cyan4", invertercores = F, idioma = "PT") 
{
    if (nomes[1] == "auto") 
        nomes = names(dad)
    names(dad) = nomes
    if (niveis[1] == "auto") 
        niveis = names(table(matrix(unlist(dad))))
    df = data.frame(Variável = nomes[1], t(desc_uni_categorica(dad[, 
        1], "", niveis, nas, T, F, F, F, F, F, dig)$result[, 
        4]))
    if (dim(dad)[2] > 1) {
        for (i in 2:dim(dad)[2]) {
            df = rbind(df, data.frame(Variável = nomes[i], t(desc_uni_categorica(dad[, 
                i], "", niveis, nas, T, F, F, F, F, F, dig)$result[, 
                4])))
        }
    }
    if (nas == T) 
        niveis = c(niveis, "N/A")
    names(df) = c("Variável", niveis)
    for (i in 1:dim(dad)[2]) dad[, i] = factor(unlist(dad[, i]), 
        levels = niveis)

  ## Aqui estou criando o banco de dados com só a categoria máxima pra fazer o resumo
     resumo <- dad %>%
                  pivot_longer(cols = everything(),
                   names_to = "variavel",
                   values_to = "categoria") %>%
                    count(variavel, categoria) %>%
                    group_by(variavel) %>%
                    mutate(proporcao = n / sum(n)) %>%
                    slice_max(n, n = 1, with_ties = FALSE) %>%
                    ungroup()

  ## Texto que vai pro 'testes'
  textos <- resumo %>%
    mutate(var_prop = paste0(variavel, " (", round(proporcao * 100, 1), "%)")) %>%
    group_by(categoria) %>%
    summarise(
      texto = paste0(
        "As categorias ",
        paste(var_prop, collapse = ", "),
        " foram mais prevalentes para a resposta ",
        categoria,
        "."
      ),
      .groups = "drop"
    )
  
    cores = ifelse(invertercores == T, "lighten(cor, seq(0, (1 - 1/(length(table(newmat$Resultado)))), \n            1/(length(table(newmat$Resultado)))))", 
        "rev(lighten(cor, seq(0, (1 - 1/(length(table(newmat$Resultado)))), 1/(length(table(newmat$Resultado))))))")
    mat = data.frame(Coluna = rep(names(dad), each = dim(dad)[1]), 
        Resultado = matrix(unlist(dad)))
    mat$Coluna = factor(mat$Coluna, levels = names(dad))
    levels(mat$Coluna) = paste0(nomes, " (n=", table(na.omit(mat)$Coluna), 
        ")")
    ordem = mat %>% group_by(Coluna) %>% dplyr::summarise(mean = mean(as.numeric(Resultado), 
        na.rm = T))
    oc = as.character(ordem$Coluna[order(ordem$mean)])
    newmat = data.frame(table(mat), n = data.frame(table(na.omit(mat)$Coluna))[, 
        2])
    newmat$label = paste(newmat$Freq, " (", round(100 * newmat$Freq/newmat$n, 
        dig), "%)", sep = "", collapse = NULL)
    newmat$Coluna = factor(newmat$Coluna, levels = oc)
    plot = ggplot(newmat, aes(y = Coluna, x = Freq/n, fill = Resultado)) + 
        geom_bar(stat = "identity", position = position_stack(reverse = T)) + 
        labs(title = "Frequência e Frequência Relativa por resultado", 
            y = NULL, x = NULL) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
        scale_x_continuous(labels = scales::percent) + geom_text(label = ifelse(newmat$label == 
        "0 (0%)", "", newmat$label), position = position_stack(reverse = T, 
        vjust = 0.5), check_overlap = T, hjust = 0.5) + scale_fill_manual(values = eval(parse(text = cores)))
    testes = data.frame(Nome1 = "", Nome2 = nomes, tipo = "catsame", 
        sig_ou_não = "cat", resumo = paste0(unique(textos$texto),collapse=" "), sup = "")
    

    return(list(testes = testes, t1 = "A análise detalhada das variáveis está na tabela a seguir: \n", 
        result = df, t2 = "\n Podemos visualizar esses resultados no seguinte gráfico: \n", 
        grafico = plot))
}


cat_same_levels_2 <- function(x,nomes="auto",nomey,levels="auto",dig=2,cor="cyan4",sepvec=50,idioma="PT"){

if(nomes[1]=="auto") nomes = names(x)
names(x)=nomes
if (levels[1]=='auto') levels = c("Sim","Não")
prop=prop.test(table(factor(unlist(x[,1]),levels=levels)))
df = data.frame("Variável"=nomes[1],"n"=length(na.omit(unlist(x[,1]))),desc_uni_categorica(x[,1],"",levels,F,F,F,F,F,F,F,dig)$result[1,c(2,3)],prop$conf.int[1],prop$conf.int[2])
for (i in 2:dim(x)[2]){
  prop=prop.test(table(factor(unlist(x[,i]),levels=levels)))
  df = rbind(df,data.frame("Variável"=nomes[i],"n"=length(na.omit(unlist(x[,i]))),desc_uni_categorica(x[,i],"",levels,F,F,F,F,F,F,F,dig)$result[1,c(2,3)],prop$conf.int[1],prop$conf.int[2]))}

names(df)=c("Variável","n",'Frequência',"Freq. Relativa", "ICmin","ICmax")
df = df[order(as.numeric(str_sub(df$`Freq. Relativa`,end=-2)), decreasing=T),]

freqs_aux = paste0(df$Variável," (",df$`Freq. Relativa`,")")

resumo = paste0("Com relação a **",nomey,"**, tivemos maior frequência de ",freqs_aux[1], ", seguido(a) de " ,printvetor(freqs_aux[-1]),".")  
interp = paste0(resumo," Podemos ver mais detalhes na tabela a seguir: \n")

df_printar=data.frame(df[,1:4],paste("(",round(100*df$ICmin,dig),"%, ",round(100*df$ICmax,dig),"%)",sep=""))
names(df_printar)=c("Variável","n",'Frequência',"Freq. Relativa", "IC 95% para Freq.")

texto=list(paste("Podemos avaliar esta tabela comparando os intervalos de confiança de cada ítem. Caso os intervalos de confiança de dois ítens se sobreponham, isso significa que não rejeitamos a diferença entre as proporções nesses dois ao nível de 5% de significância. Portanto, interpretamos da seguinte forma:"," \n",sep=""))
for (i in 1:(dim(df)[1])){
if(sum(df$ICmin[i]>df$ICmax)>1) texto = list.append(texto,paste0(" *  ",df$Variável[i]," possui frequência maior que ",printvetor(df$Variável[df$ICmin[i]>df$ICmax]),"."," \n",sep="")) else
  if(sum(df$ICmin[i]>df$ICmax)==1) texto = list.append(texto,paste(" * ",df$Variável[i]," possui frequência maior que ",df$Variável[df$ICmin[i]>df$ICmax],"."," \n", sep="")) else
    if(i==dim(df)[1]) texto = list.append(texto,paste(" * ",df$Variável[i]," não possui frequência maior que nenhum."," \n",sep="")) else {
      texto=list.append(texto, paste(" * ",printvetor(df$Variável[i:dim(df)[1]])," não possuem frequências maiores que nenhum."," \n", sep="")); break}}
texto=list.append(texto, paste("É possível visualizar esses resultados no gráfico a seguir:"," \n"))

df$Variável <- vetor_comsep_c(unlist(df$Variável),sepvec)

if(idioma=="PT") tituloeixox="Proporção em %" else tituloeixox="Proportion in %"
result <- mutate(df, Variável=fct_reorder(Variável, desc(-as.numeric(str_sub(`Freq. Relativa`,end=-2)))))
result$label = paste0(result$Frequência," (",round(as.numeric(str_sub(result$`Freq. Relativa`,end=-2)),0),"%)")
grafico=ggplot(result, aes(y = Variável, x = as.numeric(str_sub(`Freq. Relativa`,end=-2))/100)) + xlab(tituloeixox) + ggtitle(vetor_comsep_c(paste(nomey," (n=",dim(na.omit(x))[1],")",sep=""),50)) + ylab("") + theme_minimal() +
  geom_bar(stat="identity", fill=cor) +
  geom_errorbar(aes(xmax = ICmax, xmin = ICmin), color="gray", width=0.2) +
  geom_label(aes(label=label), alpha=0.8,hjust=0.5) +
  scale_x_continuous(labels = scales::percent,expand = expansion(mult = 0.1)) + theme(plot.title = element_text(hjust = 0.3))

testes=data.frame("Nome1"="","Nome2"=nomes,"tipo"="catsame","sig_ou_não"="cat","resumo"=resumo,"sup"="")

return(list("testes"=testes,
            "interp"=interp,
            "result"=df_printar,
            "caption"=rodape_tabela(nomey,testes),"\n",
            "texto"=texto,
            "gráfico"=grafico))}
