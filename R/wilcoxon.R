wilcox = function (id, time, num, nomex, moms, tipox = "numeric", nometime = "Momento",cor="cyan4") 
{
    id = factor(id)
    time = factor(time, levels = moms)
    if (tipox == "ordinal") {
        niveisordi = names(table(num)); num = as.numeric(num)
    }
    df = data.frame(id, time, num)
    df = df %>% filter(time %in% moms)
    df_wide = tidyr::pivot_wider(df, names_from = time, values_from = num)
    df_wide = na.omit(df_wide)
    df_long = tidyr::pivot_longer(df_wide, 2:dim(df_wide)[2], 
        values_to = "num", names_to = "time")
    df_long$time = factor(df_long$time, levels = moms)
    df_long$id = factor(df_long$id)
    stat.test <- df_long %>% wilcox_test(num ~ time, paired = T) %>% 
        add_significance()
    es = df_long %>% wilcox_effsize(num ~ time, paired = T)
    a2 = unname(round(stat.test$statistic, dig))
    a3 = ifelse(stat.test$p < 0.001, "<0.001", paste0("=", round(stat.test$p, 
        3)))
    dif = ifelse(stat.test$p < 0.05, " foi estatisticamente diferente", 
        " não foi estatisticamente diferente")
    
    medias = apply(df_wide[,moms],2,function(x) mean(x, na.rm=T))
    diferenca = medias[2]-medias[1]
    if(stat.test$p < 0.05) {
      if(diferenca>0) textodif = paste0(" um aumento em '",names(medias)[2],"' com relação a '",names(medias)[1],"'");
      if(diferenca<0) textodif = paste0(" uma diminuição em '",names(medias)[2],"' com relação a '",names(medias)[1],"'")} else textodif=""
    
    texto = paste0(" - A variável '", nomex, "'", dif, ", com",textodif," ao nível de 5% de significância  usando o teste de Wilcoxon (V=", a2, ",p", a3, "), com effect size =", round(unname(es$effsize), 2), ", que pode ser considerado um efeito ", ifelse(es$magnitude == 
            "small", "pequeno", ifelse(es$magnitude == "moderate", 
            "moderado", "grande")), ".", sep = "")
    resumo = str_sub(texto,start=4)
    teste = paste0("Wilcoxon V =", a2, ", p", a3)
    
  
    
    if (tipox == "numeric") {
        grafico = grafico_comp_box_pareado(id=df_long$id,cont=df_long$num,nomecont=nomex,cat=df_long$time,nomecat=nometime,moms=moms,cor=cor,teste=teste)
        res = desc_bi_cont(df_long$num, df_long$time, respcol = F)[-1, 
            -2]
        res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], 
            ")", sep = ""), rep("", dim(res)[2] - 1)), res), 
            `p-valor` = c("", paste0(pvalor(stat.test$p), "j"), 
                rep("", dim(res)[1] - 1)))
    }
    else {
        df_long$num = factor(df_long$num)
        levels(df_long$num) = niveisordi
        textograf <- substitute(paste("Teste de Wilcoxon (V=", 
            a2, ",p", a3, ")", collapse = ""), list(a2 = a2, 
            a3 = a3))
        grafico = grafico_catcat(df_long$time, nometime, df_long$num, 
            nomex, texto = textograf,cor=cor) + labs(title = vetor_comsep_c(paste0("Diferença de proporções de ", 
            nomex), 40))
        res = desc_bi_cat(df_long$num, col = df_long$time, respcol = F)[-1, 
            -2]
        res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], 
            ")", sep = ""), rep("", dim(res)[2] - 1)), res), 
            `p-valor` = c("", paste0(pvalor(stat.test$p), "j"), 
                rep("", dim(res)[1] - 1)))
    }
    testes = data.frame(Nome1 = nometime, Nome2 = nomex, tipo = "wilc", 
        sig_ou_não = ifelse(stat.test$p < 0.05, T, F), resumo = resumo, sup = NA)
    return(list(testes = testes, result = res, grafico = grafico, "\n",
        texto = texto))
}
