coch_icic = function(id,time,cat,moms,nomex,nometime,cor="cyan4"){

df=data.frame(id,time,cat)

df = df[df$time %in% moms,]

df_wide = tidyr::pivot_wider(df,names_from = time,values_from = cat)

df_wide=na.omit(df_wide)

df_long = tidyr::pivot_longer(df_wide,2:(dim(df_wide)[2]),values_to = "cat",names_to="time")
df_long$time=factor(df_long$time, levels = moms)
df_long$id = factor(df_long$id)
df_long$cat = factor(df_long$cat)

cq = rstatix::cochran_qtest(df_long, cat ~ time|id)

a1 = unname(cq$df)
a2 = unname(round(cq$statistic, dig))
a3 = ifelse(cq$p < 0.001, "<0.001", paste0("=",round(cq$p,3)))

textograf <- substitute(paste("Teste de Cochran (", chi^2, 
"(", a1, ") =", a2, ",p", a3, ")", collapse = ""), list(a1 = a1, a2 = a2, a3 = a3))

pwc = pairwise_mcnemar_test(df_long, cat ~ time|id)

n=length(pwc$p)
desc_pw = paste(pwc$group1," e ",pwc$group2," (p=",pvetor(pwc$p.adj),")",sep="")

if(cq$p<0.05) comp = ifelse(sum(pwc$p.adj<0.05)==0,
                            paste0("Apesar disso, os testes pareados de McNemar não detectaram nenhum momento diferindo dos demais (",printvetor(desc_pw,aspas=F),")."),
       ifelse(sum(pwc$p.adj<0.05)==n,paste0("Os testes pareados de McNemar detectaram diferenças de proporções em todos os momentos (",printvetor(desc_pw,aspas=F),")."),
            paste0("Os momentos que diferiram pelos testes pareados de McNemar com correção de bonferroni foram ",printvetor(desc_pw[pwc$p.adj<0.05],aspas=F)," e os que não diferiram foram ",printvetor(desc_pw[pwc$p.adj>=0.05],aspas=F),". \n"))) else comp=NULL

dif = ifelse(cq$p<0.05," teve proporções estatisticamente diferentes"," não teve proporções estatisticamente diferentes")

texto = paste(" - A variável '", nomex, "'",dif," nos momentos ao nível de 5% de significância  usando o teste de Cochran ($\\chi^2$ (", a1, ") =", a2, ",p", a3, ").",comp,sep="",collapse="")

grafico = grafico_catcat(df_long$time,nometime,df_long$cat,nomex, texto=textograf,cor=cor) + labs(title=vetor_comsep_c(paste0("Diferença de proporções de ",nomex),40))

res=desc_bi_cat(df_long$cat,col=df_long$time,)[-1,-2]

res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], ")", sep = ""), 
        rep("", dim(res)[2])), res), `p-valor` = c("", paste0(pvalor(cq$p),"l"), rep("", 
        dim(res)[1] - 1)))
  
 testes = data.frame(Nome1 = nometime, Nome2 = nomex, tipo = "qcoch", sig_ou_não = ifelse(cq$p < 0.05, T, F), resumo = paste("$\\chi^2$ (", a1, ") =", a2, ",p", a3, collapse = ""), sup = NA)

return(list("testes"=testes,"result"=res,"texto"=texto,"\n","grafico"=grafico))}
