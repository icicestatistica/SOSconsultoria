contcont = function (y, x, nomey, nomex, dig = 2, cor = "cyan4", idioma = "PT", 
    ordinalx = "auto", ordinaly = "auto", respcol=T,virgula=F) 
{
    method = "spearman"
    x = unlist(x)
    y = unlist(y)
    d = data.frame(x, y)
    if (ordinalx == "auto") 
        ordinalx = ifelse(length(table(d$x)) > 10, F, T)
    if (ordinaly == "auto") 
        ordinaly = ifelse(length(table(d$y)) > 10, F, T)
    bootcor <- function(data, indices, met) {
        df = data[indices, ]
        return(cor.test(unlist(df[, 1]), unlist(df[, 2]), method = met, exact=F)$estimate)
    }
    if (ordinalx == T | ordinaly == T) {
        sup = " A escolha pelo teste não paramétrico de correlação de spearman se deve a natureza ordinal da(s) variável(is)."} else {
        if (length(x) - sum(is.na(x)) < 3 || length(x) - sum(is.na(x)) > 
            5000 || summary(x)[1] == summary(x)[6]) {
            str1 = "N/A"
        }
        else {
            str1 = shapiro.test(x)
        }
        if (length(y) - sum(is.na(y)) < 3 || length(y) - sum(is.na(y)) > 
            5000 || summary(y)[1] == summary(y)[6]) {
            str2 = "N/A"
        }
        else {
            str2 = shapiro.test(y)
        }
        if (str1[1] == "N/A" || str2[1] == "N/A") 
            sup = c(" A suposição de normalidade das amostras não pode ser verificada pelo teste de Shapiro-Wilk, uma vez que o tamanho de pelo menos uma das amostras foi menor que 3 ou maior que 5000. Por este motivo, utilizamos o teste não paramétrico de correlação de spearman ao invés da correlação de pearson.")
        else {
            if (str1$p.value >= 0.05 & str2$p.value >= 0.05) {
                sup = c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor maior que 0.05 para as duas amostras não rejeitou a normalidade das distribuições (", 
                  nomex, " - W=", round(str1$statistic, 2), ", p-valor=", 
                  pvalor(str1$p.value), ", ", nomey, " - W=", 
                  round(str2$statistic, dig), ", p-valor=", pvalor(str2$p.value), 
                  "), atendendo a suposição do teste.")
                method = "pearson"
            }
            if (str1$p.value >= 0.05 & str2$p.value < 0.05) 
                sup = c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor menor que 0.05 rejeitou a normalidade da distribuição de ", 
                  nomey, " - W=", round(str2$statistic, dig), 
                  ", p-valor=", pvalor(str2$p.value), ", mas não de ", 
                  nomex, " - W=", round(str1$statistic, dig), 
                  ", p-valor=", pvalor(str1$p.value), ". Como a suposição de normalidade foi violada, justificamos a utilização do teste não paramétrico de correlação de spearman ao invés da correlação de pearson.")
            if (str1$p.value < 0.05 & str2$p.value >= 0.05) 
                sup = c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor menor que 0.05 rejeitou a normalidade da distribuição de ", 
                  nomex, " - W=", round(str1$statistic, dig), 
                  ", p-valor=", pvalor(str1$p.value), ", mas não de ", 
                  nomey, " - W=", round(str2$statistic, dig), 
                  ", p-valor=", pvalor(str2$p.value), ". Como a suposição de normalidade foi violada, justificamos a utilização do teste não paramétrico de correlação de spearman ao invés da correlação de pearson.")
            if (str1$p.value < 0.05 & str2$p.value < 0.05) 
                sup = c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor menor que 0.05 rejeitou a normalidade da distribuição das duas variáveis (", 
                  nomex, " - W=", round(str1$statistic, dig), 
                  ", p-valor=", pvalor(str1$p.value), ",  ", 
                  nomey, " - W=", round(str2$statistic, dig), 
                  ", p-valor=", pvalor(str2$p.value), "). Como a suposição de normalidade foi violada, justificamos a utilização do teste não paramétrico de correlação de spearman ao invés da correlação de pearson.")
        }
    }
    a = cor.test(as.numeric(y), as.numeric(x), method = method, 
        exact = F)
    if (method == "spearman") {
        letra = "g"
        greg = "$\\rho$"
        calcIC = " (calculado via bootstrap (utilizando uma aproximação da distribuição normal, com 1000 reamostragens) "
        bootc = boot(data.frame(x = as.numeric(x), y = as.numeric(y)), 
            bootcor, R = 1000, met = "spearman")
        ac = boot.ci(bootc, type = "norm")
        IC = paste0("(", round(ac$normal[2], dig), " ,", round(ac$normal[3], 
            dig), "$)^{[a]}$")
        if (ac$normal[2] * ac$normal[3] <= 0) 
            avalic = " inclui o valor '0', indicando ausência de correlação."
        else avalic = " não inclui o valor '0', indicando que esta é considerada significativa."
        det = "os postos (também chamados 'ranks') das duas variáveis."
    }
    else {
        letra = "h"
        greg = "r"
        calcIC = ""
        IC = paste0("(", round(a$conf.int[1], dig), " ,", round(a$conf.int[2], 
            dig), ")")
        if (a$conf.int[1] * a$conf.int[2] <= 0) 
            avalic = " inclui o valor '0', indicando ausência de correlação" else avalic = " não inclui o valor '0', indicando que esta é considerada significativa."
        det = "as duas variáveis."
    }
    p = paste0(pvalor(a$p.value), letra)
    rho = round(a$estimate, dig)
    rho2 = round(a$estimate^2, (2 * dig))
    if (greg == "r") 
        greg2 = "$r^2$"
    else greg2 = "${\\rho}^2$"
    coefdet = paste(" Calculamos também o coeficiente de determinação, dado pelo quadrado da correlação ", 
        greg2, "=$(", round(rho, dig), ")^2$=", rho2, ", que indica ", 
        100 * rho2, "% de variância compartilhada entre ", det, 
        sep = "")
    if (a$estimate < 0) 
        direcao = "inversa entre as variáveis (nos indivíduos em que uma é maior, a outra é menor)."
    else direcao = "direta entre as variáveis (nos indivíduos em que uma é maior, a outra é também é)."
    ictexto = paste0(" O intervalo de confiança ", IC, calcIC, 
        avalic, " Reiteramos a importância de avaliar o significado deste resultado na prática.", 
        collapse = "")
    if (abs(a$estimate) >= 0.1 & abs(a$estimate) < 0.3) 
        mag = "Cohen(1992) propõe esta magnitude de correlação como fraca."
    else if (abs(a$estimate) >= 0.3 & abs(a$estimate) < 0.5) 
        mag = "Cohen(1992) propõe esta magnitude de correlação como moderada."
    else if (abs(a$estimate) >= 0.5) 
        mag = "Cohen(1992) propõe esta magnitude de correlação como forte."
    else mag = "Cohen(1992) propõe esta magnitude de correlação como insignificante ou irrisória."
    if (a$p.value < 0.05) {
        texto = c("* **", nomex, "**: O teste de correlação de ", 
            method, " rejeitou a hipótese de nulidade de correlação (", 
            greg, "=", round(a$estimate, dig), ", p-valor=", 
            pvalor(a$p.value), "), indicando uma relação ", 
            direcao, " ", mag, coefdet, ictexto, sup, "\n")
        significativo = T
        resumo_final = paste0("Encontramos uma correlação ",ifelse(a$estimate>0,"positiva","negativa")," e estatísticamente significativa entre ",nomey," e ",nomex, " com o teste de correlação de ",method, " (",greg, "=", round(a$estimate, dig), ", p-valor=",pvalor(a$p.value),"; 95% IC=", IC,").")
    }
    else {
        texto = c("* **", nomex, "**: Não encontramos evidências através do teste de correlação de ", 
            method, " para rejeitar a hipótese de nulidade de correlação (", 
            greg, "=", round(a$estimate, dig), ", p-valor=", 
            pvalor(a$p.value), "). De fato, ", mag, coefdet, 
            ictexto, sup, " \n")
        significativo = F
        resumo_final = paste0("Não encontramos correlação estatísticamente significativa entre ",nomey," e ",nomex, " com o teste de correlação de ",method, " (",greg, "=", round(a$estimate, dig), ", p-valor=",pvalor(a$p.value),"; 95% IC=", IC,").")
    }
    rhoprint = round(rho, dig)
    pvalorprint = ifelse(a$p.value < 0.001, "<0.001", round(a$p.value, 
        3))
    methodprint = method
    if (idioma == "PT") 
        textograf = substitute(paste("Correlação de ", methodprint, 
            " (", rho, "=", rhoprint, ", p-valor=", pvalorprint, 
            ")"), list(rhoprint = rhoprint, pvalorprint = pvalorprint, 
            methodprint = methodprint))
    else textograf = substitute(paste(methodprint, " correlation (", 
        rho, "=", rhoprint, ", p=", pvalorprint, ")"), list(rhoprint = rhoprint, 
        pvalorprint = pvalorprint, methodprint = methodprint))
    result <- data.frame(Variável = nomex, `p-valor` = p, rho = rho, 
        rho2 = rho2, IC = IC)
    names(result) = c("Variável", "p-valor", "Estatística", 
        "Variância compartilhada", "IC (95%)")
    if (ordinalx == F & ordinaly == F) {
        if(respcol==T) grafico = grafico_correl(as.numeric(y), nomey, cor, as.numeric(x),nomex, textograf, idioma,virgula)
        else grafico = grafico_correl(x, nomex, cor, as.numeric(y),nomey, textograf, idioma,virgula)}       
    else if (ordinalx == F & ordinaly == T) 
        grafico = grafico_comp_box(as.numeric(x), nomex, y, nomey, 
            cor, textograf, dig, ordenar = F, idioma, dot = "auto",virgula)
    else if (ordinalx == T & ordinaly == F) 
        grafico = grafico_comp_box(as.numeric(y), nomey, x, nomex, 
            cor, textograf, dig, ordenar = F, idioma, dot = "auto",virgula)
    else grafico = grafico_catcat(x, nomex, y, nomey, cor, textograf, 
        idioma,virgula)
    testes = data.frame(Nome1 = nomey, Nome2 = nomex, tipo = paste0("correl_",ifelse(method == "spearman","s","p")), 
        sig_ou_não = ifelse(a$p.value < 0.05, T, F), resumo = resumo_final, sup = NA)
    return(list(testes = testes, result = result, texto = paste0(texto, 
        sep = "", collapse = ""), grafico = grafico))
}
