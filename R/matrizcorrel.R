matrizcorrel = function (x, nomes=NULL, metodo="pearson",use="pairwise.complete.obs", margin=100, size=3) 
{
  library(corrplot)
  if(is.null(nomes)) nomes = names(x) else names(x) = nomes
    m = cor(x, method = metodo, use = use)
    a = cor.mtest(x, method = metodo, use = use)
    n = length(nomes)
    sig = matrix(nrow = n, ncol = n)
    for (i in 1:n) for (j in 1:n) if (i == j) 
        sig[i, j] = "-"
    else if (i < j) 
        sig[i, j] = ""
    else if (a$p[i, j] < 0.01) 
        sig[i, j] = paste0(round(m[i, j], 3), "**")
    else if (a$p[i, j] < 0.05) 
        sig[i, j] = paste0(round(m[i, j], 3), "*")
    else sig[i, j] = paste0(round(m[i, j], 3))
    result = data.frame(nomes, sig)
    names(result) = c("", nomes)
    names(x) = nomes

if(is.null(nomes)) nomes <- names(x) else names(x) <- nomes

m <- cor(x, method = metodo, use = use)
a <- cor.mtest(x, method = metodo, use = use)

n <- ncol(m)

dados <- expand.grid(
  Var1 = rownames(m),
  Var2 = colnames(m)
)

dados$r <- c(m)
dados$p <- c(a$p)

dados$i <- match(dados$Var1, rownames(m))
dados$j <- match(dados$Var2, colnames(m))

dados <- dados %>%
  mutate(
    label = case_when(
      i == j ~ "-",
      i < j ~ "",
      p < 0.01 ~ sprintf("%.3f**", r),
      p < 0.05 ~ sprintf("%.3f*", r),
      TRUE ~ sprintf("%.3f", r)
    )
  )

# Apenas abaixo da diagonal
dados2 <- dados %>%
  filter(i > j)

labs <- data.frame(
  x = 1:(n-1),
  y = rev(2:n)-0.4,
  label = head(nomes, -1)
)

grafico = ggplot(dados2,
       aes(x = Var2,
           y = factor(Var1, levels = rev(rownames(m))),
           fill = r)) +

  geom_tile(color = "white") +

#  geom_text(aes(label = label), size = 4) +

  scale_fill_gradient2(
    low = "coral3",
    mid = "gray",
    high = "cyan4",
    midpoint = 0,
    limits = c(-1,1)
  ) +

  coord_equal(clip = "off") +

  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +

  theme_minimal() +

  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = margin),
    legend.position='bottom',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    data = labs,
    aes(x = x, y = y, label = label),
    angle = 45,
    hjust = 0,
    vjust = 0,
    inherit.aes = FALSE,
    size = size
  )
        
    testes = data.frame(Nome1 = "", Nome2 = printvetor(nomes, 
        aspas = F), tipo = "matcorrel", sig_ou_não = NA, resumo = NA, 
        sup = NA)
    return(list(testes = testes, result = result, grafico = grafico))
}
