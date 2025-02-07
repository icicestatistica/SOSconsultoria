get_ORs = function(dados,ngrupo,refgrupo,cols,refs,nomes){

ORs = data.frame()

nome_grupo = names(dados)[ngrupo]

for (i in 1:length(cols)) {
df = dados[,c(cols[i],ngrupo)]
df[,2]=factor(unlist(df[,2]))
df[,2] <- relevel(unlist(df[,2]), ref = refgrupo)
df[,1]=relevel(unlist(df[,1]),ref=refs[i])

modelo <- glm(as.formula(paste0("`",nome_grupo,"` ~ .")), family = binomial(link="logit"), data = df)

# OR com IC
OR = c("",
exp(cbind(OR = coef(modelo), confint(modelo))) %>% 
  round(3) %>% 
  apply(1,function(x) paste0(x[1]," (",x[2],";",x[3],")"))
)

OR[2]="ref"
names(OR)[2]=paste0(nomes[i],refs[i])
names(OR) = str_remove_all(names(OR),fixed("`"))

vari = str_sub(names(OR),end=nchar(nomes[i]))
cate = str_sub(names(OR),start=nchar(nomes[i])+1)

ORs = rbind(ORs,cbind(vari,"Característica"=cate,OR))}

return(ORs)}
