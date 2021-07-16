dados.pacientes <- read.csv('data/dadosPacientes2021.csv')

situacao.atual.pacientes <- dados.pacientes$situacao_atual
situacao.atual.pacientes

tabela.situacao.atual <- table(situacao.atual.pacientes)
tabela.situacao.atual <- sort(tabela.situacao.atual, decreasing = FALSE)
tabela.situacao.atual

cores <- sample(8:1)
categorias <- names(tabela.situacao.atual)
# Plotando o gráfico da situação atual dos pacientes
barplot(tabela.situacao.atual, main="Situação atual dos pacientes",
        col=cores,
        names.arg = c(""),
        ylim=c(0,200000),
        xlab="Situação atual",
        ylab="Números de pacientes",
        )
legend("topleft", bty="n", 
       legend=categorias,
       fill=cores)

# Plotando o gráfico de óbitos por município
pacientes.obitos <- subset(dados.pacientes, situacao.atual.pacientes == "Óbito")
tabela.pacientes.obitos.municipio <- sort(table(pacientes.obitos$municipio_residencia), decreasing=TRUE)
barplot(tabela.pacientes.obitos, 
        main="Número de óbitos por município",
        col=c("red", "black"),
        cex.names=0.7,
        xlab="Óbitos",
        ylab="Cidades",
        las=2, cex.lab=0.1, ylim=c(0,2500),
        )

# Histograma de óbitos por idade
pacientes.idade.correta <- subset(pacientes.obitos, pacientes.obitos$idade > 0)
pacientes.obito.idade <- pacientes.idade.correta$idade
hist(pacientes.obito.idade, 
     main="Frequência de óbitos por idade", 
     ylab="Óbitos",
     xlab="Idade",
     col=c("#99ff99", "#006600") 
     )
