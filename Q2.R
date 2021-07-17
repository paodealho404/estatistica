dados.pacientes <- read.csv('data/dadosPacientes2021.csv')
# Tratamento de dados

idade <- dados.pacientes$idade
dados.pacientes <- subset(dados.pacientes, idade > 0 & idade < 150)

dados.pacientes[dados.pacientes == "Mascuino"] <- "Masculino"

# Fim do tratamento de dados

# Plotando o gráfico da situação atual dos pacientes

situacao.atual.pacientes <- dados.pacientes$situacao_atual

tabela.situacao.atual <- table(situacao.atual.pacientes)
tabela.situacao.atual <- sort(tabela.situacao.atual, decreasing = FALSE)

cores <- sample(8:1)
categorias <- names(tabela.situacao.atual)

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

# Fim do gráfico da situação atual dos pacientes

# Plotando o gráfico de óbitos por município

pacientes.obitos <- subset(dados.pacientes, dados.pacientes$situacao_atual == "Óbito")
tabela.pacientes.obitos.municipio <- sort(table(pacientes.obitos$municipio_residencia), decreasing=TRUE)
barplot(tabela.pacientes.obitos.municipio, 
        main="Número de óbitos por município",
        col=c("red", "black"),
        cex.names=0.7,
        xlab="Óbitos",
        ylab="Cidades",
        las=2, cex.lab=0.1, ylim=c(0, 2500)
        )

# FIm do gráfico óbitos por município

# Óbitos por sexo e idade

par(mfrow=c(1, 1))

pacientes.masculino <- subset(pacientes.obitos, pacientes.obitos$sexo == "Masculino")
paciente.masculino.idade <- sort(pacientes.masculino$idade)

# Histograma dos óbitos de homens por idade
hist(paciente.masculino.idade, 
        main="Óbitos de pacientes do sexo masculino por idade",
        ylab="Óbitos",
        xlab="Idade",
        xaxp=c(0, 120,12),
        ylim=c(0, 1000),
        breaks=10,
        col=c("#00cc00")
)

pacientes.feminino <- subset(pacientes.obitos, pacientes.obitos$sexo == "Feminino")
paciente.feminino.idade <- sort(pacientes.feminino$idade)

# Histograma dos óbitos de feminimo por idade 
hist(paciente.feminino.idade, 
        main="Óbitos de pacientes do sexo feminino por idade",
        ylab="Óbitos",
        xlab="Idade",
        xaxp=c(0, 120,12),
        ylim=c(0, 1000),
        breaks=10,
        col=c("#cc0000")
)

# Fim do gráfico dos óbitos por sexo e idade

# Histograma de óbitos por idade

idadeMinima <- min(pacientes.obitos$idade); idadeMaxima <- max(pacientes.obitos$idade)
hist(pacientes.obitos$idade, 
     main="Frequência de óbitos por idade", 
     ylab="Óbitos",
     xlab="Idade",
     col=c("#99ff99", "#006600"),
     ylim=c(0, 1500),
     xaxp=c(0, 120,12)
     )

# Fim gráfico de óbitos por idade

# Gráfico de óbitos por mês
