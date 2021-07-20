dados.pacientes <- read.csv('data/dadosPacientes2021.csv')
# Tratamento de dados

idade <- dados.pacientes$idade
dados.pacientes <- subset(dados.pacientes, idade > 0 & idade < 150)

dados.pacientes[dados.pacientes == "Mascuino"] <- "Masculino"

# Fim do tratamento de dados

# Plotando o gráfico da situação atual dos pacientes

situacao.atual.pacientes <- dados.pacientes$situacao_atual

tabela.situacao.atual <- sort(table(situacao.atual.pacientes), decreasing = FALSE)

cores <- sample(c('#B7E2A5', '#836DAD', '#59F966', '#458FB2', '#D09622', '#448546', '#28AB78', '#A41965', '#E523FA', '#4B71F4', '#B00440', '#C1691A', '#2BFD80', '#2F38B1', '#C6C394', '#CD26F0', '#1817A0', '#AEA5FA', '#58CE3C', '#95CA74', '#8BCF38', '#D383B3', '#159944', '#03866D', '#C924A8', '#CC4D7A', '#89C740', '#5F279F', '#49A448', '#85DBCE', '#9C195B', '#0B2421', '#11B1A6', '#E6DBD6', 
                  '#9E7E93', '#83A79A', '#D3FD07', '#4F281B', '#BF0383', '#2AB595', '#D319AB', '#C2F5C1', '#939451', '#F78FED', '#8E4C16', '#8ED9E6', '#08669B', '#F8D3FD', '#12975B', '#99B181'))
categorias <- names(tabela.situacao.atual)

barplot(tabela.situacao.atual, main="Situação atual dos pacientes",
        col=topo.colors(8),
        names.arg = c(""),
        ylim=c(0,160000),
        xlab="Situação atual",
        ylab="Números de pacientes",
        )
legend("topleft", bty="n", 
        legend=categorias,
        fill=topo.colors(8))

# Fim do gráfico da situação atual dos pacientes

# Plotando o gráfico de óbitos por município

pacientes.obitos <- subset(dados.pacientes, dados.pacientes$situacao_atual == "Óbito" | dados.pacientes$situacao_atual == "óbito por outras causas")
tabela.pacientes.obitos.municipio <- sort(table(pacientes.obitos$municipio_residencia), decreasing=TRUE)
barplot(tabela.pacientes.obitos.municipio, 
        main="Número de óbitos por município",
        col=sample(cores),
        cex.names=0.7,
        xlab="Óbitos",
        ylab="Cidades",
        las=2, cex.lab=0.1, ylim=c(0, 2500)
        )

# FIm do gráfico óbitos por município

# Óbitos por sexo e idade
breaks <- seq(0, 110, by=10)
#cbind(Masculino = table(cut(subset(pacientes.obitos, sexo == "Masculino")$idade, breaks)))
pacientes.obitos.idade <- cut(pacientes.obitos$idade, breaks)
pacientes.sexo.idade <- table(pacientes.obitos$sexo, pacientes.obitos.idade)

names <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110")

barplot(pacientes.sexo.idade, beside=TRUE, las=2,
        main="Gráfico de óbitos separados por sexo e idade",
        ylab="Óbitos", names.arg=names,  
        col=c("#cc0000", "#00cc00"), ylim=c(0, 800))

legend("topleft", pch=15,col=c("#00cc00", "#cc0000"), 
       legend=c("Sexo Masculino", "Sexo Feminino"), cex=1.25,
       bty="n")

# Fim do gráfico dos óbitos por sexo e idade

# Histograma de óbitos por idade
paleta <- colorRampPalette(c("white", "#ff0000"))

labels <- as.character(table(cut(pacientes.obitos$idade, breaks)))

hist(pacientes.obitos$idade, 
     main="Frequência de óbitos por idade", 
     ylab="Óbitos",
     xlab="Idade",
     col=paleta(11),
     ylim=c(0, 1500),
     xaxp=c(0, 120,12), labels=labels
     )

# Fim gráfico de óbitos por idade

# Gráfico de óbitos por mês
datas.obitos <- cut.Date(as.Date(pacientes.obitos$data_resultado_exame), seq.Date(as.Date('2020-03-01'), as.Date('2021-06-01'), '1 month'))
datas.obitos.table <- table(datas.obitos)

names <- c("Mar/20", "Abr/20", "Maio/20", 
                "Jun/20", "Jul/20", "Ago/20",
                "Set/20", "Out/20","Nov/20", 
                "Dez/20", "Jan/21","Fev/21", 
                "Mar/21", "Abr/21", "Maio/21")

barplot(datas.obitos.table, main="Óbitos por mês", 
        ylim=c(0, 1000), names.arg=names, las=2,
        ylab="Óbitos", col=c("#4d4dff", "#ff4d4d"), cex.names=0.9,
        border=c("black"))

