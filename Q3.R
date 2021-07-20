dados.pacientes <- read.csv('data/dadosPacientes2021.csv', na.strings=c("", ".", "NA"))

#Tratamento de dados

idade <- dados.pacientes$idade
dados.pacientes <- subset(dados.pacientes, idade > 0 & idade < 150)

dados.pacientes[dados.pacientes == "Mascuino"] <- "Masculino"
dados.pacientes[dados.pacientes == "óbito por outras causas"] <- "Óbito"

dados.pacientes.maceio <- subset(dados.pacientes, municipio_residencia == "Maceió")
obitos.maceio <- subset(dados.pacientes.maceio, dados.pacientes.maceio$situacao_atual == "Óbito")

breaks <- seq(0, 110, by=10)

obitos.maceio.idade <- cut(obitos.maceio$idade, breaks)
maceio.idade <- cut(dados.pacientes.maceio$idade, breaks)

obitos.idade <- table(obitos.maceio$situacao_atual, obitos.maceio.idade)

names <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110")

barplot(obitos.idade, beside = TRUE, 
        main="Óbitos por idade da cidade de Maceió",
        ylab="Óbitos", xlab="Idades em anos",
        names.arg=names, las=2, col=heat.colors(5),
        ylim=c(0, 600))


plot.contaminacao.sexo <- table(dados.pacientes.maceio$sexo, maceio.idade)

barplot(plot.contaminacao.sexo, beside=TRUE,
        las=2, ylim=c(0, 12000),
        col=c("#cc0000", "#00cc00"), names.arg=names,
        xlab="Idade em anos", ylab="Número de Contaminados",
        main="Gráfico de contaminação por sexo e idade em Maceió")
       
legend("topright", pch=15,col=c("#00cc00", "#cc0000"), 
       legend=c("Sexo Masculino", "Sexo Feminino"), cex=1.25,
       bty="n")


plot.obitos.sexo <- table(obitos.maceio$sexo, obitos.maceio.idade)

barplot(plot.obitos.sexo, beside=TRUE, 
        col=c("#cc0000", "#00cc00"), ylim=c(0, 300),  names.arg=names, 
        las=2, main="Gráfico de óbitos por sexo e idade em Maceió",
        xlab="Idade em anos", ylab="Número de Óbitos")

legend("topleft", pch=15,col=c("#00cc00", "#cc0000"), 
       legend=c("Sexo Masculino", "Sexo Feminino"), cex=1.25,
       bty="n")

datas.contaminacao <- cut.Date(as.Date(dados.pacientes.maceio$data_resultado_exame), seq.Date(as.Date('2020-03-01'), as.Date('2021-06-01'), '1 month'))

#datas.contaminacao <- format(as.Date(dados.pacientes.maceio$data_resultado_exame), "%y/%m")

names2 <- c("Mar/20", "Abr/20", "Maio/20", 
           "Jun/20", "Jul/20", "Ago/20",
           "Set/20", "Out/20","Nov/20", 
           "Dez/20", "Jan/21","Fev/21", 
           "Mar/21", "Abr/21", "Maio/21")

barplot(table(datas.contaminacao), las=2,
        main="Contaminação por mês", names.arg=names2,
        ylim=c(0, 12000), col=heat.colors(9),
        ylab="Óbitos")

