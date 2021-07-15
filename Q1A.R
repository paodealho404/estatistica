x <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41, 55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53, 59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40); # cria a tabela com os números
tabela <- table(x); # separa a frequência de cada número da lista

hist(tabela, col=c("cyan", "red"), main="Histograma da Frequência dos Números", xlab="Frequência", ylab="Números"); # quantidade de números x frequência
media <- mean(x)

par(mfrow=c(1, 2))

boxplot(x, main="Lista de números", ylab="Números", col=c("#ff3333"), border=c("#00001a"))
boxplot(x, horizontal=TRUE, main="Lista de números", xlab="Números", col=c("#ff3333"), border=c("#00001a"))
