x <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41, 55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53, 59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40);
par(mfrow=c(1, 3))
hist(x, col=c("#9999ff", "#0000ff"), 
     main="Histograma da Frequência dos Números", 
     ylab="Frequência", xlab="Números", 
     ylim=c(0, 20), xlim = c(35, 75), 
     xaxp = c(30, 80, 10)
     ) 
media <- mean(x)
mediana <- median(x)
plot(tabela)


##Cálculo da Moda de Czuber

amp <- max(x) - min(x);amp
nc <- round(1+ (3.222 * log10(length(x))));
ampcl <- amp/nc;ampcl
limclass <- seq(min(x), max(x), ampcl);
grafico <- table(cut(x, breaks = limclass));

liminf <- limclass[which.max(grafico)];liminf

num <- max(grafico) - grafico[which.max(grafico)-1]
den <- num + (max(grafico) - grafico[which.max(grafico)+1]);
moda <- liminf + (num/den)*ampcl;




quart <- quantile(x, 0.75);
per8 <- quantile(x, 0.08);
per50 <- quantile(x, 0.5);
per80 <- quantile(x, 0.8);


moda
mediana
media
quart
per8
per50
per80


boxplot(x, main="Lista de números", ylab="Números", col=c("#ff3333"), border=c("#00001a"))
boxplot(x, horizontal=TRUE, main="Lista de números", xlab="Números", col=c("#ff3333"), border=c("#00001a"))
