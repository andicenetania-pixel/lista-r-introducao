##############################
> 
> #LISTA DE EXERCICIO 4
> 
> #TANIA PEREIRA ANDICENE
> 
> # Removendo objetos ativos
> rm(list=ls())
> 
> 
> #lendo o arquivo de dados "antropo"
> 
> antropo <- read.table("https://codeberg.org/edsonzmartinez/CursoR/raw/branch/main/antropo.txt", head=T)
> 
> ############# 
> # Exercício 1
> #############
> 
> antropo$pesokg <- antropo$Weight*0.45359237
> 
> #############
> # Exercício 2
> #############
> 
> antropo$alturacm <- antropo$Height*2.54
> 
> #############
> # Exercício 3
> #############
> 
> boxplot(antropo$Density, antropo$Fat, antropo$Neck, antropo$Chest, antropo$Abdomen, antropo$Hip, antropo$Knee, antropo$Thigh, antropo$Ankle, antropo$Biceps, antropo$Forearm, antropo$Wrist, antropo$pesokg, antropo$alturacm, names=c("Densidade", "Gordura", "Pescoço", "Peito", "Abdomen", "Quadril", "Joelho", "Coxa", "Cotovelo", "Biceps", "Antebraço", "Punho", "Peso (kg)", "Altura"), xlab="Medidas antropométricas (quantitativas contínuas)", las=1, col="lightyellow", pch=20, staplewex=0.3, whisklty=1, boxwex=0.6, cex.axis=0.6, cex=0.5)
> text(5, 190,"Pescoço, Peito, Abdomen, Quadril, Joelho, Coxa, Cotovelo, Biceps, Antebraço, Punho, Altura : em centímetros", cex=0.5)
> 
> #############
> # Exercício 4
> #############
> 
> plot(antropo$alturacm, antropo$pesokg, xlim=c(160, 200), pch=18, xlab="Altura (cm)", ylab="Peso (Kg)")
> axis(1,at=seq(160,200,5),labels=seq(160,200,5))
> identify(antropo$alturacm, antropo$pesokg)
Error in identify.default(antropo$alturacm, antropo$pesokg) : 
  graphics device closed during call to locator or identify
> 
> #############
> # Exercício 5
> #############
> 
> par(mfrow=c(1,3))
> hist(antropo$Age, prob=TRUE, ylim=c(0, 0.05), xlab="Idade", ylab="Densidade", main="Distribuição das idades")
> lines(density(antropo$Age), col="red", lwd=2)
> hist(antropo$alturacm, prob=TRUE, ylim= c(0, 0.05), xlab="Altura", ylab="Densidade", main="Distribuição das alturas")
> lines(density(antropo$alturacm), col="red", lwd=2)
> hist(antropo$pesokg, prob=TRUE, ylim= c(0, 0.05), xlab="Peso", ylab="Densidade", main="Distribuição dos pesos")
> lines(density(antropo$pesokg), col="red", lwd=2) 
> 
> ##################
> # Exercícios 6 e 7
> ##################
> 
> novoantropo<-antropo[,-c(4,5)]	# Retirando o peso (lb) e a altura (in), usaremos no seu lugar pesokg e alturacm)
> par(mfrow=c(4,4))	# Contruindo uma matriz 4x4, para conter um gráfico de dispersão em cada uma de suas posições
> par(mar=c(2,4,1,1))	# Ajustando as margens em torno de cada gráfico para otimizar o espaço, sem achatá-los
> # Construindo os gráficos de dispersão, com o percentual de gordura sempre no eixo X e as demais variáveis nos eixos Y, e o coeficiente de correlação de Pearson na base à direita da área gráfica #
> for (i in 1:15) {
+     cor_valor <- cor(novoantropo[[2]], novoantropo[[i]])
+     plot(novoantropo[[2]], novoantropo[[i]],
+          xlab = "",
+          ylab = names(novoantropo)[i],
+          main = paste("%Gordura vs", names(novoantropo)[i]),
+          pch = 18, 
+          col = "steelblue")
+     legend("bottomright", 
+            legend = paste("r =", round(cor_valor, 2)), 
+            bty = "n", 
+            cex = 1.2)
+ }
>  
> #############
> # Exercício 8
> #############
> 
> plot(antropo$Age, antropo$Abdomen, main="Idade e circunferência do abdômen", xlab="Idade (anos completos", ylab="Circunferência abdômen (cm)", col="steelblue", pch=18)
> cor.test(antropo$Age, antropo$Abdomen)

	Pearson's product-moment correlation

data:  antropo$Age and antropo$Abdomen
t = 3.7438, df = 250, p-value = 0.0002249
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.1099676 0.3441826
sample estimates:
      cor 
0.2304094 

> 
> ##
> O gráfico de dispersão entre idade e circunferência do abdômen não mostra evidência de correlação forte entre as duas características. Há leve tendência de aumento da segunda conforme aumentam os valores da primeira, com a presença de outliers (um sujeito de cerca de 50 anos com circunferência muito alta e um sujeito muito mais velho que todos os demais, com circunferência mediana. O cálculo do coeficiente de correlação de Pearson entre as características resultou em 0,2304094, um valor que indica correlação positiva e fraca. O teste t de correlação mostrou que os dados fornecem evidência de que a correlação entre essas características na população que originou essa amostra de sujeitos é diferente de zero.  
Error: unexpected symbol in "O gráfico"
> ##
> 
> #############
> # Exercício 9
> #############
> 
> novoantropo$fet <- ifelse(novoantropo$Age<=45,0,1)
> 
> ##############
> # Exercício 10
> ##############
> 
> 
> par(mfrow=c(4,4))
> par(mar=c(2, 2.5, 2, 0.5))
> for (i in 1:15) {
+     boxplot(novoantropo[[i]] ~ novoantropo$fet,
+             # \n faz a quebra de linha, mantendo o texto horizontal e estreito
+             names = c("Até\n45 anos", "Mais de\n45 anos"),	#\n para colocar o restante do nome da categoria na linha de baixo
+             xlab = "",      	    # Removendo nome do eixo X, poupando espaço, já que os nomes das categorias de fet são grandes
+             ylab = "",              # Removendo nome do eixo Y, pois o nome da variável no título basta
+             main = names(novoantropo)[i],
+             col = "steelblue",
+             cex.axis = 0.7,         
+             cex.main = 0.9,         
+             yaxt = "s"              # Para que os números do eixo Y apareçam (sem o título)
+     )
+ }
> 
> ##############
> # Exercício 11
> ##############
> 
> antropocontinuas<-novoantropo[,-3]  # Criando um conjunto de dados só com as variáveis contínuas
> #verificando a distribuição dos dados por meio de gráficos Quantil-Quantil com envelope, adicionado dos p-valores dos testes de Shapiro-Wilk e Kolmogorov-Smirnov
> 
> par(mfrow=c(4,4))
> par(mar=c(2, 2.5, 2, 0.5))
> library(ecostats)
Error in library(ecostats) : there is no package called ‘ecostats’
> for (i in 1:14) {
+     sw <- shapiro.test(antropocontinuas[[i]])
+     ks <- ks.test(antropocontinuas[[i]], pnorm, mean(antropocontinuas[[i]]), sd(antropocontinuas[[i]]))
+     qqenvelope(antropocontinuas[[i]], n.sim=5000, pch=18, cex = 0.5,
+                main=paste("QQ-Plot para", names(antropocontinuas)[i]))
+     legend("bottomright", 
+            legend = paste("p_SW =", round(sw$p.value, 3),"p_KS=", round(ks$p.value,3)), 
+                           bty = "n", 
+                           cex = 0.8)
+ }
Error in qqenvelope(antropocontinuas[[i]], n.sim = 5000, pch = 18, cex = 0.5,  : 
  could not find function "qqenvelope"
In addition: Warning message:
In ks.test.default(antropocontinuas[[i]], pnorm, mean(antropocontinuas[[i]]),  :
  ties should not be present for the one-sample Kolmogorov-Smirnov test
> 
> ##### Resposta #####
> # A normalidade das variáveis antropométricas foi avaliada por meio de testes de hipóteses (Shapiro-Wilk e Kolmogorov-Smirnov) e inspeção 
> # visual de QQ-Plots com envelope.
> # Pra a maior parte das variáveis, os gráficos mostram que, exceto por alguns outliers (em geral dois ou três) os pontos encontram-se dentro 
> # do envelope, de modo que suas distribuições não se afastam da normal significativamente. Os p-valores do teste de Shapiro-Wilk para as 
> # variáveis densidade, percentual de gordura e punho levam à não rejeição da hipótese de que os dados têm distribuição normal, pois excedem 
> # o nível de significância do teste de 5%, o mesmo se conclui a partir dos testes de kolmogorov-Smirnov. 
> # Para a variável tornozelo, ambos os teses levam à rejeição a hipótese nula de normalidade dos dados, e observam-se muitos pontos fora do 
> # envelope, de forma que conclui-se que, de fato, a distribuição das medidas de tornozelo não é normal. Para as demais variáveis, apesar de os
> # testes de Shapiro-Wilk levarem à rejeição da hipótese normalidade dos dados, observa-se que o teste de Kolmogorov-Smirnov, realizado para uma 
> # distribuição normal com média e desvio padrão iguais ao da amostra, não rejeita, e como há poucos pontos fora do envelope, pode-se concluir 
> # pela normalidade doas dados.  Além disso, os testes de normalidade, assim como todos os testes de hipóteses, tendem a rejeitar a hipótese nula
> # quando o número de observações é grande, como é o caso nesse conjunto de dados (n=252). O teste de Shapiro-Wilk é muito sensível nesse casos, 
> # tendendo a rejeitar a hipótese nula por causa de desvios pequenos e sem importância prática. Seria interessante estudar as razões pelas quais
> # há pessoas com valores extremos para algumas das características estudadas, avaliando suas condições de saúde, hábitos, alimentação, etc. 
> #######################
> 
> ##############
> # Exercício 12
> ##############
> 
> # Realizando testes t-Student para amostras independentes, precedidos por testes de homocedasticidade.
> 
> for (i in 1:14){
+     f <- var.test(antropocontinuas[[i]]~antropocontinuas$fet)
+     homoced <- ifelse (f$p.value < 0.05, F, T)
+     t <- t.test(antropocontinuas[[i]]~antropocontinuas$fet, var.equal = homoced)
+     eviddif <- ifelse (t$p.value <0.05, "Médias populacionais diferentes", "Médias populacionais não são diferentes")
+     print (paste0(names(antropocontinuas[i]), ": p = ", round(t$p.value,4), " (", eviddif, ")"))