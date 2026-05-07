##########################################
> # LISTA DE EXERCÍCIOS 5
> # Autor: Tania Pereira Andicene
> # Data: 06/05/2026
> ##########################################
> 
> # Removendo objetos ativos
> rm(list = ls())
> 
> # = EXERCÍCIO - CRIANDO FUNÇÕES NO R =
> 
> # Glossário de variáveis:
> # LDLc = Colesterol da lipoproteína de baixa densidade
> # CT = Colesterol total
> # HDLc = HDL-colesterol
> # TG = Triglicerídeos
> 
> # Função para cálculo do LDLc
> fLDLc <- function(CT, HDLc, TG, jejum = TRUE) {
+   
+   # Cálculo do LDLc (equação de Friedewald)
+   LDLc <- if (jejum) CT - HDLc - (TG/5) else CT - HDLc - (TG/8)
+   
+   # Aviso de limitação do método
+   if (TG >= 400) {
+     message("Aviso: Resultado pode ser impreciso (TG >= 400 mg/dL).")
+   }
+   
+   # Definição do limite de TG conforme jejum
+   limite_TG <- if (jejum) 150 else 175
+   
+   # Interpretação clínica
+   if (LDLc >= 160 && TG >= limite_TG) {
+     message("Aviso: Possível hiperlipidemia mista.")
+   } else if (LDLc > 160) {
+     message("Aviso: Possível hipercolesterolemia isolada.")
+   }
+   
+   # Retorno do resultado
+   return(LDLc)
+ }
> 
> # Exemplo de uso
> # fLDLc(CT = 200, HDLc = 50, TG = 180, jejum = TRUE)