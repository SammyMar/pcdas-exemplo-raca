## pacotes
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
library(forcats)
library(pander)
library(skimr)
require(survival)
require(truncnorm)
require(LaplacesDemon)
require(TeachingDemos)
require(coda)
library(foreign) 
library("reshape2")
library(tidyr)


##### Incompletude SINASC raca
## carregando dados de "Nascimentos_muni.csv" 
dados_nasc <- read_delim("bases/Nascimentos_muni.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(dados_nasc)

dados_nasc <- dados_nasc %>% 
  rename(ene = "COUNT(1)" )

# teste <- janitor::get_dupes(dados, c("CODMUNNASC", "ANO", "VARIAVEL"))

#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
dados_nasc_agr <- dados_nasc %>% 
  group_by(ano_nasc, CODMUNNASC) %>% 
  summarise(nasc = sum(ene))

# janitor::get_dupes(dados_nasc_agr, CODMUNNASC)
# 
# dados <- dplyr::distinct(dados_nasc_agr, ano_nasc, CODMUNNASC, .keep_all = TRUE)

### Leitura da base "raca_muni.csv"
raca_muni <- read_delim("bases/raca_muni.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(raca_muni)

#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
raca_muni_agr <- raca_muni %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_raca = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
  
### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, raca_muni_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_raca)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2_cor <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
raca_muni_9 <- raca_muni %>% 
  filter(Raca == 9)

dados_raca_9 <- raca_muni_9 %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorado_raca = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(dados_nasc_agr, dados_raca_9, by = c("ano_nasc", "CODMUNNASC"))

dados_raca <- dados %>% 
  mutate(ignorado_raca = ifelse(is.na(ignorado_raca), 0, ignorado_raca)) %>% 
  mutate(incom_ig_raca = round((ignorado_raca/nasc)*100,2))


  #IMPORTACAO DOS BANCOS ------------------------------------------------------------------
  variaveis <- c( "IDADEMAE" , "ESCMAE" ,"QTDPARTNOR",  "QTDPARTCES",
                 "CONSPRENAT" ,"MESPRENAT" ,"PARTO" ,"TPROBSON" , "PESO" ,  "GESTACAO" , "SEMAGESTAC")
  
  IDADEMAE <- read_delim(("bases/IDADEMAE_muni.csv"), 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
  ESCMAE <- read_delim(("bases/ESCMAE_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  QTDPARTNOR <- read_delim(("bases/QTDPARTNOR_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  QTDPARTCES <- read_delim(("bases/QTDPARTCES_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  CONSPRENAT <- read_delim(("bases/CONSPRENAT_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  MESPRENAT <- read_delim(("bases/MESPRENAT_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  PARTO <- read_delim(("bases/PARTO_muni.csv"), 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
  TPROBSON <- read_delim(("bases/TPROBSON_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  PESO <- read_delim(("bases/PESO_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  GESTACAO <- read_delim(("bases/GESTACAO_muni.csv"), 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
  SEMAGESTAC <- read_delim(("bases/SEMAGESTAC_muni.csv"), 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  
# IDADEMAE ---------#ERRO-------------------------------------------------------

IDADEMAE_agr <- IDADEMAE %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_IDADEMAE = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)


dados <- full_join(dados_nasc_agr, IDADEMAE_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados_nasc_agr$nasc)-sum(IDADEMAE_agr$nasc_IDADEMAE) #VALORES INEXISTENTE PARA ALGUNS ANOS E MUNICIPIOS
#logo a base nao coincide com de nascidos vivos
#correcao 
aux <- IDADEMAE %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_IDADEMAE = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
aux1 <- full_join(dados_nasc_agr, aux, by = c("ano_nasc", "CODMUNNASC"))
aux1.2 <- aux1[aux1$nasc_IDADEMAE |> is.na(),]
aux1.2$nasc_IDADEMAE <- aux1.2$nasc
aux2 <- cbind(aux1[aux1$nasc_IDADEMAE |> is.na(),c(1,2,3)],NA,NA)
names(aux2) <- c('Ano' ,'Municipio'   ,'Nascidos','IDADEMAE','UF')
aux3 <- rbind(IDADEMAE,aux2)

IDADEMAE_agr <- aux3 %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_IDADEMAE = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano) 

dados <- full_join(dados_nasc_agr, IDADEMAE_agr, by = c("ano_nasc", "CODMUNNASC"))
dados$diferenca <- dados$nasc - dados$nasc_IDADEMAE
dados[dados$diferenca > 0,] |> View()
sum(dados$nasc-dados$nasc_IDADEMAE) 

diferentes <- dados[dados$nasc - dados$nasc_IDADEMAE!= 0,c(1,2,5)]
faltantes_dif <- cbind(NA,diferentes,NA)
faltantes_dif |> names() <- c('UF','Ano','Municipio','Nascidos','IDADEMAE')

aux3 <- rbind(IDADEMAE,aux2,faltantes_dif)

IDADEMAE_agr <- aux3 %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_IDADEMAE = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano) 

dados <- full_join(dados_nasc_agr, IDADEMAE_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados_nasc_agr$nasc)-sum(IDADEMAE_agr$nasc_IDADEMAE)
###
dados2_IDADEMAE <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))

(dados2_IDADEMAE$n - dados2$n) |> sum()

ignorados <- aux3 %>% 
  filter(IDADEMAE == 'IGNORADOS')
faltantes <- aux3 %>% 
  filter(IDADEMAE |> is.na())

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_IDADEMAE <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_IDADEMAE$variavel <- 'IDADEMAE'

# ESCMAE ------------------------------------------------------------------

#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
ESCMAE_agr <- ESCMAE %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_ESCMAE = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, ESCMAE_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_ESCMAE)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- ESCMAE %>% 
  filter(ESCMAE |> is.na())
ignorados <- ESCMAE %>% 
  filter(ESCMAE == 9)

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_ESCMAE <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_ESCMAE$variavel <- 'ESCMAE'
# QTDPARTNOR --------------------------------------------------------------

#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
QTDPARTNOR_agr <- QTDPARTNOR %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_QTDPARTNOR = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, QTDPARTNOR_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_QTDPARTNOR)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- QTDPARTNOR %>% 
  filter(QTDPARTNOR |> is.na())
ignorados <- QTDPARTNOR %>% 
  filter(QTDPARTNOR == 'IGNORADO')

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_QTDPARTNOR <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_QTDPARTNOR$variavel <- 'QTDPARTNOR'
# QTDPARTCES --------------------------------------------------------------
#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
QTDPARTCES_agr <- QTDPARTCES %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_QTDPARTCES = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, QTDPARTCES_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_QTDPARTCES)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- QTDPARTCES %>% 
  filter(QTDPARTCES |> is.na())
ignorados <- QTDPARTCES %>% 
  filter(QTDPARTCES == 'IGNORADO')

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_QTDPARTCES <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_QTDPARTCES$variavel <- 'QTDPARTCES'
# CONSPRENAT --------------------------------------------------------------
#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
CONSPRENAT_agr <- CONSPRENAT %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_CONSPRENAT = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, CONSPRENAT_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_CONSPRENAT)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- CONSPRENAT %>% 
  filter(CONSPRENAT |> is.na())
ignorados <- CONSPRENAT %>% 
  filter(CONSPRENAT == 'IGNORADO')

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_CONSPRENAT <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_CONSPRENAT$variavel <- 'CONSPRENAT'
# MESPRENAT ---------------------------------------------------------------
#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
MESPRENAT_agr <- MESPRENAT %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_MESPRENAT = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, MESPRENAT_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_MESPRENAT)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- MESPRENAT %>% 
  filter(MESPRENAT |> is.na())
ignorados <- MESPRENAT %>% 
  filter(MESPRENAT == 'IGNORADO')

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_MESPRENAT <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_MESPRENAT$variavel <- 'MESPRENAT'


# PARTO -------------------------------------------------------------------

#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
PARTO_agr <- PARTO %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_PARTO = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, PARTO_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_PARTO)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- PARTO$PARTO %>% unique() 
  filter(PARTO |> is.na())
ignorados <- PARTO %>% 
  filter(PARTO == 9)

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_PARTO <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_PARTO$variavel <- 'PARTO'

# TPROBSON ----------------------------------------------------------------
#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
TPROBSON_agr <- TPROBSON %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_TPROBSON = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, TPROBSON_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_TPROBSON)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- TPROBSON %>% 
  filter(TPROBSON |> is.na())
ignorados <- TPROBSON %>% 
  filter(TPROBSON == 11)

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_TPROBSON <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_TPROBSON$variavel <- 'TPROBSON'
# PESO ------#ERRO--------------------------------------------------------------
#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
PESO_agr <- PESO %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_PESO = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, PESO_agr, by = c("ano_nasc", "CODMUNNASC"))
PESO_agr[PESO_agr$nasc_PESO |> is.na(),]
sum(dados$nasc-dados$nasc_PESO)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- PESO %>% 
  filter(PESO |> is.na())
ignorados <- PESO %>% 
  filter(PESO == 'IGNORADO')

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_PESO <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
# GESTACAO ----------------------------------------------------------------
#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
GESTACAO_agr <- GESTACAO %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_GESTACAO = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, GESTACAO_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_GESTACAO)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- GESTACAO %>% 
  filter(GESTACAO |> is.na())
ignorados <- GESTACAO %>% 
  filter(GESTACAO == 9)

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_GESTACAO <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_GESTACAO$variavel <- 'GESTACAO'
# SEMAGESTAC --------------------------------------------------------------

#esse passo é importante pq na API da PCDaS corremos pela UF de residência, 
#mas usamos o municipio de ocorrência (CODMUNNASC)
SEMAGESTAC_agr <- SEMAGESTAC %>% 
  group_by(Municipio, Ano) %>% 
  summarise(nasc_SEMAGESTAC = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

### esse passo aqui é só pra ver se o número total de nascimentos das
# duas bases (de nascimentos e raca) coincidem
dados <- full_join(dados_nasc_agr, SEMAGESTAC_agr, by = c("ano_nasc", "CODMUNNASC"))

sum(dados$nasc-dados$nasc_SEMAGESTAC)
# [1] 0
#como o resultado acima é 0, temos que o total das duas bases coincide

#agora vamos ver se o total de nascimentos coincide com o total 
#de nascidos vivos do painel de Indicadores Obstétricos:
#https://observatorioobstetrico.shinyapps.io/indicadores-obstetricos/#section-nascimentos
dados2 <- dados %>% 
  group_by(ano_nasc) %>% 
  summarise(n = sum(nasc))
sum(dados2$n - dados2_cor$n)
#o total de nascidos vivos por ano coincide com o painel

## Certo: dado que os testes anteriores deram certo, 
#agora vamos só trabalhar com os dados faltantes
## UMA OBSERVACAO IMPORTANTE: para essa variável raca/cor 
#não tem branco, só ignorado (código 9). Mas isso não acontece
#para as outras variáveis. TOMAR CUIDADO.
faltantes <- SEMAGESTAC %>% 
  filter(SEMAGESTAC |> is.na())
ignorados <- SEMAGESTAC %>% 
  filter(SEMAGESTAC == 'IGNORADO')

ignorados <- ignorados %>% 
  group_by(Municipio, Ano) %>% 
  summarise(ignorados = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)
faltantes <- faltantes %>% 
  group_by(Municipio, Ano) %>% 
  summarise(faltantes = sum(Nascidos)) %>% 
  rename(CODMUNNASC = Municipio,
         ano_nasc = Ano)

dados <- full_join(ignorados,faltantes, by = c("ano_nasc", "CODMUNNASC")) |>
  full_join(dados_nasc_agr, by = c("ano_nasc", "CODMUNNASC"))
dados_SEMAGESTAC <- dados %>% 
  mutate(ignorados = ifelse(is.na(ignorados), 0, ignorados)) %>% 
  mutate(faltantes = ifelse(is.na(faltantes), 0, faltantes)) |>
  mutate(incompletude = round(((ignorados+ faltantes)/nasc)*100,2))
dados_SEMAGESTAC$variavel <- 'SEMAGESTAC'


# JUNCAO BASES -----------------------------------------------------------

dados_final <- bind_rows(dados_ESCMAE ,dados_QTDPARTNOR,  dados_QTDPARTCES,
                         dados_CONSPRENAT ,dados_MESPRENAT ,dados_PARTO ,
                         dados_TPROBSON , dados_GESTACAO , dados_SEMAGESTAC,dados_IDADEMAE)


# VISUALIZACAO ------------------------------------------------------------

dados <- dados_final  %>% 
  group_by(variavel, ano_nasc) %>% 
  summarise(incompletude = sum(faltantes + ignorados),
            Total = sum(nasc))
dados$incompletude <- (dados$incompletude/dados$Total )* 100
dados$localidade <- 'BR'
g <- ggplot2::ggplot(data = dados,
                     ggplot2::aes(y = incompletude , x = ano_nasc, fill = localidade)) +
  ggplot2::geom_bar(position = "dodge", stat = "identity") +
  ggplot2::facet_grid(rows = ggplot2::vars(variavel))

g <- g + ggplot2::labs(x = NULL) +
  ggplot2::labs(y = paste0("INCOMPLETUDE (%)")) +
  ggplot2::scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(
    face = "bold",
    color = "#000000",
    size = 9,
    angle = 45
  ))
plotly::ggplotly(g, height = (length(variaveis) + 100)*10) %>%
  plotly::layout(legend = list(orientation = "h", y = 20))

write.csv(dados_final,"Dados_incompletude_SINASC_correcao.csv")


