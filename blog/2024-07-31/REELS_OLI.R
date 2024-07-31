

getwd()
setwd("C:/Users/pedro/Desktop/CONSULTORIA/MINI PROJETOS/OLIMPIADAS")
list.files()

#### Carregando Pacotes
require(dplyr)
require(tidyr)
require(ggplot2)
library(dplyr)
library(countrycode)
library(ggplot2)


#### Carregando e Pré-Visualizando os Dados
dados = read.csv(file = 'dataset_olympics.csv',header = T)
noc = read.csv(file = 'noc_region.csv',header = T)


#### Tratamento dos Dados
{
  "Em vez de ter uma coluna sem caracteres para as medalhas, atribuindo a classe Null"
  dados$Medal = ifelse(dados$Medal == '','Null',dados$Medal)
  
  "Transformar a coluna de Medalhas em outras 4 Subcolunas onde teríamos o tipo da medalha e a incidência de qual atleta conquistou. Seria importante para em futuras análises já ter uma coluna numérica pra cada uma."
  
  "Criando variável auxiliar para entrar como a contagem de medalhas nessas novas colunas"
  dados$aux = 1
  
  dados = dados |>
    pivot_wider(names_from = c(Medal),values_from = aux,values_fn = sum,values_fill = 0) |>
    as.data.frame()
  
  "Curiosidade sobre os sports"
  unique(dados$Sport)
}










require(gganimate)
require(dplyr)
require(tidyr)
require(ggplot2)

'Dados fic'
medal_data <- data.frame(
  Year = rep(2022:2024, each = 2),
  Country = rep(c("CountryA", "CountryB"), times = 6),
  Gold = sample(0:10, 6, replace = TRUE),
  Silver = sample(0:10, 6, replace = TRUE),
  Bronze = sample(0:10, 6, replace = TRUE)
)
medal_data_long <- medal_data %>%
  pivot_longer(cols = Gold:Bronze, names_to = "Medal", values_to = "Count") %>%
  group_by(Year, Country) %>%
  summarise(Total_Medals = sum(Count)) %>%
  ungroup()
medal_data_long

### gifs
'Aumentar fonte, negrito de titulo, legenda e labels, alturar o ano como se fosse inteiros, configurar manualmente as flores para dar ênfase para os times que liderarem no ano'

p <- ggplot(medal_data_long, aes(x = Year, y = Total_Medals, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Evolução do Quadro de Medalhas dos Países pro Ano", x = "", y = "") +
  theme_minimal() +
  transition_reveal(Year) +
  ease_aes('linear')



animate(p, duration = 15, fps = 25, width = 1080/3, height = 1920/3)
anim_save("medal_tally.gif", animation = last_animation())














library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)

medal_data <- data.frame(
  Year = rep(2022:2024, each = 2),
  Country = rep(c("CountryA", "CountryB"), times = 3),
  Gold = sample(0:10, 6, replace = TRUE),
  Silver = sample(0:10, 6, replace = TRUE),
  Bronze = sample(0:10, 6, replace = TRUE)
)

medal_data_long <- medal_data %>%
  pivot_longer(cols = Gold:Bronze, names_to = "Medal", values_to = "Count") %>%
  group_by(Year, Country) %>%
  summarise(Total_Medals = sum(Count)) %>%
  ungroup()

# Definindo cores para dar ênfase aos países líderes em cada ano
colors <- medal_data_long %>%
  group_by(Year) %>%
  mutate(Leader = Country[which.max(Total_Medals)]) %>%
  ungroup() %>%
  mutate(Color = ifelse(Country == Leader, "red", "grey"))
'essas cores devem se alternar de acordo com quem estiver como lidar acumulado ao longo dos anos, transformar no acumulado e não no por ano'

p <- ggplot(medal_data_long, aes(x = as.integer(Year), y = Total_Medals, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Evolução do Quadro de Medalhas dos Países por Ano", x = "Ano", y = "Total de Medalhas") +
  scale_color_manual(values = setNames(colors$Color, colors$Country)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  transition_reveal(Year) +
  ease_aes('linear')

animate(p, duration = 15, fps = 25, width = 1080/3, height = 1920/3)

















{ 


  
  p <- ggplot(medal_data_long, aes(x = Year, y = Total_Medals, color = Country, group = Country)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    labs(title = "Evolução do Quadro de Medalhas: {closest_state}", x = "Ano", y = "Total de Medalhas") +
    theme_minimal() +
    transition_states(Year, transition_length = 2, state_length = 1) +
    ease_aes('linear')
  
  p
  
  animate(p, nframes = 100, fps = 10, width = 800, height = 600)
  anim_save("medal_tally.gif", animation = last_animation())
} 








'reajustar'
library(forcats) # Pacote necessário para reorder_within
# Criar o gráfico de barras dinâmico com barras horizontais
# Criar o gráfico de barras dinâmico com barras horizontais e países reordenados
p <- ggplot(medal_data_long, aes(x = Total_Medals, y = fct_reorder(Country, Total_Medals, .desc = TRUE), fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Evolução do Quadro de Medalhas dos Países por Ano: {closest_state}", x = "Total de Medalhas", y = "País") +
  theme_minimal() +
  transition_states(Year, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  coord_flip()

# Animação
animate(p, duration = 15, fps = 25, width = 1080, height = 1920)