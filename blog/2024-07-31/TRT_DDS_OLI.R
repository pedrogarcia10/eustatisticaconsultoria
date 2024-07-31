

getwd()
setwd("C:/Users/pedro/Desktop/CONSULTORIA/MINI PROJETOS/OLIMPIADAS")
list.files()

'Jogar no Quarto, pedir gpt para explicar o meu código, corrigir e expandir meu texto, rever alguns titulos de gráficos e pronto!'

'Todo dia um gráfico novo das olímpiadas'


'Quando criança me lembro de assistir as olímpiadas sem saber muito o seu real impacto. Todos aqueles esportes diferentes que não conhecia direito suas regras, mas que eram novos pra mim, novas descobertas. A cerimônia de abertura então, me encatava muita, ver todas aquelas bandeiras (talvez ali adquiri um fascínio imediato por bandeiras que perdura até hoje), toda aquela diversidade cultural, é realmente muito bom!


Naquele período eu não era tão fascinado por futebol, esporte que se tornaria meu hiperfoco, até os dias atuais. Mas aquele meu encantamento sobre os países e suas particularidades ainda se mantém, com uma ênfase maior para o contexto futebolístico.

Hoje em um período pré-olímpico revisitarei esses sentimentos da infância, usando estatística. Adentrando ainda mais nas particularidades desse mega evento a partir de uma base de dados histórica do quadro de medalhas. Respondendo perguntas como, qual os países com maior performance por esporte? ...'

'Ressaltar que as análises vão considerar as olimpiadas de inverno e de verão e do século XXI em diante.'

#### Carregando Pacotes
require(dplyr)
require(tidyr)
require(ggplot2)
library(dplyr)
library(countrycode)
#library(ggflags)
library(ggplot2)


#### Carregando e Pré-Visualizando os Dados
dados = read.csv(file = 'dataset_olympics.csv',header = T)
noc = read.csv(file = 'noc_region.csv',header = T)
str(dados)
dados[1:10,]


'há como fazer os gráficos com fundo transparente?'


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

#### Criação de Variáveis
{
'Fazer uma limpez antes por conta de modalidades com mais de um jogador'
dados$duplicados = duplicated(dados[,c('Team','NOC','Games','Season','Sport','Event')])

dados |> group_by(Sport,Event) |> summarise(qtd = sum(duplicados)) |> arrange(-qtd) |> print(n=40)
'Dividir pelo numero de jogador por sport e event?'

'Criando algumas métricas para visualizar o desempenho das seleções no geral'

sel1 = dados |>
  filter(Year >= 2000) |> 
  group_by(NOC) |> 
  summarise(
    ger_mdl_ouro = sum(Gold),
    ger_mdl_prata = sum(Silver),
    ger_mdl_bronze = sum(Bronze),
    ger_mdl_ausente = sum(Null),
    ger_mdl_tot = ger_mdl_ouro + ger_mdl_prata + ger_mdl_bronze,
    ger_mdl_score = (ger_mdl_ouro * 0.5) + (ger_mdl_prata * 0.2) + (ger_mdl_bronze * 0.1),
    ger_mdl_aproveitamento = (ger_mdl_tot) / ((ger_mdl_tot) + (ger_mdl_ausente)),
    ger_esportes_diferentes = n_distinct(Sport),
    ger_modalidades_diferentes =  n_distinct(Event),
    ger_mediana_idade = median(Age,na.rm = T),
    ger_qtd_atletas_m = sum(ifelse(Sex == 'M',1,0),na.rm = T),
    ger_qtd_atletas_f = sum(ifelse(Sex == 'F',1,0),na.rm = T),
    ger_prop_f = ger_qtd_atletas_f/(ger_qtd_atletas_m+ger_qtd_atletas_f)
    
    
    
  ) |>
  ungroup() |>
  mutate(across(where(is.numeric), ~ rank(-.x), .names = "rank_{col}")) |>
  as.data.frame() |>
  arrange(-ger_mdl_score)


'Criando algumas métricas para visualizar o desempenho das seleções por esporte'
sel2 = dados |>
  filter(Year >= 2000) |>
  group_by(NOC,Sport) |>
  summarise(mdl_ouro = sum(Gold),
            mdl_prata = sum(Silver),
            mdl_bronze = sum(Bronze),
            mdl_ausente = sum(Null),
            mdl_tot = mdl_ouro+mdl_prata+mdl_bronze,
            mdl_score = (mdl_ouro*0.5)+(mdl_prata*0.2)+(mdl_bronze*0.1),
            esportes_diferentes = n_distinct(Sport),
            modalidades_diferentes =  n_distinct(Event),
            mediana_idade = median(Age,na.rm = T),
            qtd_atletas_m = sum(ifelse(Sex == 'M',1,0),na.rm = T),
            qtd_atletas_f = sum(ifelse(Sex == 'F',1,0),na.rm = T),
            prop_f = qtd_atletas_f/(qtd_atletas_m+qtd_atletas_f)
            
            ) |>
  group_by(Sport) |>
  mutate(across(where(is.numeric), ~ rank(-.x), .names = "rank_{col}")) |>
  as.data.frame() |>
  arrange(-mdl_score) |> ungroup()



sel = merge(sel2,sel1,all.x = T,by = c('NOC'))
sel = merge(sel,noc,by.x = 'NOC',by.y = 'noc_region',all.x = T)
sel$prop_mdl_p_esporte = sel$mdl_tot/sel$ger_mdl_tot  #dando uma ideia da distribuição da tradição dos times nos esportes
}

{
  'Desempenho das seleções no geral e por ano'
  sel_ano = dados |>
    filter(Year >= 2000) |> 
    group_by(NOC,Year) |> 
    summarise(
      ger_mdl_ouro = sum(Gold),
      ger_mdl_prata = sum(Silver),
      ger_mdl_bronze = sum(Bronze),
      ger_mdl_ausente = sum(Null),
      ger_mdl_tot = ger_mdl_ouro + ger_mdl_prata + ger_mdl_bronze,
      ger_mdl_score = (ger_mdl_ouro * 0.5) + (ger_mdl_prata * 0.2) + (ger_mdl_bronze * 0.1),
      ger_mdl_aproveitamento = (ger_mdl_tot) / ((ger_mdl_tot) + (ger_mdl_ausente)),
      ger_esportes_diferentes = n_distinct(Sport),
      ger_modalidades_diferentes =  n_distinct(Event),
      ger_mediana_idade = median(Age,na.rm = T),
      ger_qtd_atletas_m = sum(ifelse(Sex == 'M',1,0),na.rm = T),
      ger_qtd_atletas_f = sum(ifelse(Sex == 'F',1,0),na.rm = T),
      ger_prop_f = ger_qtd_atletas_f/(ger_qtd_atletas_m+ger_qtd_atletas_f)
      
      
      
    ) |>
    ungroup() |>
    mutate(across(where(is.numeric), ~ rank(-.x), .names = "rank_{col}")) |>
    as.data.frame() |>
    arrange(-ger_mdl_score)
}



'Quais os  países com mais medalhas em todos esportes'
{
visual = sel |> 
  filter(rank_ger_mdl_tot <= 20) |> 
  arrange(rank_ger_mdl_tot) |> 
  select(NOC,rank_ger_mdl_tot,reg,ger_mdl_tot,ger_esportes_diferentes,ger_mdl_score,reg) |> 
  distinct()

ggplot(visual, aes(x = reorder(reg, ger_mdl_tot), y = ger_mdl_tot)) +
  geom_bar(stat = "identity", fill = scales::gradient_n_pal(c("gold",'#C0C0C0', "#CD7F32"))(seq(0, 1, length.out = nrow(visual)))) +
  geom_text(aes(label = round(ger_mdl_tot)), vjust = 0.5, hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Ajustando o título
  ) +
  labs(title = "Quais os países que mais dominam as olímpiadas no quadro de medalhas?")

'Ressaltar - atletas da Rússia poderão participar das Olimpíadas de 2024 em Paris, mas sob condições rigorosas. Eles competirão como "atletas neutros individuais", sem representar a Rússia oficialmente, ou seja, não poderão usar a bandeira, o hino ou as cores nacionais. Essa decisão foi tomada pelo Comitê Olímpico Internacional (COI) após a invasão da Ucrânia pela Rússia. Apenas um número muito limitado de atletas que não apoiem ativamente a guerra e que não estejam contratualmente vinculados ao exército russo ou bielorrusso será elegível para competir'
}


'Quais os paíse com maior aproveitamento de medalhas'
{
visual  = sel |> 
  filter(ger_esportes_diferentes >= 10) |> 
  arrange(rank_ger_mdl_aproveitamento) |>
  select(reg,ger_mdl_tot,ger_mdl_aproveitamento,ger_esportes_diferentes) |> 
  distinct() |>
  head(15)

ggplot(visual, aes(x = reorder(reg, ger_mdl_aproveitamento), y = ger_mdl_aproveitamento)) +
  geom_bar(stat = "identity", fill = scales::gradient_n_pal(c("gold4", "gold1"))(seq(0, 1, length.out = nrow(visual)))) +
  geom_text(aes(label = paste0(round(ger_mdl_aproveitamento*100),'%')), vjust = 0.5, hjust = 1.25, size = 4, fontface = "bold",color = 'black') +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Ajustando o título
  ) +
  labs(title = "")



library(ggplot2)
library(scales)

ggplot(visual, aes(x = reorder(reg, ger_mdl_aproveitamento), y = ger_mdl_aproveitamento)) +
  geom_bar(stat = "identity", fill = scales::gradient_n_pal(c("gold4", "gold1"))(seq(0, 1, length.out = nrow(visual)))) +
  geom_text(aes(label = paste0(round(ger_mdl_aproveitamento * 100), '%')), vjust = 0.5, hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = NA, color = NA),  # Fundo do painel transparente
    plot.background = element_rect(fill = NA, color = NA)    # Fundo do gráfico transparente
  ) +
  labs(title = "Quais países têm mais aproveitamento de medalhas?")

}

'Não entram pra perder'
'Considerando pelo menos mais que 10 esportes distintos disputados'

'Países com maior score'
{
visual <- sel |>
  arrange(-ger_mdl_score) |>
  select(NOC, reg, ger_mdl_score, ger_mdl_aproveitamento, ger_mdl_tot) |>
  distinct() |>
  head(20) |>
  mutate(
    iso2 = tolower(countrycode(reg, "country.name", "iso2c")),
    color = case_when(
      row_number() == 1 ~ "gold",
      row_number() == 2 ~ "#C0C0C0",
      row_number() == 3 ~ "#CD7F32",
      TRUE ~ "#E0E0E0"
    )
  )

ggplot(visual, aes(x = reorder(reg, ger_mdl_score), y = ger_mdl_score)) +
  geom_bar(stat = "identity", aes(fill = color)) +
  geom_text(aes(label = round(ger_mdl_score)), vjust = 0.5, hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Ajustando o título
  ) +
  labs(title = "E as nações com maiores scores de medalhas?") +
  scale_fill_identity()
}



'Quais os países que tem mais tradição em um só esporte com pelo menos 5 medalhas na modalidade'
{
visual  = sel |>
  arrange(-prop_mdl_p_esporte) |>
  select(Sport,reg,prop_mdl_p_esporte,mdl_tot) |>
  filter(mdl_tot >= 5) |>
  head(20)

unique(visual$Sport)
cores_esporte <- c(
  "Athletics" = "#FF4500",  # Laranja forte, simbolizando energia e atividade física
  "Football" = "#008000",   # Verde, frequentemente associado a campos de futebol
  "Swimming" = "#1E90FF",   # Azul claro, simbolizando água
  "Ice Hockey" = "#4682B4", # Azul acinzentado, lembrando gelo e uniformes
  "Hockey" = "#FFD700",     # Dourado, simbolizando sucesso e vitória
  "Rowing" = "#8B4513",     # Marrom, lembrando os remos e barcos de madeira
  "Alpine Skiing" = "#A52A2A", # Marrom avermelhado, simbolizando montanhas e terreno
  "Water Polo" = "#00CED1",  # Turquesa, simbolizando água
  "Wrestling" = "#DC143C"    # Vermelho, simbolizando força e competição
)

ggplot(visual, aes(x = reorder(reg, prop_mdl_p_esporte), y = prop_mdl_p_esporte, fill = Sport)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(prop_mdl_p_esporte * 100), '%')), vjust = 0.5, hjust = 1.25, size = 4, fontface = "bold",color = 'white') +
  coord_flip() +
  scale_fill_manual(values = cores_esporte) +  # Configura as cores manualmente
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Países com Maior Tradição em um Esporte")
}

'Quais os  países com mais medalhas por esporte'
{
visual = sel |>
  filter(rank_mdl_tot <= 3) |>
  arrange(Sport,rank_mdl_tot) |>
  select(NOC,Sport,rank_mdl_tot,reg,mdl_tot) |>
  mutate(
    color = case_when(
      rank_mdl_tot <= 1.5 ~ "gold",
      rank_mdl_tot <= 2.5 ~ "#C0C0C0",
      rank_mdl_tot <= 3.5 ~ "#CD7F32",
      TRUE ~ "#E0E0E0"
    )
  )

length(unique(visual$Sport))

visual$reg <- tidytext::reorder_within(visual$reg, visual$mdl_tot, visual$Sport)


visual1 = visual[visual$Sport %in% unique(visual$Sport)[1:18],]
visual2 = visual[visual$Sport %in% unique(visual$Sport)[19:36],]
visual3 = visual[visual$Sport %in% unique(visual$Sport)[37:51],]

library(tidytext)

ggplot(visual1, aes(x = reorder(reg,mdl_tot), y = mdl_tot)) +
  geom_bar(stat = "identity", aes(fill = color)) +
  geom_text(aes(label = round(mdl_tot)), vjust = 0.5, hjust = 1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),# Ajustando o título
    strip.text = element_text(size = 12, face = "bold", color = "black")  # Ajustando o título do facet
    
  ) +
  labs(title = "Países com mais medalhas por esporte") +
  scale_fill_identity()+
  tidytext::scale_x_reordered() +  # Ajusta as etiquetas do eixo x
  facet_wrap(~Sport, scales = "free_y")


ggplot(visual2, aes(x = reorder(reg,mdl_tot), y = mdl_tot)) +
  geom_bar(stat = "identity", aes(fill = color)) +
  geom_text(aes(label = round(mdl_tot)), vjust = 0.5, hjust = 1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),# Ajustando o título
    strip.text = element_text(size = 12, face = "bold", color = "black")  # Ajustando o título do facet
    
  ) +
  labs(title = "Países com mais medalhas por esporte") +
  scale_fill_identity()+
  tidytext::scale_x_reordered() +  # Ajusta as etiquetas do eixo x
  facet_wrap(~Sport, scales = "free_y")



ggplot(visual3, aes(x = reorder(reg,mdl_tot), y = mdl_tot)) +
  geom_bar(stat = "identity", aes(fill = color)) +
  geom_text(aes(label = round(mdl_tot)), vjust = 0.5, hjust = 1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),# Ajustando o título
    strip.text = element_text(size = 12, face = "bold", color = "black")  # Ajustando o título do facet
    
  ) +
  labs(title = "Países com mais medalhas por esporte") +
  scale_fill_identity()+
  tidytext::scale_x_reordered() +  # Ajusta as etiquetas do eixo x
  facet_wrap(~Sport, scales = "free_y")

}



'Quais os países com maior integração das mulheres no esporte?'

{
visual = sel |> arrange(-ger_prop_f) |> select(NOC,reg,ger_mdl_tot,ger_qtd_atletas_f,ger_prop_f,ger_qtd_atletas_m) |> filter(ger_qtd_atletas_f+ger_qtd_atletas_m >= 20) |> distinct() |> head(20)
visual

ggplot(visual, aes(x = reorder(paste0(reg,' | ',NOC), ger_prop_f), y = ger_prop_f)) +
  geom_bar(stat = "identity", fill = scales::gradient_n_pal(c("deeppink", "pink", "lightpink" ))(seq(0, 1, length.out = nrow(visual)))) +
  geom_text(aes(label = paste0(round(ger_prop_f*100,1),'%')), vjust = 0.5, hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),  # Alterando os labels do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Ajustando o título
  ) +
  labs(title = "Quais os países ten maior proporção de mulheres? (considerando quem possui mais de 20 atletas)")
}











'Add bandeiras países'

ggplot(visual, aes(x = reorder(reg, ger_mdl_score), y = ger_mdl_score)) +
  geom_bar(stat = "identity", fill = scales::gradient_n_pal(c("gold4", "gold"))(seq(0, 1, length.out = nrow(visual)))) +
  geom_flag(aes(y = ger_mdl_score + 5, country = iso2), size = 0.05) +  # Ajuste o tamanho conforme necessário
  geom_text(aes(label = round(ger_mdl_score)), vjust = -0.5, hjust = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),  # Esconde o texto do eixo y
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )


#### Exploração dos Dados













  
  

  
  
  



  
  
  
  
  






## Referências
#https://www.kaggle.com/datasets/bhanupratapbiswas/olympic-data