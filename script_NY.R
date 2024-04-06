#INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS

pacotes <- c("stringr","tidyr","readr","tidytext","ggplot2","dplyr","tibble","wordcloud","magrittr", "tm","RColorBrewer","viridis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando base de dados
NY_reviews <- read_csv("New_York_reviews.csv")

View(NY_reviews)

#####################################################################################################################
#Estudo Review com nota 3
#####################################################################################################################

#Filtrar apenas reviews com nota 3
review_3 <- NY_reviews %>%  filter(rating_review == "3")

#Remover colunas desnecessarias para a analise
review_3 <- review_3[, !names(review_3) %in% c("parse_count", "review_id", "review_preview", "city", "url_restaurant", "author_id")]

#transformar comentarios em minusculas
review_3$review_full <- tolower(review_3$review_full)

#juntar todos os comentarios nota 3
#comments_rate3 <- review_3 %>% group_by(restaurant_name) %>% summarise(comentarios_concatenados = paste(review_full, collapse = ""))
comments_rate3 <- review_3 %>% summarise(comentarios_concatenados = paste(review_full, collapse = ""))

#Aplicando tibble
comments_rate3 <- as_tibble(comments_rate3)

#Aplicando unnest_tokens
#comments_rate3 <- comments_rate3 %>% unnest_tokens(word, comentarios_concatenados) %>% group_by(restaurant_name)
comments_rate3 <- comments_rate3 %>% unnest_tokens(word, comentarios_concatenados)

# remover stop_words
words_rate3 <- comments_rate3 %>%  anti_join(stop_words)

#contar e agrupar as palavras
count_words <- words_rate3 %>%  count(word, sort = TRUE) %>% group_by(word) 

#contar todas as palavras que tem algum sentimento
sentiments <- get_sentiments("bing")
contagem_sentimento_unit <- words_rate3 %>%
  inner_join(sentiments) %>%
  summarise(count_words = n())

#contagem das palavras negativas/positivas
contagem_sentimentos <- words_rate3 %>%
  inner_join(sentiments)  %>%
  group_by(sentiment) %>%
  summarise(contagem = n())

#filtrar palavras com sentimento
sentiments <- get_sentiments("bing")
contagem_bing <- words_rate3 %>%  count(word, sort = TRUE) %>% 
  group_by(word) #%>%
  inner_join(sentiments)

#filtrar palavras com sentimento positivo
contagem_bing_positivo <- contagem_bing %>% 
  filter(sentiment == "positive")

#filtrar palavras com sentimento que mais aparecem pelo menos 1000 vezes para word cloud
contagem_bing <- words_rate3 %>%  count(word, sort = TRUE) %>% 
  group_by(word) %>%
  inner_join(sentiments) %>% 
  filter(n >= 1000)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
nuvem <- contagem_bing %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

#####################################################################################################################
#n-grams
#####################################################################################################################
#n-gram = 2
rate3_bigrams <- review_3 %>% summarise(comentarios_concatenados = paste(review_full, collapse = ""))
rate3_bigrams <- as_tibble(rate3_bigrams)
rate3_bigrams <- rate3_bigrams %>% unnest_tokens(bigram, comentarios_concatenados, token = "ngrams", n = 2)

# Filtrando bigrams que começam com 'not'
bigrams_not <- rate3_bigrams %>%
  filter(str_detect(bigram, "^not "))

sentiments <- get_sentiments("bing")

# Remover stop words e considerar apenas palavras com sentimento do bigram e contar a recorrencia da word2
bigrams_not <- bigrams_not %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word2 %in% sentiments$word)
  count(word2, sort = TRUE)

# Criando um dataframe com a contagem de palavras sem stop words e que tenha sentimento bing
word_counts <- comments_rate3 %>%
  anti_join(stop_words) %>%
  inner_join(sentiments) %>%
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE)

# Juntando as tabelas
final_table <- full_join(word_counts, bigrams_not, by = c("word" = "word2"))

#Renomear as colunas
tabelas_unidas <- final_table  %>% 
  rename(total = "n.x") %>%
  rename(with_not = "n.y")
  
#calcular a porcentagem que cada palavra aparece
tabelas_unidas <- tabelas_unidas  %>% 
  group_by(word) %>% 
  mutate(percentagem = (with_not / total) * 100)

# limitar a 3 cadas decimais e adicionar o símbolo de percentagem e remover N/A
tabelas_unidas$percentagem <- round(tabelas_unidas$percentagem, 2)
tabelas_unidas$percentagem <- paste0(tabelas_unidas$percentagem, "%")
tabelas_unidas <- na.omit(tabelas_unidas)

# Adicionando a coluna de sentimentos
tabelas_unidas <- tabelas_unidas %>%
  left_join(sentiments, by = "word")

# Filtrando sentimentos positivos
final_table_positive <- tabelas_unidas %>%
  filter(sentiment == "positive")

# Removendo a coluna de sentimentos
final_table_positive <- final_table_positive %>%
  select(-sentiment)

#filtrar resultados relevantes
tabela_relevante <- final_table_positive %>% 
  filter(with_not > 112) %>%
  arrange(desc(with_not))


ggplot(tabela_relevante, aes(x = word)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge", fill = "blue", width = 0.5) +
  geom_bar(aes(y = with_not), stat = "identity", position = "dodge", fill = "red", width = 0.5) +
  labs(title = "Quantidades de Palavras total x Palavras precedidas com negativa",
       x = "Palavras",
       y = "Quantidade") +
  scale_fill_manual(values = c("Total de Palavras" = "blue", "Palavras com negação" = "red"), name = "Legenda de Cores") +
  theme_minimal()

#######################################################################################################
#n-gram = 3
rate3_trigrams <- review_3 %>% summarise(comentarios_concatenados = paste(review_full, collapse = ""))
rate3_trigrams <- as_tibble(rate3_trigrams)
rate3_trigrams <- rate3_trigrams %>%
  unnest_tokens(trigram, comentarios_concatenados, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
################################################################################################################
#Estudo reviews positivos - nota 4 e 5
#################################################################################################################
#filtrar review positivos
review_positive <- NY_reviews %>%  filter(sample == "Positive")

#Remover colunas desnecessarias para a analise
review_positive <- review_positive[, !names(review_positive) %in% c("parse_count", "review_id","sample", "review_preview","review_full", "city", "url_restaurant", "author_id")]

#quantidade de comentários e a média das notas para cada restaurante
review_positive <- review_positive %>% 
  group_by(restaurant_name) %>% 
  summarise(
    qtd_reviews = n(), 
    media_notas = mean(rating_review, na.rm = TRUE),
    desvio_padrao_notas = sd(rating_review, na.rm = TRUE),
    variacao_notas = (desvio_padrao_notas / media_notas) * 100
  )

#filtrar medias >4.5 e mais de 1000 comentarios
filtro_media_comentario <- review_positive %>%
  filter (media_notas > 4.5, qtd_reviews > 1000)
#remover desvio padrão
filtro_media_comentario <- filtro_media_comentario[, !names(filtro_media_comentario) %in% c("desvio_padrao_notas")]

ggplot(filtro_media_comentario, aes(x = qtd_reviews, y =media_notas , size = variacao_notas, color = variacao_notas)) +
  geom_point() +
  geom_text(aes(label = restaurant_name), hjust = 0.5, vjust = -1, size = 3) +
  labs(
    title = "Relação entre Média das Notas, Quantidade de Comentários e Pontuação Ponderada",
    x = "Quantidade de Comentários",
    y = "Média das Notas",
    color = "Escala de cor"
  ) +
  scale_color_gradient(low = "blue", high = "red")
#####################################################################################################################
#Melhores restaurantes
#####################################################################################################################
#fazer uma seleção dos restaurantes mais bem avaliados remover variaveis fora da analise
#e calcular a media e variancia
filtro_mais_avaliados <- NY_reviews
filtro_mais_avaliados <- filtro_mais_avaliados[, !names(filtro_mais_avaliados) %in% c("parse_count", "review_id","sample", "review_preview","review_full", "city", "url_restaurant", "author_id")]

filtro_mais_avaliados <- filtro_mais_avaliados %>%
  group_by(restaurant_name) %>% 
  summarise(
    qtd_reviews = n(), 
    media_notas = mean(rating_review, na.rm = TRUE),
    desvio_padrao_notas = sd(rating_review, na.rm = TRUE),
    variacao_notas = (desvio_padrao_notas / media_notas) * 100,
    variancia = desvio_padrao_notas*desvio_padrao_notas
  )
# Filtrar apenas os restaurantes com nota 5
media_5 <- filtro_mais_avaliados %>%
  filter(media_notas == 5) %>%
  select(restaurant_name,qtd_reviews,-media_notas, -variacao_notas, -variancia) %>%
  filter(qtd_reviews > 2) %>%
  rename(Nome_Restaurante_NY = "restaurant_name") %>%
  rename(Quantidade_comentarios_NY = "qtd_reviews")

#ordenar em forma decrescente de comentarios
media_5 <- media_5[order(media_5$Quantidade_comentarios_NY, decreasing = TRUE),]

view(media_5)

#arrumar as casas decimais
#filtro_mais_avaliados$variacao_notas <- round(filtro_mais_avaliados$variacao_notas, 2)
#filtro_mais_avaliados$variacao_notas <- paste0(filtro_mais_avaliados$variacao_notas, "%")

#filtrar medias > 4 e mais de 2000 comentarios para construção do gráfico
filtro_grafico <- filtro_mais_avaliados %>%
  filter (media_notas > 4, qtd_reviews > 2000)

ggplot(filtro_grafico, aes(x = qtd_reviews, y = media_notas , size = variacao_notas, color = variacao_notas)) +
  geom_point() +
  geom_text(aes(label = restaurant_name), hjust = 0.5, vjust = -1, size = 3) +
  labs(
    title = "Relação entre a Média das Notas e Quantidade de Comentários",
    x = "Quantidade de Comentários",
    y = "Média das Notas",
    size = "Variação das notas (%)",
    color = "Variação das notas (%)"
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  guides(color = guide_legend("Variação das notas (%)"))
#####################################################################################################################
#FIM
#####################################################################################################################