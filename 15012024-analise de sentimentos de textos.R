# Instalação dos pacotes (caso não estejam instalados)
packages <- c("tidytext", "textdata", "tidyverse")

# Verifica se os pacotes estão instalados e instala, se necessário
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Carrega os pacotes
library(tidytext) # mineração de texto usando tidy tools
library(textdata) # obter conjunto de sentimentos pelo texto
library(tidyverse) # data wrangling and viz
library(readr)    # leitura eficiente de dados

# obter dados de texto
url <- "http://gutenberg.net.au/ebooks01/0100021.txt"
text_raw <- read_lines(
  url,
  skip = 38,
  n_max = 8500,
  skip_empty_rows = TRUE,)

# Mostrar as primeiras linhas do texto
cat(head(text_raw), sep = "\n")

# Criar variáveis de para capítulo

text_df <- tibble(text = text_raw) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [1-9]", ignore_case =TRUE)))) %>%
  filter(!str_detect(text, regex("chapter [1-9]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

text_df

# obter os lexicos de sentimentos

bing <- tidytext::get_sentiments("bing")
afinn <- tidytext::get_sentiments("afinn")
nrc <- tidytext::get_sentiments("nrc")


# limitações léxicos de de sentimentos 


text_pos <- "Este é meu livro favorito. Eu gosto dele."
text_neg <- "Este não é meu livro favorito. Eu não gosto dele."

df_limitations <- tibble(
  text = c(text_pos, text_neg),
  examples = c("text_pos", "text_neg")
)

df_limitations %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word") %>%
  inner_join(bing, by = "word")


# tokenize e junte léxicos ------------------------------------------------------------ -

text_df_bing <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(bing, by = "word")

text_df_nrc <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word")

text_df_afinn <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word")


# estatísticas gerais --------------------------------------------

# número de palavras por sentimento binário pos/neg

text_df_bing %>%
  count(sentiment, sort = TRUE)

# número de palavras por emoção
text_df_nrc %>%
  count(sentiment, sort = TRUE)

# pontuação geral de sentimento
text_df_afinn %>%
  summarise(overall_score = sum(value, na.rm = TRUE))


# principais palavras por emoção--------------------------------------------------

text_df_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Palavras negativas/positivas em 1984 de Orwell",
       subtitle = "Top 10 palavras, léxico do BING",
       x = "Número de palavras",
       y = NULL)

text_df_nrc %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(!word %in% c("palavras", "sentimentos")) %>% # remove tokens
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Palavras emocionais em 1984 de Orwell",
       subtitle = "Top 5 palavras, léxicos  de NRC",
       x = NULL, y = NULL,
       caption = "https://makalister.netlify.app/")


# pontuação emocional por capítulo ----------------------------------------------

text_df_afinn_chapter_score <- text_df_afinn %>%
  group_by(chapter) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

text_df_afinn_chapter_score %>%
  mutate(positive = sentiment > 0,
         chapter = as.factor(chapter)) %>%
  ggplot(aes(chapter, sentiment)) +
  geom_col(aes(fill = positive), show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Análise de sentimento de 1984 de Orwell",
       subtitle = "Pontuação por capítulo, léxicos de afinn",
       caption = "https://makalister.netlify.app/")


# Pontuação do índice por capítulo -----------------------------------------------

text_df_index_score <- text_df_afinn %>%
  group_by(index) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

text_df_index_score %>%
  mutate(positive = sentiment > 0) %>%
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill = positive), show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Análise de sentimento de 1984 de Orwell",
       subtitle = "Pontuação para cada 50 linhas, léxico afinn",
       caption = "https://makalister.netlify.app/")