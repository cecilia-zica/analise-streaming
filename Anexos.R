# ========================================================================
# Pacotes necessários
# Carregamento das bibliotecas usadas para leitura, manipulação de dados,
# visualizações e tratamento de strings.
# ========================================================================
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

# ========================================================================
# Importação da base de dados
# Leitura do arquivo CSV contendo as respostas da pesquisa de streaming.
# ========================================================================
dados_streaming <- read.csv(
  "Pesquisa sobre Consumo de Plataformas de Streaming  (respostas) - Respostas ao formulário 1.csv",
  sep = ",",
  header = TRUE,
  encoding = "UTF-8"
)

# ========================================================================
# Renomear variáveis da base
# Renomeia colunas com nomes extensos para nomes mais curtos e legíveis.
# ========================================================================
dados_streaming <- dados_streaming %>%
  rename(
    faixa_etaria = Qual.é.sua.faixa.etária.,
    horas_uso = Quantas.horas.por.semana.você.utiliza.plataformas.de.streaming.,
    forma_acesso = Como.você.acessa.plataformas.de.streaming.atualmente.,
    valor_pago = X..Quanto.você.paga.por.mês..do.seu.próprio.bolso..pelas.plataformas.que.usa.,
    plataforma_principal = Entre.as.seguintes.opções...qual.é.a.plataforma.de.streaming.que.você.mais.utiliza..,
    plataforma_secundaria = Quais.outras.plataformas.você.também.utiliza.com.frequência.,
    tipo_conteudo = Qual.o.tipo.de.conteúdo.que.você.mais.consome.nessas.plataformas.,
    frequencia_gosto = Com.que.frequência.você.encontra.algo.que.gosta.nas.plataformas.
  )

# ========================================================================
# Padronização de respostas da variável "tipo_conteudo"
# Limpeza de espaços e categorização padronizada para facilitar análises.
# ========================================================================
dados_streaming <- dados_streaming %>%
  mutate(
    tipo_conteudo = str_trim(tipo_conteudo),
    tipo_conteudo = case_when(
      tipo_conteudo == "Séries" ~ "Séries",
      tipo_conteudo == "Filmes" ~ "Filmes",
      tipo_conteudo == "Documentários" ~ "Documentários",
      str_detect(tipo_conteudo, regex("Reality", ignore_case = TRUE)) ~ "Reality shows",
      str_detect(tipo_conteudo, regex("Esportes", ignore_case = TRUE)) ~ "Esportes ao vivo",
      str_detect(tipo_conteudo, regex("infantil", ignore_case = TRUE)) ~ "Conteúdo infantil",
      TRUE ~ "Outros"
    )
  )

# ========================================================================
# Tabelas de frequência
# Cálculo da frequência absoluta de respostas por categoria de variável.
# ========================================================================
faixa_etaria_freq <- dados_streaming %>%
  count(faixa_etaria) %>%
  arrange(desc(n))

forma_acesso_freq <- dados_streaming %>%
  count(forma_acesso) %>%
  arrange(desc(n))

plataforma_principal_freq <- dados_streaming %>%
  count(plataforma_principal) %>%
  arrange(desc(n))

tipo_conteudo_freq <- dados_streaming %>%
  count(tipo_conteudo) %>%
  arrange(desc(n))

frequencia_gosto_freq <- dados_streaming %>%
  count(frequencia_gosto) %>%
  arrange(desc(n))

plataforma_secundaria_freq <- dados_streaming %>%
  count(plataforma_secundaria) %>%
  arrange(desc(n))

# ========================================================================
# Definição de paleta de cores pastel para os gráficos
# ========================================================================
cores_pastel <- c("#FFD1DC", "#BDE0FE", "#FFF4B2", "#C1F5D6", "#E6CCFF", "#FFB3C6", 
                  "#FFCCE5", "#D1F1FF", "#FFE6B2", "#C7F7D2")

# ========================================================================
# Gráficos de barras (6 gráficos)
# Visualizações das frequências categóricas em gráficos coloridos.
# ========================================================================

# Faixa etária dos respondentes
ggplot(faixa_etaria_freq, aes(x = reorder(faixa_etaria, -n), y = n, fill = faixa_etaria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Distribuição por Faixa Etária", x = "Faixa Etária", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Forma de acesso às plataformas
ggplot(forma_acesso_freq, aes(x = reorder(forma_acesso, -n), y = n, fill = forma_acesso)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Forma de Acesso às Plataformas de Streaming", x = "Forma de Acesso", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Plataforma mais utilizada
ggplot(plataforma_principal_freq, aes(x = reorder(plataforma_principal, -n), y = n, fill = plataforma_principal)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Plataforma de Streaming Mais Utilizada", x = "Plataforma Principal", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Tipo de conteúdo mais consumido
ggplot(tipo_conteudo_freq, aes(x = reorder(tipo_conteudo, -n), y = n, fill = tipo_conteudo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Tipo de Conteúdo Mais Consumido", x = "Tipo de Conteúdo", y = "Número de Respondentes") +
  theme_light(base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 0, size = 10),
    legend.position = "none"
  )

# Frequência de satisfação dos usuários
niveis_ordenados <- str_wrap(c(
  "Raramente (menos de 1x por semana)",
  "Às vezes (1–2x por semana)",
  "Frequentemente (3–5x por semana)",
  "Sempre (quase todos os dias)"
), width = 20)

frequencia_gosto_freq$frequencia_gosto <- str_wrap(
  frequencia_gosto_freq$frequencia_gosto, width = 20
)

frequencia_gosto_freq$frequencia_gosto <- factor(
  frequencia_gosto_freq$frequencia_gosto,
  levels = niveis_ordenados
)

ggplot(frequencia_gosto_freq, aes(x = frequencia_gosto, y = n, fill = frequencia_gosto)) +
  geom_bar(stat = "identity", alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = cores_pastel) +
  labs(
    title = "Frequência de Satisfação com Conteúdo",
    x = "Frequência",
    y = "Número de Respondentes"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Plataformas secundárias utilizadas
ggplot(plataforma_secundaria_freq, aes(x = reorder(plataforma_secundaria, -n), y = n, fill = plataforma_secundaria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Plataformas Secundárias Utilizadas", x = "Plataforma Secundária", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# ========================================================================
# Estatísticas descritivas para variáveis numéricas
# Cálculo de média, mediana, desvio padrão, mínimo e máximo
# ========================================================================
summary_horas_uso <- dados_streaming %>%
  summarise(
    media = mean(horas_uso, na.rm = TRUE),
    mediana = median(horas_uso, na.rm = TRUE),
    desvio_padrao = sd(horas_uso, na.rm = TRUE),
    minimo = min(horas_uso, na.rm = TRUE),
    maximo = max(horas_uso, na.rm = TRUE)
  )

summary_valor_pago <- dados_streaming %>%
  summarise(
    media = mean(valor_pago, na.rm = TRUE),
    mediana = median(valor_pago, na.rm = TRUE),
    desvio_padrao = sd(valor_pago, na.rm = TRUE),
    minimo = min(valor_pago, na.rm = TRUE),
    maximo = max(valor_pago, na.rm = TRUE)
  )

# ========================================================================
# Boxplots comparativos (3 gráficos)
# Análises bivariadas com distribuições por grupo
# ========================================================================

# Valor pago por faixa etária
ggplot(dados_streaming, aes(x = valor_pago, 
                            y = fct_rev(faixa_etaria), 
                            fill = faixa_etaria)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = cores_pastel) +
  labs(
    title = "Valor Pago por Faixa Etária",
    x = "Valor Pago (R$)",
    y = "Faixa Etária"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

# Horas de uso por forma de acesso
ggplot(dados_streaming, aes(x = horas_uso, 
                            y = fct_rev(forma_acesso), 
                            fill = forma_acesso)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = cores_pastel) +
  labs(
    title = "Horas de Uso por Forma de Acesso",
    x = "Horas de Uso por Semana",
    y = "Forma de Acesso"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# ========================================================================
# Conversão de texto para número (valor_pago)
# Limpeza de símbolos e conversão para tipo numérico
# ========================================================================
dados_streaming <- dados_streaming %>%
  mutate(
    valor_pago = str_replace_all(valor_pago, "R\\$|\\,", ""),
    valor_pago = str_trim(valor_pago),
    valor_pago = as.numeric(valor_pago)
  )

# ========================================================================
# Resumo do valor pago por faixa etária
# Tabela com média, mediana e total de respostas por faixa
# ========================================================================
media_valor_por_faixa <- dados_streaming %>%
  group_by(faixa_etaria) %>%
  summarise(
    media_valor_pago = mean(valor_pago, na.rm = TRUE),
    mediana_valor_pago = median(valor_pago, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(media_valor_pago))

# Gráfico de barras horizontal com média por faixa etária
ggplot(media_valor_por_faixa, aes(x = reorder(faixa_etaria, media_valor_pago), y = media_valor_pago, fill = faixa_etaria)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Média de Valor Pago por Faixa Etária",
    x = "Faixa Etária",
    y = "Valor Médio Pago (R$)"
  ) +
  scale_fill_manual(values = cores_pastel) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# ========================================================================
# Boxplot final: valor pago por tipo de conteúdo
# Observa variações de gasto entre categorias de conteúdo assistido
# ========================================================================
ggplot(dados_streaming, aes(x = valor_pago, 
                            y = fct_rev(fct_infreq(tipo_conteudo)), 
                            fill = tipo_conteudo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8, show.legend = FALSE) +
  labs(
    title = "Valor Pago por Tipo de Conteúdo",
    x = "Valor Pago (R$)",
    y = "Tipo de Conteúdo"
  ) +
  scale_fill_manual(values = c(
    "#FFB6B9", "#FFDAC1", "#B5EAD7",
    "#E2F0CB", "#C7CEEA", "#F6C1D5"
  )) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10)
  )

