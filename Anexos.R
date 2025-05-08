# Carregar pacotes necessários
library(readr)
library(dplyr)
library(ggplot2)

# Importar a base de dados
dados_streaming <- read.csv(
  "Pesquisa sobre Consumo de Plataformas de Streaming  (respostas) - Respostas ao formulário 1.csv",
  sep = ",",
  header = TRUE,
  encoding = "UTF-8"
)

# Renomear variáveis para melhorar a legibilidade
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

##########################
# Tabelas de Frequências
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

##########################
# Definir cores pastel para os gráficos
cores_pastel <- c("#FFD1DC", "#BDE0FE", "#FFF4B2", "#C1F5D6", "#E6CCFF", "#FFB3C6", 
                  "#FFCCE5", "#D1F1FF", "#FFE6B2", "#C7F7D2")

##########################
# Gráficos de Barras

# Faixa Etária
ggplot(faixa_etaria_freq, aes(x = reorder(faixa_etaria, -n), y = n, fill = faixa_etaria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Distribuição por Faixa Etária", x = "Faixa Etária", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Forma de Acesso
ggplot(forma_acesso_freq, aes(x = reorder(forma_acesso, -n), y = n, fill = forma_acesso)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Forma de Acesso às Plataformas de Streaming", x = "Forma de Acesso", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Plataforma Principal
ggplot(plataforma_principal_freq, aes(x = reorder(plataforma_principal, -n), y = n, fill = plataforma_principal)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Plataforma de Streaming Mais Utilizada", x = "Plataforma Principal", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Tipo de Conteúdo
ggplot(tipo_conteudo_freq, aes(x = reorder(tipo_conteudo, -n), y = n, fill = tipo_conteudo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Tipo de Conteúdo Mais Consumido", x = "Tipo de Conteúdo", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Frequência de Satisfação
ggplot(frequencia_gosto_freq, aes(x = reorder(frequencia_gosto, -n), y = n, fill = frequencia_gosto)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Frequência de Satisfação com Conteúdo", x = "Frequência", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

# Plataforma Secundária
ggplot(plataforma_secundaria_freq, aes(x = reorder(plataforma_secundaria, -n), y = n, fill = plataforma_secundaria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Plataformas Secundárias Utilizadas", x = "Plataforma Secundária", y = "Número de Respondentes") +
  theme_light(base_family = "sans")

##########################
# Resumos Estatísticos

# Horas de Uso
summary_horas_uso <- dados_streaming %>%
  summarise(
    media = mean(horas_uso, na.rm = TRUE),
    mediana = median(horas_uso, na.rm = TRUE),
    desvio_padrao = sd(horas_uso, na.rm = TRUE),
    minimo = min(horas_uso, na.rm = TRUE),
    maximo = max(horas_uso, na.rm = TRUE)
  )

# Valor Pago
summary_valor_pago <- dados_streaming %>%
  summarise(
    media = mean(valor_pago, na.rm = TRUE),
    mediana = median(valor_pago, na.rm = TRUE),
    desvio_padrao = sd(valor_pago, na.rm = TRUE),
    minimo = min(valor_pago, na.rm = TRUE),
    maximo = max(valor_pago, na.rm = TRUE)
  )

##########################
# Análises Bivariadas

# Gráfico boxplot - Valor Pago por Faixa Etária
ggplot(dados_streaming, aes(x = faixa_etaria, y = valor_pago, fill = faixa_etaria)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Valor Pago por Faixa Etária", x = "Faixa Etária", y = "Valor Pago (R$)") +
  theme_light(base_family = "sans") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

# Gráfico boxplot - Horas de Uso por Forma de Acesso
ggplot(dados_streaming, aes(x = forma_acesso, y = horas_uso, fill = forma_acesso)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Horas de Uso por Forma de Acesso", x = "Forma de Acesso", y = "Horas de Uso por Semana") +
  theme_light(base_family = "sans") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

##########################
# Tabelas Resumo para Bivariadas

# Média Horas de Uso por Forma de Acesso
media_horas_por_forma <- dados_streaming %>%
  group_by(forma_acesso) %>%
  summarise(
    media_horas_uso = mean(horas_uso, na.rm = TRUE),
    mediana_horas_uso = median(horas_uso, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(media_horas_uso))

# Média Valor Pago por Faixa Etária
media_valor_por_faixa <- dados_streaming %>%
  group_by(faixa_etaria) %>%
  summarise(
    media_valor_pago = mean(valor_pago, na.rm = TRUE),
    mediana_valor_pago = median(valor_pago, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(media_valor_pago))

##########################
# Gráfico de barras empilhadas pastel - Frequência de Gosto × Faixa Etária
ggplot(dados_streaming, aes(x = faixa_etaria, fill = frequencia_gosto)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Proporção de Satisfação por Faixa Etária", x = "Faixa Etária", y = "Proporção") +
  theme_light(base_family = "sans") +
  theme(plot.title = element_text(hjust = 0.5))

##########################
# Gráfico boxplot rosa pastel - Valor Pago por Tipo de Conteúdo
ggplot(dados_streaming, aes(x = tipo_conteudo, y = valor_pago, fill = tipo_conteudo)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = cores_pastel) +
  labs(title = "Valor Pago por Tipo de Conteúdo", x = "Tipo de Conteúdo", y = "Valor Pago (R$)") +
  theme_light(base_family = "sans") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

