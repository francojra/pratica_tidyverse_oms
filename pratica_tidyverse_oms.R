
# Prática tidyverse ------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 30/05/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(dados)

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Subconjunto de dados do relatório anual de tuberculose da Organização Mundial 
### da Saúde.

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- dados::dados_oms
view(dados)

# Selecionando dados -----------------------------------------------------------------------------------------------------------------------

dados_1 <- dados %>%
  select(pais, ano, novos_fpp_h1524) %>%
  filter(pais %in% c("Guatemala", "Haiti", 
                     "Hungria", "Estados Unidos")) %>%
  view()
dados_1

# Resumindo os dados -----------------------------------------------------------------------------------------------------------------------

dados_2 <- dados_1 %>%
  group_by(pais) %>%
  drop_na() %>%
  summarise(media = mean(novos_fpp_h1524),
            sd = sd(novos_fpp_h1524),
            n = n(), se = sd/sqrt(n))
dados_2

glimpse(dados_2)
dados_2$pais <- as.factor(dados_2$pais)

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

ggplot(dados_2) +
  geom_col(aes(x = fct_reorder(pais, media), y = media, fill = pais)) +
  geom_errorbar(aes(x = pais, y = media, 
                    ymax = media + se, ymin = media - se),
                    width = 0.15, size = 1.5, color = "brown") +
  labs(y = "Número médio de casos", x = "Países", fill = "Países",
       title = "Casos de tuberculose em homens de 15 a 24 anos") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
