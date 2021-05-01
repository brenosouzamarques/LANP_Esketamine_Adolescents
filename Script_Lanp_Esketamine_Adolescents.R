library(haven)
library(reshape2)
library(tidyverse)
df <- read_sav("banco de dados adolescentes 01.03.21.sav")
df <- as_tibble(df)
view(df)

## Criar um banco de dados para grC!ficos de efeito antidepressivo
## e efeito antisuicida
df_g <- df %>% 
        select(Sujeito,
               MADRS_prC),
               MADRS_24h)
df_g <- melt(df_g,
             id = "Sujeito",
             variable.name = "tempo",
             value.name = "madrs_score")

df_g <- as_tibble(df_g)
df_g$Sujeito <- as.factor(df_g$Sujeito)
df_g %>% group_by(tempo) %>% 
        summarise(media = mean(madrs_score),
                  dp = sd(madrs_score))
df_g

## Intervalo de confianC'a com Boostrap para mediana
library(boot)
a <- df_g %>% filter(tempo == "MADRS_24h")
b <- a$madrs_score
boot_resultado <- boot(b,
                       function(x, j)
                               median(x[j]),
                       R = 5000)
boot_resultado
boot.ci(boot.out = boot_resultado,
        conf = 0.95,
        type = "bca")

madrs_pos <- runif(n = 5000, min = 21, max = 31)

pct <- rep(1:2500, 2)

data_boot <- data.frame(pct, madrs_pre, madrs_pos)
data_boot <- melt(data_boot,
                  id.vars = "pct",
                  variable.name = "tempo",
                  value.name = "score")

data_boot %>% ggplot(aes(tempo, score))+
        geom_jitter(aes(color = tempo), alpha = .08)+
        theme_classic()+
        geom_boxplot(alpha = .09, width = .5)
## Criar grC!fico para efeito antidepressivo

df_g %>% mutate(
        media_pre = case_when(
                tempo == "MADRS_prC)" ~ mean(madrs_score)))

library(patchwork)

## Plot colorido do EFEITO ANTIDEPRESSIVO

plot_colorido <- ggplot(df_g,
       aes(tempo, madrs_score))+
        geom_boxplot(width = 0.3, color = "grey")+
        geom_line(aes(group = Sujeito, color = Sujeito), size = 1.2, alpha = 0.5)+
        geom_point(aes(group = Sujeito, color = Sujeito), size = 1)+
        theme_classic()+
        scale_y_continuous(name = "MADRS total score",
                           breaks = seq(from = 10, to = 50, by = 5))+
        scale_x_discrete(name = "\nTime of assessment",
                         labels = c("Baseline", "24h post-infusion"))+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 9),
              axis.title.x = element_text(face = "bold", size = 13),
              axis.title.y = element_text(face = "bold", size = 13))+
        labs(title = "Antidepressant effects \nof ketamine",
             subtitle = "Total MADRS scores \nfor each patient")
plot_colorido
ggsave("plotcolorido_grey.png", dpi = 400)


## Plot em PB do EFEITO ANTIDEPRESSIVO
plot_pb <- ggplot(df_g,
                  aes(tempo, madrs_score))+
        geom_boxplot(alpha = 0.2, width = 0.3, color = "grey")+
        geom_line(aes(group = Sujeito), size = 1.2, alpha = 0.5)+
        geom_point(aes(group = Sujeito), size = 1)+
        theme_classic()+
        scale_y_continuous(name = "MADRS total score",
                           breaks = seq(from = 10, to = 50, by = 5))+
        scale_x_discrete(name = "\nTime of assessment",
                         labels = c("Baseline", "24h post-infusion"))+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 9),
              axis.title.x = element_text(face = "bold", size = 13),
              axis.title.y = element_text(face = "bold", size = 13))+
        labs(title = "Antidepressant effects \nof ketamine",
             subtitle = "Total MADRS scores \nfor each patient")
plot_pb
ggsave("plotpb.tiff", dpi = 400)


## Carregar banco de dados com efeito antianedC4nico

df_a <- read_sav("D:/OneDrive/0 - Google Drive/2 - LANP/1 - Artigos/2021/SC)rie de Casos Adolescentes - Daniela Faria/Adolescentes MADRS item 10.sav")
df_a <- as_tibble(df_a)
df_a <- df_a %>% select(1:3)
df_a <- melt(df_a,
             id = "Sujeito",
             variable.name = "tempo",
             value.name = "score")
df_a <- as_tibble(df_a)
df_a
df_a$Sujeito <- as_factor(df_a$Sujeito)
df_a

## Bootstrap para efeito antianedC4nico

a <- df_a %>% filter(tempo == "Item10_24hMADRS")
b <- c(a$score)
b

boot_resultado <- boot(b,
                       function(x, j)
                               mean(x[j]),
                       R = 5000)
boot_resultado
boot.ci(boot.out = boot_resultado,
        conf = 0.95,
        type = "bca")


## Plot colorido do EFEITO ANTIANEDONICO
plot_col_aned <- ggplot(data = df_a,
       aes(tempo, score))+
        geom_boxplot(alpha = 0.2, width = 0.3, color = "grey")+
        geom_line(aes(group = Sujeito, color = Sujeito),
                  size = 1.2, alpha = 0.5)+
        geom_point(aes(group = Sujeito, color = Sujeito), size = 1)+
        theme_classic()+
        scale_y_continuous(name = "MADRS item 10 scores",
                           breaks = seq(0, 6, 1))+
        scale_x_discrete(name = "\nTime of assessment",
                         labels = c("Baseline", "24h post-infusion"))+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 9),
              axis.title.x = element_text(face = "bold", size = 13),
              axis.title.y = element_text(face = "bold", size = 13))+
        labs(title = "Anti-suicidal effects \nof ketamine",
             subtitle = "MADRS item 10 scores \nfor each patient")
plot_col_aned

ggsave("plotcoldaned.tiff", dpi = 400)

## Plot PB do EFEITO ANTIANEDCNICO

plot_pb_aned <- ggplot(data = df_a, aes(tempo, score))+
        geom_boxplot(alpha = 0.2, width = 0.3, color = "grey")+
        geom_line(aes(group = Sujeito,), size = 1.2, alpha = 0.7)+
        geom_point(aes(group = Sujeito))+
        theme_classic()+
        scale_y_continuous(name = "MADRS item 10 scores",
                           breaks = seq(0, 6, 1))+
        scale_x_discrete(name = "\nTime of assessment",
                         labels = c("Baseline", "24h post-infusion"))+
        theme(legend.position = "none",
              axis.text.x = element_text(size = 9),
              axis.title.x = element_text(face = "bold", size = 13),
              axis.title.y = element_text(face = "bold", size = 13))+
        labs(title = "Anti-suicidal effects \nof ketamine",
             subtitle = "MADRS item 10 scores \nfor each patient")
plot_pb_aned

library(patchwork)
plot_colorido + plot_col_aned
ggsave("antidepressant_effects.tiff", dpi = 500)
ggsave("antidepressant_effects.png", dpi = 500)


plot_pb + plot_pb_aned
ggsave("antianhedonic_effects.tiff", dpi = 500)
ggsave("antianhedonic_effecits.png", dpi = 500)



alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_histogram(aes(x = c.pre..pos., fill = Time),
                       alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_histogram(aes(x = c.pre..pos., fill = Time,
                           color = Time),
                       alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_bar(aes(x = c.pre..pos., fill = Time,
                     color = Time),
                 alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_bar(aes(x = c.pre..pos.),
                 alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_jitter(aes(x = c.pre..pos.),
                    alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_jitter(aes(x = c.pre..pos.,
                        y = Time),
                    alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.,
                         color = Time),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.,
                         fill = Time),
                     alpha = .4)+
        theme_classic()
a
madrs_boot
madrs_boot %>% group_by(Time) %>%
        summarise(md = mean(c.pre..pos.),
                  sd = sd(c.pre..pos.))
df_pre <- df_g %>%
        filter(tempo == "MADRS_prC)")
boot_pre <- boot(df_p$madrs_score, function(x,j) mean(x[j],  na.rm = T), R=5000)
boot_pre
boot.ci(boot.out = boot_pre,
        conf = 0.95,
        type = "all",
        R = 5000)
df_pos <- df_g %>%
        filter(tempo == "MADRS_24h")
boot_pos <- boot(df_pos$madrs_score, function(x,j) mean(x[j],  na.rm = T), R=5000)
boot_pos
df_g %>% group_by(tempo) %>%
        summarise(md = mean(madrs_score),
                  sd = sd(madrs_score))
df_pre <- df_g %>%
        filter(tempo == "MADRS_prC)")
mean(df_pre$madrs_score)
boot_pre <- boot(df_pre$madrs_score, function(x,j) mean(x[j],  na.rm = T), R=5000)
boot_pre
plot(boot_pre)
pre <- boot_pre$t
Time <- rep(c("Baseline", "7 days"), times = 5000)
madrs_boot <- data.frame(c(pre, pos), Time)
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.,
                         fill = Time),
                     alpha = .4)+
        theme_classic()
a
madrs_boot
a <- ggplot(madrs_boot)+
        geom_histogram(aes(x = c.pre..pos.,
                           fill = Time),
                       alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_histogram(aes(x = c.pre..pos.),
                       alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.,
                         color = madrs_boot$Time),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.,
                         fill = madrs_boot$Time),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot)+
        geom_density(aes(x = c.pre..pos.,
                         fill = as.factor(madrs_boot$Time)),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot[madrs_boot$Time == "Baseline"])+
        geom_density(aes(x = c.pre..pos.,
                         fill = as.factor(madrs_boot$Time)),
                     alpha = .4)+
        theme_classic()
a <- ggplot(madrs_boot[madrs_boot$Time == "Baseline",])+
        geom_density(aes(x = c.pre..pos.,
                         fill = as.factor(madrs_boot$Time)),
                     alpha = .4)+
        theme_classic()
a
a <- ggplot(madrs_boot[,madrs_boot$Time == "Baseline"])+
        geom_density(aes(x = c.pre..pos.,
                         fill = as.factor(madrs_boot$Time)),
                     alpha = .4)+
        theme_classic()
madrs_boot
a <- ggplot(madrs_boot %>% filter(Time == "Baseline") %>%
                    geom_density(aes(x = c.pre..pos.)),
            alpha = .4)+
        theme_classic()
a <- madrs_boot %>% filter(Time == "Baseline") %>%
        ggplot()+
        geom_density(aes(x = c.pre..pos.)),
alpha = .4)+
        theme_classic()
a <- madrs_boot %>% filter(Time == "Baseline") %>%
        ggplot()+
        geom_density(aes(x = c.pre..pos.),
                     alpha = .4)+
        theme_classic()
a
a <- madrs_boot %>% filter(Time == "Baseline")
a <- madrs_boot %>% filter(Time == "Baseline")
a
madrs_boot
a <- madrs_boot %>% filter(Time == "7 days")
a
df_pre <- df_g %>%
        filter(tempo == "MADRS_prC)")
mean(df_pre$madrs_score)
df_pre <- df_g %>%
        filter(tempo == "MADRS_24h")
mean(df_pre$madrs_score)
df_pre <- df_g %>%
        filter(tempo == "MADRS_prC)")
mean(df_pre$madrs_score)
boot_pre <- boot(df_pre$madrs_score, function(x,j) mean(x[j],  na.rm = T), R=5000)
boot_pre
mean(boot_pre)
mean(boot_pre$t)
pre_madrs <- boot_pre$t
df_pos <- df_g %>%
        filter(tempo == "MADRS_24h")
mean(df_pos$madrs_score)
boot_pos <- boot(df_pos$madrs_score, function(x,j) mean(x[j],  na.rm = T), R=5000)
boot_pos
pos_madrs <- boot_pos$t
mean(pos_madrs)
Time <- rep(c("Baseline", "7days"), 5000)
pre_madrs
pos_madrs
scores_madrs <- c(pre_madrs, pos_madrs)
Time <- rep(c("Baseline", "7days"), each = 5000)
dados_densitiy <- data.frame(Time, scores_madrs)
dados_densitiy
ggplot(dados_densitiy)+
        geom_histogram(aes(y = scores_madrs, color = Time))
ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, color = Time))
ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time))
ggplot(dados_densitiy)+
        geom_bars(aes(x = scores_madrs, fill = Time))
ggplot(dados_densitiy)+
        geom_bar(aes(x = scores_madrs, fill = Time))
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time))
Time <- factor(Time,
               labels = c("Baseline", "7days"))
scores_madrs <- c(pre_madrs, pos_madrs)
dados_densitiy <- data.frame(Time, scores_madrs)
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time))
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time))+
        labs(x = "MADRS total scores")
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     alpha = .3)+
        labs(x = "MADRS total scores")
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     alpha = .5)+
        labs(x = "MADRS total scores")
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     alpha = .5,
                     color = "white")+
        labs(x = "MADRS total scores")+
        theme_classic()
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     alpha = .5,
                     color = Time)+
        labs(x = "MADRS total scores")+
        theme_classic()
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time, color = Time),
                     alpha = .5)+
        labs(x = "MADRS total scores")+
        theme_classic()
Time <- rep(c("Baseline", "7days"), each = 5000)
Time <- factor(Time,
               labels = c("Baseline", "7days"),
               ordered(Time, is.ordered(Time)))
levels = c("Baseline", "7days")
scores_madrs <- c(pre_madrs, pos_madrs)
dados_densitiy <- data.frame(Time, scores_madrs)
ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time, color = Time),
                     alpha = .5)+
        labs(x = "MADRS total scores")+
        theme_classic()
a <- ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time, color = Time),
                     alpha = .5)+
        labs(x = "MADRS total scores")+
        theme_classic()
suicidio <- c(Adolescentes_MADRS_item_10$Item10_PrC)MADRS,
Adolescentes_MADRS_item_10$Item10_24hMADRS)
tempo <- rep(c("baseline", "7d"), each = 9)
df_suicidio <- data.frame(suicidio, tempo)
df_suicidio
mean(df_suicidio$suicidio)
df_sf <- df_suicidio %>% filter(tempo == "baseline")
boot_pre_sf <- boot(df_sf$suicidio, function(x,j) mean(x[j],  na.rm = T), R=5000)
pre_suicidio <- boot_pre_sf$t
boot_pre_sf
boot.ci(boot.out = pre_suicidio,
        conf = 0.95,
        type = "all",
        R = 5000)
pre_suicidio <- boot_pre_sf$t
pre_suicidio
boot.ci(boot.out = pre_suicidio,
        conf = 0.95,
        type = "all",
        R = 5000)
boot.ci(boot.out = boot_pre_sf,
        conf = 0.95,
        type = "all",
        R = 5000)
df_sp <- df_suicidio %>% filter(tempo == "7d")
boot_pre_sp <- boot(df_sp$suicidio, function(x,j) mean(x[j],  na.rm = T), R=5000)
pre_suicidio <- boot_pre_sp$t
boot_pre_sp
boot.ci(boot.out = boot_pre_sp,
        conf = 0.95,
        type = "all",
        R = 5000)
df_sf <- df_suicidio %>% filter(tempo == "baseline")
boot_pre_sf <- boot(df_sf$suicidio, function(x,j) mean(x[j],  na.rm = T), R=5000)
pre_suicidio <- boot_pre_sf$t
df_sp <- df_suicidio %>% filter(tempo == "7d")
boot_pre_sp <- boot(df_sp$suicidio, function(x,j) mean(x[j],  na.rm = T), R=5000)
pos_suicidio <- boot_pre_sp$t
suicidio <- c(pre_suicidio, pos_suicidio)
tempo <- rep(c("Baseline", "7 days"), each = 5000)
dfs <- data.frame(suicidio, tempo)
dfs$tempo <- factor(dfs$tempo,
                    levels = c("Baseline", "7 days"))
ggplot(dfs)+
        geom_density(aes(x = suicidio, fill = tempo, color = tempo),
                     alpha = .5)
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time, color = Time),
                       alpha = .5)+
        labs(x = "MADRS total scores")+
        theme_classic()
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time, color = Time),
                       alpha = .5)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)
a
dfs
dados_densitiy
dfs
suicidio <- c(pre_suicidio, pos_suicidio)
tempo <- rep(c("Baseline", "7 days"), each = 5000)
dfs <- data.frame(tempo, suicidio)
dfs$tempo <- factor(dfs$tempo,
                    levels = c("Baseline", "7 days"))
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)+
        labs(x = "MADRS Item 10")+
        theme_classic()
dfs$tempo <- factor(dfs$tempo,
                    levels = c("7 days", "Baseline"))
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .5)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .3)+
        labs(x = "MADRS Item 10")+
        theme_classic()
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time, color = Time),
                       alpha = .4)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a
ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4)+
        labs(x = "MADRS Item 10")+
        theme_classic()
library(patchwork)
b <- ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a + b
b <- ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4,
                       binwidth = 10)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a + b
b <- ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4,
                       binwidth = 100)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a + b
b <- ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4,
                       binwidth = 10)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a + b
b <- ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4,
                       binwidth = 1)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a + b
b <- ggplot(dfs)+
        geom_histogram(aes(x = suicidio, fill = tempo, color = tempo),
                       alpha = .4,
                       binwidth = .01)+
        labs(x = "MADRS Item 10")+
        theme_classic()
a + b
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time, color = Time),
                       alpha = .4,
                       binwidth = .01)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time, color = Time),
                       binwidth = .01)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, fill = Time),
                       binwidth = .01)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        geom_histogram(aes(x = scores_madrs, color = Time),
                       binwidth = .01)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        density(aes(x = scores_madrs, color = Time),
                binwidth = .01)+
        labs(x = "MADRS total scores")+
        theme_classic()
a <- ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     binwidth = .01)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     alpha = .4)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time, color = Time),
                     alpha = .4)+
        labs(x = "MADRS total scores")+
        theme_classic()
a
a <- ggplot(dados_densitiy)+
        geom_density(aes(x = scores_madrs, fill = Time),
                     alpha = .4)+
        scale_fill_discrete(name = "Time",
                            labels = c("24h post-infusion",
                                       "Baseline"))+
        labs(x = "MADRS total scores")+
        theme_classic()
a
b <- ggplot(dfs)+
        geom_density(aes(x = suicidio, fill = tempo),
                     alpha = .5)+
        labs(x = "MADRS Item 10")+
        theme_classic()

library(patchwork)
a + b
a | b
a / b
boot_pre_sd <- boot(df_sf$suicidio, function(x,j) mean(x[j],  na.rm = T), R=10000)
pre_suicidio_sd <- boot_pre_sd$t
pre_suicidio_sd
b <- ggplot(dfs)+
        geom_density(aes(x = suicidio, fill = tempo),
                     alpha = .5)+
        scale_fill_discrete(name = "Time",
                            labels = c("24h post-infusion",
                                       "Baseline"))+
        labs(x = "MADRS Item 10",
             colour = "Time")+
        theme_classic()
a / b
b <- ggplot(dfs)+
        geom_density(aes(x = suicidio, fill = tempo),
                     alpha = .5)+
        scale_fill_discrete(name = "Time",
                            labels = c("24h post-infusion",
                                       "Baseline"))+
        labs(x = "MADRS Item 10",
             fill = "Time")+
        theme_classic()
a / b
a / b + plot_annotation(title = "Bootstraped samples of total MADRS scores \n
and MADRS item 10 scores")
a / b + plot_annotation(title = "Bootstraped samples of total MADRS scores and MADRS item 10 scores")
a / b + plot_annotation(title = "Bootstraped samples of total MADRS scores and MADRS Item 10 scores")
a / b + plot_annotation(title = "Bootstraped samples of total MADRS scores and MADRS Item 10")
getwd()
ggsave("bootstrap figure.png", dpi = 450)
a / b + plot_annotation(title = "Bootstrapped sample means for total MADRS scores and MADRS Item 10",
                        subtitle = "Density plots for baseline and 24h post-infusion")
ggsave("bootstrap figure.png", dpi = 450)
