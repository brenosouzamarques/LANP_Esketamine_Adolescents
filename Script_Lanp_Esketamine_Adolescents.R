library(tidyverse)
library(haven)
library(lsr)
library(rstatix)
library(boot)
library(kableExtra)
library(patchwork)
library(RColorBrewer)
library(ggsci)
library(car)
library(purrr)
library(broom)

## Load from GitHub repository

urlRemote  <- "https://raw.githubusercontent.com/"
pathGithub <- "brenosouzamarques/LANP_Esketamine_Adolescents/master/"
fileName   <- "Dataset.csv"

df <- paste0(urlRemote, pathGithub, fileName) %>% 
            read.csv()

### T-test - MADRS Total Scores

t.test(x = df$MADRS_pré,
       y = df$MADRS_24h,
       paired = T)
lsr::cohensD(df$MADRS_pré,
             df$MADRS_24h,
             method = "paired")

### T-test - MADRS Item 10 scores

t.test(x = df$Item10_PréMADRS,
       y = df$Item10_24hMADRS,
       paired = T)
lsr::cohensD(df$Item10_PréMADRS,
             df$Item10_24hMADRS,
             method = "paired")

## Create bootstrap function

boot_funs <- function(df, f1, f2){
            a <- df[[f1]]
            b <- df[[f2]]
            boot_a <- boot(a,
                           function(x, j) mean(x[j], na.rm = T), #Boot baseline variable
                           R = 10000)
            boot_b <- boot(b,
                           function(x, j) mean(x[j], na.rm = T), #Boot 7 days variable
                           R = 10000)
            boot_ci_a <- boot.ci(boot.out = boot_a, #Calculate baseline CI
                                 conf = 0.95,
                                 type = "bca")
            boot_ci_b <- boot.ci(boot.out = boot_b, #Calculate post-treatment CI
                                 conf = 0.95,
                                 type = "bca")
            print(boot_ci_a)
            print(boot_ci_b)
            
            ## Code to extract bootstrapped sample means
            densitiy_a <- boot_a$t
            densitiy_b <- boot_b$t
            dens <- c(densitiy_a, densitiy_b)
            time <- rep(c("Baseline", "Post-Treatment"), each = 10000)
            data_dens <<- data.frame(dens, time)
            data_dens$time <- factor(data_dens$time)
}

boot_funs(df, "MADRS_pré", "MADRS_24h")

plot_madrs <- ggplot(data_dens)+
            geom_density(aes(x = dens, fill = time),
                         color = "white",
                         alpha = 0.9)+
            labs(x = "Total MADRS scores")+
            scale_fill_brewer(name = "Time of assessment:",
                              palette = "Dark2")+
            theme_classic()+
            theme(text = element_text(family = "serif"),
                  axis.text.x = element_text(size = 14),
                  axis.title.x = element_text(size = 14),
                  axis.text.y = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  legend.text = element_text(size = 13),
                  legend.title = element_text(size = 13))


boot_funs(df, "Item10_PréMADRS", "Item10_24hMADRS")

plot_madrs10 <- ggplot(data_dens)+
            geom_density(aes(x = dens, fill = factor(time)),
                         color = "white",
                         alpha = 0.9)+
            labs(x = "Item 10 MADRS scores",
                 y = element_blank())+
            scale_fill_brewer(name = "Time of assessment:",
                              palette = "Dark2")+
            theme_classic()+
            theme(text = element_text(family = "serif"),
                  axis.text.x = element_text(size = 14),
                  axis.title.x = element_text(size = 14),
                  axis.text.y = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  legend.text = element_text(size = 13),
                  legend.title = element_text(size = 13))

## Density plots of bootstrapped sample means

## Creates patchwork object
graficos <- plot_madrs + plot_madrs10 &
            theme(legend.position = "bottom",
                  text = element_text(family = "serif"),
                  title = element_text(size = 12),
                  legend.title = element_text(size = 12))

## Creates guided patchwork object
graficos_f <- graficos + plot_layout(guides = "collect")

## Edits and annotates patchwork object
plota <- graficos_f + plot_annotation(
            title = 'Supp. Fig. 1 - Bootstraped sample means for total and item 10 MADRS scores',
            subtitle = 'Density plots of results at baseline and 7 days post-treatment')
plota


## Saves plot in high resolution
ggsave("density_plot.png", plota, width=25, height=15, units="cm", dpi=450)

## Create dataset for total MADRS scores
madrs_total <- c(df$MADRS_pré, df$MADRS_24h)
time <- rep(c("baseline", "post"), each = 10)
id <- rep(1:10, 2)
id <- factor(id)
madrs_data_plot <- tibble(id, time, madrs_total)

## plot total MADRS scores
madrs_plot <- madrs_data_plot %>% ggplot()+
            geom_boxplot(aes(x = time, y = madrs_total),
                         width = .3,
                         alpha = .3,
                         color = "grey")+
            scale_y_continuous(breaks = seq(from = 10,
                                            to = 50,
                                            by = 5))+
            scale_x_discrete(labels = c("Baseline", "24 hours post-infusion"))+
            geom_point(aes(x = time, y = madrs_total,
                           color = id,
                           group = id),
                       size = 1.5)+
            geom_line(aes(x = time, y = madrs_total,
                          color = id,
                          group = id,
                          alpha = .4),
                      size = 1.5)+
            scale_color_npg()+
            theme_classic()+
            labs(x = "Time of assessment",
                 y = "Total MADRS scores")+
            theme(axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 13),
                  axis.title.y = element_text(size = 15),
                  axis.text.y = element_text(size = 13),
                  legend.position = "none")

## Create dataset for MADRS Item 10

si_pre <- df$Item10_PréMADRS
si_post <- df$Item10_24hMADRS

madrs_item10 <- c(si_pre, si_post)
time <- rep(c("baseline", "post"), each = 10)
id <- rep(1:10, 2)
id <- factor(id)
madrs_item_plot <- tibble(id, time, madrs_item10)

item10_plot <- madrs_item_plot %>% ggplot()+
            geom_boxplot(aes(x = time, y = madrs_item10),
                         width = .3,
                         alpha = .3,
                         color = "grey")+
            scale_y_continuous(breaks = seq(from = 0,
                                            to = 6,
                                            by = 1))+
            scale_x_discrete(labels = c("Baseline", "24 hours post-infusion"))+
            geom_point(aes(x = time, y = madrs_item10,
                           color = id,
                           group = id),
                       size = 1.5,
                       position = position_dodge(0.08))+
            geom_line(aes(x = time, y = madrs_item10,
                          color = id,
                          group = id,
                          alpha = .4),
                      size = 1.5,
                      position = position_dodge(0.08))+
            scale_color_npg()+
            theme_classic()+
            labs(x = "Time of assessment",
                 y = "MADRS item 10 scores")+
            theme(axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 13),
                  axis.title.y = element_text(size = 15),
                  axis.text.y = element_text(size = 13),
                  legend.position = "none",
                  text = element_text(family = "serif"))

## Creates patchwork object
graficos_i10 <- madrs_plot + item10_plot &
            theme(text = element_text(family = "serif"),
                  title = element_text(size = 16),
                  legend.title = element_text(size = 14))

## Edits and annotates patchwork object
graficos_i10 <- graficos_i10 + plot_annotation(
            title = 'Total and item 10 MADRS scores at baseline and 24h post-treatment',
            subtitle = 'Scores are given for each patient')
graficos_i10


## Saves plot in high resolution
ggsave("MADRS_plot.png", graficos_i10, width=25, height=15, units="cm", dpi=450)



## Create dataset for total MADRS scores
madrs_total <- c(df$MADRS_pré, df$MADRS_24h)
time <- rep(c("baseline", "post"), each = 10)
id <- rep(1:10, 2)
id <- factor(id)
madrs_data_plot <- tibble(id, time, madrs_total)

## plot total MADRS scores
madrs_plot <- madrs_data_plot %>% ggplot()+
            geom_boxplot(aes(x = time, y = madrs_total),
                         width = .3,
                         alpha = .3,
                         color = "grey")+
            scale_y_continuous(breaks = seq(from = 10,
                                            to = 50,
                                            by = 5))+
            scale_x_discrete(labels = c("Baseline", "24 hours post-infusion"))+
            geom_point(aes(x = time, y = madrs_total,
                           color = id,
                           group = id),
                       size = 1.5)+
            geom_line(aes(x = time, y = madrs_total,
                          color = id,
                          group = id,
                          alpha = .4),
                      size = 1.5)+
            scale_color_npg()+
            theme_classic()+
            labs(x = "Time of assessment",
                 y = "Total MADRS scores")+
            theme(axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 13),
                  axis.title.y = element_text(size = 15),
                  axis.text.y = element_text(size = 13),
                  legend.position = "none")

## Create dataset for MADRS Item 10

si_pre <- df$Item10_PréMADRS
si_post <- df$Item10_24hMADRS

madrs_item10 <- c(si_pre, si_post)
time <- rep(c("baseline", "post"), each = 10)
id <- rep(1:10, 2)
id <- factor(id)
madrs_item_plot <- tibble(id, time, madrs_item10)

item10_plot <- madrs_item_plot %>% ggplot()+
            geom_boxplot(aes(x = time, y = madrs_item10),
                         width = .3,
                         alpha = .3,
                         color = "grey")+
            scale_y_continuous(breaks = seq(from = 0,
                                            to = 6,
                                            by = 1))+
            scale_x_discrete(labels = c("Baseline", "24 hours post-infusion"))+
            geom_point(aes(x = time, y = madrs_item10,
                           color = id,
                           group = id),
                       size = 1.5,
                       position = position_dodge(0.08))+
            geom_line(aes(x = time, y = madrs_item10,
                          color = id,
                          group = id,
                          alpha = .4),
                      size = 1.5,
                      position = position_dodge(0.08))+
            scale_color_npg()+
            theme_classic()+
            labs(x = "Time of assessment",
                 y = "MADRS item 10 scores")+
            theme(axis.title.x = element_text(size = 14),
                  axis.text.x = element_text(size = 13),
                  axis.title.y = element_text(size = 15),
                  axis.text.y = element_text(size = 13),
                  legend.position = "none",
                  text = element_text(family = "serif"))

## Creates patchwork object
graficos_i10 <- madrs_plot + item10_plot &
            theme(text = element_text(family = "serif"),
                  title = element_text(size = 16),
                  legend.title = element_text(size = 14))

## Edits and annotates patchwork object
graficos_i10 <- graficos_i10 + plot_annotation(
            title = 'Total and item 10 MADRS scores at baseline and 24h post-treatment',
            subtitle = 'Scores are given for each patient')
graficos_i10


## Saves plot in high resolution
ggsave("MADRS_plot.png", graficos_i10, width=25, height=15, units="cm", dpi=450)



sessionInfo()
