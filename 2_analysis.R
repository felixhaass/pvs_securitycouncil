# load libraries

library(dplyr)
library(foreign)
library(ggplot2)


# This section generates a new data frame with all the numeric indices for 
# word frequency following the formulas described in the Appendix of the paper.


nettoanstieg <- dfm_sc_df %>% 
  dplyr::select(year, 
                reslength, 
                MYRESANALYSIS) %>%  
  mutate(tf.res = log(MYRESANALYSIS + 1) / log(reslength)) %>%  # implements "dh" formula
  group_by(year) %>% 
  
  # this implements the other formulas in section 4
  
  # summarise for responsibility
  summarise(mean_tf.res = mean(tf.res),
            mean_tf_unweighted.res = mean(MYRESANALYSIS), 
            doc_f.res = sum(MYRESANALYSIS > 0) / n(),
            term_relevance.res = mean_tf.res * doc_f.res
            ) 

# now we write the data as Stata File for later time series analysis
library(foreign)

write.dta(nettoanstieg, "./Data/nettoanstieg.dta")

# Further data preparation is necessary for the plots...
nettoanstieg <- nettoanstieg %>% 
  gather(key = kennzahl, value, -year) %>% 
  separate(kennzahl, into = c("kennzahl", "indicator"), sep = "\\.") %>% 
  mutate(kennzahl = plyr::mapvalues(kennzahl, 
                                    from = c("term_relevance", "doc_f", "mean_tf"),
                                    to = c("Begriffsrelevanz", 
                                           "Korpusspezifische Häufigkeit", 
                                           "Dokumentenspezifische Häufigkeit")),
         indicator = plyr::mapvalues(indicator, 
                                     from = c("res"),
                                     to = c("Responsibility")))

# Plot 1: Zahl & durchschnittl Länge von Sicherheitsratsresolution --------


# data munging
length_number_reso_df <- dfm_sc_df %>% 
  dplyr::select(year, reslength) %>% 
  group_by(year) %>% 
  summarise(number_of_resos = n(),
            mean_length = mean(reslength)) %>% 
  gather(key, value, -year)

# Plot
length_number_reso_plot <- ggplot(length_number_reso_df,
                                  aes(x = year, y = value)) +
  geom_line(size = 0.6) +
  theme_bw() +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(1950, 2015, 10)) + 
  facet_wrap(~key, scales = "free_y", 
             labeller = labeller(key = c("mean_length" = "Durschnittliche Länge in Wörtern",
                                         "number_of_resos" = "Anzahl an Resolutionen")))

# save plot - PNG
ggsave(plot = length_number_reso_plot, file = "./Figures/Abbildung1.png",
       dpi= 400, width = 9, height = 5, units = "in", type = "cairo-png",
       scale = 0.8)
# PDF
ggsave(plot = length_number_reso_plot, file = "./Figures/Abbildung1.pdf",
       width = 9, height = 5, units = "in",
       scale = 0.8)




# Plot 2: absolute Entwicklung von Verantwortung --------------------------


term_freq_absolut <- dfm_sc_df %>% 
  dplyr::select(year, MYRESANALYSIS) %>% 
  group_by(year) %>% 
  summarise(resp_sum = sum(MYRESANALYSIS),
            resp_mean = mean(MYRESANALYSIS)) %>% 
  gather(key, value, -year) %>% 
  ggplot(., aes(x = year, y = value, group = key)) +
  geom_line(size = 0.6) +
  facet_wrap(~ key, scales = "free_y", 
             labeller = labeller(key = c("resp_mean" = "Durchschnitt / Jahr",
                                         "resp_sum" = "Summe / Jahr"))) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950, 2015, 10)) + 
  labs(x = "", y = "")

ggsave(plot = term_freq_absolut, file = "./Figures/Abbildung2.png",
       dpi= 400, width = 9, height = 5, units = "in", type = "cairo-png",
       scale = 0.8)

ggsave(plot = term_freq_absolut, file = "./Figures/Abbildung2.pdf",
       width = 9, height = 5, units = "in",
       scale = 0.8)

# Plot: Kennzahlen

nettoanstieg$kennzahl_ordered = factor(nettoanstieg$kennzahl, 
                                       levels=c("Dokumentenspezifische Häufigkeit",
                                                "Korpusspezifische Häufigkeit", 
                                                "Begriffsrelevanz"))

term_freq_plot <- ggplot(nettoanstieg %>% filter(kennzahl != "mean_tf_unweighted" & indicator == "Responsibility"), 
                         aes(x = year, 
                         y = value
                         # , 
                         # color = indicator,
                         # linetype = indicator
                         )) + 
  # scale_linetype_manual(guide = FALSE, 
  #                       values = c("dashed", "solid")) +
  # scale_color_manual(guide = guide_legend(title = ""), 
  #                    values = c("#0571b0", "#ca0020")) +
  geom_line(size = 0.6) +
  facet_wrap( ~ kennzahl_ordered, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(1950, 2015, 10))+ 
  theme_bw() +
  labs( x = "", y = "") +
  theme(legend.position = "bottom")

ggsave(plot = term_freq_plot, file = "./Figures/Abbildung3.png",
       dpi= 400, width = 9, height = 5, units = "in", type = "cairo-png")

ggsave(plot = term_freq_plot, file = "./Figures/Abbildung3.pdf",
       width = 9, height = 5, units = "in")


# Replicate Figure 4 (TS analysis) in R -----------------------------------

figure_four <- ggplot(nettoanstieg %>% 
                        filter(kennzahl == "Begriffsrelevanz", 
                               indicator == "Responsibility"),
                      aes(x = year, y = value * 100)) +
  geom_point(aes(shape = kennzahl)) +
  scale_shape_manual(name = "", values = 16, label = "Tatsächliche Werte") + 
  # add line regression line between 1946 and 1991
  geom_smooth(data = nettoanstieg %>% 
                filter(kennzahl == "Begriffsrelevanz", 
                       indicator == "Responsibility" & year < 1992),
              method = "lm", se =F, aes(color = indicator)) +
  # add line between 1992 and 2004
  geom_smooth(data = nettoanstieg %>% 
                filter(kennzahl == "Begriffsrelevanz", 
                       indicator == "Responsibility" & year >= 1992 & year < 2005) ,
              method = "lm", se =F, aes(color = indicator)) +
  # add line between 2005 and 2015
  geom_smooth(data = nettoanstieg %>% 
                filter(kennzahl == "Begriffsrelevanz", 
                       indicator == "Responsibility" & year >= 2005),
              method = "lm", se =F, aes(color = indicator)) +
  scale_color_manual(name  = "", values = "black", labels = "Geschätzte Werte") + 
  scale_x_continuous(breaks = seq(1950, 2015, 10)) + 
  geom_vline(aes(xintercept = 1992), linetype = 2) +
  geom_vline(aes(xintercept = 2005), linetype = 2) +
  theme_bw() + 
  labs(x = " ", y = "Begriffsrelevanz von Verantwortung") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal")


ggsave(plot = figure_four, file = "./Figures/Abbildung4.png",
       dpi= 400, width = 9, height = 5, units = "in", 
       type = "cairo-png", scale = 0.8)

ggsave(plot = figure_four, file = "./Figures/Abbildung4.pdf",
       width = 9, height = 5, units = "in", scale = 0.8)
