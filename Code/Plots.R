##############################  study 2 ####################################################
## prepare data
dflong1<-read.csv('./Output/Data/Final_grams_phase1.csv', check.names = F) # phase 1
dflong1gen<-read.csv('./Output/Data/Final_grams_phase1_standard.csv', check.names = F) # phase 1 Standard
dflong2<-read.csv('./Output/Data/Final_grams_phase2.csv', check.names = F) # phase 1
dflong2gen<-read.csv('./Output/Data/Final_grams_phase2_standard.csv', check.names = F) # phase 1 Standard

## best run 
bestrun1<-rbind(dflong1    %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification),
                dflong1gen %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification)) %>% 
  ## keep the best-performing run per prompt, as recorded in PromptMax by the
  ## prepare_data scripts, then drop the run number from the label
  filter(as.character(Prompt) == as.character(PromptMax)) %>%
  mutate(Prompt = trimws(str_remove(as.character(Prompt), '[0-9]+$'))) %>%
  mutate(Prompt=fct_relevel(factor(Prompt),c('Spoons','Range','Point','Spoons Standard','Range Standard','Point Standard')))  %>% 
  mutate(Classification=factor(Classification,levels=
                                 c('< more than 250 grams','< 100 until 250 grams','< 25 until 100 grams',
                                   '+/- 25 grams','> 25 until 100 grams','> 100 until 250 grams',
                                   '> more than 250 grams','Missing')))

bestrun2<-rbind(dflong2    %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification),
                dflong2gen %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification)) %>% 
  ## keep the best-performing run per prompt, as recorded in PromptMax
  filter(as.character(Prompt) == as.character(PromptMax)) %>%
  mutate(Prompt = trimws(str_remove(as.character(Prompt), '[0-9]+$'))) %>%
  mutate(Prompt=fct_relevel(factor(Prompt),c('Recipe Name','Ingredient List','Ingredient Quantities',
                                             'Recipe Name Standard',
                                             'Ingredient List Standard',
                                             'Ingredient Quantities Standard'))) %>% 
  mutate(Classification=factor(Classification,levels=
                                 c('< more than 250 grams','< 100 until 250 grams','< 25 until 100 grams',
                                   '+/- 25 grams','> 25 until 100 grams','> 100 until 250 grams',
                                   '> more than 250 grams','Missing')))

## these two tables are in the webappendix as one table
bestrun1 %>%
  mutate(Prompt=as.character(Prompt)) %>%
  mutate(Prompt=fct_relevel(factor(Prompt),c('Spoons','Range','Point',
                                             'Spoons Standard',  'Range Standard',  'Point Standard' ))) %>%
  tabyl(Classification,Prompt) %>%
  adorn_totals(c("row"), fill = "-", na.rm = T, name = "Total Classified") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  gt(.) %>% 
  gtsave(str_c('./Output/Tables/EXPERT_phase1_bestrun.tex'))


p9 <- rbind(
  bestrun1 %>% select(PhotoName, Prompt, PromptMax, `Grams Reported`, `Grams Predicted`, Classification),
  bestrun2 %>% select(PhotoName, Prompt, PromptMax, `Grams Reported`, `Grams Predicted`, Classification)
) %>%
  filter(Prompt %in% c("Spoons", "Range", "Point",
                       "Recipe Name", "Ingredient List", "Ingredient Quantities")) %>%
  mutate(
    Prompt = factor(Prompt, levels = c("Spoons", "Range", "Point",
                                       "Recipe Name", "Ingredient List", "Ingredient Quantities")),
    Classification = factor(
      Classification,
      levels = c("< more than 250 grams", "< 100 until 250 grams", "< 25 until 100 grams",
                 "+/- 25 grams",
                 "> 25 until 100 grams", "> 100 until 250 grams", "> more than 250 grams",
                 "Missing")
    )
  ) %>% 
  filter(!is.na(Classification), Classification != "Missing") %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  geom_point(size = 0.8) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 200, label.y = 1400,
           size = 10 / .pt, family = "Times New Roman") +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Paired")[c(6, 5, 3, 4, 3, 1, 2)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "darkgrey") +
  facet_wrap(~Prompt) +
  guides(colour = guide_legend(ncol = 4)) +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.size  = unit(4, "mm"),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(size = 10),          # facet header siz
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/EXPERT_phase1_phase2_R1_hq.png", p9,
       device = ragg::agg_png, width = 190, height = 140, units = "mm",
       dpi = 600, bg = "white")

## for appendix

p10 <- rbind(
  bestrun1 %>% select(PhotoName, Prompt, PromptMax, `Grams Reported`, `Grams Predicted`, Classification),
  bestrun2 %>% select(PhotoName, Prompt, PromptMax, `Grams Reported`, `Grams Predicted`, Classification)
) %>%
  filter(Prompt %in% c("Spoons", "Range", "Point",
                       "Spoons Standard", "Range Standard", "Point Standard")) %>%
  mutate(
    Prompt = factor(Prompt, levels = c("Spoons", "Range", "Point",              # see note
                                       "Spoons Standard", "Range Standard", "Point Standard")),
    Classification = factor(
      Classification,
      levels = c("< more than 250 grams", "< 100 until 250 grams", "< 25 until 100 grams",
                 "+/- 25 grams",
                 "> 25 until 100 grams", "> 100 until 250 grams", "> more than 250 grams",
                 "Missing")
    )
  ) %>%
  filter(!is.na(Classification), Classification != "Missing") %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  geom_point(size = 0.8) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 200, label.y = 600,
           size = 10 / .pt, family = "Times New Roman") +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Paired")[c(6, 5, 3, 4, 3, 1, 2)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "darkgrey") +
  facet_wrap(~Prompt, ncol = 3) +
  guides(colour = guide_legend(ncol = 4)) +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.size  = unit(4, "mm"),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(size = 10),          # facet header siz
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )


ggsave("./Output/Figures/EXPERT_phase1_R1_hq.png", p10,
       device = ragg::agg_png, width = 190, height = 140, units = "mm",
       dpi = 600, bg = "white")


p11 <- rbind(
  bestrun1 %>% select(PhotoName, Prompt, PromptMax, `Grams Reported`, `Grams Predicted`, Classification),
  bestrun2 %>% select(PhotoName, Prompt, PromptMax, `Grams Reported`, `Grams Predicted`, Classification)
) %>%
  filter(Prompt %in% c("Recipe Name", "Ingredient List", "Ingredient Quantities",
                       "Recipe Name Standard", "Ingredient List Standard",
                       "Ingredient Quantities Standard")) %>%
  mutate(
    Prompt = factor(Prompt, levels = c("Recipe Name", "Ingredient List", "Ingredient Quantities",
                                       "Recipe Name Standard", "Ingredient List Standard",
                                       "Ingredient Quantities Standard")),
    Classification = factor(
      Classification,
      levels = c("< more than 250 grams", "< 100 until 250 grams", "< 25 until 100 grams",
                 "+/- 25 grams",
                 "> 25 until 100 grams", "> 100 until 250 grams", "> more than 250 grams",
                 "Missing")
    )
  ) %>%
  filter(!is.na(Classification), Classification != "Missing") %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  geom_point(size = 0.8) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 200, label.y = 1400,
           size = 10 / .pt, family = "Times New Roman") +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Paired")[c(6, 5, 3, 4, 3, 1, 2)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "darkgrey") +
  facet_wrap(~Prompt, ncol = 3, labeller = label_wrap_gen(width = 22)) +
  guides(colour = guide_legend(ncol = 4)) +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.size  = unit(4, "mm"),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(size = 10),          # facet header siz
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )


ggsave("./Output/Figures/EXPERT_phase2_R1_hq.png", p11,
       device = ragg::agg_png, width = 190, height = 140, units = "mm",
       dpi = 600, bg = "white")

#################################### grams error predicted ########################################

df2 <- rbind(
  bestrun1 %>% select(PhotoName, Prompt,`Grams Reported`,`Grams Predicted`),
  bestrun2 %>% select(PhotoName, Prompt,`Grams Reported`,`Grams Predicted`))%>%
  filter(!grepl('Standard', Prompt)) %>% 
  dplyr::rename(actual=`Grams Reported`,
                aigen =`Grams Predicted`,
                prompt = Prompt) %>%
  mutate(err = abs(actual - aigen),
         diff = aigen - actual) 

df2 %>% 
  filter(prompt=='Range', !is.na(aigen), !is.na(actual)) %>% # 71
  # filter(prompt=='Point', !is.na(aigen), !is.na(actual)) %>% # 70
  # filter(prompt=='Spoons', !is.na(aigen), !is.na(actual)) %>% # 163 > over predicted
  # filter(prompt=='Ingredient List', !is.na(aigen), !is.na(actual)) %>% # 74
  # filter(prompt=='Ingredient Quantities', !is.na(aigen), !is.na(actual)) %>% # 74
  reframe(perc=sum(aigen)/sum(actual)*100)


# inspect which prompt is most likely to underpredict:
df2 %>% filter(diff > 300) %>% 
  select(prompt, actual, aigen, err, PhotoName) %>% 
  mutate(diff=actual-aigen) %>% 
  arrange(desc(diff)) %>% tabyl(prompt)

# inspect which prompt is most likely to overpredict:
df2 %>% filter(diff < -300) %>% 
  select(prompt, actual, aigen, err,PhotoName) %>% 
  mutate(diff=actual-aigen) %>% 
  arrange(desc(diff)) %>% tabyl(prompt)

# MAE per prompt
mae_by_group <- df2 %>%
  group_by(prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE = mean(err, na.rm = TRUE),
    .groups = "drop"
  )
mae_by_group

# scale
mae_scaled_by_group <- df2 %>%
  group_by( prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE_scaled = mean(err, na.rm = TRUE) / (max(actual) - min(actual)) * 100,
    .groups = "drop"
  ) 
mae_scaled_by_group


mae_by_group %>%
  left_join(mae_scaled_by_group %>% select(-n), by = c("prompt")) %>%
  mutate(`MAE (%)` = sprintf("%.2f (%.2f%%)", MAE, MAE_scaled)) %>% select(n, prompt,`MAE (%)`) %>% 
  # pivot_wider(id_cols = everything(), values_from = 'MAE (%)', names_from = 'prompt')  %>% 
  
  xtable(., type = 'latex') %>%
  # print(., include.rownames = F) %>% 
  gt(.)  %>% 
  gtsave(str_c('./Output/Tables/EXPERT_MAE_bothphases.tex'))


## table webappendix
bestrun2 %>%
  mutate(Prompt=as.character(Prompt)) %>%
  mutate(Prompt=fct_relevel(factor(Prompt),c('Recipe Name',
                                             'Ingredient List',
                                             'Ingredient Quantities',
                                             'Recipe Name Standard',
                                             'Ingredient List Standard',
                                             'Ingredient Quantities Standard'))) %>%
  tabyl(Classification,Prompt) %>%
  adorn_totals(c("row"), fill = "-", na.rm = T, name = "Total Classified") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  gt(.) %>% 
  gtsave(str_c('./Output/Tables/EXPERT_phase2_bestrun.tex'))

#################################### variability grams table study 2 ########################################
## add together across all runs to inspect variability
allruns<-rbind(dflong1 %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification),
               dflong1gen %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`, `Grams Predicted`,Classification),
               dflong2 %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`, `Grams Predicted`,Classification),
               dflong2gen %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`, `Grams Predicted`,Classification))## table with for both studies in webappendix

# overall correlation predicted & reported grams, with sd
allruns %>% 
  group_by(Prompt) %>% 
  dplyr::summarise(
    m=mean(`Grams Reported`),
    r=cor(`Grams Predicted`, `Grams Reported`, use='complete.obs'),
    N=n()) %>% 
  dplyr::rename(PromptN=Prompt) %>%
  mutate(Prompt=str_replace_all(PromptN, "[:digit:]", "")) %>%
  mutate(Prompt=trimws(Prompt)) %>%
  group_by(Prompt) %>% 
  dplyr::summarise(Mr=mean(r), SDr=sd(r))

## across all runs, variability

p5 <- allruns %>%
  group_by(Prompt) %>%
  dplyr::summarise(r = cor(`Grams Predicted`, `Grams Reported`, use = "complete.obs")) %>%
  dplyr::rename(PromptN = Prompt) %>%
  mutate(Prompt = trimws(str_replace_all(PromptN, "[:digit:]", ""))) %>%
  group_by(Prompt) %>%
  dplyr::summarise(mean = mean(r, na.rm = TRUE),
                   sd   = sd(r, na.rm = TRUE),
                   n    = sum(!is.na(r))) %>%
  mutate(Prompt = factor(Prompt, levels = c(
    "Ingredient Quantities Standard", "Ingredient List Standard",
    "Recipe Name Standard", "Point Standard", "Range Standard", "Spoons Standard",
    "Ingredient Quantities", "Ingredient List",
    "Recipe Name", "Point", "Range", "Spoons"))) %>%
  ggplot(aes(x = Prompt, y = mean)) +
  geom_col(fill = "darkgrey", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, colour = "red", linewidth = 0.3) +
  coord_flip() +
  labs(x = NULL, y = "Correlation predicted and reported grams") +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    axis.text        = element_text(size = 11),
    axis.title       = element_text(size = 11),
    axis.ticks       = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )


ggsave("./Output/Figures/EXPERT_grams_allruns_R1_hq.png", p5,
       device = ragg::agg_png, width = 190, height = 120, units = "mm",
       dpi = 600, bg = "white")



########################## precision plot  2 ###########################################

## study 2
prec1<-read.csv( './Output/Data/Final_precision_phase1.csv', check.names = F)
prec1gen<-read.csv( './Output/Data/Final_precision_phase1_standard.csv', check.names = F) # standard plate
prec2<-read.csv( './Output/Data/Final_precision_phase2.csv', check.names = F)
prec2gen<-read.csv( './Output/Data/Final_precision_phase2_standard.csv', check.names = F)

## the precision files carry no PromptMax column, so take the best-run labels
## from the Allruns files rather than retyping them
best_runs <- unique(c(
  read.csv('./Output/Data/Allruns_phase1.csv')$PromptMax,
  read.csv('./Output/Data/Allruns_phase1_standard.csv')$PromptMax,
  read.csv('./Output/Data/Allruns_phase2.csv')$PromptMax,
  read.csv('./Output/Data/Allruns_phase2_generic.csv')$PromptMax
))

precision_df2 <- rbind(prec1, prec2, prec1gen, prec2gen) %>%
  filter(Prompt %in% best_runs) %>%
  mutate(
    Prompt   = trimws(str_remove(Prompt, '[0-9]+$')),
    Standard = if_else(grepl('Standard', Prompt),
                       'Standard plate size (experts)',
                       'Actual plate size (experts)'),
    Prompt   = trimws(str_remove(Prompt, 'Standard')),
    Prompt   = factor(Prompt, levels = c('Ingredient Quantities', 'Ingredient List',
                                         'Recipe Name', 'Point', 'Range', 'Spoons'))
  ) %>%
  filter(!is.na(Precision)) %>%
  select(-Phase, -Generic)

# inspect average precisions across runs and samples
precision_df2 %>%
  group_by(Prompt, Standard) %>%
  dplyr::summarise(PrecM = mean(Precision), N = n(), .groups = 'drop') %>%
  filter(Standard == 'Actual plate size (experts)') %>%
  arrange(Prompt, PrecM)


p6 <- precision_df2 %>%
  select(Prompt, Precision, Standard) %>%
  ggplot(aes(Prompt, Precision, fill = Prompt)) +
  geom_violinhalf(trim = TRUE, scale = "width") +
  coord_flip() +
  scale_x_discrete(limits = c("Ingredient Quantities", "Ingredient List",
                              "Recipe Name", "Point", "Range", "Spoons")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(9, "BuGn")[3:9])) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~Standard, labeller = label_wrap_gen(width = 22)) +   # wraps long headers
  
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "none",
    strip.text       = element_text(size = 10, margin = margin(2, 2, 2, 2, "mm")),
    axis.text        = element_text(size = 10),
    axis.text.x      = element_text(hjust = 0.6),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.5, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )



ggsave("./Output/Figures/EXPERT_precision_bestrun_R1_hq.png", p6,
       device = ragg::agg_png, width = 190, height = 85, units = "mm",
       dpi = 600, bg = "white")

p7 <- rbind(prec1, prec2, prec1gen, prec2gen) %>%
  dplyr::rename(PromptN = Prompt) %>%
  mutate(Prompt = str_replace_all(PromptN, "[:digit:]", "")) %>%
  mutate(Prompt = trimws(Prompt)) %>%
  mutate(
    Prompt = factor(
      Prompt,
      levels = c(
        "Ingredient Quantities Standard", "Ingredient List Standard",
        "Recipe Name Standard", "Point Standard",
        "Range Standard", "Spoons Standard",
        "Ingredient Quantities", "Ingredient List",
        "Recipe Name", "Point", "Range", "Spoons"
      )
    )
  ) %>%
  ggplot(aes(y = Prompt, x = Precision)) +
  geom_boxplot(linewidth = 0.5, outlier.size = 0.8) +   # thinner strokes for print
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Precision", y = NULL) +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "none",
    axis.text        = element_text(size = 10),
    axis.title       = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/EXPERT_precision_allruns_R1_hq.png", p7,
       device = ragg::agg_png, width = 190, height = 120, units = "mm",
       dpi = 600, bg = "white")