rm(list=ls())
source('./Code/helper_functions.R')

##############################  study 1 ####################################################
## prepare data
df1 <- read.csv("./Output/hf/Final_df_grams_ingredients_converted.csv") # results HF
df2 <- read.csv("./Output/o-nhf/Final_df_grams_ingredients_converted.csv") # results O/NHF

# get number of observed ingredients
for (i in 1:6) {
  df1[[paste0("Ningr_", i)]] <- apply(
    df1 %>% select(starts_with(paste0("ingredient", i, "_"))),
    1,
    function(x) sum(!is.na(x))
  )
  
  df2[[paste0("Ningr_", i)]] <- apply(
    df2 %>% select(starts_with(paste0("ingredient", i, "_"))),
    1,
    function(x) sum(!is.na(x))
  )
}


############################# inspect ingredients ####################################################

## inspect
rbind(df1 %>% select(
  starts_with('ingredient1'),
  starts_with('ingredient2'),
  starts_with('ingredient3'),
  starts_with('ingredient4'),
  starts_with('ingredient5'),
  starts_with('ingredient6')
) %>%
  pivot_longer(everything(),names_to = 'ingredient_nr', values_to = 'ingredient'),
      df2 %>%select(
        starts_with('ingredient1'),
        starts_with('ingredient2'),
        starts_with('ingredient3'),
        starts_with('ingredient4'),
        starts_with('ingredient5'),
        starts_with('ingredient6')
      ) %>%
  pivot_longer(everything(),names_to = 'ingredient_nr', values_to = 'ingredient')) %>% 
  mutate(ingredient = str_trim(ingredient),
         ingredient = case_when( grepl('estimated grams|name|food|estimate|diced food|estimate grams|moussaka|food|bone|liquid|bowl|tray|handfuls|name|solid|napkin|napkins|bohn|mixture|ingredients|ingredient|residual|mince|grape stems|quantity|green flecks|slice|plate|glass|weight|comments|description|type|top|tablespoon|piece|type|unit|protein|packaging|estimate|chopsticks|crinkle cut fry|cup|cutlet|item|hot chocolate',
                                       tolower(ingredient))~NA, TRUE~ingredient)) %>% 
  filter(!is.na(ingredient), ingredient!='unidentified') %>% #nrow() # 10732
  # distinct(ingredient,.keep.all=T) %>% nrow() # 58
  mutate(group=case_when(ingredient %in% c('fats or oils','water or broth','spices','herbs','garlic','flour','onion','seasoning','peppers','sauce','vinaigrette','pepper') ~ 'Additives',
                         ingredient %in% c('pork','fish','poultry','beef, veal or lamb','(shell)fish','meat substitutes','charcuterie','ground (undifferentiated) meat')~ 'Protein',
                         ingredient %in% c('zucchini','eggplant','tomatoes','vegetable mix','celery','vegetables','root vegetables',
                                           'mushrooms','cucumbers','cauliflower','avocado','broccoli','asparagus', 'bell pepper','parsnip')~ 'Vegetables',
                         ingredient %in% c('cabbage','endive','leek','lettuce','spinach','slaw','radicchio','artichoke') ~ 'Leafy vegetables',
                         ingredient %in% c('pasta','bread(crumbs)','legumes','noodles','flatbread','rice','farfalle','macaroni','potatoes or fries','quinoa, couscous or bulgur',#'roti','udon','wrap',
                                           'squash','corn','baby corn','parsnip','cooked potatoes or grains','pizza','french fry') ~ 'Starches',#'french_fry','fried item','pizza','pizza base'
                         ingredient %in% c('apple','pear','pineapple','kiwi','banana')~ 'Fruits',
                         ingredient %in% c('eggs','cheese','dairy (cream)')~ 'Dairy', # 'käsereste','emmental','stracciatella'
                         ingredient %in% c('olives','nuts or raisins','lemon or lime','seeds or other toppings')~ 'Toppings', #'dumpling','falafel','filling','muffin','nachos','purple puree','wasabi'
                         TRUE~ingredient)) -> df.ingredients
df.ingredients %>% filter(group=='Protein') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Starches') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Vegetables') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Additives') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Leafy vegetables') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Dairy') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Toppings') %>% tabyl(ingredient) %>% arrange(desc(n))
df.ingredients %>% filter(group=='Fruits') %>% tabyl(ingredient) %>% arrange(desc(n))

## considers all ingredients (also nonkey)
# inspect falsely predicted
# df1 %>%
#   select(starts_with("FP")) %>%
#   pivot_longer(cols = starts_with("FP"), names_to = "Prompt", values_to = "FP") %>%
#   # filter(Prompt=='FP4'|Prompt=='FP5'|Prompt=='FP6') %>%
#   tabyl(FP,Prompt) %>%
#   adorn_totals(c("row"), fill = "-", na.rm = F, name = "Total Classified") %>%
#   adorn_percentages("col") %>%
#   adorn_pct_formatting(digits = 2) %>%
#   adorn_ns(position = "front") %>%
#   # adorn_ns() %>%
#   gt(.) %>%
#   gtsave(str_c('./Output/Tables/RESP_FP.tex'))
#   # gtsave(str_c('./Output/Tables/RESP_FP_phase2.tex'))

############################## hallucinated ingredients  ####################################################
nokeyingr <-c('apple','kiwi','pineapple','lemon or lime','nuts or raisins','olive','seeds or other toppings',
              'fats or oils','flour','garlic','herbs','onion','peppers','sauce','seasoning','spices','vinaigrette','water or broth',
              'diced food','estimate','estimate grams','estimated grams','fig pieces','food','name','pizza slice','pizza slice 1','pizza slice 2','pizza slices',
              'dairy')

df1 %>%
  # remove empty ingredient columns
  select(-ingredient2_8) %>% 
  # the parentheses cause problems for str_detect so we remove them
  mutate(
    across(
      where(is.character),
      ~ str_replace_all(.x, "bread\\(crumbs\\)", "bread")
    ),
    across(
      where(is.character),
      ~ str_replace_all(.x, "dairy \\(cream\\)", "dairy")
    ),
    across(
      where(is.character),
      ~ str_replace_all(.x, "\\(shell\\)fish", "fish")
    ),
    across(
      where(is.character),
      ~ str_replace_all(.x, "ground \\(undifferentiated\\) meat", "minced meat")
    )
  ) %>% 
  # create string out of recipe grouped ingredients
  rowwise() %>%
  mutate(
    ingredients = paste(unique(na.omit(c_across(matches("^ingredient_\\d+$")))),
                        collapse = ", ")
  ) %>% #select(ingredients,ingredientsQuantity)
  
  # # Spoons
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("ingredient1_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% #filter(!is.na(ingredient1_1)) %>% select(starts_with('hallu_ingredient1_'), ingredients)
  # 
  # 
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_ingredient1_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>%
  # ungroup()%>%
  # # filter(!is.na(ingredient1_1)) %>% select(ingredients,hallu_ingredients)
  # # filter(hallu_ingredients!='') %>% #nrow() # 207
  # # rowwise() %>%
  # # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_ningr_freq_1
  # filter(hallu_ingredients!='') %>%
  # pivot_longer(cols = starts_with('hallu_ingredient1_'), names_to = 'nr', values_to = 'ingr' ) %>%
  # filter(!is.na(ingr)) %>%
  # tabyl(ingr) %>% arrange(desc(n))-> hallu_freq_1

  # # Range
  # mutate(
  #   across(
  #     starts_with("ingredient2_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>%
  # # filter(!is.na(ingredient2_1)) %>% select(starts_with('hallu_ingredient2_'), ingredients)
  # 
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_ingredient2_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>%
  # ungroup()%>%
  # # filter(!is.na(ingredient1_1)) %>% select(ingredients,hallu_ingredients)
  # filter(hallu_ingredients!='') %>% #nrow() # 184
  # # rowwise() %>%
  # # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_ningr_freq_2
  # filter(hallu_ingredients!='') %>% 
  # pivot_longer(cols = starts_with('hallu_ingredient2_'), names_to = 'nr', values_to = 'ingr' ) %>%
  # filter(!is.na(ingr)) %>%
  # tabyl(ingr) %>% arrange(desc(n))-> hallu_freq_2

  # # Point
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("ingredient3_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>%
  # # filter(!is.na(ingredient3_1)) %>% select(starts_with('hallu_ingredient3_'), ingredients)
  # 
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_ingredient3_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>%
  # ungroup()%>%
  # filter(!is.na(ingredient1_1),hallu_ingredients!='') %>% select(hallu_ingredients,PhotoName,ingredients) %>% arrange(desc(nchar(hallu_ingredients))) %>% slice(80:120)
  # filter(hallu_ingredients!='') %>% #nrow() # 163
  # rowwise() %>%
  # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_ningr_freq_3
  
  # filter(hallu_ingredients!='') %>%
  # pivot_longer(cols = starts_with('hallu_ingredient3_'), names_to = 'nr', values_to = 'ingr' ) %>%
  # filter(!is.na(ingr)) %>%
  # tabyl(ingr) %>% arrange(desc(n))-> hallu_freq_3

  # # Recipe Name
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("ingredient4_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #     )
  #   ) %>%
  #   rowwise() %>%
  #   mutate(
  #     hallu_ingredients = paste(
  #       unique(na.omit(c_across(matches("^hallu_ingredient4_\\d+$")))),
  #       collapse = ", "
  #     )
  #   ) %>%
  #   ungroup()%>%
  #   # filter(!is.na(ingredient1_1)) %>% select(ingredients,hallu_ingredients)
  #   # filter(hallu_ingredients!='') %>% #nrow() # 142
  #   # rowwise() %>%
  #   # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_ningr_freq_4
  # 
  #   filter(hallu_ingredients!='') %>%
  #   pivot_longer(cols = starts_with('hallu_ingredient4_'), names_to = 'nr', values_to = 'ingr' ) %>%
  #   filter(!is.na(ingr)) %>%
  #   tabyl(ingr) %>% arrange(desc(n))-> hallu_freq_4

  # # Ingredient List
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("ingredient5_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>%
  # # filter(!is.na(ingredient5_1)) %>% select(starts_with('hallu_ingredient5_'), ingredients)
  # 
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_ingredient5_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>%
  # ungroup()%>%
  # # filter(!is.na(ingredient1_1)) %>% select(ingredients,hallu_ingredients)
  # filter(hallu_ingredients!='') %>%# nrow() # 47
  # rowwise() %>%
  # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_ningr_freq_5
  # 
  # filter(hallu_ingredients!='') %>%
  # pivot_longer(cols = starts_with('hallu_ingredient5_'), names_to = 'nr', values_to = 'ingr' ) %>%
  # filter(!is.na(ingr)) %>%
  # tabyl(ingr) %>% arrange(desc(n))-> hallu_freq_5

  # # Ingredient Quantities
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("ingredient6_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>%
  # 
  #   rowwise() %>%
  #   mutate(
  #     hallu_ingredients = paste(
  #       unique(na.omit(c_across(matches("^hallu_ingredient6_\\d+$")))),
  #       collapse = ", "
  #     )
  #   ) %>%
  #   ungroup()%>%
  #   # filter(!is.na(ingredient1_1)) %>% select(ingredients,hallu_ingredients)
  #   # filter(hallu_ingredients!='') %>% #nrow() # 53
  #   # rowwise() %>%
  #   # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_ningr_freq_6
  # 
  #   filter(hallu_ingredients!='') %>%
  #   pivot_longer(cols = starts_with('hallu_ingredient6_'), names_to = 'nr', values_to = 'ingr' ) %>%
  #   filter(!is.na(ingr)) %>%
  #   tabyl(ingr) %>% arrange(desc(n))-> hallu_freq_6

reduce(list(hallu_freq_3 %>% rename(n3=n, perc3=percent),
            hallu_freq_5 %>% rename(n5=n, perc5=percent)),
       full_join, by = "ingr") %>% 
  mutate(perc3= perc3*100,perc5= perc5*100) %>% 
  arrange(desc(n3)) %>% 
  slice(1:8) %>% 
  mutate(
    `Point` = sprintf("%d (%.0f%%)", n3, perc3),
    `Ingredient List` = sprintf("%d (%.0f%%)", n5, perc5)
  ) %>%
  select(ingr, `Point`, `Ingredient List`)%>%
  xtable(., type = 'latex') %>%
  print(., include.rownames = F)


########################## precision ingredients HF resp ###########################################

## study 1

### exclude nhf respondents as they have no known ingredients
# rbind(
#   df1 %>% select(PhotoName,
#     starts_with('Prec_'), starts_with('Ningr_')),
#   df2 %>% select(PhotoName,
#     starts_with('Prec_'), starts_with('Ningr_'))) %>% # 816

df1 %>% select(PhotoName,
                 starts_with('Prec_'), starts_with('Ningr_')) %>% 
  pivot_longer(
    cols = matches("^(Prec|Ningr)_"),
    names_to = c(".value", "Prompt"),
    names_sep = "_"
  ) %>% 
# pivot_longer(cols = starts_with("Prec_"), names_to = "Prompt", values_to = "Precision") %>%
  mutate(Prompt = case_when(Prompt == '1'~'Spoons',
                            Prompt == '2'~'Range',
                            Prompt == '3'~'Point',
                            Prompt == '4'~'Recipe Name',
                            Prompt == '5'~'Ingredient List',
                            Prompt == '6'~'Ingredient Quantities', TRUE~NA)) %>%
  mutate(Prompt=factor(Prompt, 
                       levels=c("Spoons", "Range","Point","Recipe Name", "Ingredient List", "Ingredient Quantities")))%>%
  mutate(Standard='Standard plate size (respondents)') %>% #nrow() # 816 images * 6 prompts = 4896
  rename(Precision=Prec) %>% 
  # filter(Precision==0) %>% tabyl(Ningr)
  filter(!is.na(Precision)) %>%  # 2951
  select(-Ningr)-> precision_df1


########################## precision plot study 1 and 2 ###########################################

## study 2
prec1<-read.csv( './Output/simulated/Final_precision_phase1.csv', check.names = F)
prec1gen<-read.csv( './Output/simulated/Final_precision_phase1_standard.csv', check.names = F) # standard plate
prec2<-read.csv( './Output/simulated/Final_precision_phase2.csv', check.names = F)
prec2gen<-read.csv( './Output/simulated/Final_precision_phase2_standard.csv', check.names = F)

rbind(prec1 , prec2, prec1gen, prec2gen) %>%# tabyl(Prompt)
  filter(Prompt %in% c('Spoons 3','Range 2','Point 4','Spoons Standard 3','Range Standard',
                       'Point Standard 5', 'Recipe Name 4','Recipe Name Standard 2',
                       'Ingredient List 2','Ingredient List Standard 4',
                       'Ingredient Quantities 3','Ingredient Quantities Standard 3')) %>% 
  # filter(Prompt=='Ingredient List 2') %>% select(Precision) # all 1 or NA 
  mutate(Prompt=case_when(Prompt=='Spoons 3' ~'Spoons',
                          Prompt=='Range 2' ~ 'Range',
                          Prompt=='Point 4'~'Point', 
                          Prompt=='Spoons Standard 3' ~'Spoons Standard',
                          Prompt=='Range Standard' ~ 'Range Standard',
                          Prompt=='Point Standard 5'~'Point Standard',
                          Prompt=='Recipe Name 4' ~'Recipe Name',
                          Prompt=='Ingredient List 2' ~ 'Ingredient List',
                          Prompt=='Ingredient Quantities 3'~'Ingredient Quantities', 
                          Prompt=='Recipe Name Standard 2' ~'Recipe Name Standard',
                          Prompt=='Ingredient List Standard 4' ~ 'Ingredient List Standard',
                          Prompt=='Ingredient Quantities Standard 3'~'Ingredient Quantities Standard', TRUE~NA),

         Standard=case_when(grepl('Standard', Prompt)~'Standard plate size (experts)',
                            TRUE~'Actual plate size (experts)')) %>% # tabyl(Prompt)
  mutate(
    Prompt=case_when(Prompt=='Spoons Standard'~'Spoons',
                     Prompt=='Range Standard'~'Range',
                     Prompt=='Point Standard'~'Point',
                     Prompt=='Recipe Name Standard'~'Recipe Name',
                     Prompt=='Ingredient List Standard'~'Ingredient List',
                     Prompt=='Ingredient Quantities Standard'~'Ingredient Quantities',TRUE~Prompt
    ),
    Prompt=factor(Prompt, levels=c( 'Ingredient Quantities','Ingredient List','Recipe Name',
                                    'Point','Range','Spoons')),
  ) %>% #nrow() # 93 images * 6 prompts * 2 standard/normal = 1116
  filter(!is.na(Precision)) %>% select(-Phase, -Generic) -> precision_df2

# inspect average precisions across runs and samples
rbind(precision_df1, precision_df2)  %>%
  group_by(Prompt, Standard) %>% 
  summarise(PrecM= mean(Precision),
            N=n()) %>% arrange(Prompt, Standard,PrecM) %>% 
  filter(Standard=='Actual plate size (experts)')


p6 <- rbind(precision_df1, precision_df2) %>%
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

ggsave("./Output/Figures/EXPERT_precision_bestrun_R1_hq.pdf", p6,
       device = cairo_pdf, width = 190, height = 85, units = "mm")

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

ggsave("./Output/Figures/EXPERT_precision_allruns_R1.pdf", p7,
       device = cairo_pdf, width = 190, height = 120, units = "mm")

ggsave("./Output/Figures/EXPERT_precision_allruns_R1_hq.png", p7,
       device = ragg::agg_png, width = 190, height = 120, units = "mm",
       dpi = 600, bg = "white")
####################### grams error predicted #######################
## prepare data
df <- rbind(df1 %>% select(respondentID,PhotoName,starts_with('TotalGrams')) %>% 
              dplyr::rename(Spoons=TotalGrams1,
                            Range=TotalGrams2,
                            `Point`=TotalGrams3, 
                            `Recipe Name`=TotalGrams4,
                            `Ingredient List`=TotalGrams5,
                            `Ingredient Quantities`=TotalGrams6,
                            `Grams Reported`=TotalGramsSelf) %>% 
              mutate(Sample='HF'),
            df2 %>% select(respondentID,PhotoName,starts_with('TotalGrams'),nhf_plate_leftovers_1) %>%
              filter(!is.na(nhf_plate_leftovers_1))  %>% 
              select(-nhf_plate_leftovers_1) %>% 
              dplyr::rename(Spoons=TotalGrams1,
                            Range=TotalGrams2,
                            `Point`=TotalGrams3, 
                            `Recipe Name`=TotalGrams4,
                            `Ingredient List`=TotalGrams5,
                            `Ingredient Quantities`=TotalGrams6,
                            `Grams Reported`=TotalGramsSelf)%>% 
              mutate(Sample='NHF'),
            df2 %>% select(respondentID,PhotoName,starts_with('TotalGrams'),o_plate_leftovers_1) %>%
              filter(!is.na(o_plate_leftovers_1)) %>%
              select(-o_plate_leftovers_1) %>% 
              dplyr::rename(Spoons=TotalGrams1,
                            Range=TotalGrams2,
                            `Point`=TotalGrams3, 
                            `Recipe Name`=TotalGrams4,
                            `Ingredient List`=TotalGrams5,
                            `Ingredient Quantities`=TotalGrams6,
                            `Grams Reported`=TotalGramsSelf)%>% 
              mutate(Sample='Other'))

# added variables from excel
dfori <- read.csv('./Output/df_tobeused.csv') # with added variables
df<-df %>% merge(.,dfori %>% select(PhotoName, country, 
                                    hf_cooking_time,nhf_cooking_time,someone_else_prep_dinner,dinner_kind), by='PhotoName') %>% 
  mutate(Sample2=case_when(dinner_kind %in% c('A meal from a delivery service restaurant or takeout',
                                              'A semiprepared readytocook fully prepared or frozen meal',
                                              'I did not eat dinner at home tonight')~'Convenience',
                           dinner_kind=='A HelloFresh meal'~'Mealbox',
                           dinner_kind %in% c('A nonHelloFresh meal cooked from scratch with fresh ingredients',
                                              'Leftovers from another meal')~'Traditional', TRUE~ NA),
         Sample2=factor(Sample2,levels=c('Mealbox','Traditional','Convenience')),
         country2=case_when(country=="BelgiumFR"~"Belgium",
                            country=="BelgiumNL"~"Belgium",
                            country=="CanadaEN"~"Canada",
                            country=="CanadaFR"~"Canada",
                            country=="US"~"USA",
                            TRUE ~ country),
         country2=fct_relevel(factor(country2),
                              c('Netherlands','Belgium','Germany','Canada','USA','UK')),
         diff =  Point - `Grams Reported`) # neg numbers indicate underprediction 
# write.csv(df,'./Output/df_tobeused_final.csv')
# rm(df1, df2, dfori)

## plot variability across countries and samples
p8 <- ggplot(df, aes(country2, diff)) +
  geom_boxplot(linewidth = 0.3, outlier.size = 0.6) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  facet_grid(rows = vars(Sample2), labeller = label_wrap_gen(width = 18)) +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.3, "lines"),
    strip.text       = element_text(margin = margin(2, 2, 2, 2, "mm")),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/RESP_country_sample2_grams.pdf", p8,
       device = cairo_pdf, width = 190, height = 130, units = "mm")

ggsave("./Output/Figures/RESP_country_sample2_grams_hq.png", p8,
       device = ragg::agg_png, width = 190, height = 130, units = "mm",
       dpi = 600, bg = "white")

summary(aov1<-aov(diff~Sample2 + country2, df))
rbind(as.data.frame(TukeyHSD(aov1)['country2']) %>%   rename_with(~ gsub("country2.", "", .x, fixed = TRUE)),
      as.data.frame(TukeyHSD(aov1)['Sample2']) %>%   rename_with(~ gsub("Sample2.", "", .x, fixed = TRUE))) %>%
  mutate_at(vars(diff, lwr, upr), ~round(., 1)) %>%
  mutate_at(vars(p.adj), ~round(., 3))%>%
  xtable(., type = 'latex') %>%
  print(., include.rownames = T)


## MAE
df2 <- df %>%
  pivot_longer(cols = c(Spoons, Range, `Point`,`Recipe Name`, `Ingredient List`, `Ingredient Quantities`), 
               values_to = "aigen", names_to = 'prompt') %>% 
  dplyr::rename(actual=`Grams Reported`) %>% 
  mutate(err = abs(actual - aigen),
         diff = aigen - actual) 

# MAE per prompt
mae_by_group <- df2 %>%
  group_by(country2, prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE = mean(err, na.rm = TRUE),
    .groups = "drop"
  )
mae_by_group

# scale
mae_scaled_by_group <- df2 %>%
  group_by(country2, prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE_scaled = mean(err, na.rm = TRUE) / (max(actual) - min(actual)) * 100,
    .groups = "drop"
  ) 
mae_scaled_by_group


tab<-mae_by_group %>%
  left_join(mae_scaled_by_group %>% select(-n), by = c("country2","prompt")) %>%
  mutate(`MAE (%)` = sprintf("%.2f (%.2f%%)", MAE, MAE_scaled)) %>% 
  select(country2, n, prompt,`MAE (%)`) %>% 
  pivot_wider(id_cols = everything(), values_from = 'MAE (%)', names_from = 'prompt') 

AvMAE_Scaled_means<- mae_by_group %>%
  left_join(mae_scaled_by_group %>% select(-n), by = c("country2","prompt")) %>% 
  group_by(country2, prompt) %>% 
  summarise(AvMAE_Scaled=mean(MAE_scaled)) %>% 
  pivot_wider(id_cols = everything(), values_from = 'AvMAE_Scaled', names_from = 'prompt') %>%
  mutate(across(where(is.double), as.numeric)) %>% ungroup()
AvMAE_means<- mae_by_group %>%
  group_by(country2, prompt) %>% 
  summarise(AvMAE=mean(MAE)) %>% 
  pivot_wider(id_cols = everything(), values_from = 'AvMAE', names_from = 'prompt') %>%
  mutate(across(where(is.double), as.numeric)) %>% ungroup()

df2 %>%
  group_by(prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE = mean(err, na.rm = TRUE),
    .groups = "drop"
  ) %>% pivot_wider(values_from = 'MAE',
                    names_from = 'prompt')
# avg handmatig toegevoegd
tab %>% 
  mutate(country2=case_when(is.na(country2)~'Average MAE (%)', TRUE~country2),
  ) %>%
  select(country2, n, Spoons, Range, Point) %>% 
  xtable(., type = 'latex') %>%
  print(., include.rownames = F) %>%
  gt(.)  %>% 
  gtsave(str_c('./Output/Tables/RESP_MAE_phase1.tex'))

tab %>% 
  bind_rows(
    ., avg_row %>% pivot_wider(names_from = prompt, values_from = `Average MAE (%)`)
  )%>% 
  mutate(country2=case_when(is.na(country2)~'Average MAE (%)', TRUE~country2),
  ) %>% 
  select(country2, n, `Recipe Name`, `Ingredient List`, `Ingredient Quantities`) %>% 
  xtable(., type = 'latex') %>%
  # print(., include.rownames = F)
  gt(.)  %>% 
  gtsave(str_c('./Output/Tables/RESP_MAE_phase2.tex'))

# inspect which prompt is most likely to overpredict:
df2 %>% filter(diff > 300) %>% 
  select(Sample2, prompt, country2, actual, aigen, err, PhotoName) %>% 
  mutate(diff=actual-aigen) %>% 
  arrange(desc(diff)) %>% tabyl(prompt)

# inspect which prompt is most likely to underpredict:
df2 %>% filter(diff < -300) %>% 
  select(Sample2, prompt, country2, actual, aigen, err,PhotoName) %>% 
  mutate(diff=actual-aigen) %>% 
  arrange(desc(diff)) %>% tabyl(prompt)

### now for sample
# MAE per prompt
mae_by_group <- df2 %>%
  group_by(Sample2, prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE = mean(err, na.rm = TRUE),
    .groups = "drop"
  )
mae_by_group

# scale
mae_scaled_by_group <- df2 %>%
  group_by(Sample2, prompt) %>%
  dplyr::summarise(
    n = n(),
    MAE_scaled = mean(err, na.rm = TRUE) / (max(actual) - min(actual)) * 100,
    .groups = "drop"
  ) 
mae_scaled_by_group


tab<-mae_by_group %>%
  left_join(mae_scaled_by_group %>% select(-n), by = c("Sample2","prompt")) %>%
  mutate(`MAE (%)` = sprintf("%.2f (%.2f%%)", MAE, MAE_scaled)) %>% select(Sample2, n, prompt,`MAE (%)`) %>% 
  pivot_wider(id_cols = everything(), values_from = 'MAE (%)', names_from = 'prompt') 

AvMAE_Scaled_means<- mae_by_group %>%
  left_join(mae_scaled_by_group %>% select(-n), by = c("Sample2","prompt")) %>% 
  group_by(Sample2, prompt) %>% 
  summarise(AvMAE_Scaled=mean(MAE_scaled)) %>% 
  pivot_wider(id_cols = everything(), values_from = 'AvMAE_Scaled', names_from = 'prompt') %>%
  mutate(across(where(is.double), as.numeric)) %>% ungroup()
AvMAE_means<- mae_by_group %>%
  group_by(Sample2, prompt) %>% 
  summarise(AvMAE=mean(MAE)) %>% 
  pivot_wider(id_cols = everything(), values_from = 'AvMAE', names_from = 'prompt') %>%
  mutate(across(where(is.double), as.numeric)) %>% ungroup()


avg_row<-tibble(
  prompt=colnames(AvMAE_means)[-1],
  `Average MAE (%)` = sprintf(
    "%.2f (%.2f)",
    colMeans(AvMAE_means %>% 
               select(-Sample2)),
    colMeans(AvMAE_Scaled_means %>%
               mutate(across(where(is.double), as.numeric)) %>% 
               select(-Sample2))
  )
)
tab %>% 
  bind_rows(
    ., avg_row %>% pivot_wider(names_from = prompt, values_from = `Average MAE (%)`)
  )%>% 
  mutate(Sample2=case_when(is.na(Sample2)~'Average MAE (%)', TRUE~Sample2),
  ) %>% 
  select(Sample2, n, Spoons, Range, Point) %>% 
  xtable(., type = 'latex') %>%
  # print(., include.rownames = F) %>% 
  gt(.)  %>% 
  gtsave(str_c('./Output/Tables/RESP_MAE_sample2_phase1.tex'))

tab %>% 
  bind_rows(
    ., avg_row %>% pivot_wider(names_from = prompt, values_from = `Average MAE (%)`)
  )%>% 
  mutate(Sample2=case_when(is.na(Sample2)~'Average MAE (%)', TRUE~Sample2),
  ) %>% 
  select(Sample2, n, `Recipe Name`, `Ingredient List`, `Ingredient Quantities`) %>% 
  xtable(., type = 'latex') %>%
  # print(., include.rownames = F)
  gt(.)  %>% 
  gtsave(str_c('./Output/Tables/RESP_MAE_sample2_phase2.tex'))

####################### grams plot prepare data  #######################

dflong<-
  df %>% 
  # pivot_longer(cols = c(`Recipe Name`, `Ingredient List`, `Ingredient Quantities`), values_to = "Grams Predicted", names_to = 'Prompt') %>% 
  pivot_longer(cols = c(Spoons, Range, `Point`,`Recipe Name`, `Ingredient List`, `Ingredient Quantities`), values_to = "Grams Predicted", names_to = 'Prompt') %>% 
  mutate(diff=`Grams Reported`-`Grams Predicted`,
         Classification=case_when(abs(diff)> 250 & diff<0 ~ '> more than 250 grams',
                                  abs(diff)> 250 & diff>0  ~ '< more than 250 grams',
                                  abs(diff)> 100 & abs(diff)<=250 & diff < 0~ '> 100 until 250 grams',
                                  abs(diff)> 100 & abs(diff)<=250 & diff > 0 ~ '< 100 until 250 grams',
                                  abs(diff)> 25 & abs(diff)<=100 & diff < 0~ '> 25 until 100 grams',
                                  abs(diff)> 25 & abs(diff)<=100 & diff > 0 ~ '< 25 until 100 grams',
                                  abs(diff)<= 25 ~ '+/- 25 grams',
                                  is.na(diff) ~ 'Missing',
                                  TRUE~NA)) %>% 
  mutate(Classification=factor(Classification,levels=
                                 c('< more than 250 grams','< 100 until 250 grams','< 25 until 100 grams',
                                   '+/- 25 grams','> 25 until 100 grams','> 100 until 250 grams',
                                   '> more than 250 grams','Missing')),
         Prompt=factor(Prompt,levels=c('Spoons', 'Range','Point',
                                       'Recipe Name', 'Ingredient List',
                                       'Ingredient Quantities')))

dflong %>%
  group_by(Prompt, Sample2) %>% 
  dplyr::summarise(
    m=mean(`Grams Reported`, na.rm=T),
    mpred=mean(`Grams Predicted`, na.rm=T),
    r=cor(`Grams Predicted`, `Grams Reported`, use='pairwise.complete.obs'),
    N=n()) %>% 
  group_by(Prompt) %>% 
  mutate(Mpred=mean(mpred),Mcor=mean(r, na.rm=T),
         SD=sd(mpred))-> dflong_summary
dflong_summary %>% arrange(r) %>% filter(Prompt=='Spoons'|Prompt=='Range'|Prompt=='Point')

dflong_summary %>% arrange(r) %>% filter(Prompt!='Spoons',Prompt!='Range',Prompt!='Point')
#################################### grams plot phase 1 ########################################

## 1 is more missing in point (and also later in recipe name) -> inspect
dflong %>%
  mutate(Prompt=as.character(Prompt)) %>%
  mutate(Prompt=factor(Prompt,levels=c('Spoons','Range','Point'))) %>%
  filter(!is.na(Prompt)) %>% 
  tabyl(Classification,Prompt) %>%
  adorn_totals(c("row"), fill = "-", na.rm = F, name = "Total Classified") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>% 
  gt(.) %>% 
  gtsave(str_c('./Output/Tables/RESP_phase1.tex'))

#per sample
dflong %>%
  mutate(Prompt=as.character(Prompt)) %>%
  mutate(Prompt=factor(Prompt,levels=c('Spoons','Range','Point'))) %>%
  filter(!is.na(Prompt)) %>% 
  tabyl(Classification,Prompt, Sample) %>%
  adorn_totals(c("row"), fill = "-", na.rm = F, name = "Total Classified") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() #%>% gt_tbl(.) %>% gtsave(str_c('./Output/Tables/RESP_phase1_sample.tex'))

  #per sample2
dflong %>%
  mutate(Prompt=as.character(Prompt)) %>%
  mutate(Prompt=factor(Prompt,levels=c('Spoons','Range','Point'))) %>%
  filter(!is.na(Prompt)) %>% 
  tabyl(Classification,Prompt, Sample2) %>%
  adorn_totals(c("row"), fill = "-", na.rm = F, name = "Total Classified") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() #%>% gt_tbl(.) %>% gtsave(str_c('./Output/Tables/RESP_phase1_sample.tex'))


# paper
p <- dflong %>%
  select(PhotoName, `Grams Reported`, `Grams Predicted`, Prompt, Classification, Sample2) %>%
  filter(Prompt %in% c("Spoons", "Range", "Point")) %>%
  filter(!is.na(Classification), Classification != "Missing") %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 400, label.y = 750,
           size = 10 / .pt, family = "Times New Roman") +   # see note below
  geom_point(size = 0.8) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Paired")[c(6, 5, 3, 4, 3, 1, 2)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "darkgrey") +
  facet_grid(rows = vars(Sample2), cols = vars(Prompt)) +
  guides(colour = guide_legend(ncol = 4)) +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.size  = unit(4, "mm"),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(size = 10),          # facet header siz
    axis.text.x      = element_text(hjust = 0.8),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/RESP_phase1_sample2_R1_hq.pdf", p,
       device = cairo_pdf, width = 190, height = 130, units = "mm")

ggsave("./Output/Figures/RESP_phase1_sample2_R1_hq.png", p,
       device = ragg::agg_png, width = 190, height = 130, units = "mm",
       dpi = 600, bg = "white")

# appendix
p2 <- dflong %>%
  select(PhotoName, `Grams Reported`, `Grams Predicted`, Prompt, Classification) %>%
  filter(Prompt %in% c("Spoons", "Range", "Point")) %>%
  filter(!is.na(Classification), Classification != "Missing") %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 400, label.y = 800,
           size = 10 / .pt, family = "Times New Roman") +
  geom_point(size = 0.8) +
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
    axis.text.x      = element_text(hjust = 0.8),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/RESP_phase1_R1_hq.pdf", p2,
       device = cairo_pdf, width = 190, height = 90, units = "mm")

ggsave("./Output/Figures/RESP_phase1_R1_hq.png", p2,
       device = ragg::agg_png, width = 190, height = 90, units = "mm",
       dpi = 600, bg = "white")

#################################### grams plot phase 2 ########################################


dflong %>% 
  mutate(Prompt=as.character(Prompt)) %>% 
  # filter(Prompt != 'Reported', !is.na(Classification), TotalGrams3<1000) %>% 
  mutate(Prompt=factor(Prompt,levels=c('Recipe Name', "Ingredient List", "Ingredient Quantities"))) %>% 
  filter(!is.na(Prompt)) %>%
  tabyl(Classification,Prompt) %>%
  adorn_totals(c("row"), fill = "-", na.rm = F, name = "Total Classified") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>% 
  gt(.) %>% 
  gtsave(str_c('./Output/Tables/RESP_phase2.tex'))
  
line_df <- expand.grid(
  Prompt  = c("Recipe Name", "Ingredient List", "Ingredient Quantities"),
  Sample2 = c("Mealbox", "Traditional", "Convenience")
) %>%
  filter(!(Prompt %in% c("Ingredient List", "Ingredient Quantities") &
             Sample2 %in% c("Traditional", "Convenience"))) %>%
  mutate(slope = 1, intercept = 0)

p3 <- dflong %>%
  select(PhotoName, `Grams Reported`, `Grams Predicted`, Prompt, Classification, Sample2) %>%
  filter(Prompt %in% c("Recipe Name", "Ingredient List", "Ingredient Quantities")) %>%
  filter(!is.na(Classification), Classification != "Missing") %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 400, label.y = 1450,
           size = 10 / .pt, family = "Times New Roman") +
  geom_point(size = 0.8) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Paired")[c(6, 5, 3, 4, 3, 1, 2)]) +
  geom_abline(data = line_df, aes(slope = slope, intercept = intercept),
              linetype = "dashed", colour = "darkgrey") +
  facet_grid(rows = vars(Sample2), cols = vars(Prompt)) +
  guides(colour = guide_legend(ncol = 4)) +
  theme_grey(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.size  = unit(4, "mm"),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(size = 10),          # facet header siz
    axis.text.x      = element_text(hjust = 0.8),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/RESP_phase2_sample2_R1_hq.pdf", p3,
       device = cairo_pdf, width = 190, height = 165, units = "mm")

ggsave("./Output/Figures/RESP_phase2_sample2_R1_hq.png", p3,
       device = ragg::agg_png, width = 190, height = 165, units = "mm",
       dpi = 600, bg = "white")

## appendix
p4 <- dflong %>%
  select(PhotoName, `Grams Reported`, `Grams Predicted`, Prompt, Classification) %>%
  filter(Prompt %in% c("Recipe Name", "Ingredient List", "Ingredient Quantities")) %>%
  filter(!is.na(Classification), Classification != "Missing") %>%   # see note
  filter(!is.na(`Grams Predicted`)) %>%
  ggplot(aes(x = `Grams Reported`, y = `Grams Predicted`, colour = Classification)) +
  stat_cor(inherit.aes = FALSE,cor.coef.name = "r",      
           aes(x = `Grams Reported`, y = `Grams Predicted`),
           method = "pearson", r.digits = 2, digits = 3, p.accuracy = 0.001,
           label.x = 450, label.y = 1500,
           size = 10 / .pt, family = "Times New Roman") +
  geom_point(size = 0.8) +
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
    axis.text.x      = element_text(hjust = 0.8),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(0.25, "lines"),
    plot.margin      = margin(2, 4, 2, 2, "mm")
  )

ggsave("./Output/Figures/RESP_phase2_R1_hq.pdf", p4,
       device = cairo_pdf, width = 190, height = 90, units = "mm")

ggsave("./Output/Figures/RESP_phase2_R1_hq.png", p4,
       device = ragg::agg_png, width = 190, height = 90, units = "mm",
       dpi = 600, bg = "white")

##############################  study 2 ####################################################
## prepare data
dflong1<-read.csv('./Output/simulated/Final_grams_phase1.csv', check.names = F) # phase 1
dflong1gen<-read.csv('./Output/simulated/Final_grams_phase1_standard.csv', check.names = F) # phase 1 Standard
dflong2<-read.csv('./Output/simulated/Final_grams_phase2.csv', check.names = F) # phase 1
dflong2gen<-read.csv('./Output/simulated/Final_grams_phase2_standard.csv', check.names = F) # phase 1 Standard
## best run 
# see files read.csv("./Output/Simulated/Allruns_{phase1}{_standard}.csv")
bestrun1<-rbind(dflong1    %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification),
                dflong1gen %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification)) %>% 
  filter(Prompt %in% c('Spoons 3','Range 2','Point 4','Spoons Standard 3','Range Standard','Point Standard 5')) %>% #tabyl(Prompt)
  mutate(Prompt=as.character(Prompt)) %>% 
  mutate(Prompt=case_when(Prompt=='Spoons 3' ~'Spoons',
                          Prompt=='Range 2' ~ 'Range',
                          Prompt=='Point 4'~'Point', 
                          Prompt=='Spoons Standard 3' ~'Spoons Standard',
                          Prompt=='Range Standard' ~ 'Range Standard',
                          Prompt=='Point Standard 5'~'Point Standard', TRUE~NA)) %>% #tabyl(Prompt)
  mutate(Prompt=fct_relevel(factor(Prompt),c('Spoons','Range','Point','Spoons Standard','Range Standard','Point Standard')))  %>% 
  mutate(Classification=factor(Classification,levels=
                                 c('< more than 250 grams','< 100 until 250 grams','< 25 until 100 grams',
                                   '+/- 25 grams','> 25 until 100 grams','> 100 until 250 grams',
                                   '> more than 250 grams','Missing')))

bestrun2<-rbind(dflong2    %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification),
                dflong2gen %>% select(PhotoName,Prompt,PromptMax,`Grams Reported`,`Grams Predicted`,Classification)) %>% 
  mutate(Prompt=as.character(Prompt)) %>% 
  mutate(Prompt=as.character(case_when(Prompt=='Recipe Name 4' ~'Recipe Name', 
                                       Prompt=='Ingredient List 2' ~ 'Ingredient List', 
                                       Prompt=='Ingredient Quantities 3'~'Ingredient Quantities',
                                       Prompt=='Recipe Name Standard 2' ~'Recipe Name Standard',
                                       Prompt=='Ingredient List Standard 4' ~ 'Ingredient List Standard',
                                       Prompt=='Ingredient Quantities Standard 3'~'Ingredient Quantities Standard',
                                       TRUE~NA))) %>%
  filter(!is.na(Prompt)) %>% 
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

ggsave("./Output/Figures/EXPERT_phase1_phase2_R1_hq.pdf", p9,
       device = cairo_pdf, width = 190, height = 140, units = "mm")

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

ggsave("./Output/Figures/EXPERT_phase1_R1_hq.pdf", p10,
       device = cairo_pdf, width = 190, height = 140, units = "mm")

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

ggsave("./Output/Figures/EXPERT_phase2_R1_hq.pdf", p11,
       device = cairo_pdf, width = 190, height = 140, units = "mm")

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

ggsave("./Output/Figures/EXPERT_grams_allruns_R1_hq.pdf", p5,
       device = cairo_pdf, width = 190, height = 120, units = "mm")

ggsave("./Output/Figures/EXPERT_grams_allruns_R1_hq.png", p5,
       device = ragg::agg_png, width = 190, height = 120, units = "mm",
       dpi = 600, bg = "white")
#################################### hallucinated ingredients ########################################

## prepare data
dflong1<-read.csv('./Output/Simulated/Final_df_grams_converted_phase1.csv', check.names = F) %>% mutate(plate='Actual') # phase 1
dflong1gen<-read.csv('./Output/Simulated/Final_df_grams_converted_phase1_standard.csv', check.names = F)  %>% mutate(plate='Standard') # phase 1 Standard
dflong2<-read.csv('./Output/Simulated/Final_df_grams_converted_phase2.csv', check.names = F)  %>% mutate(plate='Actual') # phase 1
dflong2gen<-read.csv('./Output/Simulated/Final_df_grams_converted_phase2_standard.csv', check.names = F)  %>% mutate(plate='Standard') # phase 1 Standard

full_join(dflong1 %>% select(PhotoName, starts_with('ingredients'),starts_with('ingredient_'),
                         starts_with('ingredient3_'), starts_with('ingredient7_'), starts_with('ingredient14_'),
                         starts_with('ingredient6_'), starts_with('ingredient15_')) %>%
        mutate(ingredient3_9=NA,ingredient3_10=NA,ingredient3_11=NA,ingredient3_12=NA,ingredient3_13=NA,ingredient3_14=NA,ingredient3_15=NA,
               ingredient6_9=NA,ingredient6_10=NA,ingredient6_11=NA,ingredient6_12=NA,ingredient6_13=NA,ingredient6_14=NA,ingredient6_15=NA,
               ingredient7_9=NA,ingredient7_10=NA,ingredient7_11=NA,ingredient7_12=NA,ingredient7_13=NA,ingredient7_14=NA,ingredient7_15=NA) %>%
        rename_with(~ paste0("Actual_", .x)) ,
    dflong2 %>% select(PhotoName,starts_with('ingredients'),starts_with('ingredient_'),
                       starts_with('ingredient3_'), starts_with('ingredient7_'), starts_with('ingredient14_'),
                       starts_with('ingredient6_'), starts_with('ingredient15_')) %>%
      rename_with(~ paste0("Standard_",.x)),
    by = c("Actual_PhotoName" = "Standard_PhotoName")) %>% 
  rename(ingredients = Standard_ingredients,
         ingredientsQuantity = Standard_ingredientsQuantity) %>% 
  select(- c(Actual_ingredients, Actual_ingredientsQuantity)) %>% 
  # select(Actual_ingredients)
  # the parentheses cause problems for str_detect so we remove them
  mutate(
    across(
      where(is.character),
      ~ str_replace_all(.x, "bread\\(crumbs\\)", "bread")
    ),
    across(
      where(is.character),
      ~ str_replace_all(.x, "dairy \\(cream\\)", "dairy")
    ),
    across(
      where(is.character),
      ~ str_replace_all(.x, "\\(shell\\)fish", "fish")
    ),
    across(
      where(is.character),
      ~ str_replace_all(.x, "ground \\(undifferentiated\\) meat", "minced meat")
    )
  ) %>% 
  # create string out of recipe grouped ingredients
  rowwise() %>%
  mutate(
    ingredients = paste(unique(na.omit(c_across(matches("^Actual_ingredient_\\d+$")))),
                        collapse = ", ")
  ) %>% rename(PhotoName=Actual_PhotoName) %>% 
  # select(ingredients, Actual_ingredient3_1)
  
  # # Spoons
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("Actual_ingredient3_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% #filter(!is.na(Actual_ingredient3_1)) %>% select(starts_with('hallu_Actual_ingredient3_'), ingredients)
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_Actual_ingredient3_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>% #select(ingredients, hallu_ingredients)
  # ungroup()%>%
  # filter(hallu_ingredients!='') %>% #nrow() # 27
  # rowwise() %>%
  # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_actual_ningr_freq_1
  # filter(hallu_ingredients!='') %>%
  # pivot_longer(cols = starts_with('hallu_Actual_ingredient3_'), names_to = 'nr', values_to = 'ingr' ) %>%
  # filter(!is.na(ingr)) %>%
  # tabyl(ingr) %>% arrange(desc(n))-> hallu_actual_freq_1

  # # Range
  # # save those that are hallucinated in additional column hallu_ingredient1_#
  # mutate(
  #   across(
  #     starts_with("Actual_ingredient7"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% #filter(!is.na(Actual_ingredient7_1)) %>% select(starts_with('hallu_Actual_ingredient7'), ingredients)
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_Actual_ingredient7_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>% #select(ingredients, hallu_ingredients)
  # ungroup()%>%
  # filter(hallu_ingredients!='') %>% #nrow() # 26
  # rowwise() %>%
  # summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_actual_ningr_freq_2
  # filter(hallu_ingredients!='') %>%
  # pivot_longer(cols = starts_with('hallu_Actual_ingredient7_'), names_to = 'nr', values_to = 'ingr' ) %>%
  # filter(!is.na(ingr)) %>%
  # tabyl(ingr) %>% arrange(desc(n))-> hallu_actual_freq_2

  # # Point
  # mutate(
  #   across(
  #     starts_with("Actual_ingredient14_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% #filter(!is.na(Actual_ingredient14_1)) %>% select(starts_with('hallu_Actual_ingredient14_'), ingredients)
  #   rowwise() %>%
  #   mutate(
  #     hallu_ingredients = paste(
  #       unique(na.omit(c_across(matches("^hallu_Actual_ingredient14_\\d+$")))),
  #       collapse = ", "
  #     )
  #   ) %>% #select(ingredients, hallu_ingredients)
  #   ungroup()%>%
  #   filter(hallu_ingredients!='') %>% #nrow() # 25
  #   rowwise() %>%
  #   summarise(ningr = 1 + str_count(hallu_ingredients, ",")) %>% tabyl(ningr) -> hallu_actual_ningr_freq_3
    # filter(hallu_ingredients!='') %>%
    # pivot_longer(cols = starts_with('hallu_Actual_ingredient14_'), names_to = 'nr', values_to = 'ingr' ) %>%
    # filter(!is.na(ingr)) %>%
    # tabyl(ingr) %>% arrange(desc(n))-> hallu_actual_freq_3
  
  # # Recipe Name
  # mutate(
  #   across(
  #     starts_with("Standard_ingredient3_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% 
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_Standard_ingredient3_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>% #select(ingredients, hallu_ingredients)
  # ungroup()%>%
  # filter(hallu_ingredients!='') %>% #nrow() # 0

  # # Ingredients List
  # mutate(
  #   across(
  #     starts_with("Standard_ingredient_6_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% #filter(!is.na(Standard_ingredient_6_1)) %>% select(starts_with('hallu_Standard_ingredient_6_'), ingredients)
  # rowwise() %>%
  # mutate(
  #   hallu_ingredients = paste(
  #     unique(na.omit(c_across(matches("^hallu_Standard_ingredient_6_\\d+$")))),
  #     collapse = ", "
  #   )
  # ) %>% #select(ingredients, hallu_ingredients)
  # ungroup()%>%
  # filter(hallu_ingredients!='') %>% nrow() # 0
  
  
  # # Ingredients Quantities
  # mutate(
  #   across(
  #     starts_with("Standard_ingredient_15_"),
  #     ~ if_else(
  #       !is.na(.x) & !str_detect(ingredients, fixed(.x, ignore_case = TRUE))& !(.x %in% nokeyingr),
  #       .x,
  #       NA_character_
  #     ),
  #     .names = "hallu_{col}"
  #   )
  # ) %>% #filter(!is.na(Standard_ingredient_15_1)) %>% select(starts_with('hallu_Standard_ingredient_15_'), ingredients)
  #   rowwise() %>%
  #   mutate(
  #     hallu_ingredients = paste(
  #       unique(na.omit(c_across(matches("^hallu_Standard_ingredient_15_\\d+$")))),
  #       collapse = ", "
  #     )
  #   ) %>% #select(ingredients, hallu_ingredients)
  #   ungroup()%>%
  #   filter(hallu_ingredients!='') %>% nrow() # 0

reduce(list(hallu_actual_freq_1 %>% rename(n1=n, perc1=percent),
            hallu_actual_freq_2 %>% rename(n2=n, perc2=percent),
            hallu_actual_freq_3 %>% rename(n3=n, perc3=percent)),
       full_join, by = "ingr") %>% 
  mutate(perc3= perc3*100,perc1= perc1*100,perc2= perc2*100) %>% 
  arrange(desc(n3)) %>% 
  slice(1:8) %>% 
  mutate(
    `Spoons` = sprintf("%d (%.0f%%)", n1, perc1),
    `Point` = sprintf("%d (%.0f%%)", n3, perc3),
    `Range` = sprintf("%d (%.0f%%)", n2, perc2)
  ) %>%
  select(ingr, `Spoons`, `Range`, `Point`)%>%
  xtable(., type = 'latex') %>%
  print(., include.rownames = F)

## code for integrated plot study 1 and 2 wrt precision??
