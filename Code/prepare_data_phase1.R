rm(list=ls())
source('./Code/helper_functions.R')


df<-read_xlsx('./Input/All_dataframe_hellofresh_sample.xlsx')
colnames(df)
df<-df[-1,]

######################## get ingredients ########################
ingredients <- read_xlsx("./Input/ingredients_study2.xlsx")
ingredients <- t(ingredients[,2:ncol(ingredients)])
ingredients <- as.data.frame(ingredients)
colnames(ingredients)[1:4] <- c("recipe_name_en","country","servings","url")
colnames(ingredients)[5:ncol(ingredients)] <- paste0("ingredient_", 1:(ncol(ingredients)-4))
ingredients$ingredientsQuantity <- apply(ingredients[,5:ncol(ingredients)], 1, function(x) {
  paste(na.omit(x), collapse = "; ")
})

### remove quantity from ingredients 
ingredients$ingredients <-gsub("[0-9].", "", ingredients$ingredientsQuantity)
ingredients$ingredients<-gsub("\\b(g|gr|ml|of|l |ounces|tablespoon|teaspoon|ounce|unit|piece|pieces|clove|cloves|tablespoons|thumb|pack|tbsp|tsp|cup|cups|gram|grams|g |ml |sachet)\\b",'',ingredients$ingredients)

## adding ingredients to main df
df <- merge(df, ingredients %>% select(-c(country,url)), by.y = "recipe_name_en", by.x = "Recipe Name (ENG)")

## export recipes
df %>%
  select(`Recipe Name (ENG)`,ingredients) %>%
  group_by(`Recipe Name (ENG)`) %>%
  dplyr::summarize(n=n(),
                   ingredients=tolower(ingredients)) %>%
  distinct(`Recipe Name (ENG)`,.keep_all = T) %>% remove_rownames() -> all_recipes
print(xtable(all_recipes, type = 'latex'), include.rownames=FALSE, file='./Output/Data/recipes.tex')

df['TotalGramsSelf_char']<-df['Leftovers weight (g)']
for(i in seq(1,nrow(df))) df[i,'TotalGramsSelf']<-as.numeric(df[i,'TotalGramsSelf_char'])
write.csv(df,"./Output/Data/Final_df_grams_ingredients.csv", row.names = F)

######################## get photos and chat results ########################

json_files <- list.files("./Input/PromptAnswers/", pattern="*json")
json_files <- json_files[c(1:5, 11:15, 21:25)]

for(file in 1:length(json_files)) {
  json_data <- rjson::fromJSON(file=paste0("./Input/PromptAnswers/",json_files[file]))
  
  search_terms<-'^plate [0-9]$|^top plate$|^middle plate$|^bottom plate$|^pan [0-9]$|^top pan$|^middle pan$|^bottom pan$|^bowl [0-9]$|^top bowl$|^middle bowl$|^bottom bowl$|$soup [0-9]$|^soup$'
  
  for(i in 1:length(json_data)){
    
    
    l<-which(df$PhotoName %in% names(json_data)[i])
    df[l,paste0('chat',file)] <- json_data[[i]]
    df[l,paste0('i_',file)] <- i
    
    if(grepl("[{]\n", json_data[[i]])){ 
      
      #### check results
      json<-extract_json(json_data[[i]])
      if(is.null(json)){
        print(paste0('error: ',json_data[[i]]))
      }
      
      if(TRUE %in% grepl("Soup",names(json))){
        names(json)[grepl("Soup",names(json))]<-"Broth" 
      }
      # store ingredients and quantities
      ingredients<-extract_ingr_quant(json,search_terms)
      # print(ingredients)
      if(nrow(ingredients)==0) next
      for(ingr in seq(1,nrow(ingredients))){
        ingredients[ingr,1]<-gsub('_',' ', ingredients[ingr,1] )
        df[l,paste0('ingredient',file,'_',ingr)]<-ingredients[ingr,1]
        df[l,paste0('quantity',file,'_',ingr)]<-gsub(',','',ingredients[ingr,2])
      }
    }
  }
}

inspect_json_results<-function(df, json_files){
  for(i in 1:length(json_files)){
    json_data <- rjson::fromJSON(file=paste0("./Input/PromptAnswers/",json_files[i]))
    if(!is.na(table(is.na(df[paste0('ingredient',i,'_1')]))['TRUE'])){
      print(paste('************************************ FOR CHAT',i,'*************************************'))
      print(paste(table(is.na(df[paste0('ingredient',i,'_1')]))['TRUE'], 'have missing JSON'))
      
      rows<-df[is.na(df[paste0('ingredient',i,'_1')]),c(paste0('i_',i),'PhotoName', paste0('chat',i))]
      
      for(r in 1:nrow(rows)){
        print(paste('>>>>>>>',toupper(as.character(rows[r,'PhotoName'])),'>>>>>>>'))
        print(as.character(rows[r,paste0('chat',i)]))
        print(r)
      }
    }
  }
}
inspect_json_results(df, json_files)



#################### inspect conversions ########################
transformations <- read.csv("./Input/transformations.csv", fileEncoding="latin1")
df <- convert_to_grams(df, chatnr='1', transformations)
df <- convert_to_grams(df, chatnr='2', transformations)
df <- convert_to_grams(df, chatnr='3', transformations)
df <- convert_to_grams(df, chatnr='4', transformations)
df <- convert_to_grams(df, chatnr='5', transformations)
check_unconverted(df)

#################### calculate total grams ########################

df <-as.data.frame(df)
df<- get_grams_from_json(df, colname='conversion',chatnr='1') # 1
df<- get_grams_from_json(df, colname='conversion',chatnr='2') # 1
df<- get_grams_from_json(df, colname='conversion',chatnr='3') # 1
df<- get_grams_from_json(df, colname='conversion',chatnr='4') # 1
df<- get_grams_from_json(df, colname='conversion',chatnr='5') # 1

df<- get_grams_from_json(df, colname='quantity',chatnr='6') # 2
df<- get_grams_from_json(df, colname='quantity',chatnr='7') # 2
df<- get_grams_from_json(df, colname='quantity',chatnr='8') # 2
df<- get_grams_from_json(df, colname='quantity',chatnr='9') # 2
df<- get_grams_from_json(df, colname='quantity',chatnr='10') # 2

df<- get_grams_from_json(df, colname='quantity',chatnr='11') # 3
df<- get_grams_from_json(df, colname='quantity',chatnr='12') # 3
df<- get_grams_from_json(df, colname='quantity',chatnr='13') # 3
df<- get_grams_from_json(df, colname='quantity',chatnr='14') # 3
df<- get_grams_from_json(df, colname='quantity',chatnr='15') # 3

df %>%  select(starts_with('TotalGrams')) %>% summary()


#################### inspect missings  ########################


df %>%
  select(starts_with('ingredient_')) %>%
  pivot_longer(everything(),names_to = 'ingredient_nr', values_to = 'ingredient') %>% 
  filter(!is.na(ingredient), ingredient!='unidentified') %>% 
  mutate(ingredient = str_trim(gsub('\\d+\\.*\\d*|\\d+/\\d+|g |ml |tablespoon|teaspoon|gram|grams| of ','', ingredient))) %>% # remove numbers, space, and single letter g
  mutate(ingredient = str_trim(ingredient)) %>% #nrow()#1124  reported
  distinct(ingredient,.keep.all=T) %>% print(n=100) %>% nrow()#82


df %>%
  select(#starts_with('ingredient_'),
    starts_with('ingredient1'),
    starts_with('ingredient2'),
    starts_with('ingredient3'),
    starts_with('ingredient4'),
    starts_with('ingredient5'),
    starts_with('ingredient6'),
    starts_with('ingredient7'),
    starts_with('ingredient8'),
    starts_with('ingredient9'),
    starts_with('ingredient10'),
    starts_with('ingredient11'),
    starts_with('ingredient12'),
    starts_with('ingredient13'),
    starts_with('ingredient14'),
    starts_with('ingredient15')
  ) %>%
  pivot_longer(everything(),names_to = 'ingredient_nr', values_to = 'ingredient') %>% 
  filter(!is.na(ingredient), ingredient!='unidentified') %>% 
  mutate(ingredient = str_trim(ingredient)) %>% #nrow()#4743  reported
  distinct(ingredient,.keep.all=T) %>% nrow()#180

#################### convert higher order ingredients ########################

df.converted <-df
idx<-as.numeric(rownames(df))
cols<-df.converted %>% select(starts_with('ingredient_'),
                              starts_with('ingredient1'),
                              starts_with('ingredient2'),
                              starts_with('ingredient3'),
                              starts_with('ingredient4'),
                              starts_with('ingredient5'),
                              starts_with('ingredient6'),
                              starts_with('ingredient7'),
                              starts_with('ingredient8'),
                              starts_with('ingredient9'),
                              starts_with('ingredient10'),
                              starts_with('ingredient11'),
                              starts_with('ingredient12'),
                              starts_with('ingredient13'),
                              starts_with('ingredient14'),
                              starts_with('ingredient15')
) %>% colnames()


for(c in cols){
  
  idxc<-idx[!is.na(df.converted[,c])]
  
  ## to inspect
  # converted<-cbind(df.converted[idxc,c],categorize_ingredients(df[idxc,c]))
  # print(unique(converted))
  
  ## save
  df.converted[idxc,c]<-categorize_ingredients(df.converted[idxc,c])
}

calculate_conv_matrix<-function(df, promptnr){
  checking_string <- paste0('^ingredient',promptnr,'_')
  for(elem in c('TP','FP')){
    df[paste0(elem,promptnr)]<-count_occurrences(df, "ingredient_", checking_string, elem)
  } 
  return(df)
}
df.converted<-calculate_conv_matrix(df.converted, '1')
df.converted<-calculate_conv_matrix(df.converted, '2')
df.converted<-calculate_conv_matrix(df.converted, '3')
df.converted<-calculate_conv_matrix(df.converted, '4')
df.converted<-calculate_conv_matrix(df.converted, '5')
df.converted<-calculate_conv_matrix(df.converted, '6')
df.converted<-calculate_conv_matrix(df.converted, '7')
df.converted<-calculate_conv_matrix(df.converted, '8')
df.converted<-calculate_conv_matrix(df.converted, '9')
df.converted<-calculate_conv_matrix(df.converted, '10')
df.converted<-calculate_conv_matrix(df.converted, '11')
df.converted<-calculate_conv_matrix(df.converted, '12')
df.converted<-calculate_conv_matrix(df.converted, '13')
df.converted<-calculate_conv_matrix(df.converted, '14')
df.converted<-calculate_conv_matrix(df.converted, '15')
write.csv(df.converted, "./Output/Data/Final_df_grams_converted_phase1.csv", row.names = F)


df.converted %>% 
  mutate(
    Phase = 1,
    Generic = 1,
    Prec_spoons_1=TP1/(TP1+FP1),
    Prec_spoons_2=TP2/(TP2+FP2),
    Prec_spoons_3=TP3/(TP3+FP3),
    Prec_spoons_4=TP4/(TP4+FP4),
    Prec_spoons_5=TP5/(TP5+FP5),
    Prec_range_6=TP6/(TP6+FP6),
    Prec_range_7=TP7/(TP7+FP7),
    Prec_range_8=TP8/(TP8+FP8),
    Prec_range_9=TP9/(TP9+FP9),
    Prec_range_10=TP10/(TP10+FP10),
    Prec_point_11=TP11/(TP11+FP11),
    Prec_point_12=TP12/(TP12+FP12),
    Prec_point_13=TP13/(TP13+FP13),
    Prec_point_14=TP14/(TP14+FP14),
    Prec_point_15=TP15/(TP15+FP15)
  ) %>% 
  select(PhotoName, Phase, Generic, starts_with('Prec_')) %>%
  pivot_longer(cols = starts_with("Prec_"), names_to = "Prompt", values_to = "Precision") %>% 
  mutate(Prompt=case_when(Prompt=='Prec_spoons_1'~'Spoons',
                          Prompt=='Prec_spoons_2'~'Spoons 2',
                          Prompt=='Prec_spoons_3'~'Spoons 3',
                          Prompt=='Prec_spoons_4'~'Spoons 4',
                          Prompt=='Prec_spoons_5'~'Spoons 5',
                          Prompt=='Prec_range_6'~'Range',
                          Prompt=='Prec_range_7'~'Range 2',
                          Prompt=='Prec_range_8'~'Range 3',
                          Prompt=='Prec_range_9'~'Range 4',
                          Prompt=='Prec_range_10'~'Range 5',
                          Prompt=='Prec_point_11'~'Point',
                          Prompt=='Prec_point_12'~'Point 2',
                          Prompt=='Prec_point_13'~'Point 3',
                          Prompt=='Prec_point_14'~'Point 4',
                          Prompt=='Prec_point_15'~'Point 5',
                          
  )) %>% write.csv(., './Output/Data/Final_precision_phase1.csv', row.names = F)

#################### table ingredients  ########################

df.converted %>%
  select(starts_with('ingredient_'),
         starts_with('ingredient1'),
         starts_with('ingredient2'),
         starts_with('ingredient3'),
         starts_with('ingredient4'),
         starts_with('ingredient5'),
         starts_with('ingredient6'),
         starts_with('ingredient7'),
         starts_with('ingredient8'),
         starts_with('ingredient9'),
         starts_with('ingredient10'),
         starts_with('ingredient11'),
         starts_with('ingredient12'),
         starts_with('ingredient13'),
         starts_with('ingredient13'),
         starts_with('ingredient14'),
         starts_with('ingredient15')
  ) %>%
  pivot_longer(everything(),names_to = 'ingredient_nr', values_to = 'ingredient') %>% 
  filter(!is.na(ingredient), ingredient!='unidentified') %>% 
  mutate(ingredient = tolower(str_trim(ingredient))) %>% 
  mutate(group=case_when(ingredient %in% c('fats or oils','water or broth','spices','herbs','garlic','flour','onion','seasoning','peppers','sauce','vinaigrette','pepper') ~ 'Additives',
                         ingredient %in% c('pork','fish','köfte','poultry','beef, veal or lamb','(shell)fish','meat substitutes','charcuterie') ~ 'Protein',
                         ingredient %in% c('zucchini','eggplant','tomatoes','vegetable mix','celery','vegetables','root vegetables',
                                           'bell pepper','mushrooms','cucumbers','cauliflower','avocado','broccoli','asparagus','grüne blätter','gemüsestücke') ~ 'Vegetables',
                         ingredient %in% c('cabbage','endive','leek','lettuce','spinach') ~ 'Leafy vegetables',
                         ingredient %in% c('pasta','bread(crumbs)','legumes','noodles','flatbread','rice','potatoes or fries','quinoa, couscous or bulgur','squash','corn','parsnip','cooked potatoes or grains') ~ 'Starches',
                         ingredient %in% c('apple','pear','pineapple','kiwi','banana','dried fruits')~ 'Fruits',
                         ingredient %in% c('eggs','egg','cheese','diary (cream)','käsereste')~ 'Diary',
                         ingredient %in% c('olives','nuts or raisins','lemon or lime','seeds or other toppings')~ 'Toppings', 
                         grepl('moussaka|bone|liquid|bowl|handfuls|bohn|mixture|ingredients|ingredient|residual|quantity|slice|plate|glass|weight|comments|description|type|top|tablespoon|piece|type|unit|protein|packaging|estimate',tolower(ingredient))~'Not ingredient', TRUE~ingredient)) %>% 
  filter(group != 'Not ingredient') %>% 
  # filter(!is.na(ingredient), ingredient !='unidentified') %>% distinct(.,.keep_all = T)
  mutate(ingredient=str_to_sentence(ingredient)) %>% 
  ## inspect to look for wrong ingredients
  group_by(ingredient) %>% 
  dplyr::summarise(Group=unique(group),N=n()) %>% 
  print(n=50) # to inspect N=36
# once cleaned, save the full table with all ingredients
# dplyr::arrange(Group, desc(N)) %>%
# select(Group, , ingredient, N) %>%
# xtable(., type = 'latex') %>%
# print(., include.rownames = FALSE)



#################################### phase 1 all runs ########################################

# one row per chat and photo
dflong<-
  df %>% 
  dplyr::rename(Spoons=TotalGrams1,
                `Spoons 2`=TotalGrams2,
                `Spoons 3`=TotalGrams3,
                `Spoons 4`=TotalGrams4,
                `Spoons 5`=TotalGrams5,
                Range=TotalGrams6,
                `Range 2`=TotalGrams7,
                `Range 3`=TotalGrams8,
                `Range 4`=TotalGrams9,
                `Range 5`=TotalGrams10,
                `Point`=TotalGrams11, 
                `Point 2`=TotalGrams12, 
                `Point 3`=TotalGrams13, 
                `Point 4`=TotalGrams14, 
                `Point 5`=TotalGrams15, 
                `Grams Reported`=TotalGramsSelf) %>% #nrow()
  pivot_longer(cols = c(Spoons, `Spoons 2`, `Spoons 3`, `Spoons 4`,`Spoons 5`,
                        Range,`Range 2`,`Range 3`,`Range 4`,`Range 5`,
                        `Point`,`Point 2`,`Point 3`, `Point 4`, `Point 5`), 
               values_to = 'Grams Predicted', names_to = 'Prompt') %>% 
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
         Prompt=factor(Prompt,levels=c('Spoons','Spoons 2','Spoons 3','Spoons 4','Spoons 5',
                                       'Range','Range 2','Range 3','Range 4','Range 5',
                                       'Point','Point 2','Point 3',
                                       'Point 4','Point 5')))


## select best performing run
dflong %>%
  group_by(Prompt) %>% 
  dplyr::summarise(
    m=mean(`Grams Reported`),
    mpred=mean(`Grams Predicted`, na.rm=T),
    r=cor(`Grams Predicted`, `Grams Reported`, use='complete.obs')) %>% 
  mutate(Mpred=mean(mpred),Mcor=mean(r),
         SD=sd(mpred)) ->dflong_summary
dflong_summary$PromptType <-c(rep('Spoons',5), rep('Range',5),rep('Point',5))
dflong_summary %>% 
  group_by(PromptType) %>% 
  dplyr::summarise(rMax= max(r)) %>% 
  merge(dflong_summary)-> dflong_summary
bestprompts<-as.character(dflong_summary$Prompt[dflong_summary$rMax==dflong_summary$r])
dflong_summary['PromptMax']<-c(rep(bestprompts[1],5),rep(bestprompts[2],5),rep(bestprompts[3],5))
dflong %>%  merge(.,dflong_summary, by='Prompt') -> dflong
## save all data to later compare runs
write.csv(dflong_summary, "./Output/Data/Allruns_phase1.csv", row.names = F)
write.csv(dflong, "./Output/Data/Final_grams_phase1.csv", row.names = F)

