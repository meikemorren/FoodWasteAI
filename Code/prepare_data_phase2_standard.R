rm(list=ls())
source('./Code/helper_functions.R')

df<-read.csv("./Output/Data/Final_df_grams_ingredients.csv", check.names = F)

######################## get photos and chat results ########################

df['TotalGramsSelf_char']<-df['Leftovers weight (g)']
for(i in seq(1,nrow(df))) df[i,'TotalGramsSelf']<-as.numeric(df[i,'TotalGramsSelf_char'])

json_files <- list.files("./Input/PromptAnswers/", pattern="*json")
json_files <- json_files[31:45]# phase 1

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

      if(nrow(ingredients)==0) next
      for(ingr in seq(1,nrow(ingredients))){
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
df$TotalGrams15[is.na(df$ingredient15_1)][1]<-NA

#################### inspect conversions ########################
# transformations <- read.csv("./Input/transformations.csv")
# df <- convert_to_grams(df, chatnr='1', transformations)
# df <- convert_to_grams(df, chatnr='2', transformations)
# df <- convert_to_grams(df, chatnr='3', transformations)
# df <- convert_to_grams(df, chatnr='4', transformations)
# df <- convert_to_grams(df, chatnr='5', transformations)

#################### calculate total grams ########################

df <-as.data.frame(df)
df<- get_grams_from_json(df, colname='quantity',chatnr='1') # 1
df<- get_grams_from_json(df, colname='quantity',chatnr='2') # 1
df<- get_grams_from_json(df, colname='quantity',chatnr='3') # 1
df<- get_grams_from_json(df, colname='quantity',chatnr='4') # 1
df<- get_grams_from_json(df, colname='quantity',chatnr='5') # 1

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

# all missings with recipe names
# df$TotalGrams11[is.na(df$ingredient11_1)]<-NA # 3
# df$TotalGrams11[is.na(df$ingredient11_1)][2]<-0 # 1
# df$TotalGrams12[is.na(df$ingredient12_1)]<-NA # 3
# df$TotalGrams12[is.na(df$ingredient12_1)][1]<-0 # 1
# df$TotalGrams13[is.na(df$ingredient13_1)]<-NA # 4
# df$TotalGrams14[is.na(df$ingredient14_1)]<-NA # 2
# df$TotalGrams15[is.na(df$ingredient15_1)][c(1,3,4)]<-NA # 3
# df$TotalGrams15[is.na(df$ingredient15_1)][2]<-0 # 1



#################### inspect missings  ########################

df %>%  select(starts_with('TotalGrams')) %>% summary()

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
  mutate(ingredient = str_trim(ingredient)) %>% #nrow()#1546  reported
  distinct(ingredient,.keep.all=T) %>% nrow()#340

#################### higher order ingredients ########################

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

## CHECK: reference string not until we have recipes
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
write.csv(df.converted, "./Output/Data/Final_df_grams_converted_phase2_standard.csv", row.names = F)


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
  mutate(ingredient = tolower(str_trim(ingredient))) %>% #nrow()#16979 incl recipe, 10997 reported
  # distinct(ingredient,.keep.all=T) %>% nrow()#98 
  mutate(group=case_when(ingredient %in% c('fats or oils','water or broth','spices','herbs','garlic','flour','onion','seasoning','peppers','sauce','vinaigrette','pepper') ~ 'Additives',
                         ingredient %in% c('pork','fish','köfte','poultry','beef, veal or lamb','(shell)fish','meat substitutes','charcuterie') ~ 'Protein',
                         ingredient %in% c('zucchini','eggplant','tomatoes','vegetable mix','celery','vegetables','root vegetables',
                                           'bell pepper','mushrooms','cucumbers','cauliflower','avocado','broccoli','asparagus','grüne blätter','gemüsestücke') ~ 'Vegetables',
                         ingredient %in% c('cabbage','endive','leek','lettuce','spinach') ~ 'Leafy vegetables',
                         ingredient %in% c('pasta','bread(crumbs)','legumes','noodles','flatbread','rice','potatoes or fries','quinoa, couscous or bulgur','squash','corn','parsnip','cooked potatoes or grains') ~ 'Starches',
                         ingredient %in% c('apple','pear','pineapple','kiwi','banana', 'dried fruits')~ 'Fruits',
                         ingredient %in% c('eggs','egg','cheese','diary (cream)','käsereste')~ 'Diary',
                         ingredient %in% c('olives','nuts or raisins','lemon or lime','seeds or other toppings')~ 'Toppings', 
                         grepl('moussaka|bone|liquid|bowl|handfuls|bohn|mixture|ingredients|ingredient|residual|quantity|slice|plate|glass|weight|comments|description|type|top|tablespoon|piece|type|unit|protein|packaging|estimate',tolower(ingredient))~'Not ingredient', TRUE~ingredient)) %>% 
  filter(group != 'Not ingredient') %>% 
  # filter(!is.na(ingredient), ingredient !='unidentified') %>% distinct(.,.keep_all = T)
  mutate(ingredient=str_to_sentence(ingredient)) %>% 
  ## inspect to look for wrong ingredients
  group_by(ingredient) %>% 
  dplyr::summarise(Group=unique(group),N=n()) %>% 
  print(n=50) # to inspect
# once cleaned, save the full table with all ingredients
# dplyr::arIngredients(Group, desc(N)) %>%
# select(Group, , ingredient, N) %>%
# xtable(., type = 'latex') %>%
# print(., include.rownames = FALSE)



#################################### phase 2 all runs ########################################

dflong<-
  df %>% 
  dplyr::rename(`Recipe Name Standard 1`=TotalGrams11,
                `Recipe Name Standard 2`=TotalGrams12,
                `Recipe Name Standard 3`=TotalGrams13,
                `Recipe Name Standard 4`=TotalGrams14,
                `Recipe Name Standard 5`=TotalGrams15,
                `Ingredient List Standard`=TotalGrams1,
                `Ingredient List Standard 2`=TotalGrams2,
                `Ingredient List Standard 3`=TotalGrams3,
                `Ingredient List Standard 4`=TotalGrams4,
                `Ingredient List Standard 5`=TotalGrams5,
                `Ingredient Quantities Standard`=TotalGrams6, 
                `Ingredient Quantities Standard 2`=TotalGrams7, 
                `Ingredient Quantities Standard 3`=TotalGrams8, 
                `Ingredient Quantities Standard 4`=TotalGrams9, 
                `Ingredient Quantities Standard 5`=TotalGrams10, 
                `Grams Reported`=TotalGramsSelf) %>% #nrow()
  pivot_longer(cols = c(`Recipe Name Standard 1`, `Recipe Name Standard 2`, `Recipe Name Standard 3`, `Recipe Name Standard 4`,`Recipe Name Standard 5`,
                        `Ingredient List Standard`,`Ingredient List Standard 2`,`Ingredient List Standard 3`,`Ingredient List Standard 4`,`Ingredient List Standard 5`,
                        `Ingredient Quantities Standard`,`Ingredient Quantities Standard 2`,`Ingredient Quantities Standard 3`, 
                        `Ingredient Quantities Standard 4`, `Ingredient Quantities Standard 5`), 
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
         Prompt=factor(Prompt,levels=c('Recipe Name Standard 1','Recipe Name Standard 2','Recipe Name Standard 3','Recipe Name Standard 4','Recipe Name Standard 5',
                                       'Ingredient List Standard','Ingredient List Standard 2','Ingredient List Standard 3','Ingredient List Standard 4','Ingredient List Standard 5',
                                       'Ingredient Quantities Standard','Ingredient Quantities Standard 2','Ingredient Quantities Standard 3',
                                       'Ingredient Quantities Standard 4','Ingredient Quantities Standard 5')))

## save all data and select best performing run
dflong %>%
  group_by(Prompt) %>% 
  dplyr::summarise(
    m=mean(`Grams Reported`),
    mpred=mean(`Grams Predicted`, na.rm=T),
    r=cor(`Grams Predicted`, `Grams Reported`, use='complete.obs')) %>% 
  mutate(Mpred=mean(mpred),Mcor=mean(r),
         SD=sd(mpred)) ->dflong_summary
dflong_summary$PromptType <-c(rep('Recipe Name',5), rep('Ingredient List',5),rep('Ingredient Quantities',5))
dflong_summary %>% 
  group_by(PromptType) %>% 
  dplyr::summarise(rMax= max(r)) %>% 
  merge(dflong_summary)-> dflong_summary
bestprompts<-as.character(dflong_summary$Prompt[dflong_summary$rMax==dflong_summary$r])
dflong_summary['PromptMax']<-c(rep(bestprompts[1],5),rep(bestprompts[2],5),rep(bestprompts[3],5))
dflong %>%  merge(.,dflong_summary, by='Prompt') -> dflong
write.csv(dflong_summary, "./Output/Data/Allruns_phase2_generic.csv", row.names = F)
write.csv(dflong, "./Output/Data/Final_grams_phase2_standard.csv", row.names = F)


#################### calculate precision of ingredients ########################

df.converted %>% 
  mutate(
    Phase =2,
    Generic=2,
    Prec_1=TP1/(TP1+FP1),
    Prec_2=TP2/(TP2+FP2),
    Prec_3=TP3/(TP3+FP3),
    Prec_4=TP4/(TP4+FP4),
    Prec_5=TP5/(TP5+FP5),
    Prec_6=TP6/(TP6+FP6),
    Prec_7=TP7/(TP7+FP7),
    Prec_8=TP8/(TP8+FP8),
    Prec_9=TP9/(TP9+FP9),
    Prec_10=TP10/(TP10+FP10),
    Prec_11=TP11/(TP11+FP11),
    Prec_12=TP12/(TP12+FP12),
    Prec_13=TP13/(TP13+FP13),
    Prec_14=TP14/(TP14+FP14),
    Prec_15=TP15/(TP15+FP15)
    
  ) %>%
  select(PhotoName, Phase, Generic, starts_with('Prec_')) %>%
  pivot_longer(cols = starts_with("Prec_"), names_to = "Prompt", values_to = "Precision") %>%
  mutate(Prompt=case_when(Prompt == 'Prec_1'~'Ingredient List Standard',
                          Prompt == 'Prec_2'~'Ingredient List Standard 2',
                          Prompt == 'Prec_3'~'Ingredient List Standard 3',
                          Prompt == 'Prec_4'~'Ingredient List Standard 4',
                          Prompt == 'Prec_5'~'Ingredient List Standard 5',
                          Prompt == 'Prec_6'~'Ingredient Quantities Standard',
                          Prompt == 'Prec_7'~'Ingredient Quantities Standard 2',
                          Prompt == 'Prec_8'~'Ingredient Quantities Standard 3',
                          Prompt == 'Prec_9'~'Ingredient Quantities Standard 4',
                          Prompt == 'Prec_10'~'Ingredient Quantities Standard 5',
                          Prompt == 'Prec_11'~'Recipe Name Standard 1',
                          Prompt == 'Prec_12'~'Recipe Name Standard 2',
                          Prompt == 'Prec_13'~'Recipe Name Standard 3',
                          Prompt == 'Prec_14'~'Recipe Name Standard 4',
                          Prompt == 'Prec_15'~'Recipe Name Standard 5',
                          
  )) %>% write.csv(., './Output/Data/Final_precision_phase2_standard.csv', row.names = F)
