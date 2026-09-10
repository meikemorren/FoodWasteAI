library(plyr)
library(tidyverse)
library(gt)
library(readxl)
library(janitor)
library(tableone)
library(xtable)
library(gridExtra)
library(ggpubr)
library(readxl)
library(magrittr)
# library(words2number)
library(rjson)
library(zipR)
library(see) # Horizontal half violin plot


inspect_json_results<-function(df, json_files){
  for(i in 1:length(json_files)){
    json_data <- rjson::fromJSON(file=paste0("./Input/Simulated/gpt4o/",json_files[i]))
    if(!is.na(table(is.na(df[paste0('ingredient',i,'_1')]))['TRUE'])){
      print(paste('************************************ FOR CHAT',i,'*************************************'))
      print(paste(table(is.na(df[paste0('ingredient',i,'_1')]))['TRUE'], 'have missing JSON'))
      
      rows<-df[is.na(df[paste0('ingredient',i,'_1')]),c(paste0('i_',i),'PhotoName', paste0('chat',i))]
      
      for(r in 1:nrow(rows)){
        json<-as.character(rows[r,paste0('chat',i)])

        print(paste('>>>>>>>',toupper(as.character(rows[r,'PhotoName'])),'>>>>>>>',json))
        # print()
        # print(r)
      }
    }
  }
}


find_numbers<-function(text){
  
  text_list <- str_extract_all(text, "\\d+.\\d+|\\d+")  # allow for . decimals (no commas found)
  for(t in seq(1, length(text_list))){
    if(length(text_list[[t]])==0) text_list[[t]]<-NA
  }
  numbers<-gsub('[A-z]','', unlist(text_list))
  numbers<-unlist(lapply(numbers, function(x) as.numeric(x)))
  
  # numbers<- numbers[numbers>0]
  return(numbers)
}


get_n_grams<-function(text, indicators){
  "this function finds the grams in texts.
  when multiple entities are mentioned,
  it multiples the number of entities with the grams"
  
  text<-str_trim(gsub('\"','',text))
  elems<-unlist(str_split(text,' ')) # JOSUA: check, this line was missing
  elems_with_numbers<-find_numbers(elems)
  elems_with_numbers<-elems_with_numbers[!is.na(elems_with_numbers)]
  # print(elems_with_numbers)
  # if only numbers, assume it is grams
  tryCatch(
    #try to do this
    {
      # if range
      if(length(elems[grepl('-',elems)])>0) elems_with_numbers <- get_avg_from_range(elems[grepl('-',elems)]) 
      if(length(elems_with_numbers)==1){
        
        # print(text)
        if(!is.na(as.numeric(elems_with_numbers))){
          ngram<-as.numeric(elems_with_numbers) 
          # print(ngram)
          return(ngram)
        } 
      }
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('An Error Occurred')
      print(e)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
      next
    }
  )
  # select when grams are mentioned
  gramindicators<-c('gram','grams','g','gr','gm','grammes','GR')
  tryCatch(
    
    {
      if(sum(elems %in% gramindicators)>0){
        ngram<-elems[which(elems %in% gramindicators)-1]
        ngram<-as.numeric(ngram)
        
        if(sum(elems %in% gramindicators)>0){
          
          elemsnr<-NULL
          for(e in elems) elemsnr<-c(elemsnr, try(to_number(e), silent=TRUE))
          
          numbers<-NULL
          suppressWarnings(
            for(e in elemsnr) numbers<-c(numbers,as.numeric(gsub('[A-z]','', e)))
          )
          numbers <- numbers[!is.na(numbers)]
          
          if(length(numbers)==2)  return(numbers[numbers!=ngram]*ngram)
          if(length(numbers)==1)  return(numbers) 
          if(length(numbers)==0)  print('no numbers') 
        }else{
          return(ngram)
          next
          
        }
      }
      if(sum(elems %in% gramindicators)==0){
        for(e in elems){
          if(substr(e, nchar(e),nchar(elems)) %in% gramindicators){
            e<-ifelse(substr(e, nchar(e),nchar(e)) %in% gramindicators,str_sub(e, end = -2),e) 
          }
          if(length(e)==1){
            elems<-e
          }else{
            print(paste('multiple grams reported with g:', text, sep=""))
          } 
        }
      }
      
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('An Error Occurred')
      # print(e)
      # print(text)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      message('A Warning Occurred')
      # print(w)
      # print(text)
      return(NA)
    }
  )
}


clean_ingredient <- function(ingredient) {
  # lower case
  ingredient <- tolower(ingredient)
  
  # Remove numbers and punctuation
  ingredient <- gsub("[0-9]", " ", ingredient)
  ingredient <- gsub("[[:punct:]]", " ", ingredient)
  ingredient <- gsub("[/]", "", ingredient)
  
  # Define the pattern for the unwanted words
  unwanted_words <- "\\b(g[[:space:]]+|gr[[:space:]]+|7g|ml[[:space:]]+|cm[[:space:]]+|cc[[:space:]]+|of[[:space:]]+|ounces|ounce|ouce|can[[:space:]]+|ounce|unit|units|piece|pieces|peices|pc[[:space:]]+|clove|cloves|thumb|pack|tbsp|tablespoon|tablesppon|tablespon|tsp|tsbp|tbso|teaspoon|cup|cups|gram|grams|sachet|to taste| to atste)\\b"
  
  # Remove unwanted words
  ingredient <- gsub(unwanted_words, "", ingredient)
  
  # manual changes
  ingredient<-gsub('sprinonion','spring onion',ingredient)
  ingredient<-gsub('yoghurt','yogurt',ingredient)
  ingredient<-gsub('prok','pork',ingredient)
  ingredient<-gsub('creeme','creme',ingredient)
  ingredient<-gsub('zuchinni','zucchini',ingredient)
  ingredient<-gsub('toritillas','tortillas',ingredient)
  ingredient<-gsub('poatotes','potatoes',ingredient)
  
  ingredient<-gsub('british','British',ingredient)
  ingredient<-gsub('greek','Greek',ingredient)
  ingredient<-gsub('mexican','Mexican',ingredient)
  ingredient<-gsub('italian','Italian',ingredient)
  ingredient<-gsub('thai','Thai',ingredient)
  ingredient<-gsub('gouda','Gouda',ingredient)
  ingredient<-gsub('indian','Indian',ingredient)
  
  # Trim leading and trailing whitespace
  ingredient <- trimws(ingredient, which='both')
  ingredient <- gsub('  ',' ', ingredient)
  
  return(ingredient)
}

info_from_name <- function(name){
  
  elements<-unlist(strsplit(name,"_"))
  print(elements)
  if(length(grep('.jpg|.png|.jpeg', elements))>0){
    name_photo <- elements[grep('.jpg|.png|.jpeg', elements)]
    elements <- elements[!elements %in% elements[grep('.jpg|.png|.jpeg', elements)]]
    # print(nchar(elements))
  }
  
  source <- elements[grep('HF|nonHF|semi', elements)]
  country <- elements[grep('CAN|US|GE|UK|BE|NDL|NLD', elements)]
  language <- elements[grep('EN|FR', elements)]
  # round <- elements[nchar(grep('1|2|3|4', elements)==1)]
  type <- elements[grep('after|before', elements)]
  respondent_ID<- elements[nchar(elements)==11]
  collector_ID<- elements[nchar(elements)==8]
  
  if(length(source)==0) source <- NA
  if(length(country)==0) country <- NA
  if(length(language)==0) language <- NA
  # if(length(round)==0) round <- NA
  if(length(type)==0) type <- NA
  if(length(respondent_ID)==0) respondent_ID <- NA
  if(length(collector_ID)==0) collector_ID <- NA
  
  return(as.data.frame(cbind(name, name_photo, source, country, language, #round, 
                             type, respondent_ID, collector_ID)))
}

info_testsample <- function(testsample){
  all_info <- NULL
  for(i in testsample){
    print(i)
    all_info<-rbind(all_info, info_from_name(i))
  }
  return(all_info)
}

pos_quantity<-function(ingredient){
  quantity<-unlist(strsplit(ingredient, ' '))
  qn<-grep('[0-9]', quantity)
  return(list(qn, quantity))
}

quantify_division<-function(division){
  if(grepl('/',division)){
    parts<-unlist(strsplit(division,''))
    if(length(parts)==3){
      print(as.numeric(parts[1])/as.numeric(parts[3]))
      divnum <- as.numeric(parts[1])/as.numeric(parts[3])
      # sample[i,paste0('leftover_quantity_',as.character(k))] <- as.numeric(qparts[1])/as.numeric(qparts[3])
    }else{
      print(parts)
    }
  }
  return(divnum)
}
  
ingredient_name<-function(ing){
  if(substr(ing,nchar(ing),nchar(ing))==':'){
    # print(substr(g, 3, nchar(g)))
    ing_clean<-gsub('(possibly)','', ing, fixed = TRUE)
    ing_clean<-substr(ing_clean, 3, nchar(ing_clean)-1)
    ing_clean<-str_trim(ing_clean)
    return(ing_clean)
    # sample[i,paste0('leftover_ingr_',as.character(k))] <- ing_clean
    # print(sample[i,paste0('leftover_ingr_',as.character(k))])
  }
}


extract_json<-function(string){
  
  #remove text before json
  if(grepl('\n\n```json|```json\n',string)){
    spl <- unlist(strsplit(string, "\n\n", fixed = TRUE))
    string<-spl[grep('json\n',spl)]
  }
  # # remove text after json
  # if(grepl('```\n\n',string)){
  #   spl <- strsplit(string, "\n\n", fixed = TRUE)
  #   string<-sapply(spl, "[", 1)
  # }
  # remove text after json
  if(grepl('```\nPlease',string)[1]){
    spl <- strsplit(string, "\nPlease", fixed = TRUE)
    string<-sapply(spl, "[", 1)
  }
  # if(grepl('[{]',string)[1]){
  #   spl <- strsplit(string, "[{]", fixed = TRUE)
  #   string<-sapply(spl, "[", 1)
  # }
  if(length(string)==2) string<-string[1]
  
  
  # exceptions
  string<-gsub('Not visible, ',' ', string)
  string<-gsub('Precut Mix of Red Peppers, Zucchinis, Leeks, and Yellow Carrots','Precut Mix of Red Peppers Zucchinis Leeks and Yellow Carrots', string)

  
  # remove comments starting //
  string <- gsub("//.*?\\\n", "", string)
  string <- gsub("//.*?\\.", "", string)
  string <- gsub("//.*?\\,", "", string)
  string <- gsub("//.*?\\}", "}", string)
  # remove jsons
  string <- gsub("^```json\\n|\\n|\\s{2,}", "", string)
  string <- gsub("```.*$|```$", "", string)
  # remove brackets
  string <- gsub("\\[|\\]", "", string)
  string <- gsub("\\},\\{", ",", string)
  string <- gsub("\\(.*?\\)", "", string) # remove everything between brackets
  # string <- gsub("//.*?\n", "", string)
  # gsub("\\(.*?\\)", "", json_data[[i]]) # remove everyting between brackets
  # gsub("//.*?\\.", "", json_data[[i]])
  add_quotes <- function(string) {
    string <- gsub('(:\\s*)([^",}]+)(\\s*[},])', '\\1"\\2"\\3', string)
    string <- gsub('([^"])(})([^}])', '\\1"\\2\\3', string)
    return(string)
  }
  string <- add_quotes(string)
  string <- gsub('\\}\\\"\\}', '\\}\\}', string)
  string <- gsub(', ,', ',', string)
  string <- gsub(', }', '}', string)
  
  # add closure json
  # if(){
  #   spl <- unlist(strsplit(string, "\n\n", fixed = TRUE))
  #   string<-spl[grep('json\n',spl)]
  # }
  
  json<-rjson::fromJSON(string)
  
  return(json)
}

extract_ingr_quant<-function(json, search_terms){
  ingredients <- c()
  quantities <- c()
  
  process_list <- function(x) {
    # if (length(x)==2 & is.list(x)) x<-c(x[[1]],x[[2]])
    
    
    for (name in names(x)) {
      # 
      # print(name)
      # print(item)
      # print(names(x)[duplicated(names(x))])


      # if the items occurs multiple time (like in nested lists of plates)
      if(name %in% names(x)[duplicated(names(x))]){
        n_dups_name<-sum(str_count(ingredients,name)) # becomes higher as ingr is created
        item<-ifelse(n_dups_name==0,
               x[[name]], # first name that is duplicated later
                x[duplicated(names(x))][[name]]) # duplicates # [n_dups_name]
      }else{
        item <- x[[name]]
      }

      if (is.list(item)) {
        
        # If the item is a list, add the name and an empty quantity
        ingredients <<- c(ingredients, name)
        # print(ingredients)
        quantities <<- c(quantities, "")
        # print(quantities)
        # Recursively process the sublist
        process_list(item)
        # After processing the sublist, add an empty entry to both vectors
        ingredients <<- c(ingredients, "}")
        quantities <<- c(quantities, "")
      } else {
        # If the item is a value, add the name and the value
        ingredients <<- c(ingredients, name)
        quantities <<- c(quantities, item)
      }
    }
  }
  process_list(json)
  

  # if name of higher category also in lower list, delete that one
  if(is.list(json) & is.list(json[[1]])){
    if(is.list(ingredients) & 'x' %in% names(ingredients)){
      ingredients1<-ingredients$x
      quantities<-ingredients$y
      ingredients<-ingredients1
    }else{
      
      if(length(names(json)[1])>1) print(paste('ERROR! nested list but multiple names:',x))
      if(!(names(json)[1] %in% names(json[[1]]))){ # take only first
        to_delete <- grepl(names(json)[1], ingredients, ignore.case = TRUE) # remove |description
        ingredients <- ingredients[!to_delete]
        quantities <- quantities[!to_delete]
      }
    }
  }

  # if the lower list is higher category
  # ingredients <- gsub('^[[:space:]]+|"|[[:punct:]]', '', ingredients)
  # quantities <- gsub('^[[:space:]]+|"|[[:punct:]]', '', quantities)
  
  # delete plate format
  if(TRUE %in% grepl('soup', ingredients, ignore.case = TRUE)) print(json)
  to_delete <- grepl(search_terms, ingredients, ignore.case = TRUE) # remove |description
  ingredients <- ingredients[!to_delete]
  quantities <- quantities[!to_delete] ## LATER ADDED WHEN ANALYZING O-NHF!!
  # if(names(json))
  # assume that all subnames are with the word (only check one)
  if(TRUE %in% grepl(search_terms, tolower(names(json[[1]])))){
    quantities <- quantities[quantities != '']
    ingredients <- ingredients[which(ingredients != "" & ingredients != "}" & tolower(ingredients) !="description")]
    # if(length(quantities)==length(ingredients)) n
  }else{
    if(length(ingredients)==length(quantities))  quantities <- quantities[!to_delete]
  } 

  # use remaining categories of ingredients
  indices_to_process <- which(quantities=="" & ingredients != "" & ingredients != "}" & tolower(ingredients) !="description")
  indices_to_delete <- c()
  for (i in indices_to_process) {
    current_ingredient <- ingredients[i]
    # Start from the next index
    j <- i + 1
    while (j <= length(ingredients)) {
      if (ingredients[j] == "" || ingredients[j] == "}") {
        # Stop replacing when an empty string or '}' is encountered
        break
      } else {
        # Replace ingredients[j] with current_ingredient
        ingredients[j] <- current_ingredient
      }
      j <- j + 1
    }

    indices_to_delete <- c(indices_to_delete, i)
  }
  if (length(indices_to_delete) > 0) {
    ingredients <- ingredients[-indices_to_delete]
    quantities <- quantities[-indices_to_delete]
  }
  
  # delete json format
  to_delete <- grepl("}", ingredients, ignore.case = TRUE)
  ingredients <- ingredients[!to_delete]
  quantities <- quantities[!to_delete]
  
  constr <- zipr(ingredients, quantities)
  
  return(constr)
}


# Helper function to escape special characters in regex patterns
escape_special_chars <- function(text) {
  return(str_replace_all(text, "([\\$\\.\\*\\+\\?\\(\\)\\[\\{\\\\\\|])", "\\\\\\1"))
}

# Function to convert fractional quantities to numeric
convert_fraction <- function(x) {
  if (is.na(x)) return(NA)
  if (str_detect(x, "/")) {
    parts <- str_split(tolower(gsub('[a-z]','',x)), "/", simplify = TRUE)
    return(as.numeric(parts[1]) / as.numeric(parts[2]))
  } else {
    return(as.numeric(x))
  }
}

get_avg_from_range<-function(elems){

  # for(elem in elems){
  #   # elems<-elems[grepl('-',elems)]#gsub('-',' ', elem)
  # }
  elems1<-unlist(strsplit(elems,'-'))
  elems1<-as.numeric(find_numbers(elems1))

  if(length(elems1)==2) return((elems1[1]+elems1[2])/2)
  if(length(elems1)!=2) return(elems1)
}

get_grams_from_json<-function(df, colname, chatnr){
  cols <- df %>% select(starts_with(paste0(colname, chatnr,'_'))) %>% colnames()
  # print(cols)
  # JOSUA: I removed the lapply because some grams were not correctly calculated
  # we need the get-n-gram function to take into account ranges, pieces etc
  if(colname=='quantity'){
    for(col in cols){
      for(i in seq(1, nrow(df))){
        # print(i)
        quantity<-get_n_grams(df[i,col], indicators = c('slices','leaves','pieces','each','few'))
        df[i,col]<-ifelse(length(quantity)>0, as.numeric(quantity), NA)
      }
    }
  }
  
  if(colname=='conversion'){
    for(col in cols){
      for(i in seq(1, nrow(df))){
        # print(i)
        # print(col)
        quantity<-get_n_grams(df[i,col], indicators = c('slices','leaves','pieces','each','few', 'cloves'))
        df[i,col]<-ifelse(length(quantity)>0, as.numeric(quantity), NA)
      }
    }
  }
  df[cols]<-sapply(df[cols], as.numeric)
  # print(df[i,cols])
  totalgrams<-paste0('TotalGrams',chatnr)
  df[totalgrams] <- rowSums(df[cols], na.rm = T)
  # df[df[totalgrams] == 0,totalgrams] <- NA
  gramsdiff<-paste0('GramsDiff',chatnr)
  df[gramsdiff] <- df['TotalGramsSelf'] - df[totalgrams]
  df[paste0('AbsDiff', chatnr)] <- abs(df[gramsdiff])
  return(df)
}

convert_to_grams <- function(df, chatnr, transformations) {
  
  # negligible<-c('seasoning','coating','remnant','remnants','residue','spread','traces')
  
  # Iterate over each row in the data frame
  for (i in 1:nrow(df)) {
    # print(i)
    # print(df$PhotoName[i])  
    for (j in 1:24) { # number of ingredients is maximally 10 -> changed to 24
      # print(j)
      if (chatnr=='1'){
        ingredient_col <- paste0("ingredient1_", j)
        quantity_col <- paste0("quantity1_", j)
        conversion_col <- paste0("conversion1_", j)
      }
      if (chatnr=='2'){
        ingredient_col <- paste0("ingredient2_", j)
        quantity_col <- paste0("quantity2_", j)
        conversion_col <- paste0("conversion2_", j)
      }
      if (chatnr=='3'){
        ingredient_col <- paste0("ingredient3_", j)
        quantity_col <- paste0("quantity3_", j)
        conversion_col <- paste0("conversion3_", j)
      }
      if (chatnr=='4'){
        ingredient_col <- paste0("ingredient4_", j)
        quantity_col <- paste0("quantity4_", j)
        conversion_col <- paste0("conversion4_", j)
      }
      if (chatnr=='5'){
        ingredient_col <- paste0("ingredient5_", j)
        quantity_col <- paste0("quantity5_", j)
        conversion_col <- paste0("conversion5_", j)
      }
      if (chatnr=='6'){
        ingredient_col <- paste0("ingredient6_", j)
        quantity_col <- paste0("quantity6_", j)
        conversion_col <- paste0("conversion6_", j)
      }
      
    
      if (ingredient_col %in% colnames(df) && quantity_col %in% colnames(df)) {
        ingredient <- str_trim(tolower(df[[i, ingredient_col]]))
        ingredient <- str_replace_all(ingredient,'_',' ')
        quantity <- df[[i, quantity_col]]

        if (!is.na(ingredient) && !is.na(quantity)) {
          if(ingredient=="spinach or greens") ingredient<-'spinach'
          if(ingredient=="sauce/herbs"|ingredient=="creamy dressing"|ingredient=="dressing/sauce"|ingredient=="curry sauce"|ingredient=="tomato sauce remnants") ingredient<-'sauce'
          if(ingredient=="meat sauce"|ingredient=="ground_meat_sauce"|ingredient=='sauce/gravy'|ingredient=="gravy_sauce") ingredient<-'sauce'
          if(ingredient=="tomato-based sauce") ingredient<-'tomato sauce'
          if(ingredient=='tortilla remnants') ingredient<-'tortilla'
          if(ingredient=="diced red bell peppers") ingredient<-'red bell pepper'
          if(ingredient=="red bell peppers") ingredient<-'red bell pepper'
          if(ingredient=="green bell peppers") ingredient<-'green bell pepper'
          if(ingredient=="mashed potatoes with spinach") ingredient<-'potato'
          if(ingredient=="mashed potatoes with greens"|ingredient=="mashed sweet potatoes") ingredient<-'potato'
          if(ingredient=="chicken or meat pieces"|ingredient=="chicken with mushroom sauce"|ingredient=='chicken in sauce'| ingredient=="chicken remnants") ingredient<-'chicken'
          if(ingredient=='breaded chicken'|ingredient=="breaded chicken pieces"|ingredient=="breaded_chicken"|ingredient=='fried chicken pieces') ingredient<-'chicken'
          if(ingredient=="rice and vegetable mix") ingredient<-'vegetable'
          if(ingredient=="pesto sauce remnants") ingredient<-'sauce'
          if(ingredient=="crumbled cheese"| ingredient=="cheese crumbles"|  ingredient== "cream cheese"|ingredient== "feta cheese"|ingredient=="cheese sauce"|ingredient=="cheese_sauce") ingredient<-'cheese'
          if(ingredient=="sweet potato fries") ingredient<-'fries'
          if(ingredient=="sweet potato wedges") ingredient<-'potato wedges'
          if(ingredient=="chopped herbs") ingredient<-'herbs'
          if(ingredient=="tomato soup"|ingredient=="soup with vegetables") ingredient<-'soup'
          if(ingredient=="salad with dressing"|ingredient=="salad greens") ingredient<-'salad'
          if(ingredient=="orzo pasta"|ingredient=="spaghetti squash") ingredient<-'pasta'
          if(ingredient=="oil remnants") ingredient<-'remnants'
          if(ingredient=="ground meat or tofu") ingredient<-'tofu'
          if(ingredient=="breaded fish") ingredient<-'fish'
          if(ingredient=="breaded_meat") ingredient<-'meat'
          if(ingredient=="cauliflower rice") ingredient<-'cauliflower'
          if(ingredient=="pizza crust remnants") ingredient<-'pizza'
          if(ingredient=="rice or cauliflower rice") ingredient<-'rice'
          if(ingredient=="black olives") ingredient<-'olives'
          if(ingredient=="breaded spinach filling") ingredient<-'spinach'
          if(ingredient=="breaded spinach filling") ingredient<-'spinach'
          if(ingredient=="jalapeños") ingredient<-'jalapenos'
          

          print(ingredient)
          print(quantity)
          print(i)
          print(j)
          
          # Create the conversion column if it doesn't exist
          if (!(conversion_col %in% colnames(df))) {
            df[conversion_col] <- NA
          }
          
          if(TRUE %in% str_detect(ingredient, tolower(transformations$ingredient))){
            # print(paste0('case: ', as.character(i)))
            
            ingredient_rows<-transformations[str_detect(ingredient, escape_special_chars(tolower(transformations$ingredient))),]
            ingredient_rows$term<-str_trim(ingredient_rows$term)
            
            quantity_pattern<-str_trim(tolower(sub("[^[:alpha:]]+", "", quantity)))
            if(quantity_pattern== "chicken bones small pieces of herbs small food remnants"|ingredient=="vegetable stew"){
              df[i, conversion_col] <- NA
              next
            }
            term_pattern <-ingredient_rows$term[ingredient_rows$term==quantity_pattern]
            if(length(term_pattern)==0){
              term_list <- str_extract_all(quantity_pattern,str_trim(ingredient_rows$term[ingredient_rows$term!='*']))
              for(ls in 1:length(term_list)){
                if(length(term_list[[ls]])==1){
                  term_pattern <- term_list[[ls]] # what if there are multiple???
                }
              }
            }
            if(length(term_pattern)>1){
              term_pattern<-ingredient_rows$term[ingredient_rows$term==quantity_pattern & ingredient_rows$ingredient==ingredient]
            }
            if(length(term_pattern)>1){
              term_pattern<-ingredient_rows$term[ingredient_rows$term==quantity_pattern][1] # bell pepper comes first before pepper
            }
            if(quantity_pattern=='*'){
              df[i, conversion_col] <- 0
              next
            }
           
            # if only number
            if (str_trim(tolower(gsub('[0-9]','',quantity)))==''|str_trim(tolower(gsub('[0-9]','',quantity)))=='.'){
             
              quantity_value<-as.numeric(str_extract(quantity, "\\d+\\.*\\d*|\\d+/\\d+"))
              factor_1 <-ingredient_rows$factor[ingredient_rows$ingredient==ingredient & ingredient_rows$term=='1']
              
              if(length(factor_1)==0){
                factor <- 1
              }else{
                factor<-as.numeric(factor_1)
              }
              # factor<-as.numeric(ingredient_rows$factor[ingredient_rows$ingredient==ingredient & ingredient_rows$term=='1'])
              
              # if in transformation list 
              # if(length(nchar(factor))==0){
              #   
              #   next
              # }
              # otherwise assume that number is grams
              # if(length(nchar(factor))==0){
              #   print(quantity_value)
              # 
              # }
              df[i, conversion_col] <- quantity_value * factor
              next
              
            }
            
            if(TRUE %in% str_detect(quantity_pattern,ingredient_rows$term[ingredient_rows$term!='*'])){
              if(str_detect(quantity_pattern, term_pattern)) {
                if (str_detect(quantity, "/")) {
                  quantity_value <- convert_fraction(quantity)
                  factor<-ingredient_rows$factor[ingredient_rows$term==quantity_pattern]
                  if(length(nchar(factor))>1){
                    factor<-as.numeric(ingredient_rows$factor[which(str_detect(term_pattern, ingredient_rows$term[ingredient_rows$term!='*']))])
                  }
                  if(length(nchar(factor))>1){
                    factor<-as.numeric(ingredient_rows$factor[ingredient_rows$ingredient==ingredient & ingredient_rows$term==term_pattern])
                  }
                  if(length(nchar(factor))>1) print(paste0('error with multiple ingredients:', as.character(i)))
                  df[i, conversion_col] <- quantity_value * factor
                  
                }else{
                  quantity_value<-as.numeric(str_extract(quantity, "\\d+\\.*\\d*|\\d+/\\d+"))
                  factor<-as.numeric(ingredient_rows$factor[ingredient_rows$term==term_pattern])
                  
                  if(length(nchar(factor))>1){
                    factor<-as.numeric(ingredient_rows$factor[which(str_detect(term_pattern, ingredient_rows$term[ingredient_rows$term!='*']))])
                  }
                 
                  if(length(nchar(factor))>1){
                    factor<-as.numeric(ingredient_rows$factor[ingredient_rows$ingredient==ingredient & ingredient_rows$term==term_pattern])
                  }
                  if(length(nchar(factor))>1) print(paste0('error with multiple ingredients:', as.character(i)))
                  df[i, conversion_col] <- quantity_value * factor
                }
                next
              }
            }
          }
          # if(FALSE %in% str_detect(ingredient, transformations$ingredient)){
          #   df[i, conversion_col] <- as.numeric(str_extract(quantity, "\\d+\\.*\\d*|\\d+/\\d+")) 
          #   # if(grep('gram',quantity)){
          #   #   df[i, conversion_col] <- as.numeric(str_extract(quantity, "\\d+\\.*\\d*|\\d+/\\d+")) 
          #   # }else{
          #     
          #   # } 
          # }
        }
      }
    }

  }
  return(df)
}


  
### function to categorize
categorize_ingredients<-function(ingredient){
  ingredient<-tolower(ingredient)
  ingredient<-gsub('[[:space:]]+\"|\"','',ingredient) # for the prompts related ingredients
  ingredient <- str_replace_all(ingredient,'_',' ')
  
  ## if multiple terms, the first term noted below will be used for categorization
  ingredient<- 
    #protein
    ifelse(grepl("roll|bun$|buns$|^sub$", ingredient)==T, 'bread(crumbs)', # to make sure that breaded chicken and burger bun are correctly classified
         ifelse(grepl("bouillon|broth|stock|soup|water", ingredient)==T, 'water or broth', 
           ifelse(grepl("chicken|turkey|duck", ingredient)==T, 'poultry',# ifelse(grepl("lamb", ingredient)==T, 'lamb', 
               ifelse(grepl("pork|bacon|bratwurst|varkenshaaspuntjes|prok|specks|cross rib roast|cevapcici", ingredient)==T, 'pork',
                      ifelse(grepl("kofte|köfte|kräuter/gewürze|veal|kebab|ground_beef|beef|steak|steack|entrecote|lamb|american filet|escalope|stew|merguez", ingredient)==T, 'beef, veal or lamb', 
                             ifelse(grepl("meat|ground meat|ground_meat|minced meat|hamburger|burger|patty|schnitzel|skewer|skewers|chipolata|chipolatas|dumpling|sausages|sausage", ingredient)==T, 'ground (undifferentiated) meat',
                                    ifelse(grepl("^ham$|slices ham|chorizo|smoked ham|jerk|parma|pancetta|pepperoni|diced ham|salami|serrano ham", ingredient)==T, 'charcuterie',
                                           ifelse(grepl("prawn|scallop|scallops|fish$|catfish|cooked fish|pangasius|mussel|skin|cod|hake|salmon|pollack|fish|shrimp|barramundi|prawns|trout|tilapia|scampi|tempura mix|tuna|fillet|fillets|fish sticks|crab sticks|fish stir-fry|imitation crab|anchovies", ingredient)==T, '(shell)fish', # assume that skin refers to fish
                                                  ifelse(grepl("tofu|tempeh|beyond meat|seitan|vegetarian strips|falafel|plant-based meat", ingredient)==T, 'meat substitutes',
                                                         ifelse(grepl("bouillon|broth|stock|soup|water", ingredient)==T, 'water or broth', 
                                                                ingredient))))))))))
  
  ingredient<-
    # veggies
      ifelse(grepl("precut mix|gemüsestücke|dauphinoise gratin|grüne blätter|green_leaves|herb or vegetable pieces|vegetable cubes|white vegetable sticks|vegetable or|other vegetables|^vegetable$|greens|mashed red/purple vegetable|red vegetable/pepper|vegetable mix|mix of vegetables|vegetable pieces|pieces of vegetables|mixed greens|mirepoix|^vegetable pieces$|diced vegetables|vegetable stems|vegetables|green vegetable", ingredient)==T, 'vegetable mix',            
             ifelse(grepl("potatoes|potato|french fries|fries|french_fries|patatas bravas|french fry|french_fry|fried item|mashed potatoes", ingredient)==T, 'potatoes or fries', 
           ifelse(grepl("tomatoes|tomato|passata|tomtatoes|tomaten", ingredient)==T, 'tomatoes', 
                  ifelse(grepl("lettuce|arugula|mixed salad|field salad|romana salad|mesclun|rocket|spring mix|salad|caesar salad|grüne blätter|green leaves|^slaw$", ingredient)==T, 'lettuce',
                         ifelse(grepl("cabbage|kale|choy|choi|coleslaw|sauerkraut|chimichurri", ingredient)==T, 'cabbage', 
                                ifelse(grepl("^corn$|sweetcorn|corn kernel|corn kernels|canned corn|cooked corn|polenta|vegetable/corn", ingredient)==T, 'corn',
                                       ifelse(grepl("bell pepper|bell_pepper|red_bell_pepper|bell_peppers|bellpepper|green_peppers|roasted peppers|yellow_peppers", ingredient)==T, 'bell pepper', 
                                              ifelse(grepl("spinach", ingredient)==T, 'spinach', ifelse(grepl("asparagus", ingredient)==T, 'asparagus', 
                                                      ifelse(grepl("butternut|squash|pumpkin", ingredient)==T, 'squash', 
                                                             ifelse(grepl("broccoli|brocolini", ingredient)==T, 'broccoli',
                                                                    ifelse(grepl("carrot|beet|radish|orange vegetable|carrots|karotten|jicama sticks|jicamasticks|chicory|turnips|bamboo shoots|kohlrabi|parsnip", ingredient)==T, 'root vegetables',
                                                                           # ifelse(grepl("parsnip", ingredient)==T, 'parsnip',
                                                                                  ifelse(grepl('leak|leek', ingredient)==T,'leek',
                                                                                         ifelse(grepl('artichoke', ingredient)==T,'artichoke',
                                                                                     ifelse(grepl('mushrooms|mushroom|champignons|champions', ingredient)==T,'mushrooms',
                                                                                            ifelse(grepl('cucumber|pickle', ingredient)==T,'cucumbers',
                                                                                                   ifelse(grepl('celery|celeriac mash', ingredient)==T,'celery',
                                                                                                          ifelse(grepl('zucchini|zuchinni|courgette', ingredient)==T,'zucchini',
                                                                                                               ifelse(grepl('eggplant', ingredient)==T,'eggplant',
                                                                                                                  ifelse(grepl('cauliflower', ingredient)==T,'cauliflower',
                                                                                                                         ifelse(grepl("endive|endives|krulandijvie|kurlandijvie", ingredient)==T, 'endive',
                                                                                                                                ifelse(grepl('guacamole|avocado', ingredient)==T,'avocado',
                                                                                                                                       ingredient))))))))))))))))))))))
  
  ingredient<-
    # bread 
    ifelse(grepl("flatbread|tortilla|toritillas|naan|naan bread|sandwich|burrito|taco|taco shell", ingredient)==T, 'flatbread',         
           ifelse(grepl("^brot$|ciabatta|bread|flatbread|roti|wrap|roll|bun$|buns$|brioche|baguette|bread crumbs|breadcrumbs|crumbs|crumbles|crust|dough|crouton|^crumb$|paneer|^sub$", ingredient)==T, 'bread(crumbs)', 
                  # legumes, rice etc
                  # ifelse(grepl('polenta|mashed potatoes', ingredient)==T,'cooked potatoes or grains',
                  ifelse(grepl('pizza', ingredient)==T,'pizza',
                        ifelse(grepl('rice|risotto', ingredient)==T,'rice',
                         ifelse(grepl('lentils|peas|beans|edemame|pea|green bean|bean|edamame|entil', ingredient)==T,'legumes',
                                ifelse(grepl('flour|^flower$|plain flower', ingredient)==T,'flour',
                                       ifelse(grepl('noodles|noodle|spätzle|fresh spaetzle|udon', ingredient)==T,'noodles',
                                              ifelse(grepl('orzo|tagliatella|cannelloni|tagliatelle|tortelloni|tortellini|gnocchi|macaroni|farfalle|lasagna|fusilli|cavatappi|rigatoni|penne|linguine|spaghetti|pappardelle|pasta|ravioli', ingredient)==T,'pasta',
                                                     ifelse(grepl('quinoa|couscous|bulgur|grains', ingredient)==T,'quinoa, couscous or bulgur',
                                                            ingredient)))))))))
  
  ingredient<-
    # diary & nuts 
    ifelse(grepl("cheese|pecorino|chese|danablu|grana padano|halloumi|käsereste|ricotta|burrata|parmesan|gouda|cheddar|feta|gorgonzola|mozarella|mozzarella|parmigiano|kaas|käsereste|emmental", ingredient)==T, 'cheese',
        ifelse(grepl("^egg$|hen s eggs|hens egg|3 egg|hen's eggs|^eggs$|cooked eggs|yolk|eggwhite|scrambled|omelette|fried egg|cooked egg", ingredient)==T, 'eggs', 
           ifelse(grepl("cream|fraiche|fraîche|mayonnaise|aioli|schmand|yoghurt|yogurt|milk|stracciatella|dairy (cream)", ingredient)==T, 'dairy (cream)',
                ifelse(grepl("^nut$|nuts|pecan|hazelnut|hazelnuts|almons|almond|almonds|raisins|pine nut|hummus|walnut|walnuts|cashew|cashews", ingredient)==T, 'nuts or raisins',
                      ingredient))))
  
  ingredient<- 
    #seasoning
    ifelse(grepl("seasoning|spice|spices|gochujang|paste|nutmeg|powder|dukkah|salt and pepper|salt|za atar|black pepper|^pepper$|sal and pepper|sugar|bbqpepper mix|bbq pepper mix|bbq-pepper mix|cracked pepper", ingredient)==T, 'spices', 
           ifelse(grepl("saffron|cinnamon|oregano|cumin|paprika|fennel|anise|truffle|sumac|garam masala|zaatar|italian season|turmeric|dukkah mix|chilli flake|chili flake|flakes|wasabi", ingredient)==T, 'spices', 
                  ifelse(grepl("^herb|fresh herb|green herb|herbs|parsely|parsley|parlsey|cilantro|thyme|rosemary|rosemar|basil|dill|coriander|mint|^sage$|tarragon|peterselie", ingredient)==T, 'herbs',            
                         ifelse(grepl("garlic", ingredient)==T, 'garlic', 
                                ifelse(grepl("onion|onions|chive|shallot|scallions|sallions", ingredient)==T,'onion', 
                                       ifelse(grepl("cayenne pepper|jalapeño|jalapeno|red pepper|red_peppers|red_pepper|green pepper|chilli|chili|yellow pepper|poblano pepper|red pointed pepper|hot pepper", ingredient)==T, 'peppers',
                                              ifelse(grepl("cornstarch|ginger|leaf|gomasio", ingredient)==T, 'seasoning', 
                                                     ifelse(grepl("oil|oils|butter|coconut|gravy|grease", ingredient)==T, 'fats or oils', # peanut butter incl
                                                            ifelse(grepl('lime|lemon', ingredient)==T,'lemon or lime',
                                                                   # sauce & toppings
                                                                   ifelse(grepl("champignonsaus|teriyaki|marinade|salsa|dip|pizza base|sauce|pesto|jam$|purple puree|syrup|filling|marmalade|honey|jelly|ketjap manis|sriracha|chutney|ketchup|curry", ingredient)==T, 'sauce',
                                                                          ifelse(grepl("mustard|vinegar|balsamic|vinaigrette|wine|trappist|beer|dressing|miso", ingredient)==T, 'vinaigrette',
                                                                                 ifelse(grepl("seeds|seed|pumpkin_seeds|garnish|cranberries|sprouts|seaweed|caramel|chips|doritos|nachos|snack|capers|muffin|sides", ingredient)==T, 'seeds or other toppings',
                                                                                        ifelse(grepl("olives|olive", ingredient)==T, 'olives', 
                                                                                               ingredient))))))))))))) 
  
  ingredient<-
    # fruits
    ifelse(grepl("apple (diced)|^apples$|of apple|^apple|red apple", ingredient)==T, 'apple', 
           ifelse(grepl("pear|pears", ingredient)==T, 'pear',ifelse(grepl("banana|bananas", ingredient)==T, 'banana',
              ifelse(grepl("kiwi|kiwis", ingredient)==T, 'kiwi',
                     ifelse(grepl("pineapple", ingredient)==T, 'pineapple',
                            ifelse(grepl("apricots|dried fruits", ingredient)==T, 'dried fruits',
                            ingredient))))))
  ingredient<-
    ifelse(grepl('residue|remnants|crumbled|brown substance|unidentified|orange bits|^bits$|unidentifiable|crumb-like|crumbled food pieces|orange particles|other food particles|other non-food items', ingredient)==T, NA, # CHECK restricted pieces
           ifelse(grepl('red-colored ingredient|mixedingredients|mashed or pureed food|plate|paper napkin|other ingredients|black-colored ingredient|small particles|small particle|food particles|food particle', ingredient)==T, NA, 
                  ifelse(grepl('comments|knife|cooked cubes|crispy skin|white item|fork|scraps|enchilada|burnt edges|white ingredient bits|^other$|not visible|crumb particles', ingredient)==T, NA, ingredient)))
  
  
  return(ingredient)
}


count_occurrences <- function(df, reference_string, checking_string, type = "TP") {
  # Filter the reference columns
  reference_cols <- grep(reference_string, names(df), value = TRUE)
  # print(reference_cols)
  # Filter the checking columns
  checking_cols <- grep(checking_string, names(df), value = TRUE)
  # print(checking_cols)
  # Initialize a vector to store the count of occurrences
  count_vector <- numeric(nrow(df))
  
  # Iterate over each row
  for (i in 1:nrow(df)) {
    # Extract the reference (actual) values for the current row
    reference_values <- as.character(df[i, reference_cols])
    reference_values <- unique(reference_values[!is.na(reference_values)])
    reference_values <- reference_values[reference_values != "NA"]
    # print(reference_values)
    # Extract the checking (predicted) values for the current row, remove NAs, and get unique values
    checking_values <- as.character(df[i, checking_cols])
    checking_values <- unique(checking_values[!is.na(checking_values)])
    checking_values <- checking_values[checking_values != "NA"]
    # print(checking_values)
    # Count according to the type
    if (type == "TP") {
      # True Positives: In both reference and checking values
      count_vector[i] <- sum(checking_values %in% reference_values)
    } else if (type == "FP") {
      # False Positives: In checking values but not in reference values
      count_vector[i] <- sum(!checking_values %in% reference_values)
     } #else if (type == "TN") {
    #   # True Negatives: Neither in reference nor in checking values
    #   count_vector[i] <- sum(
    #     !(reference_values %in% checking_values) & 
    #       !(checking_values %in% reference_values)
    #   )
    # } else if (type == "FN") {
    #   # False Negatives: In reference values but not in checking values
    #   count_vector[i] <- sum(!reference_values %in% checking_values)
    # }
  }
  
  return(count_vector)
}

# get_avg_from_range<-function(elems) {
#   for(elem in elems){
#     # elems<-elems[grepl('-',elems)]#gsub('-',' ', elem)
#     elems<-unlist(strsplit(elems,'-'))
#     elems<-as.numeric(elems)
#   }
#   return((elems[1]+elems[2])/2)
# }

check_unconverted<-function(df){
  all_unconverted<-NULL
  for(i in 1:7){
    # print(i)
    nameconv <- paste('conversion1', i, sep='_')
    # print(nameconv)
    nameingr <- paste('ingredient1',i,sep='_')
    namequan <- paste('quantity1',i,sep='_')
    df[nameingr]<-tolower(df[,nameingr])
    
    list_not_converted<-df[is.na(df[nameconv]), c(nameingr, namequan)]
    colnames(list_not_converted)<-c('ingredient','quantity')
    # print(unique(list_not_converted))
    all_unconverted<-rbind(all_unconverted,list_not_converted[!is.na(list_not_converted$ingredient),])
  }
  print(unique(all_unconverted[order(all_unconverted$ingredient),]))
  print(nrow(unique(all_unconverted)))
}
