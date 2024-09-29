

# set up env

config <- list(
 #  personal_lib = "C:\\Users\\cecetov\\Documents\\R\\win-library\\3.5",
  data_path = "C:\\Users\\LENOVO\\Desktop\\GigaHack\\Projects_Assets_2\\R_GH_24" 

  )

# .libPaths( new = config$personal_lib )
# .libPaths()

setwd( config$data_path )


## Upload libraries

## eda tools
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(stringr)

library(ggplot2)
library(corrplot)

library(lubridate)

library(caret)  # train test data

# library(plyr)
# library(doParallel)

 # nlp tools
library(tidytext)
library(vader)
library(textstem)
library(sentimentr)
library(lexicon)



# python related tools
# library(reticulate)
# reticulate::use_python(config$python_path)

library(prophet) # for forecasting



#### Next , EDA of raw data in excel file ----

# upload  excel with outages 

d1_raw <- read_excel( "repair_cases_fv.xlsx", sheet = "import" )

# debug
.udf_calc_na_val(d1_raw)

glimpse(d1_raw)

dim( d1_raw )

## trim before translation 
#d1_raw$Reported_Defect <- trimws ( tolower( d1_raw$Reported_Defect  ) )

#d1_raw$Diagnostic_Description <- trimws(tolower( d1_raw$Diagnostic_Description  ))

#d1_raw$Operation_Description <- trimws(tolower( d1_raw$Operation_Description  ))

# sum( duplicated( d1_raw$com_id ) ) # must be 0


## Continue, change names of Regions

d1_raw_2 <- d1_raw


d1_raw_2 <- d1_raw_2 %>%
  mutate(Agency = case_when(
    Agency == '53b66e32e2eaa443dc7258830c27dfe8:05a91cab0cabc729d09d4c7558' ~ 'Agency_1',
    Agency == '795f43dc01a1575ad0916652d6a0ebd5:5bc3fd0775eeeae64e78e9747bbac553' ~ 'Agency_2',
    Agency == '52b7ca8de11e6edcd6a9207a1cb1254c:fcfc0c798dd0b17c1fafd9352bc7fb' ~ 'Agency_3',
    Agency == '9d4841207a157e78ace69ae80eb77b2a:e849cb568387cf6f968915a4' ~ 'Agency_4',
    Agency == '920ffe8e13a96f20a57a248235b1f54b:c1ae9cc62cc88d8d38ea11c4' ~ 'Agency_5',
    Agency == '4146535e51cbb230233e8e36e3a8d828:aef198e9e9a0ecb40a71170a4b28' ~ 'Agency_6',
    Agency == '8e684482d8bc56433e74bdf6e76e9239:947d046e590695aff75eb7' ~ 'Agency_7',
    Agency == '449e6f200982d0da36de7bca6d6b3666:3bf51ee2aad574f04a51d4909d9a6a1e' ~ 'Agency_8',
    Agency == 'e21961aa31f0637e6bf2f3b215f78a8e:dde3e3aee036e73bc91944' ~ 'Agency_9',
    Agency == 'f29273713f544231eba5b026a517625c:7933fccd207087a1c039b0' ~ 'Agency_10',
    Agency == 'dd3c58dd2fbbb75b5a5ed5980f67d70f:a23a64500f0f8d54ff9b9a1a' ~ 'Agency_11',
    Agency == 'b7310b019a3b141fa4f1fdd374be6a4a:6b105a10d75e94673fd4dcb8' ~ 'Agency_12',
    Agency == '1b0773f54f27006efbe7d533eedf58e7:0a86f2003607a432ba011493' ~ 'Agency_13',
    Agency == 'baf47797041ca0d144f95c194cab171d:c175e332a4ed238851a5f09336d1b34769' ~ 'Agency_14',
    Agency == '507dc50f38a2d7b8d343bfd6188761f4:772fb2beda2a0a0736cf3f9c7abeb1' ~ 'Agency_15',
    Agency == '34918896516f9a0add9ef9af26432293:71205c92d78d07cd644e143fae' ~ 'Agency_16',
    TRUE ~ Agency  # Keep original value if no match
  ))

unique( d1_raw_2$Agency )

# [1] "Agency_1"  "Agency_2"  "Agency_3"  "Agency_4"  "Agency_5"  "Agency_6"  "Agency_7" 
# [8] "Agency_8"  "Agency_9"  "Agency_10" "Agency_11" "Agency_12" "Agency_13" "Agency_14"
# [15] "Agency_15" "Agency_16"


## Store separately  46 rows which indicate totals

d1_totals <- d1_raw_2 %>% filter( is.na( Ticket_No ) )
dim(d1_totals)
# 45 64  # correct 
# View(d1_totals )


### Keep columns which are related to names of equipment

sku_names <- unique(d1_raw_2$Equipment_Type)
sku_names <- sku_names [ (!is.na(sku_names)) ]

col_select_totals <- c( "Agency" , sku_names )

d1_totals <- d1_totals %>% select( any_of( col_select_totals ) )

## remove duplicates and estimate sums per Agency 

d1_totals_2  <- d1_totals %>%
  group_by(Agency) %>%
  summarize(across(everything(), \(x) sum(x, na.rm = TRUE)))



## Next  Select columns with non missing values ----

buf <- .udf_calc_na_val(d1_raw_2) %>% 
           filter( na_ratio > 0.97 ) %>% select( col_name)

# paste(buf$col_name, collapse = ",")

## estimate number of unique values in character column
d1_raw_2 %>%
  # Select columns of character type
  select(where(is.character)) %>%
  # Calculate the number of unique values in each column and create a tidy dataframe
  summarise(across(everything(), ~n_distinct(.))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "num_unique_values") %>% 
      as.data.frame()

# Reported_Defect
# Diagnostic_Description
# Operation_Description


## Check number fo characters in data to translate
# buf_df <- d1_raw %>% select( buf$col_name )
# glimpse(buf_df)

# sum(nchar(unique(buf_df$Reported_Defect)), na.rm=TRUE) 1 5933 64
# sum(nchar(unique(buf_df$Diagnostic_Description)), na.rm=TRUE) 1 766 084
# sum(nchar(unique(buf_df$Operation_Description)), na.rm=TRUE) # 4 178 008



### Export cleaned dataset for sharing ----

d1_cleaned <- d1_raw_2 %>% select( -c( buf$col_name ) )

dim( d1_cleaned ) # 68879    64 for all

# write.csv( d1_cleaned , "repair_cases_fv_clean.csv" , row.names = FALSE)
# share with team

# View( d1_cleaned )



### Continue, remove rows without tickets and calculate number of cases ----
##  when equipment brakes

glimpse( d1_cleaned )

d1_cleaned_2 <- d1_cleaned  %>% 
  
            filter( !(is.na(Ticket_No)) ) %>%  
   
            group_by( Equipment_Type, Agency  ) %>% 
             
              summarise( num_of_incidents = n_distinct( Ticket_No ) ) %>% 
    
               ungroup()


d1_cleaned_2 <- d1_cleaned_2 %>%
  mutate(ExtractedNumber = case_when(
    grepl("_", Agency) ~ as.integer(sub(".*_(\\d+)$", "\\1", Agency)),
    TRUE ~ NA_integer_  # Handle cases without underscores, if any
  ))



## store data for Power Bi Visual , sheet 1
# path_store <- "C:\\Users\\LENOVO\\Desktop\\GigaHack\\Projects_Assets_2\\R_GH_24"

# write.csv( d1_cleaned_2 , paste(path_store , "v1.csv" , sep ="\\") , row.names = FALSE)



### Export Average repair Time ----

d1_cleaned_3 <- d1_cleaned  %>% 
  
  filter( !(is.na(Ticket_No)) & !(is.na(Equipment_Type))  & !(is.na(Repair_Time)) ) %>%  
  
  group_by( Agency , Equipment_Type  ) %>% 
  
  summarise( avg_rep_time = mean( Repair_Time, na.rm = TRUE ) ) %>% 
  
  ungroup()

.udf_calc_na_val( d1_cleaned_3 )

## extract id to maintain order in Power bi
d1_cleaned_3 <- d1_cleaned_3 %>%
  mutate(ExtractedNumber = case_when(
    grepl("_", Agency) ~ as.integer(sub(".*_(\\d+)$", "\\1", Agency)),
    TRUE ~ NA_integer_  # Handle cases without underscores, if any
  ))


# View(  d1_cleaned_3 )

dim( d1_cleaned_3 )

# path_store <- "C:\\Users\\LENOVO\\Desktop\\GigaHack\\Projects_Assets_2\\R_GH_24"

## store data for sheet 2

# write.csv( d1_cleaned_3 , paste(path_store , "v2_repair.csv" , sep ="\\") , row.names = FALSE)

unique( d1_cleaned_3$Agency  )


### Eq baselines

d1_baselines <- d1_cleaned_3 %>% 
                   group_by( Equipment_Type ) %>% 
                      summarise( mean_rep_time = mean(avg_rep_time ) )

# path_store <- "C:\\Users\\LENOVO\\Desktop\\GigaHack\\Projects_Assets_2\\R_GH_24"

# write.csv( d1_baselines , paste(path_store , "v2_repair_baseline.csv" , sep ="\\") , row.names = FALSE)



#### Continue, NLP topic Modelling ----


## Import stop words 

Afinn <- read_rds( "Afinn_v2.rds" )

stop_words_custom <- stop_words %>% filter( !(word %in% Afinn$word ))
# we excluded intersecting unigrams


# glimpse( d1_cleaned )
text_df <- d1_cleaned %>% select( Equipment_Type,  ENG_Reported_Defect)

# debug
.udf_calc_na_val(text_df)

text_df <- text_df %>% filter( !(is.na( ENG_Reported_Defect )) )

text_df <- text_df %>% rename( com_fv = ENG_Reported_Defect )

text_df <- text_df %>% filter( nchar( com_fv ) > 3  )

# View( text_df )


##  Continue generate lemmas dictionary
lemma_dictionary_lexicon  <- make_lemma_dictionary( text_df$com_fv , 
                                                    engine = "lexicon" )

## Prepare lemmas
tidy_survey <- text_df %>% 
  
  unnest_tokens( word, com_fv ) %>%
  
  anti_join( select( stop_words_custom , word ) , 
             by=c("word") ) %>% # remove words with typos
  
  mutate( lemma = lemmatize_strings( word,
                                     dictionary = lemma_dictionary_lexicon) ) %>% # keep lemmas only
  
  select( -c(word) ) %>% 
  
  anti_join(stop_words_custom , by=c("lemma"="word")) %>%  # count not included
  
  filter( !(grepl("^[0-9]" , lemma, fixed=FALSE) ) )


## Topic frequency
frequency <- tidy_survey %>% 
  
                count(  Equipment_Type, lemma, sort = TRUE )

# topic which appear only once
hapax_legomena <- frequency %>% group_by( lemma ) %>% summarise( vol = sum(n) ) %>% 
  ungroup() %>% filter( vol == 1 )


# View( hapax_legomena )
# View(text_df)
# View( frequency %>%  filter(lemma %in%  hapax_legomena$lemma  )  )

## remove respective words 

frequency <- frequency %>% filter( !(lemma %in% hapax_legomena$lemma) )

# Estimate tf_idf
frequency_tf_idf <- frequency %>% 
                    bind_tf_idf( lemma, Equipment_Type, n )


# Visualise initial result
frequency_tf_idf %>% filter( Equipment_Type %in% c(   "CA","BE")  ) %>%
  
  group_by(Equipment_Type) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  
  ggplot(aes(tf_idf, forcats::fct_reorder(lemma, tf_idf), fill = Equipment_Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ Equipment_Type, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL , title = "Top 30 TF-IDF Scores by Equipment_Type") +
  
  theme(axis.text.y = element_text(size = 10) ,
        plot.title = element_text(size = 10, face = "bold") )


topics_main_driver <- frequency_tf_idf %>% 
  
  group_by(Equipment_Type) %>%
  
  slice_max(tf_idf, n = 50 ) %>% # specific terms
  
  slice_head( n = 25 )  %>% ungroup()# need to rewrite


topics_main_driver <- topics_main_driver %>% distinct()

topics_main_driver <- topics_main_driver %>% 
        filter( tf_idf > 0 & !(is.na( Equipment_Type )) )

## Export data for NLP solution 

# write.csv( topics_main_driver , 
#             paste(path_store , "topics_modelling.csv" , sep ="\\") , row.names = FALSE
#            
#            )
  
           

#### Proceed with time series forecasting ----

# glimpse( d1_cleaned )

d1_cleaned_ts <- d1_cleaned  %>% 
                       select( Request_Creation_Date , Equipment_Type , Ticket_No ) %>% 
    
                       filter( !(is.na( Ticket_No ))  ) %>% 
                       filter( !(is.na( Equipment_Type ))  )


d1_cleaned_ts_v2 <- d1_cleaned_ts %>%
  
  mutate(
    Date_Full = as.Date(Request_Creation_Date),   # Extract the day
    Month = month(Request_Creation_Date), # Extract the month
    Year = year(Request_Creation_Date)    # Extract the year
  )     %>% 
   
            select( -c(Request_Creation_Date) )


d1_cleaned_ts_v2 <- d1_cleaned_ts_v2 %>% 
           group_by(  Month , Year, Equipment_Type) %>% 
  
         summarize( break_vol = n_distinct( Ticket_No ) ) %>% 
  
           ungroup()

# Add date column

d1_cleaned_ts_v2 <- d1_cleaned_ts_v2 %>%
       mutate(Date_Full = as.Date(paste(Year, Month, "01", sep = "-")))

d1_cleaned_ts_v2 <- d1_cleaned_ts_v2 %>% group_by(Equipment_Type,  Date_Full ) %>% 
                        summarise( break_vol = sum(break_vol) ) %>% ungroup()

## generate calendar
date_sequence <- seq(from = min(d1_cleaned_ts_v2$Date_Full), 
                     to = max(d1_cleaned_ts_v2$Date_Full), 
                     by = "month")


## Create cross product to avoid breaks in time series 

date_tibble <- tibble(Date_Full = date_sequence)

date_tibble_full <- date_tibble %>% 
  
       cross_join( 
         
         select( d1_cleaned_ts_v2 , Equipment_Type ) %>% distinct()
         
         )

## debug check max data for each product

date_tibble_full %>% group_by( Equipment_Type ) %>% 
                summarise( max(Date_Full) ) %>% View()

# all till 2024-07-01

### Add actual Data 

date_tibble_full <- date_tibble_full %>% 
      left_join(
        
        d1_cleaned_ts_v2 , 
               
                by=c("Date_Full","Equipment_Type")
        
      )

# debug
.udf_calc_na_val( date_tibble_full )

date_tibble_full <- date_tibble_full %>% 
            replace_na( list( break_vol = 0 ) )

# View(date_tibble_full)


### Continue, arrange data before proceeding to loop 

date_tibble_full <- date_tibble_full %>% 
          select( Equipment_Type, Date_Full , break_vol ) %>% 

           arrange( Equipment_Type ,Date_Full ) 


list_data <- split(date_tibble_full , f = date_tibble_full$Equipment_Type )

# View(list_data[[2]])

i <- 1

## Proceed to prediction with prophet library

for (i in 1:length(list_data)    ) {   
  
  df_buf<-list_data[[i]]
  
  uni_et = df_buf$Equipment_Type[1] # store feature
  
  f_per <- 6
  
  prophet_input<-data.frame( ds=df_buf$Date_Full ,
                             y = df_buf$break_vol )
  
  
  ## Generate predictions
  
  ## to impose monthly seasonality we first assign FALSE values to defaut settings
  m_ini_prophet <- prophet::prophet(yearly.seasonality=FALSE,
                                    weekly.seasonality=FALSE,
                                    daily.seasonality=FALSE,
                                    growth="linear",seasonality.mode = "additive",
                                    n.changepoints=2) # initiate model
  
  
  # now we add montly seasonality
  m_ini_prophet <- add_seasonality(m_ini_prophet, name='monthly', period=12, fourier.order=5,
                                   mode="additive") # add montly fourier terms
  
  m_ini_prophet <- fit.prophet(m_ini_prophet, prophet_input,
                               seed="1234" ,algorithm="Newton" ) # fit the model
  
  
  frst_per_dates <- seq( max(df_buf$Date_Full), by="1 months",length.out = 7 )[-1] # 6 months ahead
  
  
  prophet_future_frame<-prophet::make_future_dataframe(m_ini_prophet,
                                                       length(frst_per_dates),freq="month",
                                                       include_history=FALSE) 
  
  
  prophet_future_frame <- cbind(prophet_future_frame)
  
  # generate forecast 
  prophet_frst <- predict(m_ini_prophet, prophet_future_frame)
  
  m_1_frst<-tail( prophet_frst$yhat , length(frst_per_dates) )
  
  ## data to append 
  
  buf_df_frst <- tibble( Date_Full = frst_per_dates , 
                         Frst_breaks = m_1_frst,
                         Equipment_Type = uni_et
                        )
  
  df_buf <- bind_rows( df_buf , buf_df_frst )
  
 # df_buf$Frst_breaks <- if_else( is.na( df_buf$Frst_breaks) , df_buf$break_vol , df_buf$Frst_breaks ) 
  
  # View(df_buf)
  
  list_data[[i]] <- df_buf
  
  }

fv_frst <- bind_rows( list_data )

## transform to long format for power bi time series 
fv_frst_long <- fv_frst %>%
  pivot_longer(cols = c(break_vol, Frst_breaks),
               names_to = "Type",
               values_to = "Volume")
# View(fv_frst_long)

fv_frst_long <- fv_frst_long %>% filter( !(is.na(Volume)) )

.udf_calc_na_val( fv_frst_long )

fv_frst_long <- fv_frst_long %>%
  mutate(Type = if_else(Type == "break_vol", "Fact_Breakdown", "Prediction_Breakdown"))


### Export for Sheet 3

# write.csv( fv_frst_long , 
#            paste(path_store , "volume_forecast.csv" , sep ="\\") , row.names = FALSE
#            
# )


## Proceed to Cross Validation by specific product ----

selected_equipment <- "CA"  # Replace with the specific equipment type you want to cross-validate
df_selected <- date_tibble_full %>%
  filter(Equipment_Type == selected_equipment) %>%
  arrange(Date_Full)

# Prepare the Prophet input for the selected equipment
prophet_input <- data.frame(ds = df_selected$Date_Full,
                            y = df_selected$break_vol)

# Initiate and fit the Prophet model
m_ini_prophet <- prophet::prophet(
  yearly.seasonality = FALSE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  growth = "linear",
  seasonality.mode = "additive",
  n.changepoints = 2
)

# Add monthly seasonality
m_ini_prophet <- add_seasonality(m_ini_prophet, name = 'monthly', 
                                 period = 12, fourier.order = 5, mode = "additive")

# Fit the model with your data
m_ini_prophet <- fit.prophet(m_ini_prophet, prophet_input, 
                             seed = "1234", algorithm = "Newton")

# Perform cross-validation
df_cv <- cross_validation(
  m_ini_prophet, 
  initial = 365 * 1,    
  period = 30 * 2,      
  horizon = 30 * 6,     
  units = 'days'        
)

# Calculate performance metrics (RMSE, MAE, etc.)
df_p <- performance_metrics(df_cv)
print(df_p)
  
mean_values <- lapply(df_p, mean)

mean_df <- as.data.frame(t(as.data.frame(mean_values)))

# Optionally, rename the row or column names for clarity
colnames(mean_df) <- "Mean_Value"

mean_df

# Mean_Value
# horizon  105.6765 days
# mse            14271.9
# rmse          105.7462
# mae           81.71132
# mape         0.2870158 ++ 
# mdape        0.1895811
# smape        0.2380644
# coverage     0.2426471


# The model's Mean Absolute Percentage Error is around 28.7%. 
# on average, the forecasts are off by about 28% 
# compared to the actual values. Typically, a MAPE under 25% is 
# considered best, so while this model is doing okay, there's definitely 
# room for improvement to make the predictions more accurate.



















