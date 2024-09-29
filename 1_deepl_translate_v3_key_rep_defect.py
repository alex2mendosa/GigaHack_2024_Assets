# -*- coding: utf-8 -*-


# import libraries

import pandas as pd
import deepl
import time


## ++ Attempt api

api_key_free = 'insert_api_key'

# test translation from french to english
translator = deepl.Translator(api_key_free, server_url="https://api.deepl.com")

result = translator.translate_text("remontée de données peages", 
                                   source_lang="FR" ,
                                   target_lang="EN-GB")

print( result )



### ++ Reconfigure solution 

# pandas option to dispalay all columns in the data frame
pd.set_option('display.max_columns', None)


### Create translator object

translator = deepl.Translator(api_key_free, server_url="https://api.deepl.com")

#  upload commnets
d4_gern_short  = pd.read_csv( "\\\\xxxx.csv",
                              delimiter = "~",
                              encoding = "ISO-8859-1")


# debug
print(d4_gern_short.head(10))

print(d4_gern_short.shape)

print(d4_gern_short.isna().sum())

print(d4_gern_short[d4_gern_short.isna().any(axis=1)])



### ++ Continue, select unique comments

del translator

sam_size =  d4_gern_short.shape[0]  

sample = d4_gern_short.head(sam_size).copy()

sample = sample[["Reported_Defect"]]

## remove duplicates

sample = sample.drop_duplicates(subset=['Reported_Defect'], keep='first').reset_index(drop=True)

# sample.shape
# type(sample)

print(sample.shape[0] - d4_gern_short.shape[0] )


## ++ Continue Define function for translations

translator = deepl.Translator(api_key_free, server_url="https://api.deepl.com")

# https://developers.deepl.com/docs/api-reference/translate#request-body-descriptions

def translate_text(text):
    try:
        # Reinitialize the Translator object for each translation to avoid blocking from the API
        translated = translator.translate_text(text, 
                                               source_lang="FR", 
                                               target_lang="EN-GB", 
                                               split_sentences=1,
                                               context = None)
           
        return translated.text
    except Exception as e:
        print(str(e))
        return None  # Return None or text itself if translation fails


# Apply translations in batches of 50000
batch_size = 50000
total_batches = (len(sample) + batch_size - 1) // batch_size


## Continue , initiate atranslation 
start_time = time.time()

# https://stackoverflow.com/questions/49497391/googletrans-api-error-expecting-value-line-1-column-1-char-0

# Loop through the dataframe in batches
for i in range(0, len(sample), batch_size):
    
    end = i + batch_size # min(i + batch_size, len(sample))
    print(f"Translating batch {i//batch_size + 1} of {total_batches}...")
    
    # Applying translations in batches
    translated_texts = sample.iloc[i:end]['Reported_Defect'].apply(translate_text)
    sample.loc[i:end, 'eng_trans'] = translated_texts
    
    # Count the number of NaN values in the translated texts
    nan_count = translated_texts.isna().sum()
    print(f"Batch {i//batch_size + 1} processed. NaN count: {nan_count}")
    
    time.sleep(2)  # Pause for 2 seconds before the next batch
    
    
## running time
time.time() - start_time



## ++ Continue debug

sample

sample["Reported_Defect"].isin( d4_gern_short["Reported_Defect"] ).all() # must be true

sample.shape[0]==d4_gern_short.shape[0] # small dif due to duplicateed removed


## check for missing values
sample["eng_trans"].isna().sum()

sample[ sample["eng_trans"].isna() ]



### Continue join with original dataset

sample_without_lang = sample

result = pd.merge( d4_gern_short , sample_without_lang , on = "Reported_Defect" ,  how="left" )

result

# Export file in csv

result['Reported_Defect'] = result['Reported_Defect'].replace({'\n': ' ', '\r': ' ', '-': ' '}, regex=True)
result['eng_trans'] = result['eng_trans'].replace({'\n': ' ', '\r': ' ', '-': ' '}, regex=True)

out_1 = result.drop(["Reported_Defect","lang"] , axis = 1)
out_1

out_1.to_csv("\\\\xxxxx.csv",
              index=False, sep="~", encoding='utf-8')

## Done









































