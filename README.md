This project:

1. Collects articles about women from the New York Times (more papers to follow), 1990-2014
2. Categorizes each article by country + region
3. Uses Stanford's Named Entity Recognizer to remove proper nouns from article texts
4. Uses STM (R package) to analyze topical trends in the corpus over time and across region
5. Applies word separating algorithms to compare coverage across region. 



| Task | Input | File | Output |
| -----| ----- | ---- | ------ |
| Clean Metadata | Data/all-raw.csv | clean-and-categorize.R | Data/women-foreign.csv, Data/women-all.csv |
| Describe  | Data/women-foreign.csv | descriptive.R | Results/ |
| Pre-Process | Data/women-foreign.csv | preprocess.ipynb | Data/women-processed.csv |
| Sentiment Analysis  | Data/women-processed.csv | sentiments.R | Results/ |
| Discriminating Words | Data/dtm-python.csv | distinctive-words.R | Results/distinctive-words/ |
| Structural Topic Model | Data/women-processed.csv | stm.R | Results/ |
