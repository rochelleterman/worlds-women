This project:

1. Collects articles about women from the New York Times and Washington Post, 1980-2014
2. Categorizes each article by country + region
3. Uses Stanford's Named Entity Recognizer to remove proper nouns from article texts
4. Uses STM (R package) to analyze topical trends in the corpus over time and across region
5. Compare coverage across region using word separating alogrithms and other techniques.
6. Conducts statistical analysis regressing mean topic distributions on country level variables (note the country level dataset is not included in this repo)



| Task | Input | File | Output |
| -----| ----- | ---- | ------ |
| Clean Metadata | Data/all-raw.csv | clean-and-categorize.R | Data/women-foreign.csv, Data/women-all.csv |
| Describe  | Data/women-foreign.csv | descriptive.R | Results/ |
| Pre-Process | Data/women-foreign.csv | preprocess.ipynb | Data/women-processed.csv |
| Sentiment Analysis  | Data/women-processed.csv | sentiments.R | Results/ |
| Structural Topic Model | Data/women-processed.csv | stm.R | Data/meta-topics,.csv/, Results/ |
| Comparing topics across Region | Data/meta-topics.csv | same-topic-different-region.R | Results/ |
| Regress | Data/meta-topics.csv | regressions.R | Results/regression-results |

