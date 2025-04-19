# Reproduction Code for "Detecting and Measuring Social Media Attacks on American Election Officials"

## Abstract
The 2020 presidential election saw election officials experience physical and social media threats, harassment, and animosity. While little research exists regarding animosity towards US election officials, observers noted a sharp increase in 2020 in animosity towards American election officials. The harassment of election officials hindered their work in administering a free and fair election, and may have sown seeds of distrust in electoral integrity. Our study proposes a unique measurement and modeling strategy applicable across many social media networks to study toxicity directed at officials, institutions, or groups.  We collect a novel dataset and apply joint sentiment-topic modeling to identify toxicity from the public and the election officials’ reactions. We then use dynamic vector autoregression models to determine the temporal structure of the toxic conversations directed at election officials.   We find that the level of animosity towards election officials spikes immediately after the election. Second, we find that hostile topics overall make up about a quarter of the discussion share during this period, but this share increases to about 60\% following the election. Finally, hostile topics come from both left and right wing partisans. Our paper demonstrates how similar data collection and topic modeling approaches could be deployed in future elections to monitor trolling and harassment of election officials, and perhaps help mitigate threats to successful election administration globally.


## Getting Started
The following R libraries are required to run this repository:
* ggfortify 
* tidytext
* quanteda
* rJST
* stm
* text2vec
* topicmodels
* dplyr
* parallel
* ggplot2
* lubridate
* factoextra
* fpc
* tseries
* vars
* vegan
* xtable
* stargazer
* tidyr
* readr
* wesanderson
* zoo
* magrittr
* progress

And the following Python packages:
* nltk
* pickle
* pandas
* numpy
* gc
* datetime



## Preparing Data
Our dataset was generated via the snowball-sampling analogue described in the paper. In order to run the rest of the repository, tweet data should be provided in the following formats.


### layer_tweet_dict.pickle
For each tweet in the dataset, create a `leos_member_twitter` object containing its information. Collect these into a dictionary and pickle them into `dataset/layer_tweet_dict.pickle`. Following the generation of these files, run `JST_data_create.ipynb`.

### alltweets.csv
Next, create a csv file for all tweets in the dataset, where each row corresponds to a tweet. The csv should consist of the following columns: `author_id`, `text`, `created_at`, `tweet`, and `docID`.

### hand_labeled.csv
This csv contains hand-labeled data classifying users as LEOs or non-LEOs. In it should be three columns: `handle`, `classification`, and `link`. The classification should be one of 1, 0, -1, where 1 classifies the user as a LEO, 0 classifies as non-LEO, and -1 means the user was not found. The link column consists of the base https://twitter.com/intent/user?user_id= + the handle number.



## Running JST

Once the data files have been generated and are located in LEOs_Animosity/dataset/, run:

```
JST/JST-run.R 
JST/time_series_results_new.R 
```

This will run JST and generate non-VAR visualizations. 



## Result Visualization

To get VAR plots, run:
```
VAR/VAR_results.R
```
