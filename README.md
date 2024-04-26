# COVID-19 Sentiment Analysis

## Project Overview

This R program performs sentiment analysis on COVID-19-related tweets and analyzes COVID-19 data using various data visualization techniques. It utilizes Twitter data to gauge the sentiment around the pandemic and provides insights into the spread of the virus across different countries and regions.

## Features
- **Data Import:** Imports COVID-19 data and tweets related to COVID-19 using the `readr` package.
- **Data Cleaning:** Cleans and preprocesses the imported data, including removing punctuation, converting text to lowercase, and removing stopwords.
- **Data Visualization:** Utilizes various plotting libraries such as `ggplot2`, `plotly`, and `circlize` to create informative visualizations such as time series plots of COVID-19 cases, geographical distribution of tweets, and sentiment analysis results.
- **Sentiment Analysis:** Conducts sentiment analysis on tweets using lexicons like AFINN and Bing, and analyzes the overall sentiment and mood in tweets.
- **Word Clouds:** Generates word clouds based on sentiment analysis results to visualize frequently used words and their sentiment polarity.
- **Statistical Analysis:** Performs statistical analysis to identify significant trends and patterns in COVID-19 data and tweet sentiments.

## Getting Started

### Requirements
- R programming language
- R packages: `readr`, `ggplot2`, `tidyverse`, `magick`, `tm`, `wordcloud`, `textmineR`, `RWeka`, and others (specified in the code)

### How to Use
1. Install R on your system if not already installed.
2. Install the required R packages listed in the code using the `install.packages()` function.
3. Download the provided R script file (`sentiment_analysis.R`) and the required dataset files (`covid-19-all.csv`, `covid19_tweets.csv`, `worldcitiespop.csv`, `Afinn.csv`, `Bing.csv`, `NRC.csv`).
4. Place the R script and dataset files in the same directory.
5. Run the R script in your preferred R environment (e.g., RStudio) to execute the sentiment analysis and generate visualizations.

## Data Cleaning and Preprocessing

The program cleans and preprocesses the data by removing punctuation, converting text to lowercase, and removing stopwords to prepare it for analysis.

## Data Visualization

The program utilizes various plotting libraries to create visualizations such as time series plots of COVID-19 cases, geographical distribution of tweets, chord diagram for relationship between sentiment and countries, radar chart for tweets and emotions based on countries, and sentiment analysis results.

## Conclusion

The program provides insights into the sentiment around the COVID-19 pandemic based on Twitter data, allowing for a better understanding of public opinion and mood during the crisis.

## Notes
- Ensure that all required dataset files are present in the working directory or provide the correct file paths in the code.
- This program may require sufficient computational resources and processing time, especially when analyzing large datasets or performing complex sentiment analysis tasks.

## Acknowledgments
- The COVID-19 data used in this analysis is sourced from a curated version of 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE.
- Twitter data is obtained from the official Twitter API.

## License

**NOT FOR COMMERCIAL USE**

_If you intend to use any of my code for commercial use please contact me and get my permission._

_If you intend to make money using any of my code please get my permission._