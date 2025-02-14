# 🤖 Artificial Intelligence, Machine Learning, Natural Language Processing (NLP), and Deep Learning for Text analysis & Object recognition in R

This project focuses on artificial intelligence (AI), text analysis, sentiment analysis, and machine learning techniques applied to various datasets, including PDFs, customer reviews, classic literature, stock reports, and image classification. It leverages AI methods like Natural Language Processing (NLP) and deep learning to perform complex tasks such as sentiment analysis, topic modeling, and object detection.

## 🚀 Introduction

These scripts perform the following tasks:

- Text extraction and cleaning from PDFs.
- Sentiment analysis and word frequency analysis.
- Named entity recognition and syntactic analysis using SpaCy.
- N-gram analysis and term correlations.
- Topic modeling using LDA.
- Sentiment-based stock market analysis.
- Image recognition and classification with machine learning.

## 📁 Contents

- **Tekstanalyse.R**: Text analysis of PDFs, including word frequency and sentiment analysis.
- **Spacy – Tekst & Sentiment Analyse.R**: Sentiment and vocabulary analysis of product reviews.
- **Austen - Spacy.R**: Named entity recognition and part-of-speech analysis of Jane Austen's texts.
- **Tekstanalyse - 4 - Relationships between words- n-grams and correlations.R**: N-gram analysis and word correlations in Jane Austen’s books.
- **Text Mining with R - Kapitel 5.R**: Topic modeling and sentiment analysis using tidy text formats.
- **Ordoptælling, Wordcloud, Sentiment & Topic Modelling.R**: Word frequency analysis, word clouds, and LDA topic modeling.
- **Aktie, Wordcloud, Sentiment & Topicmodel.R**: Sentiment analysis of Mærsk stock reports (2019-2022) and stock price analysis.
- **Billede Algoritme.R**: Image processing and classification using machine learning techniques.
- **Objekt genkendelse.R**: Object detection and classification using convolutional neural networks (CNNs).
- **ElgigPower.R**: Sentiment analysis and readability analysis of customer reviews from Elgiganten and Power.

## 📊 Data Analysis

### Tekstanalyse.R

- **Text Extraction**:
  - Reads and extracts text from PDF files.
  - Cleans text by removing punctuation, numbers, and stop words.
  
- **Word Frequency Analysis**:
  - Counts total words and creates a word frequency table.
  - Removes stop words and sorts words by frequency.
  
- **Sentiment Analysis**:
  - Performs sentiment analysis using "afinn" and "nrc" lexicons.
  - Visualizes sentiment distribution with word clouds and histograms.

### Spacy – Tekst & Sentiment Analyse.R

- **Product Review Analysis**:
  - Reads and cleans customer reviews from Elgiganten and Power.
  - Calculates LIX readability score for each review.
  
- **Sentiment Analysis**:
  - Uses Sentida for sentiment scoring.
  - Extracts and compares the most frequent nouns from reviews.
  
- **Bigram and Sentiment Analysis**:
  - Identifies and visualizes significant bigrams.
  - Analyzes sentiment polarity of bigrams.

### Austen - Spacy.R

- **Text Processing with SpaCy**:
  - Parses and tokenizes Jane Austen texts.
  - Extracts adjectives, verbs, and named entities.
  
- **Syntactic and Named Entity Analysis**:
  - Performs POS-tagging and entity recognition.
  - Analyzes the distribution of persons and verbs in Austen’s novels.
  
### Tekstanalyse - 4 - Relationships between words- n-grams and correlations.R

- **N-Gram Tokenization**:
  - Tokenizes text into bigrams and trigrams.
  - Analyzes word combinations related to gender ("he" and "she").
  
- **TF-IDF and Negation Analysis**:
  - Calculates TF-IDF to identify key word combinations.
  - Examines relationships between words and negations using AFINN lexicon.

### Text Mining with R - Kapitel 5.R

- **Tidy Text Processing**:
  - Converts document-term matrices to tidy format.
  
- **Sentiment and Topic Modeling**:
  - Matches words against sentiment dictionaries.
  - Performs Latent Dirichlet Allocation (LDA) for topic modeling.

### Ordoptælling, Wordcloud, Sentiment & Topic Modelling.R

- **Word Frequency and Word Clouds**:
  - Tokenizes and filters text from PDFs.
  - Generates word clouds based on word frequency.
  
- **Sentiment and Topic Modeling**:
  - Uses "bing" lexicon for sentiment scoring.
  - Applies LDA to identify key topics.

### Aktie, Wordcloud, Sentiment & Topicmodel.R

- **Stock Price and Sentiment Analysis**:
  - Fetches Mærsk stock prices from Yahoo Finance.
  - Analyzes sentiment in annual reports from 2019-2022.
  
- **Topic Modeling**:
  - Applies LDA to extract key topics from reports.
  - Conducts log-ratio analysis to compare themes across years.

### Billede Algoritme.R

- **Image Processing**:
  - Loads and analyzes images using `imager` and `jpeg`.
  - Prepares images for classification by applying transformations.
  
- **Machine Learning for Image Classification**:
  - Splits images into training and test sets.
  - Develops classification models for image recognition.

### Objekt genkendelse.R

- **Object Detection and Classification**:
  - Preprocesses image data for machine learning.
  - Builds a convolutional neural network (CNN) for object recognition.
  - Evaluates model performance and visualizes results.

### ElgigPower.R

- **Customer Review Analysis**:
  - Loads and cleans reviews from Elgiganten and Power.
  - Performs sentiment analysis using Sentida.
  
- **Readability and Word Frequency**:
  - Computes LIX readability scores.
  - Analyzes bigrams and their sentiment polarity.

## 🛠️ Technologie
AI: Using machine learning and deep learning techniques for data analysis and predictive modeling.
Natural Language Processing (NLP): For processing and analyzing textual data, including sentiment analysis, entity recognition, and topic modeling.
R: Primary language for data analysis and visualization.
keras, tensorflow: For deep learning-based image classification and object recognition (AI).


- **R**: Primary language for data analysis and visualization.
- **AI**: Using machine learning and deep learning techniques for data analysis and predictive modeling.
- **Natural Language Processing (NLP)**: For processing and analyzing textual data, including sentiment analysis, entity recognition, and topic modeling.
- **tidytext, dplyr, tidyr**: Data manipulation and text mining.
- **ggplot2**: Data visualization.
- **spacyr**: Named entity recognition and syntactic analysis.
- **tm, topicmodels**: Text mining and topic modeling.
- **imager, jpeg**: Image processing.
- **keras, tensorflow**: Deep learning for object recognition.

## 📌 Requirements

- R and RStudio must be installed.
- The following packages should be installed:

    ```r
install.packages(c("tidytext", "dplyr", "tidyr", "ggplot2", "spacyr", "tm", "topicmodels", "imager", "jpeg", "keras", "tensorflow"))
    ```

## 🤝 Contributing

Contributions are welcome! If you have suggestions or improvements, feel free to fork the repository and submit a pull request.