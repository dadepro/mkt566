# MKT566: Decision Making using Marketing Analytics

### Course Description
This course is designed to equip you with the skills necessary to effectively utilize marketing data and reports, enabling you to make informed and critical decisions based on that data. The instructor will guide students on a journey of data exploration, beginning with data collection, visualization, and analysis, and concluding with the application of new methods (such as machine learning) and the utilization of diverse data types (including unstructured big data, such as text data) to address various marketing challenges faced by firms.

For more information about this course, please look at the **[syllabus](https://raw.githack.com/dadepro/mkt566/main/syllabus/mkt566-syllabus-proserpio.pdf)**.

### Lectures

### Week 1
- Monday, Aug. 25: [slides](https://raw.githack.com/dadepro/mkt566/main/w1/w1-1-intro.pdf)
    - [Install R/Rstudio](https://bookdown.org/content/6ef13ea6-4e86-4566-b665-ebcd19d45029/#download-and-install-r-and-rstudio) 
- Wednesday, Aug. 27:
  - [Slides](https://raw.githack.com/dadepro/mkt566/main/w1/w1-2-data-viz.pdf)
  - Required readings:
    - [Chapter 3 of R for Data Science](https://r4ds.had.co.nz/data-visualisation.html)
    - [The Groupon Effect on Yelp Ratings: A Root Cause Analysis](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2560825)
  - Optional readings:
    - [Chapter 1 of  Data Visualization: A practical introduction](https://socviz.co/lookatdata.html)
    - [Lecture 5 of Data Storytelling for Marketers](https://raw.githack.com/dadepro/mkt-615/main/lectures/07-dataviz/07-dataviz.html#1)
  - Code
    - [Chart types](https://github.com/dadepro/mkt566/blob/main/w1/w1-2-chart-types-class.R)
    - [Beautify plot](https://github.com/dadepro/mkt566/blob/main/w1/w1-2-data-viz-beautify.R)

### Week 2
- Wednesday, Sept. 3:
    - [Slides](https://raw.githack.com/dadepro/mkt566/main/w2/w2-1-exploratory-data-analysis.pdf)
    - Required readings:
      - [Chapter 7 of R for Data Science](https://r4ds.had.co.nz/exploratory-data-analysis.html)
    - Optional readings:
      - Chapters 3, 4, 5 of R for Marketing Analytics
    - Code:
      - Replicate slides' figures: [code](https://github.com/dadepro/mkt566/blob/main/w2/w2-1-eda-marketing-economics-dataset.R)
      - Simulate marketing dataset: [code](w2-1-simulate-marketing-dataset.R)
      - Variation case: [html](https://raw.githack.com/dadepro/mkt566/main/w2/w2-1-variation-case.html) and [R Markdown](https://github.com/dadepro/mkt566/blob/main/w2/w2-1-variation-case.Rmd), [data](https://github.com/dadepro/mkt566/blob/main/w2/data/marketing_eda.csv)
  
### Week 3
- Monday & Wednesday, Sept. 8, 10
  - [Slides](https://raw.githack.com/dadepro/mkt566/main/w3/w3-1-exploratory-data-analysis.pdf)
  - Required readings:
    - [Chapter 7 of R for Data Science](https://r4ds.had.co.nz/exploratory-data-analysis.html)
  - Optional readings:
    - Chapters 3, 4, 5 of R for Marketing Analytics
- Code:
  - Replicate slides' figures: [code](https://github.com/dadepro/mkt566/blob/main/w3/w3-1-eda-marketing-dataset.R)
  - RateBeer case: [html](https://raw.githack.com/dadepro/mkt566/main/w3/beer-case/w3-eda-case.html) and [R Markdown](https://github.com/dadepro/mkt566/blob/main/w3/beer-case/w3-eda-case.Rmd), [dataset](https://github.com/dadepro/mkt566/blob/main/w3/beer-case/w3-ratebeer-sampled.csv.gz), [partial solution/helper](https://raw.githack.com/dadepro/mkt566/main/w3/beer-case/w3-eda-case-helper.html), [full solution](https://raw.githack.com/dadepro/mkt566/main/w3/beer-case/w3-eda-case-solution.html)

### Week 4
- Monday & Wednesday, Sept. 15, 17
    - [Slides OLS](https://raw.githack.com/dadepro/mkt566/main/w4/w4-1-ols.pdf)
    - [Slides Logit](https://raw.githack.com/dadepro/mkt566/main/w4/w4-2-logit.pdf)
    - Required readings:
      - Chapters [3.4](https://bookdown.org/content/6ef13ea6-4e86-4566-b665-ebcd19d45029/secondarydata.html#linear-regression) and [3.6](https://bookdown.org/content/6ef13ea6-4e86-4566-b665-ebcd19d45029/secondarydata.html#logistic) of R for Marketing Students
    - Optional readings:
      - [Lecture 6 of Data Storytelling for Marketers](https://raw.githack.com/dadepro/mkt-615/main/lectures/08-regression/08-regressions.html#1)
      - - Chapters 7, 9.2 of R for Marketing Analytics
    - Code:
      - In class Airbnb analysis: [OLS](https://github.com/dadepro/mkt566/blob/main/w4/w4-1-regression-example-for-class.R), [Logit](https://github.com/dadepro/mkt566/blob/main/w4/w4-2-logit-class.R)
      - Airbnb exercise: [html](https://raw.githack.com/dadepro/mkt566/main/w4/airbnb-case/w4-airbnb-case.html) and [.Rmd](https://github.com/dadepro/mkt566/blob/main/w4/airbnb-case/w4-airbnb-case.Rmd), solution [html](https://raw.githack.com/dadepro/mkt566/main/w4/airbnb-case/w4-airbnb-case-solutions.html) and [R Markdown](https://github.com/dadepro/mkt566/blob/main/w4/airbnb-case/w4-airbnb-case-solutions.Rmd)

### Week 5
- Monday & Wednesday, Sept 22, 24
  - Slides
    - [Clustering](https://raw.githack.com/dadepro/mkt566/main/w5/w5-1-clustering.pdf)
    - [Recommender systems](https://raw.githack.com/dadepro/mkt566/main/w5/w5-2-recommendations.pdf)
  - Required readings:
    - Chapters [5](https://bookdown.org/content/6ef13ea6-4e86-4566-b665-ebcd19d45029/pca_office.html), [6](https://bookdown.org/content/6ef13ea6-4e86-4566-b665-ebcd19d45029/pca_toothpase.html), and [7](https://bookdown.org/content/6ef13ea6-4e86-4566-b665-ebcd19d45029/cluster.html) of R for Marketing Students
    - [Netflix Billion Dollar Secret](https://www.linkedin.com/pulse/netflixs-billion-dollar-secret-how-recommendation-systems-qin-phd-7zece/)
    - [Marketing Automation: Recommendation Systems](https://medium.com/geekculture/marketing-automation-recommendation-systems-ae39d61aa38)
  - Optional readings:
    - Chapter 11.1–11.3 of R for Marketing Analytics
    - [Two decades of recommender systems at Amazon](https://assets.amazon.science/76/9e/7eac89c14a838746e91dde0a5e9f/two-decades-of-recommender-systems-at-amazon.pdf)
  - Code:
    - PCA in-class exercise: [code](https://github.com/dadepro/mkt566/blob/main/w5/w5-1-pca.R) and [data](https://github.com/dadepro/mkt566/blob/main/w5/data/perceptual_map_office.csv)
    - Clustering in-class exercise: [code](https://github.com/dadepro/mkt566/blob/main/w5/w5-1-clustering.R) and [data](https://github.com/dadepro/mkt566/blob/main/w5/data/segmentation_office.xlsx)
    - [Streaming platforms in-class discussion](https://raw.githack.com/dadepro/mkt566/main/w5/case/recommender-discussion-assigment.pdf), [solution](https://raw.githack.com/dadepro/mkt566/main/w5/case/w5-clustering-exe-solution.html)

### Week 6
- Monday, Sept 29
  - Guest speaker: [Jonathan Elliot](https://www.linkedin.com/in/jonnynelliott/), [Slides](https://raw.githack.com/dadepro/mkt566/main/articles-papers/Investing%20in%20Film%20with%20Machine%20Learning.pdf)
- Wednesday, Oct 1
  - In-class exercise (clustering analysis): [html](https://raw.githack.com/dadepro/mkt566/main/w5/case/w5-clustering-exe.html), [R Markdown](http://github.com/dadepro/mkt566/blob/main/w5/case/w5-clustering-exe.Rmd), [data](https://github.com/dadepro/mkt566/blob/main/w5/case/customer_clustering_data.csv)
    
## Week 7
- Monday, Oct 6
  - Work on group project ([slides](https://raw.githack.com/dadepro/mkt566/main/w7/w7-1-class-work.pdf))
- Wednesday, Oct 8
  - No class (Fall recess)

## Week 8
- Mid-term project proposal presentations

## Week 9
- Monday, Oct 20
  - [Slides](https://raw.githack.com/dadepro/mkt566/main/w9/w9-classifiers.pdf)
  - Optional readings:
    - Chapter 11.4–11.6 of R for Marketing Analytics
- Wednesday, Oct 22
  - Exercise: Predicting Click-Through-Rate with Logistic Regression: [R Markdown](https://github.com/dadepro/mkt566/blob/main/w9/case/w9-predicting-ad-click-w-logit.Rmd), [HTML](https://raw.githack.com/dadepro/mkt566/main/w9/case/w9-predicting-ad-click-w-logit.html), [click data](https://github.com/dadepro/mkt566/blob/main/w9/data/ad_click_data.csv), [R Markdown solution](https://github.com/dadepro/mkt566/blob/main/w9/case/w9-predicting-ad-click-w-logit-solutions.Rmd), [HTML solution](https://raw.githack.com/dadepro/mkt566/main/w9/case/w9-predicting-ad-click-w-logit-solutions.html)
