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
    - [Netflix Prize](https://ajay1997.medium.com/from-a-million-dollar-prize-to-a-billion-dollar-engine-inside-the-netflix-recommendation-system-1bcdbf5e69ed)
    - [Marketing Automation: Recommendation Systems](https://medium.com/geekculture/marketing-automation-recommendation-systems-ae39d61aa38)
  - Optional readings:
    - Chapter 11.1–11.3 of R for Marketing Analytics
    - [Two decades of recommender systems at Amazon](https://assets.amazon.science/76/9e/7eac89c14a838746e91dde0a5e9f/two-decades-of-recommender-systems-at-amazon.pdf)
  - Code:
    - PCA exercise: [code](https://github.com/dadepro/mkt566/blob/main/w5/w5-1-pca.R) and [data](https://github.com/dadepro/mkt566/blob/main/w5/data/perceptual_map_office.csv)
    - Clustering exercise: [code](https://github.com/dadepro/mkt566/blob/main/w5/w5-1-clustering.R) and [data](https://github.com/dadepro/mkt566/blob/main/w5/data/segmentation_office.xlsx)
      
