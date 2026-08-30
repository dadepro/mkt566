# MKT566: Decision Making using Marketing Analytics

**Fall 2026 — Tuesdays & Thursdays, 11:00–12:20 pm, JKP 112**

Instructor: Davide Proserpio (proserpi@usc.edu). Office hours: Tuesdays 2–4 pm, HOH 332

Teaching Assistant: Raghav Sarmukaddam (sarmukad@usc.edu). Office hours: Thursdays 2:30–4:30 pm, HOH 311

**Key dates (Fall 2026):** Classes begin Aug. 24 · Fall recess Oct. 8–9 · Veterans Day Nov. 11 · Thanksgiving holiday Nov. 25–29 (no class Nov. 24 & 26) · Classes end Dec. 4 · Final exams Dec. 9–16

### Course Description
This course is designed to equip you with the skills necessary to effectively utilize marketing data and reports, enabling you to make informed and critical decisions based on that data. The instructor will guide students on a journey of data exploration, beginning with data collection, visualization, and analysis, and concluding with the application of new methods (such as machine learning) and the utilization of diverse data types (including unstructured big data, such as text data) to address various marketing challenges faced by firms.

For more information about this course, please look at the **[syllabus](https://raw.githack.com/dadepro/mkt566/main/syllabus/mkt566-syllabus-proserpio.pdf)**.

**Group project:** add your group members to the **[sign-up sheet](https://docs.google.com/spreadsheets/d/1Jna-Noy_q3fSUGYgK7wW49jKpS5l3lKt2Q14QZ004xw/edit?usp=sharing)**.

**[Peer evaluations form](https://github.com/dadepro/mkt566/blob/main/syllabus/peer-eval-form.docx)**

### Lectures

Course materials (slides, cases, code, and the syllabus) will be posted here as the semester progresses.

### Week 1: Intro and data viz
- Tuesday, Aug. 25: slides ([html](https://raw.githack.com/dadepro/mkt566/main/w1/w1-1-intro.html), [pdf](https://raw.githack.com/dadepro/mkt566/main/w1/w1-1-intro.pdf))
    - [Install R](https://cran.r-project.org/)
    - [Install VS Code](https://code.visualstudio.com/download) and configure it with [Claude Code](https://code.claude.com/docs/en/vs-code), [Codex](https://developers.openai.com/codex/ide), or [GitHub Copilot](https://code.visualstudio.com/docs/copilot/overview)
    - New to coding? Follow our beginner-friendly, step-by-step **[VS Code + AI assistant setup guide](https://raw.githack.com/dadepro/mkt566/main/w1/vscode-setup.html)**, then the companion **[Running R Code in VS Code guide](https://raw.githack.com/dadepro/mkt566/main/w1/r-setup.html)** (install R and the course packages, run scripts line by line)
    - Git basics: [git - the simple guide](https://rogerdudler.github.io/git-guide/) and the [GitHub quickstart](https://docs.github.com/en/get-started/start-your-journey/hello-world) (all in the browser, no command line needed); or use the point-and-click [GitHub Desktop app](https://docs.github.com/en/desktop/overview/getting-started-with-github-desktop)
- Thursday, Aug. 27:
  - Slides ([html](https://raw.githack.com/dadepro/mkt566/main/w1/w1-2-data-viz.html), [pdf](https://raw.githack.com/dadepro/mkt566/main/w1/w1-2-data-viz.pdf))
  - Required readings:
    - [Chapter 3 of R for Data Science](https://r4ds.had.co.nz/data-visualisation.html)
    - [The Groupon Effect on Yelp Ratings: A Root Cause Analysis](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2560825)
  - Optional readings:
    - [Chapter 1 of  Data Visualization: A practical introduction](https://socviz.co/01-look-at-data.html)
    - [Lecture 5 of Data Storytelling for Marketers](https://raw.githack.com/dadepro/mkt-615/main/lectures/07-dataviz/07-dataviz.html#1)
  - Code and data (download everything: [w1-code.zip](https://github.com/dadepro/mkt566/raw/main/w1/w1-code.zip), or browse the [w1/code](https://github.com/dadepro/mkt566/tree/main/w1/code) folder):
    - [Chart types](https://github.com/dadepro/mkt566/blob/main/w1/code/w1-2-chart-types-class.R)
    - [Beautify plot](https://github.com/dadepro/mkt566/blob/main/w1/code/w1-2-data-viz-beautify.R)
    - [Datasets](https://github.com/dadepro/mkt566/tree/main/w1/code/data)
  - After class, try the **[Writing in Markdown guide](https://raw.githack.com/dadepro/mkt566/main/w1/markdown-setup.html)**: turn the beautify script's code and figures into a shareable PDF report (10 minutes, beginner friendly)

### Week 2: Exploratory data analysis: Variation

- Tuesday, Sept. 1:
  - Slides ([html](https://raw.githack.com/dadepro/mkt566/main/w2/w2-1-exploratory-data-analysis.html), [pdf](https://raw.githack.com/dadepro/mkt566/main/w2/w2-1-exploratory-data-analysis.pdf))
  - Required readings:
    - [Chapter 7 of R for Data Science](https://r4ds.had.co.nz/exploratory-data-analysis.html)
  - Optional readings:
    - Chapters 3, 4, 5 of R for Marketing Research and Analytics
  - Code and data (download everything: [w2-code.zip](https://github.com/dadepro/mkt566/raw/main/w2/w2-code.zip), or browse the [w2/code](https://github.com/dadepro/mkt566/tree/main/w2/code) folder):
    - [Every chart from the slides](https://github.com/dadepro/mkt566/blob/main/w2/code/w2-1-eda-variation-class.R)
    - [Simulate the case dataset](https://github.com/dadepro/mkt566/blob/main/w2/code/w2-1-simulate-marketing-dataset.R) (no need to run it: the dataset is already saved in [w2/code/data](https://github.com/dadepro/mkt566/tree/main/w2/code/data))
- Thursday, Sept. 3:
  - In-class exercise, the variation case: **[handout](https://raw.githack.com/dadepro/mkt566/main/w2/w2-1-variation-case.html)** and [data](https://github.com/dadepro/mkt566/blob/main/w2/code/data/marketing_eda.csv). A vibecoding exercise with no starter code: bring your laptop with the [week 1 setup](https://raw.githack.com/dadepro/mkt566/main/w1/vscode-setup.html) working
  
