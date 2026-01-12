# Forecasting `[ECON 5753]`

|                |                                                      |
|----------------|------------------------------------------------------|
| Instructor:    | [Dr. Kyle Butts](https://kylebutts.com/)             |
| Lecture:       | Tuesday, Thursday 11am -- 12:15pm at WCOB 207        |
| Office Hours:  | Tuesday, Wednesday 1pm -- 3pm at WCOB 408            |
|                |                                                      |


## Course Summary

This course will provide an introduction to forecasting methods. The class will teach you how to take a set of input variables and produce predictions of some outcome variable. We will survey a set of forecasting methods for your toolbox including: bivariate and multivariate regression; non-parametric and partially linear models; time-series regression; smoothing methods in time-series, and, if time-permitting spatial forecasting methods. The class will teach these methods theoretically and also teach you to estimate these models in the `R` programming language.

Though the class will also teach you fundamental principles of forecasting: goals of forecasting, fitting of models, evaluating model fit, and limitations of the models. By doing this, the class will equip you with the foundations to expand your toolbox over time and implement these tools as a *careful econometrician*. 

Last, the course will try to highlight limitations of forecasting methods; trade-offs between forecasting methods (e.g. interpretability versus predictive accuracy); and help you understand what forecasting methods can not due (e.g. establish causality). 



## Course Materials

There are two primary textbooks that will be referenced in the class. These textbooks are both available for free online. You may buy a print version, but it is \emph{not necessary} for the course. 

1. **"ISLR"**: Gareth, J., Daniela, W., Trevor, H., \& Robert, T. (2013). "[An introduction to statistical learning: with applications in R (2nd edition)](https://www.statlearning.com)". Spinger.
2. **"FPP3"**: Hyndman, R. J., \& Athanasopoulos, G. (2018). "[Forecasting: principles and practice (3rd edition)](https://otexts.com/fpp3)". OTexts.

The course outline will assign readings from each textbook as well as additional readings that complement the textbooks.


### Coding Software

You will need to download *two* programs:
1. Install R from <https://cloud.r-project.org/>.
2. Install Positron from <https://github.com/posit-dev/positron/releases>. 

Positron is a new (and better) coding environment than RStudio, so I recommend it. However, you can use RStudio if Positron is giving you problems.

Mastering `R` will take time and dedication, but it is a powerful and adaptable tool that is highly valued by many employers. Invest the necessary effort and time, and you will see the benefits.


## Assignments and Exams

**Problem sets**: Throughout the course, problem sets will be assigned. 
These will feature theoretical questions that will require you to write out responses. 
Additionally, portions of the assignments will ask you to code up different estimators and interpret your findings in words.
Students are encouraged to work in groups of 2 to discuss how the approach the problem sets, but each student must hand in their own set of answers. 
Missing or late problem sets will receive no credit.

**Exams**: There will be a in-class midterm and a final exam for the class. 

**Projects**: The class will feature two ``capstone'' projects that will ask you to select a datset and use that data to create a forecasting model. Each project will accompany distinct parts of the course: forecasting with cross-sectional data and with time-series data. 

The breakdown of your final grade will be as follows:

| **Assignment**             | **Percent of grade** |
|----------------------------|----------------------|
| Problem Sets               | 20%                  |
| Cross Sectional Project    | 15%                  |
| Time Series Project        | 15%                  |
| Midterm                    | 25%                  |
| Final                      | 25%                  |
|                            |                      |



## Policies

The student who missed exam must provide an official proven emergency which prevents you from attending class on the scheduled exam date within 24 hours after the missed exam to be allowed to take a makeup. Otherwise the student is not eligible to take a makeup exam and the missed exam equals zero points.

There will be due dates on the assignments. Like you, I am a busy person. I may grade the next day or a few days later. You have until I start grading assignments to turn it in without penalty, so take your chances. 

If you have any questions during the lecture, feel free to ask right away. Your questions can benefit both you and other students who might have the same questions. If you arrive late or need to leave early, please sit near the door to minimize disruption to the class.

### Access and Accomodations

Your experience in this class is important to me. University of Arkansas Academic [Policy Series 1520.10](https://policies.uark.edu/academic/152010.php) requires that students with disabilities are provided reasonable accommodations to ensure their equal access to course content. If you have already established accommodations with the [Center for Educational Access (CEA)](https://cea.uark.edu), please request your accommodations letter early in the semester and contact me privately, so that we have adequate time to arrange your approved academic accommodations.

If you have **not** yet established services through CEA, but have a documented disability and require accommodations (conditions include but not limited to: mental health, attention-related, learning, vision, hearing, physical, health  or temporary impacts), contact CEA directly to set up an Access Plan. CEA facilitates the interactive process that establishes reasonable accommodations.  For more information on CEA registration procedures contact 479-575-3104, ada@uark.edu or visit https://cea.uark.edu.


## Course Outline

### 1. Linear Algebra

[Slides](https://nbviewer.org/github/kylebutts/UARK_5753/blob/main/01-Linear_Algebra/01-Linear_Algebra.pdf)

*Topics:*
- Matrices and vectors
- Transpose
- Identity matrix and matrix inverse
- Dot product as $v' v$
- Statistics as matrix operations
- Derivatives as linear approximation
- Useful matrix derivative rules

*Readings:*
- Review Notes: [Probability and Statistics](https://nbviewer.org/github/kylebutts/UARK_5753/blob/main/00-Review_Probability_and_Statistics/Review_Probability_and_Statistics.pdf)
- [Introduction to Computational Finance and Financial Econometrics with R](https://bookdown.org/compfinezbook/introcompfinr/Matrix-Algebra-Review.html): Chapter 3
- Video: [So You Think You Know How to Take Derivatives? | Steven Johnson](https://www.youtube.com/watch?v=-l7JHalBubw)


### 2. Introduction to Forecasting

*Topics:*
- Goals of forecasting
  - Prediction and Inference
- How to evaluate a model
  - loss-function and mean-squared prediction error
- Overfitting and some solutions
  - Sample-splitting and Cross-validation
- Types of Datasets
  - cross-sectional, time-series, and panel

*Readings:*
- ISLR: All of 2.1, 2.2 thru 2.2.2


### 3. Regression Theory

*Topics:*
- Conditional expectation function $\mathbb{E}(y \ \vert \ X)$
- Bivariate Regression 
  - Derivation of bivariate regression
  - Deriviation of standard errors 
- Forecasting
  - Derivation of standard errors
- Marginal (Predictive) Effects

*Readings:*

- ISLR: All of 3.1, 3.2, 3.3, and 3.4
- [Introduction to Econometrics with R](https://www.econometrics-with-r.org/4.5-tsdotoe.html): Section 4.5 on Sample Distribution of $\hat{\beta}$ 
- [Marginal Effects book](https://marginaleffects.com/chapters/predictions.html): Chapter 5 on Prediction


### 4. Regression in Practice

- Common Regression Terms
  - Polynomials
  - Indicator variables
  - Discrete variables
  - Bins 
  - Partially linear-model and Splines 
- $\log$ outcome variables
- 0/1 Outcome variables
  - Linear probability model
  - Logistic regression

*Readings:*

- ISLR: All of 7.1, 7.2, 7.3, and 7.4
- $\log$ readings: [Lecture Notes on Linear Regression Models with Logarithmic Transformations](https://kenbenoit.net/assets/courses/ME104/logmodels2.pdf) and [FAQ How Do I Interpret A Regression Model When Some Variables Are Log Transformed?](https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/)
- ISLR: All of 4.1, 4.2, and 4.3
- [Marginal Effects book](https://marginaleffects.com/chapters/slopes.html): Chapter 6 on Slopes


### 5. Time-series Forecasting

*Topics:*
- Time-series regression
  - Estimating holiday and seasonal patterns (day of week, monthly effects, etc.)
  - Linear time-trends
  - Piecewise linear time trends
    - Pre-selected breaks
    - Data selected breaks
- Smoothing methods
  - One-sided and two-sided rolling averages
  - Exponential smoothing methods
- Prophet model

*Readings:*


### 6. Spatial Forecasting (time permitting)

*Topics:*
- Kriging and other smoothing methods

*Readings:*





## Tentative Schedule

This is a tentative schedule. This is the first time I've taught this course, so take this with a heavy dose of skepticism. In particular, do not set up holidays a class before or after the midterm. 

<!-- Schedule -->
| Week | Dates | Tuesday | Thursday |
|----|----|----|----|
| 01 | 01/13 - 01/15 | Getting Setup for R | Syllabus and Repeated Sampling |
| 02 | 01/20 - 01/22 | Linear Algebra | Linear Algebra |
| 03 | 01/27 - 01/29 | Introduction to Forecasting | Conditional Expectation |
| 04 | 02/03 - 02/05 | Bivariate Regression | Bivariate Regression |
| 05 | 02/10 - 02/12 | Bivariate Regression | Multivariate Regression |
| 06 | 02/17 - 02/19 | Multivariate Regression | Nonparametrics and Partially Linear |
| 07 | 02/24 - 02/26 | Nonparametrics and Partially Linear | Log Transformations |
| 08 | 03/03 - 03/05 | Logistic Regression | R Day |
| 09 | 03/10 - 03/12 | Cross-sectional Project | Cross-sectional Project |
| 10 | 03/17 - 03/19 | Midterm Exam | Time-series Regression |
| 11 | 03/24 - 03/26 | Time-series Regression | Time-series Regression |
| 12 | 03/31 - 04/02 | Smoothing Methods | Smoothing Methods |
| 13 | 04/07 - 04/09 | Smoothing Methods | Smoothing Methods |
| 14 | 04/14 - 04/16 | Smoothing Methods | Prophet |
| 15 | 04/21 - 04/23 | Time-series Project | Time-series Project |
| 16 | 04/28 - 04/30 |  |  |
<!-- Schedule -->



<!-- 
## Gratitudes
This course was inspired by a lot of material that I have blended together into the course. These include: 
-->





