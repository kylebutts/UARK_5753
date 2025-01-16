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

1. Gareth, J., Daniela, W., Trevor, H., \& Robert, T. (2013). "[An introduction to statistical learning: with applications in R (2nd edition)](https://www.statlearning.com)". Spinger.
2. Hyndman, R. J., \& Athanasopoulos, G. (2018). "[Forecasting: principles and practice (3rd edition)](https://otexts.com/fpp3)". OTexts.

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

| **Assignment** | **Percent of grade** |
|----------------|----------------------|
| Problem Sets   | 20%                  |
| Project 1      | 15%                  |
| Project 2      | 15%                  |
| Midterm        | 25%                  |
| Final          | 25%                  |
|                |                      |



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
- [Chapter 3 of Introduction to Computational Finance and Financial Econometrics with R](https://bookdown.org/compfinezbook/introcompfinr/Matrix-Algebra-Review.html)
- Video: [So You Think You Know How to Take Derivatives? | Steven Johnson](https://www.youtube.com/watch?v=-l7JHalBubw)


### 2. Introduction to Forecasting

*Topics:*
- Goals of forecasting
  - Prediction and Inference
- How to evaluate a model
  - loss-function and mean-squared prediction error
- Overfitting and some solutions
  - Cross-validation
- Types of Datasets
  - cross-sectional, time-series, and panel

*Readings:*


### 3. Cross-sectional Forecasting

*Topics:*
- Conditional expectation function $\mathbb{E}(y \ \vert \ X)$
- Bivariate Regression 
  - Derivation of bivariate regression
  - Deriviation of standard errors 
  - Indicator variables
- Multivariate Linear Regression
  - Discrete variables
  - Polynomials and other bases
- Non-parametric regression of 1 explanatory variable
  - Trade-offs with linear regression
- Partially linear-model
- Logistic regression for 0/1 variables

*Readings:*


### 4. Time-series Forecasting

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


### 5. Spatial Forecasting (time permitting)

*Topics:*
- Kriging and other smoothing methods

*Readings:*





## Tentative Schedule

This is a tentative schedule. This is the first time I've taught this course, so take this with a heavy dose of skepticism. In particular, do not set up holidays a class before or after the midterm. 

<!-- Schedule -->
| Week | Dates | Tuesday | Thursday |
|----|----|----|----|
| 1 | 01/14 - 01/16 | Syllabus and Linear Algebra | Linear Algebra |
| 2 | 01/21 - 01/23 | Introduction to Forecasting | Introduction to Forecasting |
| 3 | 01/28 - 01/30 | Conditional Expectation | Bivariate Regression |
| 4 | 02/04 - 02/06 | Bivariate Regression | Bivariate Regression |
| 5 | 02/11 - 02/13 | R Day | Multivariate Regression |
| 6 | 02/18 - 02/20 | Multivariate Regression | Nonparametrics and Partially Linear |
| 7 | 03/04 - 03/06 | R Day | Logistic Regression |
| 8 | 03/11 - 03/13 | Logistic Regression | Midterm Exam |
| 9 | 03/18 - 03/20 | Cross-sectional Project | Cross-sectional Project |
| 11 | 04/01 - 04/03 | Time-series Regression | Time-series Regression |
| 12 | 04/08 - 04/10 | R Day | Smoothing Methods |
| 13 | 04/15 - 04/17 | Smoothing Methods | Smoothing Methods |
| 14 | 04/22 - 04/24 | R Day | Prophet |
| 15 | 04/29 - 05/01 | Time-series Project | Time-series Project |
<!-- Schedule -->



<!-- 
## Gratitudes
This course was inspired by a lot of material that I have blended together into the course. These include: 
-->





