# Forecasting `[ECON 5753]`

Spring 2025 • Instructor: Kyle Butts

<!-- Monday, Wednesday 1 -- 2:15PM at RCED 103 -->

<!-- Office Hours: Monday, Wednesday 11am -- 1pm at WCOB 408 -->


## Course Summary

...

All empirical analysis will be based in the R programming language


## Course Materials

...


### Coding Software

You will need to download *two* programs:
1. Install R from <https://cloud.r-project.org/>.
2. Install Positron from <https://github.com/posit-dev/positron/releases>. 

Positron is a new (and better) coding environment than RStudio, so I recommend it. However, you can use RStudio if Positron is giving you problems.

Mastering `R` will take time and dedication, but it is a powerful and adaptable tool that is highly valued by many employers. Invest the necessary effort and time, and you will see the benefits.


## Assignments and Exams

**Problem sets** will usually be due two weeks after assigned. 
The problem sets will require you to analyze datasets using techniques discussed in class. 
Students are encouraged to work in groups to discuss how the approach the problem sets, but each student must hand in his or her own set of answers. 
Missing or late problem sets will receive no credit.

**Exams** will be held during class time on October 7th and November 13th.

**Projects**



## Course Outline

### [1. Linear Algebra](https://nbviewer.org/github/kylebutts/UARK_5753/blob/main/01-Linear_Algebra/01-Linear_Algebra.pdf)
- Matrices and vectors
- Transpose
- Identity matrix and matrix inverse
- Dot product as $v' v$
- Statistics as matrix operations
- Derivatives as linear approximation
- Useful matrix derivative rules

*Readings:*
- Review Notes: [Probability and Statistics](https://nbviewer.org/github/kylebutts/UARK_5753/blob/main/01-Linear_Algebra/Review_Probability_and_Statistics/Review_Probability_and_Statistics.pdf)
- [Chapter 3 of Introduction to Computational Finance and Financial Econometrics with R](https://bookdown.org/compfinezbook/introcompfinr/Matrix-Algebra-Review.html)
- Video: [So You Think You Know How to Take Derivatives? | Steven Johnson](https://www.youtube.com/watch?v=-l7JHalBubw)


### 2. Introduction to Forecasting
- Goals of forecasting
  - Prediction and Inference
- How to evaluate a model
  - loss-function and mean-squared prediction error
- Overfitting and some solutions
  - Cross-validation
- Types of Datasets
  - cross-sectional, time-series, and panel

### 3. Cross-sectional Forecasting
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

### 4. Time-series Forecasting
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

### 5. (time permitting) Spatial Forecasting 
- Kriging and other smoothing methods


### Tentative Schedule

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

Tentative Schedule
<!-- Schedule -->





<!-- 
## Gratitudes
This course was inspired by a lot of material that I have blended together into the course. These include: 
-->





