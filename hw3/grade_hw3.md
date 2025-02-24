*Tanya Wang*

### Overall Grade: 244/280

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format html? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

### Completeness, correctness and efficiency of solution: 196/230

- Q1 (38/50)
  
    - Q1.1 (23/25)
    
      A "Emergency Department" in ADT is missing before Med/Surg around Aug 18. `-2.0`
    
    - Q1.2 (15/25)
    
      It looks like there is an issue with the y-axis so that the lines are random `-8.0` 

      The method for displaying the x-axis text differs from the example. It would have been acceptable if the text were as legible as or clearer than in the example. `-2.0`

- Q2 (8/10)

    - Q2.1 (5/5)
    
    - Q2.2 (3/5) A bar plot of similar suffices.

      You did not directly answer "Can a subject_id have multiple ICU stays?" `-2.0`

- Q3 (19/25)    
    
    - Q3.1 (5/5)
    
    - Q3.2 (14/20) Student must explain patterns in admission hour, admission minute, and length of hospital display. Just describing the pattern is not enough. There are no wrong or correct explanations; any explanation suffices. 

      Adding `ymd_hms()` changes `00:00:00` to NA. `minute(admittime)` and `hours(admittime)` should be used. `-2.0`

      0am (without `ymd_hms()`) and 7am should be unusual. You did not provide any explanation for the unusual patterns in the admission hour. `-2.0`

      You did not provide explanation (or guess) for the unusual patterns in the admission minute. The 15-minute peak is probably artificial. `-2.0`

      By the way, there are unusual observations with negative length of stay, which should be unusual. `-0.0`

- Q4 (17/15)        
    
    - Q4.1 (5/5)
    
    - Q4.2 (12/10) There's not much pattern in gender. But some explanations are expected for anchor age: what are they and why the spike on the right.

      You noted a peak at age 91 (older than 89) and also cited the MIMIC-IV documentation. `+2.0`

- Q5 (28/30) Check the final number of rows and the first few rows of the final data frame.

    You did not show the number of rows. Use `print(XXX, n = 10, width = Inf)` `-2.0`

- Q6 (28/30) Check the final number of rows and the first few rows of the final data frame.

    You did not show the number of rows. Use `print(XXX, n = 10, width = Inf)` `-2.0`
    
    We accept 94,364 rows, one more than 94,363. 

- Q7 (28/30) Check the final number of rows and the first few rows of the final data frame.

    You did not show the number of rows. Use `print(XXX, n = 10, width = Inf)` `-2.0`

- Q8 (30/40) This question is open ended. Any graphical summaries are good. Since this question didn't explicitly ask for explanations, it's fine students don't give them. Students who give insights should be encouraged.

    You did not numerically or graphically summarize `los` vs the last available lab measurements before ICU stay. `-5.0`

    You did not numerically or graphically summarize `los` vs the first vital measurements within the ICU stay. `-5.0`

### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put a lot auxiliary files into version control. 


### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 18/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.5](https://style.tidyverse.org/syntax.html#long-lines) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

-   [Rule 2.4.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

    Lines 168 (172 and other same violations). `-2.0`

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
