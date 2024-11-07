<h1> The sumvar package</h1>

The sumvar package speeds up data analysis tasks, for reviewing continuous and categorical variables.
sumvar aims to bring the ease and simplicity of the "sum" and "tab" functions from stata, to R. 

To explore the distribution of a single continuous variable use <b>dist_one()</b>. <br>
To explore a continuous variable stratified by a grouping variable use <b>dist_sum()</b>.

To summarise a single categorical variable use <b>tab1()</b>, and for a twoway table from two categorical variables- use <b>tab()</b>. 
Both <b>dist_sum()</b> and <b>tab()</b> include options for performning frequestist hypothesis tests.

Explore duplicates with <b>dup()</b> and missing values with <b>miss()</b>.

All funcitons are tidyverse/dplyr-friendly and accept the %>% pipe from a tibble or data frame, and output summaries as a tibble.
Tibble outputs for all sumvar functions can be saved and then manipulated.
