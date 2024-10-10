library(dplyr)
library(kableExtra)
library(knitr)

# Sample data
df <- data.frame(
  header = c("A", "A", "A", "B", "B", "C", "C", "C", "C"),
  value1 = 1:9,
  value2 = letters[1:9]
)

# Create a new column that blanks out repeated headers
df <- df %>%
  mutate(header = ifelse(duplicated(header), "", header))

# Create the table for the report
kable(df) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

 
