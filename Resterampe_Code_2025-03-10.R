## Resterampe
```{r}
# only consider rows with assigned cat_1 (once all categories are assigned)
# df_rq2 <- df_rq2 %>%
#  filter(!(is.na(cat_1)))
```

```{r check filtering and which data is included, include=FALSE, eval=FALSE}
# check which values are not matched
df_rq2_test <- df_rq2 %>%
  filter(is.na(cat_1))

# only consider the values that are matched in df_rq2
df_rq2_test <- df_rq2 %>%
  filter(!is.na(cat_1))

# without "keine angabe", "nichts", "other" and is.na
df_rq2_test <- df_rq2 %>%
  filter(!is.na(cat_1)
         & str_detect(cat_1, "keine angabe", negate = TRUE)
         & str_detect(cat_1, "nichts", negate = TRUE) 
         & str_detect(cat_1, "other", negate = TRUE))

# only "keine angabe", "nichts", "other" and is.na
df_rq2_test <- df_rq2 %>%
  filter(!is.na(cat_1) & 
           ( str_detect(cat_1, "keine angabe")
             | str_detect(cat_1, "nichts")))

# df_rq2_ <- df_rq2_
```


```{r prepare the data, include=FALSE}
# put the data into the long format
# df_long <- df_rq2 %>%
#  pivot_longer(cols = c(cat_1),
#               names_to = "category_type", 
#               values_to = "category")

# rename cat_1 to category
df_rq2_long <- df_rq2 %>%
  rename(category = X5_en) # English names
# rename(category = cat_1) # German names


df_rq2_long <- df_rq2_long %>%
  filter(!is.na(category) & category != "")

# df_rq2_long without following values in cat_1: "keine angabe", "sonstige", "nichts", "vermutliche falscheingabe"
df_rq2_long <- df_rq2_long %>%
  filter(!(str_detect(cat_1, "keine angabe") | str_detect(cat_1, "nichts") | str_detect(cat_1, "vermutliche falscheingabe"))) # | str_detect(cat_1, "sonstige")

# counting the occurrence for every activity and each category
table_rq2a <- df_rq2_long %>%
  group_by(act_no, category) %>%
  summarize(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = category, values_from = count, values_fill = 0)

# sum of the rows (for each activity)
table_rq2a_ <- table_rq2a %>%
  mutate(act_no = as.character(act_no)) %>%
  mutate(sum = rowSums(select(., -act_no)))

# sum of the columns (for each category)
#####

# Calculate column sums for each learning outcome category
column_sums <- colSums(select(table_rq2a_, -act_no, -sum), na.rm = TRUE)

# Create a tibble for the totals row
total_row <- tibble(
  act_no = "Total",
  !!!column_sums  # Using the `!!!` operator to spread the sums into columns
)

# Bind the totals row to the original table
table_rq2a_ <- table_rq2a_ %>%
  bind_rows(total_row)

table_rq2a <- table_rq2a_
rm(table_rq2a_)
# rm(table_rq2a_with_totals)
rm(total_row)
```



```{r parent_cat, include=FALSE, eval=FALSE}
# Step 1: Join table_rq2a with lo_framework to get parent categories
# We assume the categories in table_rq2a are in a column named `category` (replace it with the actual name).
category_totals <- table_rq2a_ %>%
  filter(act_no == "Total") %>%  # Filter for the total row
  select(-act_no, -sum) %>%  # Remove the 'Total' identifier
  pivot_longer(everything(), names_to = "category", values_to = "total") %>%
  left_join(lo_framework %>% select(X5), by = c("category" = "X5"))  # Join to find parent categories

# Trim whitespace and then perform the match
category_totals$parent_cat <- lo_framework$X1_en[match(trimws(category_totals$category), trimws(lo_framework$X5))]

#####
# Step 2: Aggregate totals by parent category
parent_category_totals <- category_totals %>%
  group_by(parent_cat) %>%
  summarise(total = sum(total, na.rm = TRUE))

# Step 3: Construct the new table with parent categories
new_table <- parent_category_totals %>%
  mutate(act_no = "Total") %>%
  select(act_no, parent_cat, total) %>%
  pivot_wider(names_from = parent_cat, values_from = total, values_fill = list(total = 0))  # Create the wide format

```