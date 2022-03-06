library(gapminder)
df <- gapminder

#Tidyverse data manipulation
#nearly function can be taken out of the pipeline by passing df as the first argument


#Basic column manipulation: @select, @filter, @arrange, @group_by, @ungroup, @mutate,@transmute
df_new <- df %>%  #This is a pipeline operator (takes next instruction)
  select(col_name1, col_name2, colnam3) %>% # choose select columns
  select(-col_name3) %>% #keep other columns except select ones
  select(col_name3 = col_name2) %>% #select and rename column b to a
  filter(col_name1 > 0) #filter by cell values
  arrange(desc(col_name1)) %>% #sort data , default is increasing, desc is descending
  group_by(col_name1, col_name3) %>% #group data by columns, if more than one column, grouping = all possible combination
  #all operations after group will only be performed within the group
  ungroup() %>% #will ungroup everything
  mutate(new_col = col_name1 + col_name2) #create new column, will replace old one if having the same name
  transmuate(col_name1, col_name2, new_col = col_name1 - col_name2) # transmute = select + mutate

  
#Cleaning data: @count, @n, @duplicated, @replace, @distinct, @is., @as., 
df_new <- df %>%
  mutate(total_observation = n())%>% #n() returns total observation within a group
  count(counted_col)%>% #return frequency of each value within the column, or all possible combinations, new column name is n
  filter(duplicated(df)) %>%#duplicated return if there is a duplicated
  distinct(col1, col2, .keep_all = TRUE) %>% # get rid of duplicates only considering selected cols and keep all other cols
  filter(is.numeric(col)) %>% #is.* tests if something is a certain type
  replace(col_name, col_name_other == 1, NA) %>% #replace values based on condition
  mutate(col_name = as.numeric(as.character(factor_col))) #as.* cast var to new type, for factors, needs to be casted to char before to num
#1. use na.rm = TRUE will remove na data in function
#2. use vis_miss(df) to visualize na data


#Cell value manipulation:@str, @fct_collapse, @date
df_new <- df %>%
  mutate(col_1 = str_to_lower(col_1), col_2 = str_trim(col_2),col_3 = str_replace_all(col3, "-", "/")) %>% #string manipulation: lowercase, trim space, and replace all chars
  filter(str_dect(col_4, "-"), str_length(col_4 >3)) %>% #detect chars in string, and check string length
  fct_collapse(col_1, new_name = c("a","b", "c")) %>% # map many to one value
  mutate(col_1, ifelse(col_1<0, 0, col_1)) %>% #change value based on criteria
  mutate(diff = as.numeric(old_date %--% today(), "years"))%>% #calculate difference in years, %--% can be applied to do diff in dates
  mutate(new_date = parse_date_time(col, order = c("%Y-%m-%d"))) # parse date to a specific format, see ?strftime
#1. lag(col) will give you a lagged vector

#Join tables: 
df_final <- df_1 %>%
  inner_join(df_2, by = c("col_1" = "col2"), suffix = c("_suf1", "_suf2")) %>% # intersection 
  left_join(df_3, by = "same_col") %>% #left 
  right_join(df_4, by = c("col_1", "col_2")) %>% #right 
  full_join(df_5, by = "same_col") %>% # union
  semi_join(df_6, by = "same_col") %>% # U(A,B) - B
  anti_join(df_7, by = "same_col") %>% # U(A,B) - A
  stringdist_left_join(df, col, by = target_str, method="lcs", max_dist = 2) #non-exact matchingstrings
  
#non-exact joining:
pair_blocking(df_a, df_b, blocking_var = "col") %>% #generate pairs of matching col from a,b 
  compare_pairs(by = c("col_1","col_2","col_3"), default_comparator = lcs()) %>% #compare stringdist of desired columns
  score_simsum() %>% #calculate total scores by sum scores
  score_problink() %>% #calculate total scores by probability
  select_n_to_m() %>% #choose top scores, can specify threshold
  link() #link data
  