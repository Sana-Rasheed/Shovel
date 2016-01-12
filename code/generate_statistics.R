get_benchmark_df <- function(year, state_fp){
  df <- read.csv("data/2013-5yrACS - Copy.csv")
  df <- subset(df, state == state_fp)
  df <- df[sort(names(df))]
  df <- sapply(df, function(x) as.numeric(as.character(x)))
  df <- as.data.frame(df)
  df
}

get_stats <- function(pums_df, pums_vars, benchmark_df, benchmark_var){
  get_num_matches <- function(x){
    get_matches <- logical()
    for(i in seq_along(pums_vars)){
      pums_var <- pums_vars[[i]]
      var_name <- names(pums_vars)[i]
      
      if(names(pums_var) == "equal"){
        if(length(get_matches) == 0){
          get_matches <- x[[var_name]] %in% pums_var[[1]] 
        }
        else{
          get_matches <- get_matches & (x[[var_name]] %in% pums_var[[1]])
        }
      }
      else if(names(pums_var) == "ge"){
        if(length(get_matches) == 0){
          get_matches <- x[[var_name]] >= pums_var[[1]] 
        }
        else{
          get_matches <- get_matches & (x[[var_name]] >= pums_var[[1]])
        }
      }
      else if(names(pums_var) == "le"){
        if(length(get_matches) == 0){
          get_matches <- x[[var_name]] <= pums_var[[1]] 
        }
        else{
          get_matches <- get_matches & (x[[var_name]] <= pums_var[[1]])
        }
      }
    }
    sum(get_matches)
  }
  g <- sapply(split(pums_df, pums_df[["COUNTY_NAME"]]), get_num_matches)
  maxes <- mapply(max, g, benchmark_df[[benchmark_var]])
  percents <- round(100 - abs((g - benchmark_df[[benchmark_var]])/ maxes) * 100, 2)
  percents
}

get_all_percents <- function(dfs, pums_vars_list, benchmark_vars){
  all_percents <- NULL
  
  for(i in seq_along(pums_vars_list)){
    pums_vars <- pums_vars_list[[i]]
    
    for(df in dfs){
      percents <- get_stats(df, pums_vars, benchmark_df, benchmark_vars[i])
      all_percents <- c(all_percents, round(mean(percents), 2), percents)
    }
  }
  all_percents
}

get_row_names <- function(real_names, testing_categories){
  c(sapply(real_names, function(x) paste0(testing_categories, "-", x)))
}

print_total_acc <- function(results){
  total_acc <- NULL
  
  for(i in seq(1, nrow(results), by = length(dfs))){
    new_acc <- results[i,1] - results[i+1, 1]
    total_acc <- c(total_acc, new_acc)
    # names(total_acc) <- c(names(total_acc), rownames(df)[i])
  }
  print("old is better than new by")
  print(sum(total_acc))
  
  total_acc <- NULL
  
#   for(i in seq(1, nrow(results), by = length(dfs))){
#     new_acc <- results[i+1,1] - results[i+2, 1]
#     total_acc <- c(total_acc, new_acc)
#     # names(total_acc) <- c(names(total_acc), rownames(df)[i])
#   }
#   
#   print("best is better than new by")
#   print(sum(total_acc))
}

######################
# Config variables
######################


# get_stats(df_no_vars, list("RAC1P" = list("equal" = 6:6)), benchmark_df, "DP05_0039E")
# get_stats(df_no_vars, list("MAR" = list("equal" = 5:5), "SEX" = list("equal" = 1:1), "AGEP" = list("ge" = 15)), benchmark_df, "DP02_0025E")

pums_vars_list <- list(list("AGEP" = list("ge" = 15), "SEX" = list("equal" = 1:1)),
                       
                       list("AGEP" = list("le" = 4)),
                       list("AGEP" = list("equal" = 15:19)),
                       list("AGEP" = list("equal" = 25:34)),
                       list("AGEP" = list("equal" = 60:64)),
                       
                       list("RAC1P" = list("equal" = 1:1)),
                       list("RAC1P" = list("equal" = 2:2)),
                       list("RAC1P" = list("equal" = 6:6)),
                       
                       list("AGEP" = list("ge" = 25), "SCHL" = list("equal" = 1:11)),
                       list("AGEP" = list("ge" = 25), "SCHL" = list("equal" = 12:15)),
                       list("AGEP" = list("ge" = 25), "SCHL" = list("equal" = 16:17)),
                       list("AGEP" = list("ge" = 25), "SCHL" = list("equal" = 20:20)),
                       list("AGEP" = list("ge" = 25), "SCHL" = list("equal" = 21:21)),
                       list("AGEP" = list("ge" = 25), "SCHL" = list("equal" = 22:24)),
                       
                       list("SCH" = list("equal" = 2:3)),
                       
                       list("FER" = list("equal" = 1:1)),
                       
                       list("MAR" = list("equal" = 5:5), "SEX" = list("equal" = 1:1), "AGEP" = list("ge" = 15)),
                       list("MAR" = list("equal" = 5:5), "SEX" = list("equal" = 2:2), "AGEP" = list("ge" = 15)),
                       list("MAR" = list("equal" = 3:3), "SEX" = list("equal" = 1:1)),
                       list("MAR" = list("equal" = 3:3), "SEX" = list("equal" = 2:2)))

# percents <-  get_stats(new_df, list("AGEP" = list("equal" = 25:34)), benchmark_df, "DP05_0007E")
# percents
# mean(percents[11:20])
# percents <-  get_stats(new_df, list("RAC1P" = list("equal" = 1:1)), benchmark_df, "DP05_0032E")
# percents
# mean(percents[11:20])


benchmark_vars <- c("DP02_0024E", 
                    "DP05_0004E", "DP05_0007E", "DP05_0009E", "DP05_0013E",
                    "DP05_0032E", "DP05_0033E", "DP05_0039E",
                    "DP02_0059E", "DP02_0060E", "DP02_0061E", "DP02_0063E", "DP02_0064E", "DP02_0065E",
                    "DP02_0052E",
                    "DP02_0036E",
                    "DP02_0025E", "DP02_0031E", "DP02_0029E", "DP02_0035E")

real_names <- c("Male/Age >= 25", 
                "Ages 0-4", "Ages 15-19", "Ages 25-34", "Ages 60-64",
                "Race only white", "Race only Black", "Race only Asian",
                "Grade 0-8/Age >= 25", "Grade 9-12 no diploma/Age >= 25", 
                "High School Grad/Age >= 25", "Associates/Age >= 25",
                "Bachelor's degree/Age >= 25", "Above Bachelors or Prof./Age >= 25",
                "Enrolled in school", 
                "Age 15-50 women gave birth past 12 months",
                "Never Married Male/Age >= 15", "Never Married Female/Age >= 15",
                "Divorced Male/Age >= 15", "Divorced Female/Age >= 15")

# testing_categories <- c("BASE", "BEST", "NEW")
# testing_categories <- c("OLD POPS", "NEW POPS")
testing_categories <- c("RACE", "NEW")

new_df <- read.csv("data/pnh13.csv")
# dfs <- list(base_df, best_df, new_df)
# dfs <- list(new_df, new_df2)
dfs <- list(base_df, new_df)

all_percents <- get_all_percents(dfs, pums_vars_list, benchmark_vars)

row_names <- get_row_names(real_names, testing_categories)

fixed_cycling <- as.data.frame(matrix(data = all_percents, ncol = length(percents) + 1, byrow = TRUE), row.names = row_names)
names(fixed_cycling) <- c("Mean %", names(percents))

print_total_acc(fixed_cycling)

# cycling with dump_iter shenanigans improved total_acc by 1.5, but had substantial changes on local level.

# reversing order of applying algorithm improves total_acc by 22.95 (little over 1 per category)

# try getting our own rac1p split from pums file and use that in addition to race across counties
# so these will be like "PUMA00=100 & RAC1P=2".
# compare the percentages of RAC1P=2, PUMA00=100 and RAC1P=2, COUNTY=A and PUMA00=100, COUNTY=A
# (make puma00 match RAC1P=2/COUNTY=A)
# make it so we distribute right number of rac1p=2, puma00=100 rows to countya
# look across all pumas so we know how much to rac1p=2 rows they have contributing

# race_county_pops 
# var_pops <- sapply(split(nh_pums, nh_pums$PUMA10), function(x) sapply(split(x, x$RAC1P), function(y) sum(y$PWGTP)))
# # race_county_dists
# var_dist <- sapply(split(nh_pums, nh_pums$PUMA10), function(x) sapply(split(x, x$RAC1P), function(y) sum(y$PWGTP)/sum(x$PWGTP)))
# 
# mean_var <- NULL
# 
# for(i in 1:9){
#   mean_var <- c(mean_var, round(sd(sapply(var_dist, function(x) ifelse(i %in% names(x), x[[as.character(i)]], 0))), 4))
# }
# # FIRST THING: when distributing new no_matched rows, push all dumped onto 
# # actually, after redistributing again, and doing that for all new categories, go once more and splitup no_matched
# # push all changes to a stack and once its full we can start doing dist stuff as it resolves.
# # use summary file prediction values too for new dists for determining how far off
# 
# names(mean_var) <- 1:9
# mean_var

# the race pop values deviate within each puma (which we can find) and within each county (using to predict)





# maybe the file i pass in doesnt use plain puma00s at all... its all "PUMA00=100 & RAC1P=2" codes
# but then if we make SEX=1 it will just chain more. need to stick with current variable naming/using methodology
#   but i need to take into account number of rac1p in each puma.

######################
# Generate the statistics
######################

# new_results2 is based on jwmnp
# new_results is based on sex




# DP05_0031E: RACE CODES
# Important to check: various ages are distributed correctly, races, education, work/occupation stats, income (family)


# df_old <- read.csv("pnh13(dp).csv")
# df_new <- read.csv("pnh13.csv")
# df_no_vars <- read.csv("pnh13(no_vars).csv")
# df_travel <- read.csv("pnh13(travel).csv")
# df_travel_schl_age <- read.csv("pnh13(travel_schl_age).csv")
# df_dps <- read.csv("pnh13(all_dp).csv")
# dfs <- list(df_old, df_new)


# benchmark_df <- get_benchmark_df(13, 33)
# percents <-  get_stats(new_df, list("AGEP" = list("ge" = 15), "SEX" = list("equal" = 1:1)), benchmark_df, "DP02_0024E")

