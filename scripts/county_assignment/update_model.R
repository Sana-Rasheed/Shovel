library(jsonlite)
library(stringr)

tract_map <- read.csv("data/helpers/2010_Census_Tract_to_2010_PUMA.txt")
tract_pops <- read.csv("data/helpers/us2010trf.txt", header = FALSE)
fips_names <- read.csv("data/helpers/national_county.txt", header = FALSE)
puma_map <- read.csv("data/helpers/puma2k_puma2010.csv")

names(fips_names) <- c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "CLASSFP")
fips_names <- fips_names[,c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME")]

names(tract_pops) <- unlist(strsplit("STATE00,COUNTY00,TRACT00,GEOID00,POP00,HU00,PART00,AREA00,AREALAND00,STATE10,COUNTY10,TRACT10,GEOID10,POP10,HU10,PART10,AREA10,AREALAND10,AREAPT,AREALANDPT,AREAPCT00PT,ARELANDPCT00PT,AREAPCT10PT,AREALANDPCT10PT,POP10PT,POPPCT00,POPPCT10,HU10PT,HUPCT00,HUPCT10", split = ","))
tract_pops <- tract_pops[,c("STATE10", "COUNTY10", "TRACT10", "POP10")]

puma_map <- puma_map[,c("state","puma2k","puma12","afact")]

############################
# Finding puma code population value functions
############################

translate_county <- function(county){
  fips_names$COUNTYNAME[which(fips_names$STATEFP == state_fp & fips_names$COUNTYFP == county)]
}

translate_statefp <- function(state){
  fips_names$STATE[which(fips_names$STATEFP == state)][[1]]
}

translate_state <- function(state){
  fips_names$STATEFP[which(fips_names$STATE == state)][[1]]
}

translate_puma10 <- function(state, puma){
  subs <- puma_map[which(puma_map$state == state & puma_map$puma12 == puma),]
  t <- subs$afact
  names(t) <- subs$puma2k
  t
}

get_pop <- function(state_tract_pops, county, tract){
  subset(state_tract_pops, state_tract_pops$COUNTY10 == county & state_tract_pops$TRACT10 == tract)$POP10[1]
}

get_state_dist <- function(state){
  # generates puma10 distribution for state
  state_tracts <- subset(tract_map, STATEFP == state)
  state_tract_pops <- subset(tract_pops, tract_pops$STATE10 == state)
  
  call_get_pop <- function(x) get_pop(state_tract_pops, x[["COUNTYFP"]], x[["TRACTCE"]])
  get_puma_pop <- function(y) lapply(split(y, y$PUMA5CE), function(x) apply(x, 1, call_get_pop))
  
  t <- lapply(split(state_tracts, state_tracts$COUNTYFP), get_puma_pop)
  
  # (puma, pop) pairs for each county
  county_puma_pops <- lapply(t, function(x) sapply(x, sum))   
  # (puma, pop) pairs for the state
  puma_pops <- sapply(get_puma_pop(state_tracts), sum)
  # Now we calculate (puma, pop percentage) pairs for each county
  county_puma_dist <- county_puma_pops
  
  for(i in seq_along(county_puma_pops)){
    county <- t[[i]]
    for(j in seq_along(county)){
      county_puma_dist[[i]][[j]] <- county_puma_dist[[i]][[j]] / puma_pops[[as.character(names(county)[[j]])]]
    }
  }
  county_puma_dist
}

state_dist_10_to_00 <- function(state, state_dist10){
  # we create a puma00 distribution in the same format as get_state_dist
  # we break down each puma to its puma00 codes in each county, multiplying by the corresponding conv. factor
  lapply(state_dist10, function(county_dist10){
    full_puma00_dist <- NULL
    
    for(j in seq_along(county_dist10)){
      puma10 <- names(county_dist10)[j]
      old_pumas <- translate_puma10(state, puma10)
      
      puma00_dist <- sapply(old_pumas, function(x) x * county_dist10[[j]])
      
      for(k in seq_along(puma00_dist)){
        puma00 <- names(puma00_dist)[k]
        if(puma00 %in% names(full_puma00_dist)){
          full_puma00_dist[[puma00]] <- puma00_dist[[k]] + full_puma00_dist[[puma00]]
        }
        else{
          full_puma00_dist <- c(full_puma00_dist, puma00_dist[[k]])
          names(full_puma00_dist)[length(full_puma00_dist)] <- puma00
        }
      }
    }
    
    full_puma00_dist
  })
}

invert_state_dist <- function(state_dist){
  # inverts the state_dist so we have it ordered by puma, county
  puma_county_dist <- list()
  
  for(i in seq_along(state_dist)){
    county <- names(state_dist)[i]
    county_dist <- state_dist[[i]]
    
    for(j in seq_along(county_dist)){
      puma <- names(county_dist)[j]
      
      if(!(puma %in% names(puma_county_dist))){
        puma_county_dist[[puma]] <- list()
      }
      
      puma_county_dist[[puma]][[county]] <- county_dist[[j]]
    }
  }
  puma_county_dist
}

get_puma_dist <- function(county_puma_dist){
  puma_county_dist <- invert_state_dist(county_puma_dist)
  
  pumas <- sort(as.integer(names(puma_county_dist)))
  counties <- names(county_puma_dist)
  
  df <- as.data.frame(matrix(0, nrow = length(pumas), ncol = length(counties)))
  rownames(df) <- pumas
  colnames(df) <- counties
  
  for(i in seq_along(county_puma_dist)){
    county <- names(county_puma_dist)[i]
    puma_dist <- county_puma_dist[[i]]
    
    for(j in seq_along(puma_dist)){
      puma <- names(puma_dist)[j]
      
      insert_at_row <- which(rownames(df) == puma)
      insert_at_col <- which(colnames(df) == county)
      
      df[insert_at_row, insert_at_col] <- puma_dist[[j]]
    }
  }
  df
}
############################
# Summary file API related functions
############################

request_codes <- function(year, state_fp, codes){
  # year is last 2 digits, codes are codes to read in
  key <- "d6622f46c9dfb7184a6c988673952bdc9711f06b"
  df <- array()
  for(code in seq_along(codes)){
    variable <- codes[code]
    if(grepl("DP", variable)){
      url <- paste0("http://api.census.gov/data/20", year, "/acs5/profile?get=",variable,"&for=county:*&in=state:*&key=", key)
    }
    else{
      url <- paste0("http://api.census.gov/data/20", year, "/acs5?get=",variable,"&for=county:*&in=state:*&key=", key)
    }
    data <- readLines(url)
    arr <- fromJSON(data)
    colnames(arr) <- arr[1,]
    arr <- arr[-1, ]

    if(is.na(df)){
      df <- arr
    }
    else{
      df <- merge(df, arr, by = c("state","county"), all = TRUE)
    }
  }
  df <- subset(df, as.numeric(as.character(df$state)) == state_fp)
  df <- df[, !(colnames(df) %in% c("state"))]
  names(df) <- c("county", categories)
  
  df
}

get_var_names <- function(data_profile, first, start, end){
  if(data_profile){
    sapply(start:end, function(x) paste0("DP", sprintf("%02d", first), "_", sprintf("%04d", x), "E"))
  }
  else{
    sapply(start:end, function(x) paste0("B", sprintf("%05d", first), "_", sprintf("%03d", x), "E"))
  }
}

############################
# Model config
############################

categories = NULL

# categories = c(categories, "PINCP=1-9999", "PINCP=10000-14999", "PINCP=15000-24999", "PINCP=25000-34999", 
#            "PINCP=35000-49999", "PINCP=50000-64999", "PINCP=65000-74999", "PINCP=75000-9999999")

# categories = c(categories, "JWMNP=0-4", "JWMNP=5-9", "JWMNP=10-14", "JWMNP=15-19", "JWMNP=20-24", "JWMNP=25-29", 
#                "JWMNP=30-34", "JWMNP=35-39", "JWMNP=40-44", "JWMNP=45-59", "JWMNP=60-89", "JWMNP=90-200")

# categories = c(categories, "JWDP=1-18", "JWDP=19-24", "JWDP=25-30", "JWDP=31-36", "JWDP=37-42", "JWDP=43-48", "JWDP=49-54",
#                "JWDP=55-60", "JWDP=61-66", "JWDP=67-78", "JWDP=79-84", "JWDP=85-90", "JWDP=91-114", "JWDP=115-150")

# categories = c(categories, sapply(c("SCHL=1-11", "SCHL=12-15", "SCHL=16-17", "SCHL=18-19", "SCHL=20", "SCHL=21", "SCHL=22-24"),
#                                   paste0, " & AGEP=25-99"))

categories = c(categories, "AGEP=0-4", "AGEP=5-9", "AGEP=10-14", "AGEP=15-19", "AGEP=20-24", "AGEP=25-34", "AGEP=35-44", 
                "AGEP=45-54", "AGEP=55-59", "AGEP=60-64", "AGEP=65-74", "AGEP=75-84", "AGEP=85-99")
# # 
# categories = c(categories, "SEX=1", "SEX=2")

# categories = c(categories, sapply(c("MAR=5", "MAR=1", "MAR=4", "MAR=2", "MAR=3"), paste0, " & SEX=1 & AGEP=15-99"))

# categories = c(categories, sapply(c("MAR=5", "MAR=1", "MAR=4", "MAR=2", "MAR=3"), paste0, " & SEX=2 & AGEP=15-99"))

categories = c(categories, "RAC1P=1", "RAC1P=2", "RAC1P=3-5", "RAC1P=6", "RAC1P=7", "RAC1P=8", "RAC1P=9")



# income_codes <- get_var_names(FALSE, 6010, 4, 11)
# traveltime_codes <- get_var_names(FALSE, 8303, 2, 13)
# leavetime_codes <- get_var_names(FALSE, 8302, 2, 15)
# schl_var_codes <- get_var_names(TRUE, 2, 59, 65)
age_var_codes <- get_var_names(TRUE, 5, 4, 16)
# sex_var_codes <- get_var_names(TRUE, 5, 2, 3)
# men_mar_codes <- get_var_names(TRUE, 2, 25, 29)
# women_mar_codes <- get_var_names(TRUE, 2, 31, 35)
race_codes <- c(get_var_names(FALSE, 2001, 2, 8))

# codes <- c(traveltime_codes, leavetime_codes, schl_var_codes, age_var_codes, sex_var_codes,
#            men_mar_codes, women_mar_codes)
# codes <- c(leavetime_codes, age_var_codes, sex_var_codes)
# codes <- c(traveltime_codes, sex_var_codes)
codes <- c(age_var_codes, race_codes)
# codes <- c(race_codes)
year <- 13
state_fp <- 33

############################
# Now we start running script
############################

df <- request_codes(year, state_fp, codes)

state_dist_10 <- get_state_dist(state_fp)
state_dist_00 <- state_dist_10_to_00(state_fp, state_dist_10)

puma_dist_00 <- get_puma_dist(state_dist_00)
puma_dist_10 <- get_puma_dist(state_dist_10)

nh_subs <- subset(nh_pums, as.numeric(nh_pums[["PUMA10"]]) == -9)
puma00_totals <- sapply(split(nh_subs, nh_subs$PUMA00), function(x){
  sum(x$PWGTP)
})

nh_subs <- subset(nh_pums, as.numeric(nh_pums[["PUMA00"]]) == -9)
puma10_totals <- sapply(split(nh_subs, nh_subs$PUMA10), function(x){
  sum(x$PWGTP)
})

puma_names <-  sapply(rownames(puma_dist_00), function(puma) paste0("PUMA00=", puma))
for(puma_name in puma_names){
  df[,puma_name] <- numeric()
}
insert_index <- which(puma_names[1] == names(df))
df <- df[,c(1,insert_index:ncol(df), 2:(insert_index-1))]

for(county in names(puma_dist_00)){
  county_index <- which(as.numeric(as.character(df[["county"]])) == county)
  
  for(i in seq_along(puma_dist_00[[county]])){
    df[county_index, i + 1] <- round(puma_dist_00[[county]][i] * puma00_totals[i])
  }
}

puma_names <-  sapply(rownames(puma_dist_10), function(puma) paste0("PUMA10=", puma))
for(puma_name in puma_names){
  df[,puma_name] <- numeric()
}
insert_index <- which(puma_names[1] == names(df))
df <- df[,c(1,insert_index:ncol(df), 2:(insert_index-1))]

for(county in names(puma_dist_10)){
  county_index <- which(as.numeric(as.character(df[["county"]])) == county)
  
  for(i in seq_along(puma_dist_10[[county]])){
    df[county_index, i + 1] <- round(puma_dist_10[[county]][i] * puma10_totals[i])
  }
}


df[["county"]] <- as.character(apply(df, 1, function(x) translate_county(as.numeric(x[["county"]]))))

write.csv(df, paste0("data/sum_", year, "_files.csv"), row.names = FALSE)


# CURRENTLY ASSUMES nh_pums=ss13pnh.csv is read in to get total puma code numbers. 
# if we keep assuming this, then would have to read in pums file when making model
# sum over all puma10 codes plus sum over all puma00 codes in sum_13_files.csv is the total number of rows in ss13pnh

# suppose breaks down by puma00 codes first.
#   places all puma10 only data in its own no_match bucket
#   once we add in puma10 code levels, the whole no_match bucket will be distributed ok
#   the already distributed pum00 codes will make a level for each of the puma10 codes.
#     this entire level should go into no_match and carry over the num_people for later breakdowns.

# a central idea is that we are determining the size of each no_match when compiling the hierarchy,
#   rather than when we are applying it row by row
# When we already know it, this enables the previous comment block

# to implement more advanced subtraction methods is as simple as returning a different iterator then cycle in new_traverse

# on row level, need to have a if var==no_matches handling

# t <- read.csv("data/pnh13.csv")
# s <- split(t, t$COUNTY_NAME)
# generated <- sapply(s, function(x) nrow(x))
# generated <- sapply(s, function(x) sum(x[["PWGTP"]]))
# 
# actual <- c(60305, 47399, 76115, 31653, 89658, 405184, 147171, 300621, 125604, 43103)
# 
# real_actual <- c(60088, 47818, 77117, 33055, 89118, 400721, 146445, 295223, 123143, 43742)
# 
# 
# names(actual) <- names(generated)
# 
# # 
# 
# s <- split(nh_pums, nh_pums$PUMA00)
# 
# sapply(s, function(x) sum(x[["PWGTP"]]))


