## Uganda Market Monitoring - Food MEB and MEB Calculations
## Last modified 16/06/2020

## Load March price
march_mebs <- read.xlsx("./inputs/wfp_march_mebs_2021.xlsx") %>% 
  mutate(yrmo="202103")


## Medians Calculation

# meb_items <- item_prices %>%
#   select (-uuid, -market_final, -price_maize_g, -price_underwear, -price_charcoal,
#           -price_pads, -price_DAP, -price_NKP, -price_malathion, -price_millet_f, -contains("_price")) %>% 
#   group_by(settlement, district, regions, month) %>% 
#   summarise_all(funs(median(., na.rm = TRUE))) %>% 
#   filter(month %in% prev2_month_number:month_number) 
  
meb_items <- item_prices %>%
  select (-uuid, -market_final, -price_maize_g, -price_underwear, -price_charcoal,
          -price_pads, -price_DAP, -price_NKP, -price_malathion, -price_millet_f, -contains("_price"), -month) %>% 
  group_by(regions, district,settlement, yrmo) %>% 
  summarise_all(funs(median(., na.rm = TRUE))) #%>% 
  # filter(month %in% prev2_month_number:month_number) 
  




## Calculate proximity: if a price is missing take the mean of settlement, or district, otherwise, regions
## this was previous code it fails to preserve previous round meb values that match previous factsheet because
## imputation was done using different data sets. FIXED in code below.

meb_items<- meb_items %>% 
  group_by(settlement, yrmo) %>% 
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
  ungroup() %>% 
  group_by(district, yrmo) %>% 
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
  ungroup() %>% 
  group_by(regions, yrmo) %>% 
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.)))


# this is all prone to error and we need to shift to yrmo instead of month framework anyways. updated above
# meb_items <- meb_items %>%
#   group_by(settlement, month) %>%
#   mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
# 
# meb_items <- meb_items %>% group_by(district, month) %>%
#   mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
# 
# meb_items <- meb_items %>% group_by(regions, month) %>%
#   mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% 
#   ungroup()

prior_2_rounds<- c(yrmo_to_include[length(yrmo_to_include)-1],yrmo_to_include[length(yrmo_to_include)-2])
current_and_prior_round<- c(yrmo_to_include[length(yrmo_to_include)],yrmo_to_include[length(yrmo_to_include)-1])
## If NA put price from last round
meb_items_for_prev_month_imputation<-meb_items %>%
  filter(yrmo %in% prior_2_rounds)

meb_items_for_this_month_imputation<-meb_items %>%
  filter(yrmo %in% current_and_prior_round)

# this improvement should yield better values since we start by aggregating at settlement level
meb_items_this_round <- meb_items_for_this_month_imputation %>%
  group_by(settlement) %>% 
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
  ungroup() %>% 
  group_by(district) %>% 
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
  ungroup() %>% 
  group_by(regions) %>% 
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
  filter(yrmo %in% yrmo_constructed)
# 
# meb_items_this_round <- meb_items_for_this_month_imputation %>%
#   group_by(regions) %>% 
#   mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
#   filter(yrmo %in% yrmo_constructed)

# just for this round i will keep this the same so that the values match... next round, we must change to the same 
# aggregation scheme as above

# To be used from March dataset -------------------------------------------

meb_items_last_round <- meb_items_for_prev_month_imputation %>%
  group_by(settlement) %>%
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>%
  ungroup() %>%
  group_by(district) %>%
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>%
  ungroup() %>%
  group_by(regions) %>%
  mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>%
  filter(yrmo %in% prior_2_rounds[1])


# meb_items_last_round <- meb_items_for_prev_month_imputation %>%
#   group_by(regions) %>% 
#   mutate(across(where(is.numeric),~ifelse(is.na(.),mean(.,na.rm=T),.))) %>% 
#   filter(yrmo %in% yrmo_to_include[length(yrmo_to_include)-1])


# meb_items_this_round <- meb_items_for_this_month_imputation %>%
#   group_by(regions) %>%
#   mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% 
#   filter(month %in% month_number)
# 
# meb_items_last_round <- meb_items_for_prev_month_imputation %>%
#   group_by(regions) %>%
#   mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% 
#   filter(month %in% prev1_month_number)


meb_items<- bind_rows(meb_items_this_round, meb_items_last_round)
    # month=month(month, label = T, abbr=F))

## Calculate MEB for food items
meb_items$meb_maize_f <- meb_items$price_maize_f * 8.7 * 5
meb_items$meb_beans <- meb_items$price_beans * 5.4 * 5
meb_items$meb_sorghum <- meb_items$price_sorghum * 1.5 * 5
meb_items$meb_oil <- meb_items$price_oil * 0.75 * 5
meb_items$meb_salt <- meb_items$price_salt *2* 0.15 * 5 # price of salt is per 500g, meaning(price_salt * 2 )
meb_items$meb_milk <- meb_items$price_milk * 0.3 * 5
meb_items$meb_dodo <- meb_items$price_dodo * 3 * 5
meb_items$meb_fish <- meb_items$price_fish * 0.6 * 5
meb_items$meb_cassava <- meb_items$price_cassava * 0.6 *5


## Calcualte MEB for non-food items
meb_items$meb_soap <- meb_items$price_soap * 0.45 * 5
meb_items$meb_firewood <- meb_items$price_firewood * 1.1 * 30 * 5


## Calcualte MEB for Hygiene items
meb_items$meb1_reusable_pads <- 4667               ## Yearly one-off
meb_items$meb1_jerry_can <- 1090                   ## March
meb_items$meb1_bucket <- 632                       ## Yearly one-off
meb_items$meb1_hand_washing <- 208                 ## Yearly one-off



meb_items <- meb_items %>% mutate(meb_hygiene = meb1_reusable_pads + meb1_jerry_can +
                                                    meb1_bucket + meb1_hand_washing + meb_soap)


## Extra Items
## Add extra columns
meb_items$meb_clothing <- 3806          ## March    
meb_items$meb_water <- 3750             ## March
meb_items$meb_livelihoods <- 37705      ## March
meb_items$meb_education <- 28667        ## March
meb_items$meb_transport <- 11001        ## March
meb_items$meb_health <- 2669            ## March
meb_items$meb_communication <- 4256     ## March
meb_items$meb1_lighting <- 5000         ## March

## One-off Items - Once a year
blanket <- 45000
pans <- 13125
plates <- 4885
spoon <- 3538
cups <- 3985
mingle <- 1000

meb_items <- meb_items %>% mutate(meb_other_hdd = sum(blanket, pans, plates, spoon, cups, mingle)/12)


## MEB Energy
meb_items <- meb_items %>% mutate(meb_energy = meb1_lighting + meb_firewood)


## Food MEB Calcuations
meb_items <- meb_items %>% mutate(meb_food = meb_maize_f + meb_beans + meb_sorghum +
                                                      meb_oil + meb_milk + meb_cassava + meb_salt +
                                                      meb_dodo + meb_fish)


## Full MEB Calcuations
meb_items <- meb_items %>% mutate(meb_full = meb_food + meb_clothing + meb_water +
                              meb_livelihoods + meb_education + meb_transport +
                              meb_health + meb_communication + meb_hygiene +
                              meb_other_hdd + meb_energy
                              )

## Clean Table, round up, and aggregate
## Settlement
meb_items <- meb_items %>% select(-starts_with("price_"), -starts_with("meb1_"))

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

meb_items <- round_df(meb_items, digits = 0)

### Rbind march mebs in
meb_items <- plyr::rbind.fill(march_mebs, meb_items)
# changed to yrmo
meb_items<- meb_items %>% 
  mutate(collection_order = ifelse(yrmo == yrmo_constructed, 4,
                                   ifelse(yrmo == yrmo_to_include[length(yrmo_to_include)-1],3, 1))
  )

meb_items %>% select(yrmo, collection_order)



meb_items <- meb_items %>% select("month", everything())
meb_items <- meb_items %>% select(yrmo, everything())
  



## regions
meb_items_regional <- meb_items %>%  select(-district,-settlement) %>% 
  group_by(regions,yrmo) %>% 
  summarise(across(where(is.numeric),~mean(.,na.rm=T)))


meb_items_regional <- round_df(meb_items_regional, digits = 0)

names(meb_items_regional)[names(meb_items_regional) == "meb_food"] <- "regional_meb_food"
names(meb_items_regional)[names(meb_items_regional) == "meb_full"] <- "regional_meb_full"

## National
# meb_items_national <- meb_items %>%  select(-district,-settlement, -regions) %>% 
#   group_by(month) %>% 
#   summarise_all(funs(mean(., na.rm = TRUE)))

meb_items_national<- meb_items %>%  select(-district,-settlement, -regions) %>% 
  group_by(yrmo) %>% 
  summarise(across(where(is.numeric),~mean(.,na.rm=T)))

meb_items_national$regions <- "nationwide"

meb_items_national <- round_df(meb_items_national, digits = 0)

names(meb_items_national)[names(meb_items_national) == "meb_food"] <- "national_meb_food"
names(meb_items_national)[names(meb_items_national) == "meb_full"] <- "national_meb_full"

## Calculate the top three most expansive settlements
rank_settlments <- meb_items %>% filter(yrmo == yrmo_constructed)

top_settlments <- rank_settlments %>% ungroup () %>% select(settlement, meb_full) %>%
                                 arrange(desc(meb_full))%>% mutate(rank = 1:nrow(rank_settlments)) %>% filter(rank <= 3)




## Calculate the top three least expansive settlemets
bottom_settlments <- rank_settlments %>% ungroup () %>% select(settlement, meb_full) %>%
                                 arrange(desc(meb_full)) %>% mutate(rank = 1:nrow(rank_settlments)) %>% filter(rank >=11)

rank_settlments <- rbind(top_settlments, bottom_settlments)
 
