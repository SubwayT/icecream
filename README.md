# icecream
#アイスクリームの需要推定

data <- readr::read_csv(file.choose(), locale = locale(encoding = "shift-jis"))

mutate(data,nossum2020=nos20201_3+nos20204_6+nos20207_9+nos202010_12)

data <-
  data %>%
  dplyr::mutate(nossum2020=nos20201_3+nos20204_6+nos20207_9+nos202010_12)

data<-
  data %>%
  dplyr::mutate(nossum2021=nos20211_3+nos20214_6+nos20217_9+nos202110_12)


data<-
  data %>%
  dplyr::mutate(nopsum2020=nop20201_3+nop20204_6+nop20207_9+nop202010_12)

data<-
  data %>%
  dplyr::mutate(nopsum2021=nop20211_3+nop20214_6+nop20217_9+nop202110_12)

data<-
  data %>%
  dplyr::mutate(log_nossum2020 = log(nossum2020),
                log_nossum2021 = log(nossum2021),
                log_nopsum2020 = log(nopsum2020),
                log_nopsum2021 = log(nopsum2021))

data<-
  data %>%
  dplyr::mutate(log_price = log(price))


ols_intro <-
  fixest::feols(log_nossum2020 ~ log_price + capa + energy + solids + fat,data=data)

fixest::etable( ols_intro, 
                se = "hetero",
                signif.code = NA, 
                fitstat = c("r2", "n" ) , 
                dict = c(log_price = "log(価格)",
                         capa = "容量",
                         energy = "エネルギー",
                         solids = "乳固形分",
                         fat= "乳脂肪分",
                         `(Intercept)` = "定数項"),
                digits = 2,
                digits.stats = 2,
                depvar = FALSE)

data <-
  data %>%
  dplyr::mutate(inside_totalA = sum(nossum2020)) 

data <-
  data %>%
  dplyr::mutate(inside_totalB = sum(nossum2021)) 

data <-
  data %>%
  dplyr::mutate(outside_totalA = 109747000- inside_totalA,
                outside_totalB = 109186000- inside_totalB)

data <-
  data %>%
  dplyr::mutate(shareA =  nossum2020/109747000,
                shareA0 = outside_totalA / 109747000) 
data <-
  data %>%  
  dplyr::mutate(shareB =  nossum2021/109186000,
                shareB0 = outside_totalB /109186000 )

data <-
  data %>%
  dplyr::mutate(logit_shareA = log(shareA) - log(shareA0))

data <-
  data %>%
  dplyr::mutate(logit_shareB = log(shareB) - log(shareB0))

olsA <-
  fixest::feols(logit_shareA ~ price + capa + energy + solids + fat,data=data)

olsB <-
  fixest::feols(logit_shareB ~ price + capa + energy + solids + fat,data=data)

summary(olsA)
summary(olsB)

data <-
  data %>%
  dplyr::group_by(maker) %>%
  dplyr::mutate(
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sum_own = ~ sum(.x, na.rm = TRUE) )   ),
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sqr_sum_own = ~ sum(.x^2, na.rm = TRUE) ) ),
    group_n = n()
  ) %>%
  dplyr::ungroup()

data <- 
  data %>% 
  dplyr::mutate( 
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sum_mkt = ~ sum(.x, na.rm = TRUE) )  ),
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sqr_sum_mkt = ~ sum(.x^2, na.rm = TRUE) )    ),
    mkt_n = n())

data <- 
  data %>% 
  dplyr::mutate(
    iv_BLP_own_capa = capa_sum_own - capa,
    iv_BLP_own_energy = energy_sum_own - energy,
    iv_BLP_own_solids = solids_sum_own - solids,
    iv_BLP_own_fat = fat_sum_own - fat,
    iv_BLP_other_capa = capa_sum_mkt - capa_sum_own,
    iv_BLP_other_energy = energy_sum_mkt - energy_sum_own,
    iv_BLP_other_solids = solids_sum_mkt - solids_sum_own,
    iv_BLP_other_fat = fat_sum_mkt - fat_sum_own) 

iv_BLPA <-
  fixest::feols(
    logit_shareA ~ capa + energy + solids + fat | 0 |
      price ~ iv_BLP_own_capa + iv_BLP_own_energy + iv_BLP_own_solids + iv_BLP_own_fat + 
      iv_BLP_other_capa + iv_BLP_other_energy + iv_BLP_other_solids + iv_BLP_other_fat,
    data = data)

iv_BLPB <-
  fixest::feols(
    logit_shareB ~ capa + energy + solids + fat | 0 |
      price ~ iv_BLP_own_capa + iv_BLP_own_energy + iv_BLP_own_solids + iv_BLP_own_fat + 
      iv_BLP_other_capa + iv_BLP_other_energy + iv_BLP_other_solids + iv_BLP_other_fat,
    data = data)

summary(iv_BLPA)
summary(iv_BLPB)

data <-
  data %>%
  dplyr::group_by(maker,jenre) %>%
  dplyr::mutate(
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sum_own = ~ sum(.x, na.rm = TRUE) )   ),
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sqr_sum_own = ~ sum(.x^2, na.rm = TRUE) ) ),
    group_n = n()
  ) %>%
  dplyr::ungroup()

data <- 
  data %>% 
  dplyr::group_by(jenre) %>%
  dplyr::mutate( 
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sum_mkt = ~ sum(.x, na.rm = TRUE) )  ),
    dplyr::across( c("capa", "energy", "solids" , "fat"),
                   list( sqr_sum_mkt = ~ sum(.x^2, na.rm = TRUE) )    ),
    mkt_n = n()
  ) %>%
  dplyr::ungroup() 

data <- 
  data %>% 
  dplyr::mutate(
    iv_BLP_own_capa_nest = capa_sum_own - capa,
    iv_BLP_own_energy_nest =energy_sum_own - energy,
    iv_BLP_own_solids_nest = solids_sum_own - solids,
    iv_BLP_own_fat_nest = fat_sum_own - fat,
    iv_BLP_other_capa_nest = capa_sum_mkt - capa_sum_own,
    iv_BLP_other_energy_nest = energy_sum_mkt - energy_sum_own,
    iv_BLP_other_solids_nest = solids_sum_mkt - solids_sum_own, 
    iv_BLP_other_fat_nest = fat_sum_mkt - fat_sum_own,
    iv_BLP_own_num_nest = group_n - 1, 
    iv_BLP_other_num_nest = mkt_n - group_n) 

data <- 
  data %>% 
  dplyr::group_by(jenre) %>% 
  dplyr::mutate( sum_A_body = sum(nossum2020)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( inside_shareA = nossum2020 / sum_A_body, 
                 log_inside_shareA = log(nossum2020 / sum_A_body) )

data <- 
  data %>% 
  dplyr::group_by(jenre) %>% 
  dplyr::mutate( sum_B_body = sum(nossum2020)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( inside_shareB = nossum2020 / sum_B_body, 
                 log_inside_shareB = log(nossum2020 / sum_B_body) )

olsA <-
  fixest::feols(logit_shareA ~ price + log_inside_shareA + capa + energy + solids + fat | 0 , data = data)

olsB <-
  fixest::feols(logit_shareB ~ price + log_inside_shareB + capa + energy + solids + fat | 0 , data = data)

summary(olsA)
summary(olsB)

iv_BLP2A <-
  fixest::feols(
    logit_shareA ~ capa + energy + solids + fat | 0 |
      price + log_inside_shareA ~ iv_BLP_own_capa_nest + iv_BLP_own_energy_nest + iv_BLP_own_solids_nest + iv_BLP_own_fat_nest + 
      iv_BLP_other_capa_nest + iv_BLP_other_energy_nest + iv_BLP_other_solids_nest + iv_BLP_other_fat_nest + iv_BLP_own_num_nest +  
      iv_BLP_other_num_nest,
    data = data
  )

iv_BLP2B <-
  fixest::feols(
    logit_shareB ~ capa + energy + solids + fat | 0 |
      price + log_inside_shareB ~ iv_BLP_own_capa_nest + iv_BLP_own_energy_nest + iv_BLP_own_solids_nest + iv_BLP_own_fat_nest + 
      iv_BLP_other_capa_nest + iv_BLP_other_energy_nest + iv_BLP_other_solids_nest + iv_BLP_other_fat_nest + iv_BLP_own_num_nest +  
      iv_BLP_other_num_nest,
    data = data
  )
summary(iv_BLP2A)
summary(iv_BLP2B)
