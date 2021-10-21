
dataset_clean <- function(dataset, sheet_type){
  #Table 4 data
  #column 8 is the start of the current phase information - selecting by column number because of english/french versions
    if(sheet_type == 'current'){
    print("Selecting current Data")
    c1 <-  8
  }else if(sheet_type == 'projected'){
    c1 <- 20
    print("Selecting projected Data")
  }
  #including the first 3 columns which contain geographic info, last 5 columns contain - remove any total field
  dataset_current <- 
    dataset[,c(1,2,3,c1:(c1))] %>% 
    as_tibble()
  #rename columns
  names(dataset_current) = c(
    'adm0_name',
    'adm1_name',
    'adm2_name',
    'final_phase'
  )
  #dont include any totals or info where the geo info is missing
  dataset_current <- dataset_current %>% filter(!is.na(adm0_name))
  #make sure phase information is numeric
  dataset_current <- dataset_current %>% 
    mutate(across(final_phase, as.numeric))
  
  return(dataset_current)
  
}

snc_format <- function(dataset){
  a = dataset
    a  <-
      a[4:nrow(a),c(1,2,4,5,6,7)] %>%
      as_tibble()

    #rename columns
    names(a) = c(
      'adm1_name',
      'adm2_name',
      'foodconsumption_phase',
      'livelihoods_phase',
      'nutrition_phase',
      'mortality_phase'
    )

    a[is.na(a)] = '0'
   
     a = a %>% 
      mutate(across(foodconsumption_phase:mortality_phase, as.numeric))


    a <- a %>% mutate(final_phase_calc = case_when(
      livelihoods_phase == 0 &  nutrition_phase == 0 & mortality_phase == 0 ~ foodconsumption_phase,
      foodconsumption_phase != 0 & livelihoods_phase != 0 & nutrition_phase == 0 & mortality_phase == 0 ~ (3*foodconsumption_phase + 2 * livelihoods_phase)/5,
      foodconsumption_phase != 0 & livelihoods_phase != 0 & nutrition_phase != 0 & mortality_phase == 0 ~ (3*foodconsumption_phase + 2 * livelihoods_phase + nutrition_phase)/6,
      foodconsumption_phase != 0 & livelihoods_phase != 0 & nutrition_phase != 0 & mortality_phase != 0 ~ (3*foodconsumption_phase + 2 * livelihoods_phase + nutrition_phase +mortality_phase)/7)
    )
    a <- a  %>% mutate(final_phase_calc = round(final_phase_calc,0))
    return(a)
}

