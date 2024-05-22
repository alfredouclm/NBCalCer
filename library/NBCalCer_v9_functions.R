
load_config <- function(CONFIG_FILE){
  
  dcnf <<- read.xlsx(CONFIG_FILE,sheet = 1)
  
  aux_rfile <- get_config_parameter("SUBCOUNTRY_REGION_CODES",txt=T)
  if(!file.exists(aux_rfile)){
    print(paste0("Warning: Subcountry region codes file not existing (",aux_rfile,")"))
    REGION_CODES <- c()
  }else{
    aux_regions <- read.csv2(aux_rfile)
    REGION_CODES <- sort(unique(aux_regions[,1]))
  }
  
  CURVE_TYPE  <- str_to_lower(get_config_parameter("CURVE_TYPE",txt = T))
  CURVE_TYPE_RICE  <- str_to_lower(get_config_parameter("CURVE_TYPE_RICE",txt = T))
  
  # CURVE_SEL <- "MEAN"
  CROP_PRICE_MULT <- 1
  FERT_PRICE_MULT <- 1
  
  PARAM_FARM_GATE_PRICES  <- get_config_parameter("PARAM_FARM_GATE_PRICES")
  FERT_PRICE_SOURCE  <- get_config_parameter("FERT_PRICE_SOURCE",txt=T)
  
  GDP_ELAST  <- get_config_parameter("GDP_ELAST")
  COSTS_OTHER_INPUTS_NL  <- get_config_parameter("COSTS_OTHER_INPUTS_NL")
  LABOUR_COSTS_NL  <- get_config_parameter("LABOUR_COSTS_NL")
  
  USE_YIELD_MULTIPLIER <- get_config_parameter("USE_YIELD_MULTIPLIER")

  COUNTRY_CODES <- get_country_codes()
  
  AVOID_YIELD_DECREASE_WITH_INCR_N <- (get_config_parameter("AVOID_YIELD_DECREASE_WITH_INCR_N")==1)
  MAXIMUM_YIELD_DECREASE_WITH_INCR_N <- get_config_parameter("MAXIMUM_YIELD_DECREASE_WITH_INCR_N")
  
  RESULTS_DECIMAL_PLACES <- get_config_parameter("RESULTS_DECIMAL_PLACES")
  
  ALLOW_NEGATIVE_SONR <- get_config_parameter("ALLOW_NEGATIVE_SONR")
  ALLOW_NEGATIVE_EONR <- get_config_parameter("ALLOW_NEGATIVE_EONR")
  PROCESS_REGIONS <- get_config_parameter("PROCESS_REGIONS")
  
  
  STUBBLE_RATIO <- get_config_parameter("STUBBLE_RATIO")
  AVAILABLE_STRAW_RECOVERY_RATIO <- get_config_parameter("AVAILABLE_STRAW_RECOVERY_RATIO")
  
  # Columns behavior for region aggregation
  # Columns to be added, weighted according to the COLBASE_WEIGHT and to be added (independently if there is presence
  # or not in the country of the base variable)

  COLS_REGIONS_ADD <- c("area", "arable_coverage", "absbenef", "absfertcost", "absmeannbenefsyn", "absextcostnloss",
                        "absyield","absyield_straw",
                        "abs_marine_cost","abs_pm25_nh3_cost","abs_msa_cost","absextcostnloss","abs_yll_pm25_nh3")

  COLBASE_WEIGHT <- "area"
  COLBASE_WEIGHT_REQUIRED <- "ypred" # if their value are NA then they will not be included
  
  COLS_REGIONS_WEIGHT <- c("arable_coverage","ymax", "yact", "crpprc", "ndep", "bnf" ,              
                           "SN", "nfert", "nfert_ef", "nfert_year", "nfert_crop","cropintens", 
                           "nman", "nman_max", "nman_ef", "nfert_ef", "perc_urea",    
                           "FEV_MANURE", "FEV_UREA", "FESMan", "Nav", "ypred", "ypred_straw","ypredsnman",            
                           "ncont","ncont_straw", "nremoval","nremoval_grain","nremoval_straw", "nsurplus", "nlosses", "NUE", "NUE_synth","nrfmax",            
                           "fert_price", "other_inputs_price", "labour_cost", "eonr", "sonr",              
                           "nshare1", "nshare2", "margnbenef", "meannbenefsyn", # "meannbenefsynm",    
                           "margcostsurp" , "crpprcyear", "fertpriceyear" ,
                           "nh3em_synth","nh3em_total","udc_pm25_nh3_cost",#"dif_yptrials_farmman","dif_st_lt",
                           "ndep_nr_ratio","RET_GRW","RET_SRO","UC_marine_rivmouth","nh3_manure_perc","WTP_MSA","BC")
  
  
  COLS_REGIONS_ADD_ALL <- c("population", "agric_area", "area_crop_in", "area_crop_ex", "area_grass_in", 
                            "area_grass_ex")
  
  
  COLBASE_WEIGHT_2 <- "population"
  COLS_REGIONS_WEIGHT_2 <- c("GDP")
  
  
  # Columns to add when aggregating the results by adding several crops and/or irrigated/rainfed
  COLS_ADD_AGGREGATE_RESULTS <-  c("area","absbenef","absfertcost","absmeannbenefsyn","absextcostnloss","absyield","absyield_straw",
                                   "abs_marine_cost","abs_pm25_nh3_cost","abs_msa_cost","abs_yll_pm25_nh3")
  
  
  return(list(
    REGION_CODES                          = REGION_CODES                        ,   
    CROP_PRICE_MULT                       = CROP_PRICE_MULT                     , 
    FERT_PRICE_MULT                       = FERT_PRICE_MULT                     , 
    PARAM_FARM_GATE_PRICES                = PARAM_FARM_GATE_PRICES              , 
    FERT_PRICE_SOURCE                     = FERT_PRICE_SOURCE                   , 
    GDP_ELAST                             = GDP_ELAST                           , 
    COSTS_OTHER_INPUTS_NL                 = COSTS_OTHER_INPUTS_NL               , 
    LABOUR_COSTS_NL                       = LABOUR_COSTS_NL                     , 
    COUNTRY_CODES                         = COUNTRY_CODES                       , 
    AVOID_YIELD_DECREASE_WITH_INCR_N      = AVOID_YIELD_DECREASE_WITH_INCR_N    , 
    MAXIMUM_YIELD_DECREASE_WITH_INCR_N    = MAXIMUM_YIELD_DECREASE_WITH_INCR_N  , 
    RESULTS_DECIMAL_PLACES                = RESULTS_DECIMAL_PLACES              , 
    COLS_REGIONS_ADD                      = COLS_REGIONS_ADD                    ,
    COLS_REGIONS_ADD                      = COLS_REGIONS_ADD                    , 
    COLBASE_WEIGHT                        = COLBASE_WEIGHT                      , 
    COLBASE_WEIGHT_REQUIRED               = COLBASE_WEIGHT_REQUIRED             , 
    COLS_REGIONS_WEIGHT                   = COLS_REGIONS_WEIGHT                 , 
    COLS_REGIONS_ADD_ALL                  = COLS_REGIONS_ADD_ALL                , 
    COLS_REGIONS_WEIGHT_2                 = COLS_REGIONS_WEIGHT_2               ,
    COLBASE_WEIGHT_2                      = COLBASE_WEIGHT_2                    , 
    COLS_REGIONS_WEIGHT_2                 = COLS_REGIONS_WEIGHT_2               ,
    COLS_ADD_AGGREGATE_RESULTS            = COLS_ADD_AGGREGATE_RESULTS          ,
    CURVE_TYPE                            = CURVE_TYPE                          ,
    CURVE_TYPE_RICE                       = CURVE_TYPE_RICE                     ,
    STUBBLE_RATIO                         = STUBBLE_RATIO                       ,
    AVAILABLE_STRAW_RECOVERY_RATIO        = AVAILABLE_STRAW_RECOVERY_RATIO      ,
    USE_YIELD_MULTIPLIER                  = USE_YIELD_MULTIPLIER                ,
    PROCESS_REGIONS                       = PROCESS_REGIONS                     ,
    ALLOW_NEGATIVE_SONR                   = ALLOW_NEGATIVE_SONR                 ,
    ALLOW_NEGATIVE_EONR                   = ALLOW_NEGATIVE_EONR
  ))
  
}


calc_SONR_new <- function(YMAX,CRPRICE,FERT_PRICE,YMULT,RET_SRO,RET_GRW,
                           NCONT_GRAIN_D_CONSTANT,NCONT_GRAIN_E_CONSTANT,NCONT_GRAIN_F_CONSTANT,
                           LTN_res,
                           CROP,FEV_SYNTH,FEV_MANURE,SN,NMAN_EF,
                           MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N,
                           CURVE_TYPE_ACT,NCONT_STRAW_ACT,STRAW_GRAIN_RATIO,
                           STRAW_FACTOR,MUC_nrivermouth_marine,NMAN,NMAN_MAX,
                           UDC_NH3_COST,NH3_MANURE_PERC,AREA,ABS_MSA_COST_BASE,
                           EMISS_BASE,ALLOW_NEGATIVE_SONR){
  
  # create an auxiliary funcion for this particular N content
  auxt <- get_config_parameter(paste0("NCONT_",str_to_upper(CROP)),txt = T)
  auxt <- paste0("temp_function_ncont <<- function(X,FEV_SYNTH,SN,NMAN_EF){return(",str_replace(auxt,"NAV","(X*FEV_SYNTH+SN+NMAN_EF)"),")}")
  eval(parse(text=auxt))
  LFUNC <- list(func_ncont=temp_function_ncont)
 
  
  XIN <- seq((-1)*floor(min(NMAN,NMAN_MAX)),400,by=1)
  aux <- sapply(XIN,benef_at_new,
                YMAX=YMAX,
                CRPRICE=CRPRICE,
                FERT_PRICE=FERT_PRICE,
                YMULT=YMULT,
                RET_SRO=RET_SRO,
                RET_GRW=RET_GRW,
                NCONT_GRAIN_D_CONSTANT=NCONT_GRAIN_D_CONSTANT,
                NCONT_GRAIN_E_CONSTANT=NCONT_GRAIN_E_CONSTANT,
                NCONT_GRAIN_F_CONSTANT=NCONT_GRAIN_F_CONSTANT,
                LTN_res=LTN_res,
                CROP=CROP,
                SN=SN,
                FEV_SYNTH=FEV_SYNTH,
                FEV_MANURE=FEV_MANURE,
                NMAN_EF=NMAN_EF,
                MAXIMUM_YIELD_DECREASE_WITH_INCR_N=MAXIMUM_YIELD_DECREASE_WITH_INCR_N,
                AVOID_YIELD_DECREASE_WITH_INCR_N=AVOID_YIELD_DECREASE_WITH_INCR_N,
                CURVE_TYPE_ACT=CURVE_TYPE_ACT,
                NCONT_STRAW_ACT=NCONT_STRAW_ACT,
                STRAW_GRAIN_RATIO=STRAW_GRAIN_RATIO,
                STRAW_FACTOR=STRAW_FACTOR,
                MUC_nrivermouth_marine=MUC_nrivermouth_marine,
                NMAN=NMAN,
                NMAN_MAX=NMAN_MAX,
                UDC_NH3_COST=UDC_NH3_COST,
                NH3_MANURE_PERC=NH3_MANURE_PERC,
                AREA=AREA,
                ABS_MSA_COST_BASE=ABS_MSA_COST_BASE,
                EMISS_BASE=EMISS_BASE,
                LFUNC=LFUNC)
  
  posmax <- which(aux==max(aux))

  if(length(posmax)>1){
    posmax <- min(posmax)
  }
  if(ALLOW_NEGATIVE_SONR>0){
    return(XIN[posmax])
  }
  if(XIN[posmax]<0){ return(0) }
  return(XIN[posmax])
}


benef_at_new <- function(X,YMAX,CRPRICE,FERT_PRICE,YMULT,RET_SRO,RET_GRW,
                         NCONT_GRAIN_D_CONSTANT,NCONT_GRAIN_E_CONSTANT,NCONT_GRAIN_F_CONSTANT,
                         LTN_res,
                         CROP,FEV_SYNTH,FEV_MANURE,SN,NMAN_EF,
                         MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N,
                         CURVE_TYPE_ACT,NCONT_STRAW_ACT,STRAW_GRAIN_RATIO,
                         STRAW_FACTOR,MUC_nrivermouth_marine,NMAN,NMAN_MAX,
                         UDC_NH3_COST,NH3_MANURE_PERC,AREA,ABS_MSA_COST_BASE,
                         EMISS_BASE,LFUNC){
  
  if(X<0){
    NMAN <- min(NMAN,NMAN_MAX) # this is not needed I think but just in case
    NMAN <- NMAN - abs(X)
    NMAN <- max(NMAN,0)
    NMAN_EF <- NMAN*FEV_MANURE
    X <- 0
  }
  
  NCONT_ACT <- LFUNC[["func_ncont"]](X,FEV_SYNTH,SN,NMAN_EF)
  
  LTN_a <- LTN_res$value[which(LTN_res$param=="a")]
  LTN_b <- LTN_res$value[which(LTN_res$param=="b")]
  LTN_c <- LTN_res$value[which(LTN_res$param=="c")]
  
  AUX <- calculate_yields_curve(LTN_a,LTN_b,LTN_c,
                                (X*FEV_SYNTH+SN+NMAN_EF),YMAX,CURVE_TYPE_ACT,CROP
                                ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N)
  YCALC <- AUX[1]
  YCALC <- YCALC * YMULT
  
  # PM25
  NEW_EMISSIONS <- (X-X*FEV_SYNTH+NMAN_EF*NH3_MANURE_PERC)*AREA
  COST_PM25_ACT <- UDC_NH3_COST*NEW_EMISSIONS
  

  # For MSA
  # EMISS_BASE <- (dres$nfert[cnt]-dres$nfert_ef[cnt]+NMAN_EF*NH3_MANURE_PERC)*AREA
  # EMISIONES*dg$ndep_nr_ratio[i] ---- ABS_MSA_COST_BASE*(10^6)
  # NUEVAS_EMISIONES* dg$ndep_nr_ratio[i] ---- xx
  COST_MSA_ACT <- NEW_EMISSIONS*ABS_MSA_COST_BASE*(10^6)/EMISS_BASE

  NREM_CALC <- NCONT_ACT * YCALC * 10 + NCONT_STRAW_ACT * YCALC * STRAW_FACTOR * 10
  NSURP_CALC <- SN + X + min(NMAN,NMAN_MAX) - NREM_CALC
  
  # For Marine
  NLOSS <- (1-RET_SRO)*X+(1-RET_GRW)*max((NSURP_CALC-(X-X*FEV_SYNTH+NMAN_EF*NH3_MANURE_PERC)),0)
  COST_MARINE_ACT <- MUC_nrivermouth_marine*NLOSS*AREA
  
  SPENT_FERT <- (X-SN-NMAN_EF)*FERT_PRICE
  COST_IMPACTS_PHA <- (COST_PM25_ACT+COST_MSA_ACT+COST_MARINE_ACT)/AREA
  
  
  BENEF_CROP <- CRPRICE*YCALC
  
  BENEF_TOTAL <- BENEF_CROP-SPENT_FERT-COST_IMPACTS_PHA
  return(BENEF_TOTAL)
}


deflate_consumer_price <- function(COUNTRY_CODE,ORIGIN_YEAR, DEST_YEAR, ORIG_VALUE, VERBOSE=F){
  DEF_VAL_OY <- get_value(COUNTRY_CODE,NA,ORIGIN_YEAR,"CONSUMER_PRICE_DEFLATORS")
  DEF_VAL_DY <- get_value(COUNTRY_CODE,NA,DEST_YEAR,"CONSUMER_PRICE_DEFLATORS")
  
  if(is.na(DEF_VAL_OY) || is.na(DEF_VAL_DY)){
    print(paste0("Consumer price deflator/s not found for ",COUNTRY_CODE," using ",ORIGIN_YEAR,"..."))
    return(ORIG_VALUE)
  }
  
  PRICE_NEW <- round(ORIG_VALUE * DEF_VAL_DY/DEF_VAL_OY,3)
  if(VERBOSE){ 
    print(paste0("Orig val: ",DEF_VAL_OY," Dest: ",DEF_VAL_DY))
    print(paste0("PRICE DEFLATED FROM ",ORIGIN_YEAR,"(",ORIG_VALUE,") to ",DEST_YEAR,"(",PRICE_NEW,")")) 
  }
  return(PRICE_NEW)
}

get_file_fast <- function(CONFIG_PARAMETER){
  
  aux <- paste0("dREAD_",CONFIG_PARAMETER)
  if(!exists(aux)){
    CSV_NAME <- get_config_parameter(CONFIG_PARAMETER,T)
    
    READ_FUNCTION <- ""
    if(endsWith(str_to_lower(CSV_NAME),".csv")){
      if(CONFIG_PARAMETER=="COUNTRY_CODES_FILE"){
        READ_FUNCTION <- "read.csv" 
      }else{
        READ_FUNCTION <- "read.csv2"
      }
    }
    if(endsWith(str_to_lower(CSV_NAME),".xlsx")){
      READ_FUNCTION <- "openxlsx::read.xlsx"
    }
    if(str_length(READ_FUNCTION)<2){
      print(CSV_NAME)
      error_file_input_extension_not_supported
    }
    auxt <- paste0(aux,"<<- ",READ_FUNCTION,"(CSV_NAME)")
    eval(parse(text=auxt))
    
  }
  auxt <- paste0("d <- ",aux)

  eval(parse(text=auxt))
  return(d)
  
}


get_baseshp_fast <- function(BASEMAPS_FOLDER,BASEMAPS_NAME){
  
  aux <- paste0("dSHP_",BASEMAPS_NAME)
  if(!exists(aux)){
    
    if(!file.exists(paste0(BASEMAPS_FOLDER,BASEMAPS_NAME,'.shp'))){
      print(paste0("Map for ",BASEMAPS_NAME," not found, skipping..."))
      return(NA)
    }
    print(paste0(BASEMAPS_FOLDER,BASEMAPS_NAME,'.shp'))
    auxt <- paste0(aux,"<<- st_read(paste0(BASEMAPS_FOLDER,BASEMAPS_NAME,'.shp'))")
    eval(parse(text=auxt))
  }
  auxt <- paste0("d <- ",aux)
  eval(parse(text=auxt))
  return(d)
  
}


get_code_other <- function(COUNTRY_CODE,id){
  
  dc <- get_file_fast("CODE_EQUIVALENCE_FILE")
  dc$country_code[str_to_lower(dc$country_origname)=="namibia"] <- "NA" # otherwise interpreted as Not Available
  
  x <- dc[which(dc$country_code==COUNTRY_CODE & dc$variable==id),]
  if(length(x[,1])==0){ return(NA)}
  return(x$data)
  
}

get_country_region_list <- function(){
  
  dc <- get_file_fast("REGION_CODES_FILE")
  drclass <- get_config_parameter("REGION_CLASSIFICATION",txt = T)
  
  auxt <- paste0("dregaux <- dc[,which(is.element(colnames(dc),c('iso2c','",str_to_lower(drclass),"_region')))]")
  eval(parse(text=auxt))
  
  colnames(dregaux)[2] <- "region_code"
  return(dregaux)
}


get_region_name <- function(REGION_CODE){
  
  dc <- get_file_fast("REGION_CODES_FILE")
  
  aux <- str_split(REGION_CODE,"_")
  aux <- aux[[1]]
  
  auxt <- paste0("region_name <- dc$",str_to_lower(aux[1]),"_region_name[which(dc$",str_to_lower(aux[1]),"_region=='",aux[2],"')]")
  eval(parse(text=auxt))
  region_name <- unique(region_name)
  if(length(region_name)>1){ unexpected_error_c3 }
  if(length(region_name)==0){ return(NA) }
  return(region_name[1])
  
}

get_region_code <- function(COUNTRY_CODE){
  
  dc <- get_file_fast("REGION_CODES_FILE")
  drclass <- get_config_parameter("REGION_CLASSIFICATION",txt = T)
  
  dc$iso2c[str_to_lower(dc$country)=="namibia"] <- "NA" # otherwise interpreted as Not Available
  
  x <- dc[which(dc$iso2c==COUNTRY_CODE),]
  if(length(x[,1])==0){ return(NA)}
  
  if(grepl("#",drclass)){
    # several ones
    
    aux <- str_split(drclass,"#")
    aux <- aux[[1]]
    
    res <- ""
    for(i in 1:length(aux)){
      auxt <- paste0("resact <- dc$",str_to_lower(aux[i]),"_region[which(dc$iso2c==COUNTRY_CODE)]")
      eval(parse(text=auxt))
      res <- paste0(res,resact)
      if(i<length(aux)){
        res <- paste0(res,"#")
      }
    }
  }else{
    auxt <- paste0("res <- dc$",str_to_lower(drclass),"_region[which(dc$iso2c==COUNTRY_CODE)]")
    eval(parse(text=auxt))
  }
  
  return(res)
  
}

value_or_default <- function(v,default){
  if(is.na(v)) { return(default) }
  return(v)
}

get_value <- function(COUNTRY_CODE,CROP,YEAR,id,SYSTEM=NA,COLUMN="data"){
  vact <- get_value_core(COUNTRY_CODE,CROP,YEAR,id,SYSTEM,COLUMN)
 
  if(exists(paste0("UAPRCH_",id)) & COLUMN=="data"){

    if(id=="FEV_MANURE"){ return(vact) } # already changed in get_value_core
    if(id=="FEV_UREA"){ return(vact) } # already changed in get_value_core
    if(id=="NH3_MANURE_PERC"){ return(vact) } # already changed in get_value_core
    if(id=="RETENTION_SRO_AGRI" | id=="RETENTION_AGRI_SURP"){ 
      
      comp <- 1-vact
      t <- paste0("pertcomp <- comp*(100+UAPRCH_",id,")/100")
      eval(parse(text=t))
      vnew <- 1-pertcomp
      
      return(vnew)
      
    }
    
    t <- paste0("vnew <- vact*(100+UAPRCH_",id,")/100")
    eval(parse(text=t))
    
    return(vnew)
  }
  return(vact)
   
}

get_value_core <- function(COUNTRY_CODE,CROP,YEAR,id,SYSTEM=NA,COLUMN="data"){

  DEFAULT <- as.numeric(get_config_parameter(id,default=T))
  MULT <- get_config_parameter(paste0("MULT_",id))
  if(is.na(MULT)){ MULT <- 1}

  if(id=="FEV_MANURE"){
    FEV <- get_config_parameter("FEV_MANURE")
    return(FEV)
  }
  if(id=="FEV_UREA"){
    FEV <- get_config_parameter("FEV_UREA")
    return(FEV)
  }
  if(id=="NH3_MANURE_PERC"){
    return(get_config_parameter("NH3_MANURE_PERC"))
  }
  
  if(id=="AGRIC_AREA"){
    return(value_or_default(get_value(COUNTRY_CODE,CROP,YEAR,"AREA_CROP_IN",SYSTEM,COLUMN)+
             get_value(COUNTRY_CODE,CROP,YEAR,"AREA_CROP_EX",SYSTEM,COLUMN)+
             get_value(COUNTRY_CODE,CROP,YEAR,"AREA_GRASS_IN",SYSTEM,COLUMN)+
             get_value(COUNTRY_CODE,CROP,YEAR,"AREA_GRASS_EX",SYSTEM,COLUMN),DEFAULT))
  }
  if(id=="FERT_IMPORT_PRICE"){
    
    purea <- get_value(COUNTRY_CODE,CROP,YEAR,"UREA_PERCENTAGE",SYSTEM,COLUMN="data")
    urea_price <- get_value(COUNTRY_CODE,CROP,YEAR,"UREA_IMPORT_PRICE",SYSTEM,COLUMN)
    can_price <- get_value(COUNTRY_CODE,CROP,YEAR,"CAN_IMPORT_PRICE",SYSTEM,COLUMN)

    if(is.element(COLUMN,c("units","year"))){
      aux <- c(urea_price,can_price)
      aux <- aux[!is.na(aux)]
      if(length(aux)==0){ return(NA)}
      return(unique(aux))
    }else{
      return(value_or_default(urea_price*purea + can_price *(1-purea),DEFAULT))
    }

  }
 
  file_act <- get_config_parameter(id,txt=T)

  param_act <- get_config_parameter(id,field = T)
  m_years_act <- get_config_parameter(id,margin_years = T)
  if(id=="YMAX"){
   param_act <- "yw" 
   if(SYSTEM=="irrigated"){
     param_act <- "yp"
   }
  }
  if(!is.na(SYSTEM)){
    param_act <- str_replace_all(param_act,"<system>",SYSTEM)
  }
  if(is.na(m_years_act)){ m_years_act <- 0 }
  
  # Several sources, using each of them sequentially
  if(grepl("#",file_act)){
    
    files_act <- str_split(file_act,"#")[[1]]
    if(grepl("#",m_years_act)){
      ms_years_act <- as.numeric(str_split(m_years_act,"#")[[1]])
    }else{
      ms_years_act <- rep(as.numeric(m_years_act),length(files_act))
    }
  }else{
    files_act <- c(file_act)
    ms_years_act <- c(as.numeric(m_years_act))
  }
  
  
  for(i in 1:length(files_act)){
    
    ract <- get_value_from_file(files_act[i],param_act,COUNTRY_CODE,CROP,YEAR,ms_years_act[i],SYSTEM,COLUMN)

    # For some ymax values
    if(is.na(ract) & !is.na(SYSTEM)){
      ract <- get_value_from_file(files_act[i],param_act,COUNTRY_CODE,paste0(SYSTEM,"_",CROP),YEAR,ms_years_act[i],SYSTEM,COLUMN)
    }
    if(!is.na(ract)){ 
      res <- value_or_default(ract,DEFAULT)
      if(COLUMN=="year"){return(as.numeric(res)*MULT)} # year must be numeric
      if(is.na(as.numeric(res)*MULT)){ return(res) } # assuring not to return text
      return(res*MULT)
    }
  }
  return(value_or_default(NA,DEFAULT))
  

}

get_value_from_file <- function(file_act,param_act,cc,crop=NA,year=NA,margin_years=0,system=NA,column="data"){
  
  cc <- str_to_lower(cc)
  param_act <- str_to_lower(param_act)
  
  dACT <- get_file_fast(file_act)
  
  # otherwise interpreted as Not Available                    
  dACT$country_code[str_to_lower(dACT$country_origname)=="namibia"] <- "NA" 

  dACT$country_code <- str_to_lower(dACT$country_code)
  dACT$variable <- str_to_lower(dACT$variable)
  
  auxyear <- year
  auxcrop <- crop

  s <- subset(dACT,variable==param_act & country_code==cc & crop==auxcrop &  year==auxyear)

  if(length(s[,1])==0){
    s <- subset(dACT,variable==param_act & country_code==cc & crop==auxcrop &
                  year>=(auxyear-margin_years)  & year<=(auxyear+margin_years))
  }
  if(length(s[,1])==0){
    s <- subset(dACT,variable==param_act & country_code==cc & crop==auxcrop & is.na(year))
  }
  if(length(s[,1])==0){
    s <- subset(dACT,variable==param_act & country_code==cc & is.na(crop) & year>=(auxyear-margin_years) & year<=(auxyear+margin_years))
  }
  if(length(s[,1])==0){
    s <- subset(dACT,variable==param_act & country_code==cc & is.na(crop) & is.na(year))
  }
  
  if(length(s$data)>1){
    s$year <- as.numeric(s$year)
    dist <- abs(s$year-auxyear)
    pos <- which(dist==min(dist))
    s <- subset(s,year==s$year[pos[length(pos)]]) # closest looking to the future
  }

  if(column=="data"){
    aux <- check_for_errors(s,param_act)
    if(!is.na(aux)){ return(aux) }
  }

  if(column=="data"){ return(s$data[1]) }
  if(column=="year"){ return(s$year[1]) }
  if(column=="crop"){ return(s$crop[1]) }
  if(column=="units"){ return(s$units[1]) }
  
  return(NA)
  
}

get_usd_year <- function(UNIT,YEAR){
  
  if(is.na(UNIT) & is.na(YEAR)){ return(NA) }
  for(y in 1900:2199){
    if(grepl(y,UNIT)){
      return(y)
    }
  }
  if(is.na(YEAR)){
    stop("Year for USD not defined")
  }
  return(YEAR)
}

get_results_dataframe <- function(MAX_LENGTH){
  AUX <- rep(NA,MAX_LENGTH)
  dres <- data.frame(
    country=AUX,country_code=AUX,
    region_code=AUX,
    cc_iso3=AUX,
    cc_uni=AUX,
    cc_undp=AUX,
    cc_faostat=AUX,
    cc_gaul=AUX,
    crop=AUX,
    system=AUX,
    GDP=AUX,population=AUX,
    agric_area=AUX,
    area_crop_in=AUX,area_crop_ex=AUX,
    area_grass_in=AUX,area_grass_ex=AUX,
    arable_coverage=AUX,
    ymax=AUX,
    yact=AUX,
    area=AUX,
    crpprc=AUX,
    crpprcyear=AUX,
    ndep=AUX,bnf=AUX,SN=AUX,
    nfert=AUX,
    nfert_ef=AUX,
    nfert_year=AUX,
    nfert_crop=AUX,
    cropintens=AUX,
    nman=AUX,nman_max=AUX,
    nman_ef=AUX,
    perc_urea=AUX,
    FEV_MANURE=AUX,
    FEV_UREA=AUX,
    FESMan=AUX,
    Nav=AUX,
    ypred=AUX,
    ncont=AUX,
    ypred_straw=AUX,
    ncont_straw=AUX,
    nremoval=AUX,
    nremoval_grain=AUX,
    nremoval_straw=AUX,
    nsurplus=AUX,
    NUE=AUX,
    NUE_synth=AUX,
    nrfmax=AUX,
    fert_price=AUX,
    fertpriceyear=AUX,
    other_inputs_price=AUX,labour_cost=AUX,
    eonr=AUX,
    sonr=AUX,
    nshare1=AUX,
    nshare2=AUX,
    margnbenef=AUX,
    meannbenefsyn=AUX,
    absbenef=AUX,
    absmeannbenefsyn=AUX, 
    absfertcost=AUX,
    margcostsurp=AUX,
    nlosses=AUX,
    absextcostnloss=AUX,
    absyield=AUX,
    absyield_straw=AUX,
    nh3_manure_perc=AUX,
    ypredsnman=AUX,
    nh3em_synth=AUX,
    nh3em_total=AUX,
    udc_pm25_nh3_cost=AUX,
    abs_yll_pm25_nh3=AUX,
    abs_pm25_nh3_cost=AUX,
    abs_marine_cost=AUX,
    UC_marine_rivmouth=AUX,
    RET_SRO=AUX,
    RET_GRW=AUX,
    abs_msa_cost=AUX,
    WTP_MSA = AUX,
    ndep_nr_ratio=AUX,
    BC=AUX
  )
  
  return(dres)
}



add_units_to_results <- function(dres){
  
  ADD_UNITS <- get_config_parameter("ADD_UNITS_TO_RESULTS")
  if(ADD_UNITS==0){ return(dres) }
  
  dou <- get_output_units()

  # Add the units to the first line
  for(i in 1:length(colnames(dres))){
    x <- which(str_to_lower(colnames(dres)[i])==str_to_lower(dou$output))
    if(length(x)==1){
      colnames(dres)[i] <- paste0(colnames(dres)[i],"_(",dou$units[x],")")
    }
  }

  return(dres)
}

msa_loss_f_core <- function(ndep){
  return( 1 - 1/(1+exp(-(2.245-0.748*log10(ndep)))) )
}

msa_loss_f <- function(ndep){
  vact <- msa_loss_f_core(ndep)
  
  # Change percentage (for Uncertainty analysis)
  if(exists("UAPRCH_MSA_LOSS")){
      vnew <- vact*(100+UAPRCH_MSA_LOSS)/100
      return(vnew)  
  }
  return(vact)

}


get_long_term_N_response_curve <- function(crop,CURVE_TYPE){
  AUX <- rep(NA,3)
  LTN_res <- data.frame(param=AUX,value=AUX)
  LTN_res$param <- c("a","b","c")
  
  if(CURVE_TYPE=="quadratic"){
    if(str_to_lower(crop)=="rice"){
      LTN_res$value <- c(get_config_parameter("RICE_LTN_RESPONSE_CURVE_A"),
                         get_config_parameter("RICE_LTN_RESPONSE_CURVE_B"),NA)
      return(LTN_res)
    }
    LTN_res$value <- c(get_config_parameter("LTN_RESPONSE_CURVE_A"),
                       get_config_parameter("LTN_RESPONSE_CURVE_B"),NA)
  }else{
    if(CURVE_TYPE=="george"){
      LTN_res$value <- c(get_config_parameter("LTN_GEORGE_RESPONSE_CURVE_A"),
                         get_config_parameter("LTN_GEORGE_RESPONSE_CURVE_B"),
                         get_config_parameter("LTN_GEORGE_RESPONSE_CURVE_C"))
      
    }else{
      stop(paste0("Curve type not supported:",CURVE_TYPE))
    }
  }
  return(LTN_res)
  
}

get_config_parameter <- function(param,txt=F,field=F,margin_years=F,default=F){
  
  vact <- get_config_parameter_core(param,txt,field,margin_years,default)
  if(is.na(vact)){ return(vact) }
  if(txt==T){ 
    if(!grepl("NCONT_",param)){ return(vact) }
    if(exists(paste0("UAPRCH_",param))){
      taux <- paste0("changeaux <- UAPRCH_",param)
      eval(parse(text=taux))
      t <- paste0("(",vact,")*(100+(",changeaux,"))/100")
      return(t) 
    }
    if(exists(paste0("UAABS_",param))){
      eval(parse(text=paste0("vnew <- UAABS_",param)))
      return(vnew)
    }
    return(vact)
  }
  if(field==T){ return(vact) }
  if(margin_years==T){ return(vact) }
  
  # Change percentage (for Uncertainty analysis)
  if(exists(paste0("UAPRCH_",param))){
    if(param=="FEV_UREA"){ 
      comp <- 1-vact
      t <- paste0("pertcomp <- comp*(100+UAPRCH_",param,")/100")
      eval(parse(text=t))
      vnew <- 1-pertcomp
      return(vnew)
      
    }else{
    
      t <- paste0("vnew <- vact*(100+UAPRCH_",param,")/100")
      eval(parse(text=t))
      return(vnew)  
    }
  }
  if(exists(paste0("UAABS_",param))){
    eval(parse(text=paste0("vnew <- UAABS_",param)))
    return(vnew)
  }
  
  return(vact)
  
}

get_config_parameter_core <- function(param,txt=F,field=F,margin_years=F,default=F){
  aux <- which(dcnf$parameter==param)
  if(length(aux)==0){ return(NA) }
  if(length(aux)>1){
    print(paste0("Parameter ",param," redefined, using the first appareance"))
    aux <- aux[1]
  }
  if(txt){return(dcnf$textvalue[aux])}
  if(field){return(dcnf$fieldname[aux])}
  if(margin_years){return(dcnf$margin[aux])}
  if(default){return(dcnf$default[aux])}
  return(as.numeric(dcnf$numbervalue[aux]))
  return(res)
}



get_output_units <- function(){
  dou <- get_file_fast("OUTPUT_UNITS_FILE")
  return(dou)
}

get_country_codes <- function(){

  dcc <- get_file_fast("COUNTRY_CODES_FILE")
  COUNTRY_CODES <- dcc$Code
  COUNTRY_CODES[str_to_lower(dcc$Name)=="namibia"] <- "NA" # otherwise interpreted as Not Available
  COUNTRY_CODES <- unique(COUNTRY_CODES)
  COUNTRY_CODES <- COUNTRY_CODES[str_length(COUNTRY_CODES)==2]
  rm("dcc")
  return(COUNTRY_CODES)
}


get_blank_database <- function(){
  
  AUX <- rep(NA,10000)
  d <- data.frame(country_origname=AUX,country_code=AUX,
                  crop=AUX,variable=AUX,units=AUX,year=AUX,
                  campaign=AUX,data=AUX,source=AUX,
                  comments=AUX
                  )
  return(d)
  
}

get_country_by_code <- function(cc){
  
  dcc <- get_file_fast("COUNTRY_CODES_FILE")
  dcc$Code[str_to_lower(dcc$Name)=="namibia"] <- "NA" # otherwise interpreted as Not Available
  
  # to avoid unreadable characters 
  if(cc=="AX"){ return("alandislands")}
  if(cc=="CI"){ return("ivorycoast")} 
  if(cc=="CW"){ return("curacao")} 
  if(cc=="RE"){ return("reunion")} 
  if(cc=="BL"){ return("saintbarthelemy")} 
  
  aux <- which(dcc$Code==cc)
  if(length(aux)>0){
    return(unify_text(dcc$Name[aux[1]]))
  }
  if(cc=="EU 15"){ return(cc) }
  if(cc=="EU 12"){ return(cc) }
  if(cc=="EU 27"){ return(cc) }
  if(cc=="EFMA 29"){ return(cc) }
  
  unexpected_error_country_by_code
}

unify_text <- function(t){
  t <- str_to_lower(t)
  t <- str_replace_all(t," ","")
  t <- str_replace_all(t,"Ã¡","a")
  t <- str_replace_all(t,"Ã©","e")
  t <- str_replace_all(t,"Ã­","i")
  t <- str_replace_all(t,"Ã³","o")
  t <- str_replace_all(t,"Ãº","u")
  t <- str_replace_all(t,"-","")
  t <- str_replace_all(t,"Ã±","n")
  return(t)
  
}

get_country_code <- function(cname){
  
  cc <- get_file_fast("COUNTRY_CODES_FILE")
  cname <- unify_text(cname)
  
  aux <- which(cc$Name==cname)
  if(length(aux)>0){
    return(cc$Code[aux])
  }
  
  if(cname=="eu28"){ return("EU28") }
  if(cname=="eu27"){ return("EU27") }
  if(cname=="world") { return("WORLD") }
  if(cname=="row"){ return("REST_OF_WORLD")}
  if(cname=="viet_nam"){ return(get_country_code("vietnam")) }
  if(cname=="united_states_of_america"){ return(get_country_code("usa")) }
  if(cname=="united_kingdom"){ return(get_country_code("unitedkingdom")) }
  if(cname=="syrian_arab_republic"){ return(get_country_code("syria")) }
  if(cname=="sri_lanka"){ return(get_country_code("srilanka")) }
  if(cname=="south_africa"){ return(get_country_code("southafrica")) }
  
  if(grepl("_",cname)){ return(get_country_code(str_replace_all(cname,"_","")))}
  
  if(cname=="cÃ´ted'ivoire"){ return(get_country_code("cotedivoire")) }
  if(cname=="korearepublicof"){ return(get_country_code("korea,republicof")) }
  
  if(grepl("koreademocraticpeople",cname)){ return("KP") }
  if(grepl("caboverde",cname)){ return("CV") }
  
  if(grepl("china,mainland",cname)){ return("CN_M") }
  if(grepl("wallisandfutunaislands",cname)){ return("WF") }
  
  
  if(length(which(grepl(cname,REGIONS_IMAGE_USA)))>0){ return(paste0("RG_USA_(",cname,")")) }
  if(length(which(grepl(cname,REGIONS_IMAGE_CHINA)))>0){ return(paste0("RG_CN_(",cname,")")) }
  if("xizang(tibet)"==cname){ return(paste0("RG_CN_(xizang_tibet)")) }
  
  if(length(which(grepl(cname,REGIONS_IMAGE_GENERAL)))>0){ return(paste0("RG_WRL_(",cname,")")) }
  if(length(which(grepl(cname,REGIONS_IMAGE_EUROPA)))>0){ return(paste0("RG_EU_(",cname,")")) }
  
  print(paste0(cname," not found"))
  return("CODE_NOT_FOUND")
}

trim_database <- function(db){
  aux <- min(which(is.na(db[,1])))-1
  db <- db[1:aux,]
  return(db)
}

check_for_errors <- function(s,value,SHOW_NOT_FOUND_WARNING=T){
  
  value <- str_to_lower(value)
  s <- s[which(!is.na(s$data)),]

  if(length(s$data)>1){
    if(length(s$data)==2){
      if(s$data[1]!=s$data[2]){
        
        # Special cases
        if(str_to_upper(s$country_code[1])=="IN" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        if(str_to_upper(s$country_code[1])=="BR" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        if(str_to_upper(s$country_code[1])=="JP" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        if(str_to_upper(s$country_code[1])=="MX" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        if(str_to_upper(s$country_code[1])=="ZA" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        if(str_to_upper(s$country_code[1])=="TR" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        if(str_to_upper(s$country_code[1])=="US" & value=="n-deposition"){ return(max(s$data)) } # Duplicated value
        
        if(str_to_upper(s$country_code[1])=="VG"){ return(max(s$data)) } # Duplicated value
        
        if(str_to_upper(s$country_code[1])=="NL"){
          if(str_to_lower(str_replace_all(s$country_origname[1]," ",""))=="netherlandsantilles"){ return(s$data[2])}
          if(str_to_lower(str_replace_all(s$country_origname[2]," ",""))=="netherlandsantilles"){ return(s$data[1])}
          
        }
        
        perror <- abs(s$data[1]-s$data[2])/(min(s$data))
        if(perror<0.00001){ # this happened with US with 2 data almost identical
          return(s$data[1])
        }
        print(paste0("Percentage error: ",perror))
        print(s$data[1])
        print(s$data[2])
        print(s$data[1]-s$data[2])
        print(value)
        print(s)
        unexpected_error_c1
      }
    }else{
      unexpected_error_c2
    }
  }
  if(length(s$data)==0){
    return(NA)
  }
  
  return(NA) 
}


calculate_yields_curve <- function(LTN_a,LTN_b,LTN_c,NAV,YMAX,CURVE_TYPE,CROP
                              ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N){
  if(CURVE_TYPE=="quadratic"){
    return(calculate_yields_curve_quadratic(LTN_a,LTN_b,NAV,YMAX,CROP,
                                            MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N))
  }
  if(CURVE_TYPE=="george"){
    return(calculate_yields_curve_george(LTN_a,LTN_b,LTN_c,NAV,YMAX))
  }
  stop(paste0("Curve type not found: ",CURVE_TYPE))
}



calculate_yields_curve_george  <- function(cpA,cpB,cpC,NAV,YMAX){
  
  resY <- c()
  for(i in 1:length(cpA)){
    pA <- as.numeric(cpA[i])
    pB <- as.numeric(cpB[i])
    pC <- as.numeric(cpC[i])
    
    if(!is.na(NAV)){
      
      YPRED <- max((pA+pB*(0.99^NAV)+pC*NAV)*YMAX,0)
      resY <- c(resY,YPRED)
      
    }else{
      resY <- c(resY,NA)
    }
    
  }
  return(resY)
  
}

calculate_yields_curve_quadratic <- function(cpA,cpB,NAV,YMAX,crop,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N){
  
  MAX_YDECR <- MAXIMUM_YIELD_DECREASE_WITH_INCR_N
  if(crop=="maize"){ MAX_YDECR <- 0 }
  
  resY <- c()
  for(i in 1:length(cpA)){
    pA <- as.numeric(cpA[i])
    pB <- as.numeric(cpB[i])
    
    if((!AVOID_YIELD_DECREASE_WITH_INCR_N) || is.na(NAV) || is.na(YMAX)){
      YPRED <- max(min(1,NAV*pA + (NAV^2)*pB)*YMAX,0)
      resY <- c(resY,YPRED)
      next
    }
    
    # derivate: pA+2*NAV*pB
    # equals to 0 for maximum
    # max -pA/(2*pB)
    X_APPL_MAX <- -pA/(2*pB)
    YPRED_MAX <- YMAX
    YPREDaux <- min(1,NAV*pA + (NAV^2)*pB)*YMAX
    
    if(NAV<X_APPL_MAX){
      YPRED <- YPREDaux
    }else{
      YPRED <- max(YPREDaux,YPRED_MAX*(100-MAX_YDECR)/100)
      # YPRED <- min(YPREDaux,YPRED_MAX*(100-MAX_YDECR)/100)
    }
    
    resY <- c(resY,max(YPRED,0))
  }
  return(resY)
  
}



aggregate_results <- function(dres,systems,crops,
                              COLBASE_WEIGHT,
                              COLBASE_WEIGHT_REQUIRED,
                              COLS_ADD_AGGREGATE_RESULTS,
                              first_num_col="GDP"){
  
  col_weight <- COLBASE_WEIGHT
  col_req_weight <- COLBASE_WEIGHT_REQUIRED
  COLS_ADD <- COLS_ADD_AGGREGATE_RESULTS
  col_aggr <- "region_code"
  if(is.element("country_code",colnames(dres))){ col_aggr <- "country_code" }
  
  
  unique_elements <- unique(dres[,which(colnames(dres)==col_aggr)])
  dagg <- dres[1:length(unique_elements),]
  dagg[,] <- NA
  
  numfc <- (which(colnames(dres)==first_num_col))
  
  cnt <- 1  
  for(i in unique_elements){
    
    auxt <- paste0("x <- subset(dres,",col_aggr,"=='",i,"')")
    eval(parse(text=auxt))

    auxt <- paste0("x <- x[!is.na(x$",col_aggr,")]")
    eval(parse(text=auxt))
    
    dagg[cnt,1:(numfc-1)] <- x[1,1:(numfc-1)]
    x <- subset(x,is.element(crop,crops) & is.element(system,systems))

    if(length(x[,1])==0){  next}

    
    xnum <- x[,numfc:dim(dres)[2]]
    xnonum <- x[,1:(numfc-1)]
    
    auxt <- paste0("aux <- which(is.na(xnum$",col_req_weight,"))")
    eval(parse(text=auxt))
    if(length(aux)>0){
      auxt <- paste0("xnum$",col_weight,"[aux]<-0")
      eval(parse(text=auxt))
    }
    
    aux_sum <- apply(xnum,c(2),sum,na.rm=T)

    auxt <- paste0("aux <- xnum*xnum$",col_weight,"/sum(xnum$",col_weight,",na.rm=T)")
    eval(parse(text=auxt))
    
    aux_weight <- apply(aux,c(2),sum,na.rm=T)
    
    lcrop <- paste(crops,collapse="_")
    lsys <- paste(systems,collapse="_")
    if(lsys=="irrigated_rainfed"){ lsys <- "total" }
    if(grepl("_",lcrop)){
      lcrop <- str_replace_all(lcrop,"maize","mz")
      lcrop <- str_replace_all(lcrop,"barley","ba")
      lcrop <- str_replace_all(lcrop,"rice","rc")
      lcrop <- str_replace_all(lcrop,"wheat","wh")
    }
    
    dagg$crop[cnt] <- lcrop
    dagg$system[cnt] <- lsys
    
    for(j in numfc:dim(dres)[2]){
      if(is.element(colnames(dres)[j],COLS_ADD)){
        dagg[cnt,j] <- aux_sum[j-numfc+1]
      }else{
        dagg[cnt,j] <- aux_weight[j-numfc+1]
      }
      if(length(which(is.na(x[,j])))==length(x[,j])){
        dagg[cnt,j] <- NA # Na instead of 0 if everyone is NA
      }
      
    }
    
    cnt <- cnt + 1
  }
  
  aux <- which(is.na(dagg[,1]))
  if(length(aux)>0){
    dagg <- dagg[-aux,]  
  }
  
  return(dagg)
  
  
}

limit_length <- function(cadenas, maxlen) {
  
  
  cadenas <- str_replace_all(cadenas,"crop_in","cp_in")
  cadenas <- str_replace_all(cadenas,"crop_ex","cp_ex")
  cadenas <- str_replace_all(cadenas,"grass_in","gr_in")
  cadenas <- str_replace_all(cadenas,"grass_ex","gr_ex")
  # We limit the length of the strings
  str_limited <- substr(cadenas, 1, maxlen)
  
  i <- 2
  aux <- which(duplicated(str_limited))
  print(aux)
  while(any(duplicated(str_limited))){
    for(a in aux){
      str_limited[a] <- paste0(substr(str_limited[a],1,maxlen-str_length(paste0(i,""))),i)
      i <- i + 1
    }
  }
  
  return(str_limited)
}


save_shp <- function(shp,shp_file,overwrite=F){
  
  colnames(shp) <- limit_length(colnames(shp),10)
  
  if(file.exists(shp_file)){
    if(overwrite){
      file.remove(shp_file)
      file.remove(str_replace_all(shp_file,".shp",".dbf"))
      file.remove(str_replace_all(shp_file,".shp",".shx"))
      st_write(shp,shp_file)
    }
  }else{
    st_write(shp,shp_file)
  }
  
}



save_results_in_files <- function(dres,sfolder,
                                  COLBASE_WEIGHT,
                                  COLBASE_WEIGHT_REQUIRED,
                                  COLS_ADD_AGGREGATE_RESULTS,overwr=T){
  
  
  SAVE_MAPS <- get_config_parameter("SAVE_MAPS")
  if(SAVE_MAPS==1){
    BASEMAPS_FOLDER <- get_config_parameter("BASEMAPS_FOLDER",txt=T)
    SUBCOUNTRY_MAPS <- get_config_parameter("SUBCOUNTRY_MAPS",txt=T)
  }
  
  dcountry <- dres[which(!grepl("_",dres$country_code)),]
  dsubnational <- dres[which(grepl("_",dres$country_code) & !grepl("region",dres$country)),]
  
  
  dregions <- dres[which(grepl("_",dres$country_code) & grepl("region",dres$country)),]
  # Add regions names and remove some columns
  dregions <- dregions[,-c(1,3,5:8)]
  colnames(dregions)[1] <- "region_code"
  colnames(dregions)[2] <- "region_name"
  dregions$region_name <- mapply(get_region_name,dregions$region_code)
  
  
  REGION_CLASSIFICATION <- get_config_parameter("REGION_CLASSIFICATION",txt=T)
  
  if(!grepl("#",REGION_CLASSIFICATION)){
    regions_files <- c(paste0("world_regions_",REGION_CLASSIFICATION))
    regions_names <- c(REGION_CLASSIFICATION)
  }else{
    aux <- str_split(REGION_CLASSIFICATION,"#")[[1]]
    regions_names <- aux
    regions_files <- c()
    for(i in 1:length(aux)){
      regions_files <- c(regions_files,paste0("world_regions_",aux[i]))
    }
  }
  
  # Generate different datasets for each region
  dregions_datasets <- c()
  for(i in 1:length(regions_names)){
    dregion_act <- subset(dregions,grepl(paste0(str_to_lower(regions_names[i]),"_"),
                                         str_to_lower(region_code)))
    auxt <- paste0("dregion_",regions_names[i]," <- dregion_act")
    eval(parse(text=auxt))
    rm("dregion_act")
    dregions_datasets <- c(dregions_datasets,paste0("dregion_",regions_names[i]))
  }
  
  NAMES <- c(regions_files,"world_countries","country_states")
  DATASETS <- c(dregions_datasets,"dcountry","dsubnational")

  for(i in 1:length(NAMES)){

    MISSING_VALUE <- get_config_parameter("MISSING_VALUE")
    auxt <- paste0(DATASETS[i],"[is.na(",DATASETS[i],")]<- MISSING_VALUE")
    eval(parse(text=auxt))
    
    
    print(paste0("Processing ",NAMES[i],"..."))
    dir_act <- paste0(sfolder,NAMES[i],"/")
    dir_maps_act <- paste0(dir_act,"maps/")
    dir.create(dir_act,showWarnings = F)
    
    auxt <- paste0("dres_act <- ",DATASETS[i])
    eval(parse(text=auxt))
    
    print(DATASETS[i])
    
    if(nrow(dres_act)==0){ next }
    
    print(DATASETS[i])
    
    dres_agg <- dres_act
    
    systems <- sort(unique(dres_act$system))
    crops <- sort(unique(dres_act$crop))
    
    if((get_config_parameter("PROCESS_COMBINATIONS",txt = F)>0) |
       (exists("PROCESS_COMBINATIONS_WORLD_COUNTRIES") && 
        NAMES[i]=="world_countries" & PROCESS_COMBINATIONS_WORLD_COUNTRIES==T)){

      for(a in 1:length(systems)){
        
        st <- combinations(length(systems),a,systems)
        for(aa in 1:length(st[,1])){
          for(b in 1:length(crops)){
            cp <- combinations(length(crops),b,crops)
            for(bb in 1:length(cp[,1])){
              
              if(length(st[aa,])==1 & length(cp[bb,])==1){ next }
              
                dres_agg <- rbind(dres_agg,aggregate_results(
                                                  dres_act,st[aa,],c(cp[bb,]),
                                                  COLBASE_WEIGHT,
                                                  COLBASE_WEIGHT_REQUIRED,
                                                  COLS_ADD_AGGREGATE_RESULTS
                                                  ))
            }
          }
        }
      }
    }

    openxlsx::write.xlsx(add_units_to_results(dres_agg),paste0(dir_act,NAMES[i],".xlsx"),overwrite = overwr)
    
    dres_act <- dres_agg # so it saves also combinations
    if(SAVE_MAPS==1){
      
      dir.create(dir_maps_act,showWarnings = F)
      if(DATASETS[i]=="dsubnational"){

        aux <- strsplit(SUBCOUNTRY_MAPS,"#")
        aux <- aux[[1]]
        
        for(rg in aux){
          
          aux2 <- strsplit(rg,"_")[[1]]
          isocode <- aux2[1]
          selcol <- aux2[2]
          
          for(sy in unique(dres_act$system)){
            for(cr in unique(dres_act$crop)){
              
              if(!overwr & file.exists(paste0(dir_maps_act,"map_",isocode,"_",cr,"_",sy,".shp"))){
                next
              }
              
              dactrg <- subset(dres_act,grepl(paste0(isocode,"_"),country_code) & crop==cr & system==sy)
              dactrg <- dactrg %>% separate(country_code,c("iso2","state"),"_")
    
              shpact <- get_baseshp_fast(BASEMAPS_FOLDER,isocode)
              if(is.na(shpact)){ next }
              colnames(dactrg)[which(colnames(dactrg)=="state")] <- selcol

              shpact <- left_join(shpact,dactrg,selcol)
              save_shp(shpact,paste0(dir_maps_act,"map_",isocode,"_",cr,"_",sy,".shp"),overwr)
              
            }
          }
        }
        
        
      }else{
        
        if(DATASETS[i]=="dcountry"){
          for(sy in unique(dres_act$system)){
            for(cr in unique(dres_act$crop)){
              
              if(!overwr & file.exists(paste0(dir_maps_act,"map_",cr,"_",sy,".shp"))){
                next
              }
              
              dact <- subset(dres_act,crop==cr & system==sy)
              
              wraux <- str_to_lower(str_replace(NAMES[i],"world_",""))
              # the country shapefile is based on the info from the package
              # rworldmap, not used for having some functions already deprecated
              shpact <- get_baseshp_fast(BASEMAPS_FOLDER,wraux)
              if(is.na(shpact)){ next }
              dact$ISO_A2 <- dact$country_code # to do the join
              
              shpact <- left_join(shpact,dact,"ISO_A2")
              save_shp(shpact,paste0(dir_maps_act,"map_",cr,"_",sy,".shp"),overwr)

            }
          }
        }else{
          # World regions
          for(sy in unique(dres_act$system)){
            for(cr in unique(dres_act$crop)){
              
              if(!overwr & file.exists(paste0(dir_maps_act,"map_",cr,"_",sy,".shp"))){
                next
              }
              
              dact <- subset(dres_act,crop==cr & system==sy)
              
              wraux <- str_to_lower(str_replace(NAMES[i],"world_regions_",""))
              shpact <- get_baseshp_fast(BASEMAPS_FOLDER,wraux)
              if(is.na(shpact)){ next }
              dact$region_cod <- dact$region_code # at the shp it is called region_cod (10 characters max)
              dact$region_cod <- str_replace(dact$region_cod,paste0(str_to_lower(wraux),"_"),"")
              dact$region_cod <- str_replace(dact$region_cod,paste0(str_to_upper(wraux),"_"),"")
              
              
              if(str_to_lower(wraux)=="image"){
                dact$region_cod <- as.numeric(dact$region_cod)
              }
              
              shpact <- left_join(shpact,dact,"region_cod")
              save_shp(shpact,paste0(dir_maps_act,"map_",cr,"_",sy,".shp"),overwr)

            }
              
          }
        }
      }
      
    }
  }
  
}
