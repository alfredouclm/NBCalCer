
NBCalCer_run <- function(CONFIG_FILE, RESULTS_FOLDER,SIMULATION_ID=NA,SUMMARY_FILE=NA,CALCULATE_SONR=F,
    PROCESS_COMBINATIONS_WORLD_COUNTRIES=F){

  PROCESS_COMBINATIONS_WORLD_COUNTRIES <<- PROCESS_COMBINATIONS_WORLD_COUNTRIES

  CONFIG_DATA <- load_config(CONFIG_FILE)

  REGION_CODES                          = CONFIG_DATA$REGION_CODES
  CROP_PRICE_MULT                       = CONFIG_DATA$CROP_PRICE_MULT
  FERT_PRICE_MULT                       = CONFIG_DATA$FERT_PRICE_MULT
  PARAM_FARM_GATE_PRICES                = CONFIG_DATA$PARAM_FARM_GATE_PRICES
  FERT_PRICE_SOURCE                     = CONFIG_DATA$FERT_PRICE_SOURCE
  GDP_ELAST                             = CONFIG_DATA$GDP_ELAST
  COSTS_OTHER_INPUTS_NL                 = CONFIG_DATA$COSTS_OTHER_INPUTS_NL
  LABOUR_COSTS_NL                       = CONFIG_DATA$LABOUR_COSTS_NL
  COUNTRY_CODES                         = CONFIG_DATA$COUNTRY_CODES
  AVOID_YIELD_DECREASE_WITH_INCR_N      = CONFIG_DATA$AVOID_YIELD_DECREASE_WITH_INCR_N
  MAXIMUM_YIELD_DECREASE_WITH_INCR_N    = CONFIG_DATA$MAXIMUM_YIELD_DECREASE_WITH_INCR_N
  RESULTS_DECIMAL_PLACES                = CONFIG_DATA$RESULTS_DECIMAL_PLACES
  COLS_REGIONS_ADD                      = CONFIG_DATA$COLS_REGIONS_ADD
  COLS_REGIONS_ADD                      = CONFIG_DATA$COLS_REGIONS_ADD
  COLBASE_WEIGHT                        = CONFIG_DATA$COLBASE_WEIGHT
  COLBASE_WEIGHT_REQUIRED               = CONFIG_DATA$COLBASE_WEIGHT_REQUIRED
  COLS_REGIONS_WEIGHT                   = CONFIG_DATA$COLS_REGIONS_WEIGHT
  COLS_REGIONS_ADD_ALL                  = CONFIG_DATA$COLS_REGIONS_ADD_ALL
  COLS_REGIONS_WEIGHT_2                 = CONFIG_DATA$COLS_REGIONS_WEIGHT_2
  COLBASE_WEIGHT_2                      = CONFIG_DATA$COLBASE_WEIGHT_2
  COLS_REGIONS_WEIGHT_2                 = CONFIG_DATA$COLS_REGIONS_WEIGHT_2
  COLS_ADD_AGGREGATE_RESULTS            = CONFIG_DATA$COLS_ADD_AGGREGATE_RESULTS
  CURVE_TYPE                            = CONFIG_DATA$CURVE_TYPE
  CURVE_TYPE_RICE                       = CONFIG_DATA$CURVE_TYPE_RICE
  STUBBLE_RATIO                         = CONFIG_DATA$STUBBLE_RATIO
  AVAILABLE_STRAW_RECOVERY_RATIO        = CONFIG_DATA$AVAILABLE_STRAW_RECOVERY_RATIO
  USE_YIELD_MULTIPLIER                  = CONFIG_DATA$USE_YIELD_MULTIPLIER
  PROCESS_REGIONS                       = CONFIG_DATA$PROCESS_REGIONS
  ALLOW_NEGATIVE_SONR                   = CONFIG_DATA$ALLOW_NEGATIVE_SONR
  ALLOW_NEGATIVE_EONR                   = CONFIG_DATA$ALLOW_NEGATIVE_EONR

  dir.create(RESULTS_FOLDER,showWarnings=F)
  dir.create(paste0(RESULTS_FOLDER,"results/"),showWarnings=F)

  if(PROCESS_REGIONS<1){
    REGION_CODES <- c()
  }

  YEAR <- get_config_parameter("YEAR")
  YEAR_USD <- YEAR #get_config_parameter("YEAR_USD")

  auxSYS <- get_config_parameter("SYSTEMS",txt = T)
  auxCRP <- get_config_parameter("CROPS",txt = T)

  SYSTEMS <- strsplit(auxSYS,"#")[[1]]
  CROPS <- strsplit(auxCRP,"#")[[1]]

  dres <- get_results_dataframe((length(COUNTRY_CODES)+length(REGION_CODES))*
                                  (length(CROPS)+1)*
                                  length(SYSTEMS)
                                  +1)
  cnt<- 1
  cdot <- 1

  for(SYSTEM in SYSTEMS){
    for(COUNTRY_CODE in c(COUNTRY_CODES,REGION_CODES)){

      COUNTRY_CODE_ORIG <- COUNTRY_CODE
      for(CROP in CROPS){

        CROPPING_INTENSITY <- 1
        if(CROP=="rice"){
          CROPPING_INTENSITY <- get_config_parameter("CROPPING_INTENSITY_RICE")
        }

        COUNTRY_CODE <- COUNTRY_CODE_ORIG
        REGION_CODE <- NA

        # Regions
        if(grepl("_",COUNTRY_CODE)){
          aux <- strsplit(COUNTRY_CODE,"_")
          aux <- aux[[1]]
          COUNTRY_CODE <- aux[1]
          REGION_CODE <- aux[2]
          if(length(aux)>2){
            REGION_CODE <- paste0(REGION_CODE,"_",aux[3])
          }
        }

        # Get the parameters each time when the crop changes
        if(CROP=="rice"){
          CURVE_TYPE_ACT <-CURVE_TYPE_RICE
        }else{
          CURVE_TYPE_ACT <-CURVE_TYPE
        }
        LTN_res <- get_long_term_N_response_curve(CROP,CURVE_TYPE_ACT)

        LTN_a <- LTN_res$value[which(LTN_res$param=="a")]
        LTN_b <- LTN_res$value[which(LTN_res$param=="b")]
        LTN_c <- LTN_res$value[which(LTN_res$param=="c")]

        COUNTRY_NAME <- get_country_by_code(COUNTRY_CODE)

        AUX_CODE <- COUNTRY_CODE
        if(!is.na(REGION_CODE)){
          AUX_CODE <- paste0(COUNTRY_CODE,"_",REGION_CODE)
        }
        
        AGRIC_AREA <- get_value(AUX_CODE,CROP,YEAR,"AGRIC_AREA")
        AREA_CROP_IN <- get_value(AUX_CODE,CROP,YEAR,"AREA_CROP_IN")
        AREA_CROP_EX <- get_value(AUX_CODE,CROP,YEAR,"AREA_CROP_EX")
        AREA_GRASS_IN <- get_value(AUX_CODE,CROP,YEAR,"AREA_GRASS_IN")
        AREA_GRASS_EX <- get_value(AUX_CODE,CROP,YEAR,"AREA_GRASS_EX")

        AREA <- get_value(AUX_CODE,CROP,YEAR,"AREA",SYSTEM)

        GDP <- get_value(AUX_CODE,CROP,YEAR,"GDP")

        GDP_units <-  get_value(AUX_CODE,CROP,YEAR,"GDP",COLUMN = "units")
        GDP_year<-  get_value(AUX_CODE,CROP,YEAR,"GDP",COLUMN = "year")
        GDP_USD_year <- get_usd_year(GDP_units,GDP_year)

        if(is.na(GDP)){
          # If not available try the country code (in case this is a subnational region)
          GDP <- get_value(COUNTRY_CODE,CROP,YEAR,"GDP")
          GDP_units <-  get_value(COUNTRY_CODE,CROP,YEAR,"GDP",COLUMN = "units")
          GDP_year<-  get_value(COUNTRY_CODE,CROP,YEAR,"GDP",COLUMN = "year")
          GDP_USD_year <- get_usd_year(GDP_units,GDP_year)
        }

        POP <- get_value(AUX_CODE,CROP,YEAR,"POPULATION")*1000

        AREA_IR <- get_value(AUX_CODE,CROP,YEAR,"AREA","irrigated")
        AREA_RF <- get_value(AUX_CODE,CROP,YEAR,"AREA","rainfed")
        AREA_ACT <- get_value(AUX_CODE,CROP,YEAR,"AREA",SYSTEM)
        if(is.na(AREA_IR)){ AREA_IR <- 0 }
        if(is.na(AREA_RF)){ AREA_RF <- 0 }
        if(is.na(AREA_ACT)){ AREA_ACT <- 0 }


        NFERT <- get_value(AUX_CODE,CROP,YEAR,"NFERT",SYSTEM)
        if(is.na(NFERT) & grepl("_",AUX_CODE)){
          print(paste0("NFERT not found for ",AUX_CODE," using country (",COUNTRY_CODE,")"))
          NFERT <- get_value(COUNTRY_CODE,CROP,YEAR,"NFERT",SYSTEM)
        }

        NFERT <- NFERT / CROPPING_INTENSITY

        NFERT_year <- get_value(AUX_CODE,CROP,YEAR,"NFERT",SYSTEM,COLUMN="year")
        aux_crop <- get_value(AUX_CODE,CROP,YEAR,"NFERT",SYSTEM,COLUMN="crop")
        if(is.na(aux_crop)){
          NFERT_crop <- 0
        }else{
          NFERT_crop <- 1
        }

        CC_ISO3 <- get_code_other(COUNTRY_CODE,"ISO3")
        CC_UNI <- get_code_other(COUNTRY_CODE,"UNI")
        CC_UNDP <- get_code_other(COUNTRY_CODE,"UNDP")
        CC_FAOSTAT <- get_code_other(COUNTRY_CODE,"FAOSTAT")
        CC_GAUL <- get_code_other(COUNTRY_CODE,"GAUL")

        WORLD_REGION_CODE <- get_region_code(COUNTRY_CODE)


        YACT <- get_value(AUX_CODE,CROP,YEAR,"YACT",SYSTEM)
        YMAX <- get_value(AUX_CODE,CROP,YEAR,"YMAX",SYSTEM)

        YMULT <- 1
        if(USE_YIELD_MULTIPLIER>0){
          # I use the wheat multiplier (if any) for barley too
          YMULT <- get_value(AUX_CODE,str_replace_all(CROP,"barley","wheat"),YEAR,"YIELD_MULTIPLIER",SYSTEM)
          if(is.na(YMULT)){ YMULT <- 1 }
        }

        YMULT <- YMULT*CROPPING_INTENSITY

        YMAX_IR <- get_value(AUX_CODE,CROP,YEAR,"YMAX","irrigated")
        YMAX_RF <- get_value(AUX_CODE,CROP,YEAR,"YMAX","rainfed")


        # calculation for updating when both ymax are available and areas are both larger than 0ha
        # and current area is larger than 50kha
        if(!is.na(YMAX_IR) & !is.na(YMAX_RF) & AREA_ACT>50000 &
           AREA_IR>0 & AREA_RF>0){# & AREA_IR>50000 & AREA_RF>50000){

          SYNT_N_AUX <- (AREA_IR+AREA_RF)*NFERT

          PRC_AREA_IR <- AREA_IR / (AREA_IR+AREA_RF)
          PRC_AREA_RF <- AREA_RF / (AREA_IR+AREA_RF)
          PRC_YW <- YMAX_RF / YMAX_IR

          PROP_A <- PRC_AREA_RF*PRC_YW
          PROP_B <- PRC_AREA_IR*1

          RATIO_RF_IR <- PROP_A/PROP_B

          # RATIO_RF_IR times in rainfed each 1 of irrigated
          # SYNTH_IR + RATIO_RF_IR*SYNTH_IR = SYNT_N_AUX
          # (1+RATIO_RF_IR)*SYNTH_IR = SYNT_N_AUX
          SYNTH_IR = SYNT_N_AUX / (1+RATIO_RF_IR)
          SYNTH_RF = SYNTH_IR * RATIO_RF_IR

          if(!is.na(SYNT_N_AUX)){
          if(abs((SYNTH_IR+SYNTH_RF)-SYNT_N_AUX)>1){
            error_this_should_not_happen
          }
          }

          NFERT_RF <- SYNTH_RF/AREA_RF
          NFERT_IR <- SYNTH_IR/AREA_IR
          if(AREA_RF==0){ NFERT_RF <- 0 }
          if(AREA_IR==0){ NFERT_IR <- 0 }

          if(SYSTEM=="irrigated"){
            NFERT <- NFERT_IR
          }else{
            NFERT <- NFERT_RF
          }

        }

        CRPRICE <-  get_value(COUNTRY_CODE,CROP,YEAR,"CRPRICE")
        CRPRICE_year <-  get_value(COUNTRY_CODE,CROP,YEAR,"CRPRICE",COLUMN="year")
        CRPRICE_units <-  get_value(COUNTRY_CODE,CROP,YEAR,"CRPRICE",COLUMN = "units")

        CRPRICE_USD_year <- get_usd_year(CRPRICE_units,CRPRICE_year)

        # Deflating crop prices to base year if needed
        if(!is.na(CRPRICE) & YEAR_USD!=CRPRICE_USD_year){
          CRPRICE <- deflate_consumer_price(COUNTRY_CODE,CRPRICE_USD_year,YEAR_USD,CRPRICE,VERBOSE=F)
        }
        # if(!is.na(new_CROP_PRICE_MULT)){ CROP_PRICE_MULT <- new_CROP_PRICE_MULT }
        CRPRICE <- CRPRICE * CROP_PRICE_MULT

        NDEP <- get_value(AUX_CODE,CROP,YEAR,"NDEP")
        if(is.na(NDEP)){ NDEP <- get_value(COUNTRY_CODE,CROP,YEAR,"NDEP") }

        NMAN_MAX <- get_value(COUNTRY_CODE,CROP,YEAR,"NMAN_MAX")
        NMAN <- get_value(AUX_CODE,CROP,YEAR,"NMAN")
        
        if(is.na(NMAN)){ NMAN <- get_value(COUNTRY_CODE,CROP,YEAR,"NMAN") }
        NMAN <- min(NMAN,NMAN_MAX)

        PERC_UREA <- get_value(COUNTRY_CODE,CROP,YEAR,"UREA_PERCENTAGE")

        FEV_MANURE <- get_value(COUNTRY_CODE,CROP,YEAR,"FEV_MANURE")
        FEV_UREA <- get_value(COUNTRY_CODE,CROP,YEAR,"FEV_UREA")

        FEV_MANURE <- min(FEV_MANURE,1)
        FEV_UREA <- min(FEV_UREA,1)

        FESNM <- get_value(COUNTRY_CODE,CROP,YEAR,"FESNM")

        BNF <- get_value(AUX_CODE,CROP,YEAR,"BNF")
        if(is.na(BNF) & grepl("_",AUX_CODE)){ print(paste0("BNF not found for ",AUX_CODE," using country (",COUNTRY_CODE,")"))
          BNF <- get_value(COUNTRY_CODE,CROP,YEAR,"BNF") }

        NFERT_EF <- NFERT*PERC_UREA*FEV_UREA+NFERT*(1-PERC_UREA)
        FEV_SYNTH <- PERC_UREA*FEV_UREA+(1-PERC_UREA)*1

        SN <- NDEP+BNF

        NMAN_EF <- NMAN*FEV_MANURE
        NAV <- SN + NFERT_EF + NMAN_EF

        NH3_MANURE_PERC <- get_value(COUNTRY_CODE,CROP,YEAR,"NH3_MANURE_PERC")

        UDC_NH3_COST <- get_value(COUNTRY_CODE,CROP,YEAR,"UDC_PM25_NH3_COST")
        YLL_NH3 <- get_value(COUNTRY_CODE,CROP,YEAR,"YLL_PM25_NH3")

        AUX <- calculate_yields_curve(LTN_a,LTN_b,LTN_c,NAV,YMAX,CURVE_TYPE_ACT,CROP
                               ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N)

        YPRED_mean <- AUX[1]
        YPRED_mean <- YPRED_mean * YMULT

        GDP_NL <- get_value("NL",CROP,YEAR,"GDP")
        COST_OTHER_INPUTS <- (GDP/GDP_NL)^GDP_ELAST*COSTS_OTHER_INPUTS_NL
        LABOUR_COSTS <- (GDP/GDP_NL)^GDP_ELAST*LABOUR_COSTS_NL

        if(FERT_PRICE_SOURCE=="fert_imp_price"){
          # Fertilizer price derived from the fert importer price

          # FGprice = IMPprice (1+ A x0.2)
          FERT_IMP_PRICE <- get_value(COUNTRY_CODE,CROP,YEAR,"FERT_IMPORT_PRICE")
          FERT_IMP_year <-  get_value(COUNTRY_CODE,CROP,YEAR,"FERT_IMPORT_PRICE",COLUMN = "year")
          FERT_IMP_units <-  get_value(COUNTRY_CODE,CROP,YEAR,"FERT_IMPORT_PRICE",COLUMN = "units")
          FERT_PRICE_year <- FERT_IMP_year
          FERT_IMP_USD_year <- get_usd_year(FERT_IMP_units,FERT_IMP_year)

          # Deflating fert prices to base year if needed
          if(!is.na(FERT_IMP_PRICE) & YEAR_USD!=FERT_IMP_USD_year){
            FERT_IMP_PRICE <- deflate_consumer_price(COUNTRY_CODE,FERT_IMP_USD_year,YEAR_USD,FERT_IMP_PRICE,VERBOSE=F)
          }
          FERT_PRICE <- FERT_IMP_PRICE * PARAM_FARM_GATE_PRICES
          FERT_PRICE <- FERT_PRICE * FERT_PRICE_MULT
        }else{
          if(FERT_PRICE_SOURCE=="fert_price"){
            # Fertilizer price directly from the source
            FERT_PRICE <- get_value(COUNTRY_CODE,CROP,YEAR,"FERTILIZER_PRICE")
            FERT_PRICE_units <-  get_value(COUNTRY_CODE,CROP,YEAR,"FERTILIZER_PRICE",COLUMN = "units")
            FERT_PRICE_year <-  get_value(COUNTRY_CODE,CROP,YEAR,"FERTILIZER_PRICE",COLUMN = "year")

            FERT_PRICE_USD_year <- get_usd_year(FERT_PRICE_units,FERT_PRICE_year)

            # Deflating fert prices to base year if needed
            if(!is.na(FERT_PRICE) & YEAR_USD!=FERT_PRICE_USD_year){
              FERT_PRICE <- deflate_consumer_price(COUNTRY_CODE,FERT_PRICE_USD_year,YEAR_USD,FERT_PRICE,VERBOSE=F)
            }

          }else{
            stop(paste0("Invalid fertilizer price method: ",FERT_PRICE_SOURCE))
          }
        }


        if(CURVE_TYPE_ACT=="quadratic"){

          # Synthetic fertilizer amount (before apply ef) to get maximum yield
          NRFMAXY_mean <- ((-LTN_a/(2*LTN_b))-SN-NMAN_EF)/FEV_SYNTH

          # Synthetic fertilizer amount (before apply ef) for the EONR
          NRFOPTY_mean <-
            ( ((FERT_PRICE-YMAX*YMULT*CRPRICE*LTN_a*FEV_SYNTH)/
                 (2*LTN_b*YMAX*YMULT*CRPRICE*FEV_SYNTH)       )-SN-NMAN_EF)/
            FEV_SYNTH

        }
        if(CURVE_TYPE_ACT=="george"){
          # Synthetic fertilizer amount (before apply ef) to get maximum yield
          NRFMAXY_mean <- (((log( (-LTN_c) / (LTN_b*log(0.99)) ))/(log(0.99)))-SN-NMAN_EF)/FEV_SYNTH

          # Synthetic fertilizer amount (before apply ef) for the EONR
          AUXgg <- (-LTN_c*YMAX*YMULT*CRPRICE+FERT_PRICE/FEV_SYNTH)/(LTN_b*log(0.99)*YMAX*YMULT*CRPRICE)
          NRFOPTY_mean <- ((log(AUXgg)/log(0.99))-SN-NMAN_EF)/FEV_SYNTH

        }

        if(ALLOW_NEGATIVE_EONR<1){
          EONR <- max(0,NRFOPTY_mean)
        }else{
          EONR <- NRFOPTY_mean
        }

        Nshare1_mean <- 1-(calculate_yields_curve(LTN_a,LTN_b,LTN_c,
                                                   (NAV-NFERT_EF),YMAX,CURVE_TYPE_ACT,CROP
                                                  ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N))*YMULT/YPRED_mean

        Nshare2_mean <- 1-(calculate_yields_curve(LTN_a,LTN_b,LTN_c,
                                                  (NAV-NFERT_EF-FESNM*min(NMAN,NMAN_MAX)),YMAX,CURVE_TYPE_ACT,CROP
                                                  ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N))*YMULT/YPRED_mean

        if(CURVE_TYPE_ACT=="quadratic"){

          MARGNBenef_mean <- LTN_a*YMAX*YMULT*CRPRICE+
            LTN_b*2*YMAX*YMULT*CRPRICE*NAV
        }
        if(CURVE_TYPE_ACT=="george"){
          MARGNBenef_mean <- YMAX*YMULT*CRPRICE*(LTN_b*(0.99^NAV)*log(0.99)+LTN_c)
        }

        AUX <- calculate_yields_curve(LTN_a,LTN_b,LTN_c,
                               (SN + NMAN_EF),YMAX,CURVE_TYPE_ACT,CROP
                               ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N)
        YPRED_SNandMAN_mean <- AUX[1]
        YPRED_SNandMAN_mean <- YPRED_SNandMAN_mean * YMULT


        AUX <- calculate_yields_curve(LTN_a,LTN_b,LTN_c,
                                      SN,YMAX,CURVE_TYPE_ACT,CROP
                                      ,MAXIMUM_YIELD_DECREASE_WITH_INCR_N,AVOID_YIELD_DECREASE_WITH_INCR_N)
        YPRED_SN_mean <- AUX[1]
        YPRED_SN_mean <- YPRED_SN_mean * YMULT

        ABSBenef_mean <- (YPRED_mean-YPRED_SN_mean)*AREA*CRPRICE/(10^6)

        NCONT_GRAIN_D_CONSTANT <- get_config_parameter("NCONT_GRAIN_D_CONSTANT")
        NCONT_GRAIN_E_CONSTANT <- get_config_parameter("NCONT_GRAIN_E_CONSTANT")
        NCONT_GRAIN_F_CONSTANT <- get_config_parameter("NCONT_GRAIN_F_CONSTANT")

        NCONT <- NA

        auxt <- get_config_parameter(paste0("NCONT_",str_to_upper(CROP)),txt = T)
        if(!is.na(auxt)){
          eval(parse(text=paste0("NCONT <- ",auxt)))
        }

        NCONT_STRAW <- get_config_parameter(paste0("NCONT_STRAW_",str_to_upper(CROP)))
        STRAW_GRAIN_RATIO <- get_config_parameter(paste0("STRAW_GRAIN_RATIO_",str_to_upper(CROP)))
        
        STRAW_FACTOR <- STRAW_GRAIN_RATIO * (1-STUBBLE_RATIO) * AVAILABLE_STRAW_RECOVERY_RATIO
        YSTRAW <- YPRED_mean  * STRAW_FACTOR # tDM/ha
       
        NREMOVAL_GRAIN <- NCONT * YPRED_mean * 10
        NREMOVAL_STRAW <- NCONT_STRAW * YPRED_mean * STRAW_FACTOR * 10
        NREMOVAL <- NREMOVAL_GRAIN + NREMOVAL_STRAW

        NSURPLUS <- SN + NFERT + min(NMAN,NMAN_MAX) - NREMOVAL
        NUE <- NREMOVAL/(SN+NFERT + min(NMAN,NMAN_MAX))

        NREMOVAL_SNandMAN <- NCONT * YPRED_SNandMAN_mean * 10 + YPRED_SNandMAN_mean * STRAW_FACTOR * NCONT_STRAW * 10
        NUE_synth <- (NREMOVAL-NREMOVAL_SNandMAN)/NFERT


        MEAN_N_SYNBenef_chord_mean <- (max((YPRED_mean-YPRED_SNandMAN_mean),0)*CRPRICE)/(NFERT*FERT_PRICE)
        ABSMEANBenef_mean <- MEAN_N_SYNBenef_chord_mean*AREA*NFERT/(10^6)

        MRIVERMOUTH_F_CONSTANT <- get_config_parameter("MRIVERMOUTH_F_CONSTANT")
        eval(parse(text=paste0("MUC_nrivermouth_marine <- ",get_config_parameter("MARGIN_UC_RIVERMOUTH",txt=T))))

        RET_SRO <- get_value(AUX_CODE,NA,YEAR,"RETENTION_SRO_AGRI")
        RET_GRW <- get_value(AUX_CODE,NA,YEAR,"RETENTION_AGRI_SURP")

        # If not available try the country code (in case this is a subnational region)
        if(is.na(RET_SRO)){
          RET_SRO <- get_value(COUNTRY_CODE,NA,YEAR,"RETENTION_SRO_AGRI")
        }
        if(is.na(RET_GRW)){
          RET_GRW <- get_value(COUNTRY_CODE,NA,YEAR,"RETENTION_AGRI_SURP")
        }

        NLOSS <- (1-RET_SRO)*(NFERT+min(NMAN,NMAN_MAX))+(1-RET_GRW)*
          max( (NSURPLUS-(NFERT-NFERT_EF)-(min(NMAN,NMAN_MAX)*NH3_MANURE_PERC)), 0)
        
        if(sum(c(AREA_CROP_EX,AREA_CROP_IN),na.rm=T)==0){
          ARABLE_COVERAGE <- 0
          if(is.na(AREA_CROP_EX) | is.na(AREA_CROP_IN)){
            ARABLE_COVERAGE <- NA
          }
        }else{
          ARABLE_COVERAGE <- AREA / (AREA_CROP_EX+AREA_CROP_IN)
        }

        dres$country[cnt] <- get_country_by_code(COUNTRY_CODE)
        dres$country_code[cnt] <- AUX_CODE
        dres$region_code[cnt] <- WORLD_REGION_CODE

        dres$cc_iso3[cnt] <- CC_ISO3
        dres$cc_uni[cnt] <- CC_UNI
        dres$cc_undp[cnt] <- CC_UNDP
        dres$cc_faostat[cnt] <- CC_FAOSTAT
        dres$cc_gaul[cnt] <- CC_GAUL

        dres$crop[cnt] <- CROP
        dres$system[cnt] <- SYSTEM
        dres$GDP[cnt] <- GDP
        dres$population[cnt] <- POP
        dres$agric_area[cnt] <- AGRIC_AREA
        dres$area_crop_in[cnt] <- AREA_CROP_IN
        dres$area_crop_ex[cnt] <- AREA_CROP_EX
        dres$area_grass_in[cnt] <- AREA_GRASS_IN
        dres$area_grass_ex[cnt] <- AREA_GRASS_EX
        dres$arable_coverage[cnt] <- ARABLE_COVERAGE
        dres$ymax[cnt] <- YMAX
        dres$yact[cnt] <- YACT
        dres$area[cnt] <- AREA

        dres$crpprc[cnt] <- CRPRICE
        dres$crpprcyear[cnt] <- CRPRICE_year
        dres$ndep[cnt] <- NDEP

        dres$bnf[cnt] <- BNF
        dres$SN[cnt] <- SN
        dres$nfert[cnt] <- NFERT
        dres$cropintens[cnt] <- CROPPING_INTENSITY
        dres$nfert_ef[cnt] <- NFERT_EF
        dres$nfert_year[cnt] <- NFERT_year
        dres$nfert_crop[cnt] <- NFERT_crop

        dres$nman[cnt] <- NMAN
        dres$nman_max[cnt] <- NMAN_MAX
        dres$nman_ef[cnt] <- NMAN_EF
        dres$nh3_manure_perc[cnt] <- NH3_MANURE_PERC

        dres$perc_urea[cnt] <- PERC_UREA
        dres$FEV_MANURE[cnt] <- FEV_MANURE
        dres$FEV_UREA[cnt] <- FEV_UREA

        dres$FESMan[cnt] <- FESNM
        dres$Nav[cnt] <- NAV
        dres$ypred[cnt] <- YPRED_mean
        dres$ypredsnman[cnt] <- YPRED_SNandMAN_mean # new

        dres$ncont[cnt] <- NCONT
        dres$ncont_straw[cnt] <- NCONT_STRAW

        dres$nremoval[cnt] <- NREMOVAL
        dres$nremoval_grain[cnt] <- NREMOVAL_GRAIN
        dres$nremoval_straw[cnt] <- NREMOVAL_STRAW


        dres$nsurplus[cnt] <- NSURPLUS
        dres$NUE[cnt] <- NUE
        dres$NUE_synth[cnt] <- NUE_synth

        dres$fert_price[cnt] <- FERT_PRICE
        dres$fertpriceyear[cnt] <- FERT_PRICE_year
        dres$other_inputs_price[cnt] <- COST_OTHER_INPUTS

        dres$labour_cost[cnt] <- LABOUR_COSTS
        dres$nrfmax[cnt] <- NRFMAXY_mean
        dres$eonr[cnt] <- EONR

        dres$sonr[cnt] <- NA

        dres$nshare1[cnt] <- Nshare1_mean
        dres$nshare2[cnt] <- Nshare2_mean

        dres$margnbenef[cnt] <- MARGNBenef_mean

        dres$absbenef[cnt] <- ABSBenef_mean

        dres$meannbenefsyn[cnt] <- MEAN_N_SYNBenef_chord_mean
        dres$absmeannbenefsyn[cnt] <- ABSMEANBenef_mean

        # Due only to marine (rest are affected only by NH3 emmissions)
        dres$margcostsurp[cnt] <- (1-RET_GRW)*MUC_nrivermouth_marine

        dres$nlosses[cnt] <- NLOSS

        dres$nh3em_synth[cnt] <- (NFERT-NFERT_EF)
        dres$nh3em_total[cnt] <- (NFERT-NFERT_EF+NMAN_EF*NH3_MANURE_PERC)
        dres$udc_pm25_nh3_cost[cnt] <- UDC_NH3_COST
        dres$abs_yll_pm25_nh3[cnt] <- YLL_NH3*(NFERT-NFERT_EF+NMAN_EF*NH3_MANURE_PERC)*AREA
        dres$abs_pm25_nh3_cost[cnt] <- UDC_NH3_COST*(NFERT-NFERT_EF+NMAN_EF*NH3_MANURE_PERC)*AREA/(10^6)

        dres$abs_marine_cost[cnt] <- MUC_nrivermouth_marine*NLOSS*AREA/(10^6)

        dres$UC_marine_rivmouth[cnt] <- MUC_nrivermouth_marine
        dres$RET_SRO[cnt] <- RET_SRO
        dres$RET_GRW[cnt] <- RET_GRW

        dres$abs_msa_cost[cnt] <- NA


        AUX_NDEPNR_RATIO <- get_value(AUX_CODE,CROP,YEAR,"CEREAL_NH3_LOSS_DEPOS_TERR_NAT_PROP")
        if(is.na(AUX_NDEPNR_RATIO)){ AUX_NDEPNR_RATIO <- get_value(COUNTRY_CODE,CROP,YEAR,"CEREAL_NH3_LOSS_DEPOS_TERR_NAT_PROP") }
        dres$ndep_nr_ratio[cnt] <- AUX_NDEPNR_RATIO


        dres$absextcostnloss[cnt] <- NA

        NRATE <- NFERT_EF + NMAN_EF
       
        dres$absyield[cnt] <-  dres$ypred[cnt]*AREA
        dres$ypred_straw[cnt] <- YSTRAW
        dres$absyield_straw[cnt] <-  dres$ypred_straw[cnt]*AREA

        dres$absfertcost[cnt] <- dres$fert_price[cnt]*dres$nfert[cnt]*dres$area[cnt]*dres$cropintens[cnt]/(10^6)

        cnt <- cnt + 1

        
        if((cdot%%100)==0){
          cat("\n")
          cdot <- 0
        }
        cdot <- cdot + 1
        cat(".")


      } # Crops

    } # Country code


  }# System

  aux <- which(is.na(dres[,1]))
  aux <- min(aux)-1
  dres <- dres[1:aux,]

  dst <- subset(dres,!grepl("region_",country) & grepl("_",country_code))
  if(nrow(dst)>0){
    
    get_belonging_country <- function(state_code){
      aux <- str_split(state_code,"_")
      return(aux[[1]][1])
    }
    
    # countries with detailed information
    coun_details <- unique(sapply(dst$country_code,get_belonging_country))
    
    # agrego a partir de la columna ymax
    for(cc in coun_details){
      
      print(paste0("Upscaling substate info from ",cc,"..."))
      for(cr in unique(dst$crop)){
        
        if(cr=="barley"){ next } # subnational data from barley not yet available
        for(sy in unique(dst$system)){
          
          dx <- subset(dst,grepl(cc,country_code) & crop==cr & system==sy)
          COL_INI <- which(colnames(dx)=="ymax")
          dx[,1:(COL_INI-1)] <- NA
          
          dxs <- apply(dx,c(2),sum,na.rm=T)
          auxt <- paste0("dxw <- dx*dx$",COLBASE_WEIGHT)
          eval(parse(text=auxt))
          dxw <- apply(dxw,c(2),sum,na.rm=T)
          auxt <- paste0("dxw <- dxw/dxs[colnames(dx)=='",COLBASE_WEIGHT,"']")
          eval(parse(text=auxt))
          
          # I already have the sums and the weighted ones, now I select the appropriate one
          
          dxt <- dxs*NA
          for(nc in COL_INI:ncol(dx)){
            
            if(is.element(colnames(dx)[nc],COLS_REGIONS_ADD)){
              dxt[nc] <- dxs[nc]
            }else{
              if(is.element(colnames(dx)[nc],COLS_REGIONS_WEIGHT)){
                dxt[nc] <- dxw[nc]
              }else{
                stop("all columns must be specified to be weighted or added")
              }
            }
          }
          
          xx <- which(dres$country_code==cc & dres$crop==cr & dres$system==sy)
          if(length(xx)==1){
            dres[xx,COL_INI:ncol(dres)] <- dxt[COL_INI:ncol(dres)]
          }
  
        }
      }
    }
  
  
  } # summarize state/province level information to country

  
  d <- subset(dres,!grepl("region_",country))
  d <- subset(dres,!grepl("_",country_code))

  d$aux <- d$GDP*d$population
  GLOBAL_GDP <- sum(d$aux,na.rm=T)/sum(d$pop,na.rm=T)

  d$aux <- d$nh3em_total*d$area

  p <- function(v){
    return(v[1])
  }
  dg <- d %>% group_by(country,country_code) %>%
    summarise(nh3em_abs=sum(aux,na.rm=T),
              gdp=p(GDP),
              ndep_nr_ratio=p(ndep_nr_ratio),
              # region_code=p(region_code),
              pop=p(population),
              ndep=p(ndep))

  dg <- subset(dg,!is.na(nh3em_abs) & !is.nan(nh3em_abs) & nh3em_abs>0 &
                 !is.na(gdp) & !is.nan(gdp) &
                 !is.na(pop) & !is.nan(pop))


  # Steps
  # take the ndep which is the deposit on total natural areas
  # take the volatilization of ammonia and then apply the value % that falls again in natural areas
  # subtract it to obtain the deposit from sources other than cereals
  # With each national value, do the algorithm to distribute it proportionally
  
  dg$msa_loss_perc <- NA
  dg$abs_msa_cost <- NA
  dg$tot_ndep_debug <- NA
  dg$cer_ndep_debug <- NA
  dg$abs_msa_cost_alldep <- NA
  dg$WTP_MSA <- NA

  WTP_MSA_PARAMETER <- get_config_parameter("WTP_MSA_PARAMETER")
# print(WTP_MSA_PARAMETER)


  for(i in 1:nrow(dg)){

    cact <- dg$country_code[i]

    NDEP_OTH <- get_value(cact,NA,YEAR,"NDEP_NATURE_OTHERTHAN_FERT_CEREALS")
    AREA_NATURAL <- get_value(cact,NA,YEAR,"AREA_NATURE_TOTAL")

    if(is.na(NDEP_OTH)){ next }
    if(NDEP_OTH<0){
      # print(paste0("Country: ",cact," skipped"))
      next
    }

    nh3em = dg$nh3em_abs[i]/AREA_NATURAL
    NDEP_CER = nh3em*dg$ndep_nr_ratio[i]


    msa_total_perc <- msa_loss_f(NDEP_CER+NDEP_OTH)*100
    dg$tot_ndep_debug[i] <- (NDEP_CER+NDEP_OTH)
    dg$cer_ndep_debug[i] <- NDEP_CER
    # msa_total_perc --- NDEP_CER+NDEP_OTH
    # x ---------------- NDEP_CER
    
    dg$msa_loss_perc[i] <- NDEP_CER*msa_total_perc/(NDEP_CER+NDEP_OTH)

    WTP_MSA <- WTP_MSA_PARAMETER*((dg$gdp[i]/GLOBAL_GDP)^0.45)
    cost_msa_pp <- WTP_MSA*dg$msa_loss_perc[i]
    dg$WTP_MSA[i] <- WTP_MSA
    dg$abs_msa_cost[i] <- cost_msa_pp*dg$pop[i]/(10^6)
    dg$abs_msa_cost_alldep[i] <- WTP_MSA*msa_total_perc*dg$pop[i]/(10^6)
  }

  x <- which(is.na(dg$abs_msa_cost))
  if(length(x)>0){
    dg <- dg[-x,]
  }
  # Now I translate it backwards to crop/system level

  for(i in 1:nrow(d)){

    x <- which(dg$country_code==d$country_code[i])
    if(length(x)==0){ next }

    nh3em_act <- d$nh3em_total[i]*d$area[i]
    if(is.na(nh3em_act)){ next }
    nh3em <- dg$nh3em_abs[x]
    msalossperc <- dg$msa_loss_perc[x]
    aux <- dg$abs_msa_cost[x]
    d$abs_msa_cost[i] <- nh3em_act*aux/nh3em

    d$WTP_MSA[i] <- dg$WTP_MSA[x]
    dres$abs_msa_cost[which(dres$country_code==d$country_code[i] & dres$crop==d$crop[i] & dres$system==d$system[i])] <- d$abs_msa_cost[i]
    dres$WTP_MSA[which(dres$country_code==d$country_code[i] & dres$crop==d$crop[i] & dres$system==d$system[i])] <- d$WTP_MSA[i]

  }

  
  dres$absextcostnloss <- dres$abs_msa_cost+dres$abs_marine_cost+dres$abs_pm25_nh3_cost
  dres$BC <- (dres$absbenef-dres$fert_price*dres$nfert*dres$area*dres$cropintens/(10^6))/
    (
       dres$abs_msa_cost+
       dres$abs_marine_cost+
       dres$abs_pm25_nh3_cost+
       dres$fert_price*dres$nfert*dres$area*dres$cropintens/(10^6))

  rm("dg")

  if(CALCULATE_SONR){

    print("Calculating SONR...")
    for(cnt in 1:nrow(dres)){

      # Gathering all the needed value
      FEV_SYNTH <- dres$perc_urea[cnt]*dres$FEV_UREA[cnt] + (1-dres$perc_urea[cnt])*1
      SN <- dres$SN[cnt]
      NMAN_EF <- dres$nman_ef[cnt]
      NMAN <- dres$nman[cnt]
      FEV_MANURE <- dres$FEV_MANURE[cnt]
      
      CROP <- dres$crop[cnt]
      if(CROP=="rice"){
        CURVE_TYPE_ACT <-CURVE_TYPE_RICE
      }else{
        CURVE_TYPE_ACT <-CURVE_TYPE
      }
      LTN_res <- get_long_term_N_response_curve(CROP,CURVE_TYPE_ACT)
      LTN_a <- LTN_res$value[which(LTN_res$param=="a")]
      LTN_b <- LTN_res$value[which(LTN_res$param=="b")]
      LTN_c <- LTN_res$value[which(LTN_res$param=="c")]

      YMAX <- dres$ymax[cnt]
      SYSTEM <- dres$system[cnt]
      COUNTRY_CODE <- dres$country_code[cnt]
      AUX_CODE <- COUNTRY_CODE
      YMULT <- 1
      if(USE_YIELD_MULTIPLIER>0){
        YMULT <- get_value(AUX_CODE,str_replace_all(CROP,"barley","wheat"),YEAR,"YIELD_MULTIPLIER",SYSTEM)
        if(is.na(YMULT)){ YMULT <- 1 }
      }

      CRPRICE <- dres$crpprc[cnt]
      FERT_PRICE <- dres$fert_price[cnt]
      NCONT_STRAW_ACT <- dres$ncont_straw[cnt]

      STRAW_GRAIN_RATIO <- get_config_parameter(paste0("STRAW_GRAIN_RATIO_",str_to_upper(CROP)))
      STRAW_FACTOR <- STRAW_GRAIN_RATIO * (1-STUBBLE_RATIO) * AVAILABLE_STRAW_RECOVERY_RATIO

      # Marine
      MUC_nrivermouth_marine <- dres$UC_marine_rivmouth[cnt]
      RET_SRO <- dres$RET_SRO[cnt]
      RET_GRW <- dres$RET_GRW[cnt]

      NMAN <- dres$nman[cnt]
      NMAN_MAX<- dres$nman_max[cnt]

      # PM25
      UDC_NH3_COST <- dres$udc_pm25_nh3_cost[cnt]
      NH3_MANURE_PERC <-  dres$nh3_manure_perc[cnt]
      AREA <- dres$area[cnt]

      # For MSA
      ABS_MSA_COST_BASE <- dres$abs_msa_cost[cnt]
      EMISS_BASE <- (dres$nfert[cnt]-dres$nfert_ef[cnt]+NMAN_EF*NH3_MANURE_PERC)*AREA

      if(!is.na(CRPRICE) & !is.na(FERT_PRICE) & !is.na(YMAX) & !is.na(SN) & !is.na(NMAN_EF) &
         !is.na(MUC_nrivermouth_marine) & !is.na(RET_SRO) & !is.na(RET_GRW) & !is.na(UDC_NH3_COST) &
         !is.na(NH3_MANURE_PERC) & !is.na(AREA) & !is.na(ABS_MSA_COST_BASE)
         ){
            dres$sonr[cnt] <- calc_SONR_new(
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
              FEV_SYNTH=FEV_SYNTH,
              FEV_MANURE=FEV_MANURE,
              SN=SN,
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
              ALLOW_NEGATIVE_SONR=ALLOW_NEGATIVE_SONR
            )
      }
     
    }
    print("SONR calculated!")
  }

  drclass <- get_config_parameter("REGION_CLASSIFICATION",txt = T)
  if(grepl("#",drclass)){
    
    drclass <- str_split(drclass,"#")[[1]]
    aux <- dres %>% separate(region_code,drclass,"#")
    aux <- subset(aux,T,drclass)
  }else{
    aux <- subset(dres,T,region_code)
    colnames(aux) <- drclass
  }

  for(auxc in 1:length(colnames(aux))){
    wrg <- colnames(aux)[auxc]
    for(rg in sort(unique(aux[,auxc]))){

      if(is.na(rg)){ next }

      # Take the countries from that region but not the sub-state results
      # x <- dres[which(dres$region_code==rg & !grepl("_",dres$country_code)),]
      x <- dres[which(aux[,auxc]==rg & !grepl("_",dres$country_code)),]

      for(st in unique(x$system)){
        for(cr in unique(x$crop)){

          xa <- x[which(x$system==st & x$crop==cr),]

          # From each country I take the gdp, pop etc
          xa_all <- x %>%  group_by(country_code) %>%
            summarise_all("max")

          auxt <- paste0("xa1 <- xa[which(!is.na(xa$",COLBASE_WEIGHT,") & !is.na(xa$",COLBASE_WEIGHT_REQUIRED,")),]")
          eval(parse(text=auxt))
          if(length(xa1[,1])==0){ next }


          xf <- xa1[1,]
          xf[1,] <- NA
          xf$country <- str_to_lower(paste0("region_",wrg,"_",rg))

          xf$country_code <- paste0(str_to_upper(wrg),"_",rg)
          xf$region_code <- rg
          xf$crop <- cr
          xf$system <- st


          for(i in 1:length(COLS_REGIONS_ADD_ALL)){
            auxt <- paste0("xf$",COLS_REGIONS_ADD_ALL[i]," <- sum(xa_all$",COLS_REGIONS_ADD_ALL[i],",na.rm=T)")
            eval(parse(text=auxt))
          }

          for(i in 1:length(COLS_REGIONS_ADD)){
            auxt <- paste0("xf$",COLS_REGIONS_ADD[i]," <- sum(xa1$",COLS_REGIONS_ADD[i],",na.rm=T)")
            eval(parse(text=auxt))
          }
          for(i in 1:length(COLS_REGIONS_WEIGHT)){
            auxt <- paste0("dact <- xa1$",COLS_REGIONS_WEIGHT[i])
            eval(parse(text=auxt))

            auxt <- paste0("varact <- xa1$",COLBASE_WEIGHT)
            eval(parse(text=auxt))

            if(length(which(is.na(dact)))==length(dact)){ vact <- NA
            }else{
              varact[is.na(dact)] <- NA
              wact <- varact*dact/sum(varact,na.rm=T)
              vact <- sum(wact,na.rm=T)
            }
            auxt <- paste0("xf$",COLS_REGIONS_WEIGHT[i]," <- vact")
            eval(parse(text=auxt))
          }

          auxt <- paste0("xa2 <- xa[which(!is.na(xa$",COLS_REGIONS_WEIGHT_2,")),]")
          eval(parse(text=auxt))

          for(i in 1:length(COLS_REGIONS_WEIGHT_2)){
            auxt <- paste0("dact <- xa2$",COLS_REGIONS_WEIGHT_2[i])
            eval(parse(text=auxt))

            auxt <- paste0("varact <- xa2$",COLBASE_WEIGHT_2)
            eval(parse(text=auxt))

            if(length(which(is.na(dact)))==length(dact)){ vact <- NA
            }else{
              varact[is.na(dact)] <- NA
              wact <- varact*dact/sum(varact,na.rm=T)
              vact <- sum(wact,na.rm=T)
            }
            auxt <- paste0("xf$",COLS_REGIONS_WEIGHT_2[i]," <- vact")
            eval(parse(text=auxt))
          }

          dres <- rbind(dres,xf)
        }
      }
    }
  }# every world region


  # Rounding decimals
  x <- which(is.element(str_to_lower(colnames(dres)),str_to_lower(get_output_units()$output)))
  dres[,x] <- round(dres[,x],RESULTS_DECIMAL_PLACES)

  openxlsx::write.xlsx(add_units_to_results(dres),paste0(RESULTS_FOLDER,"all_results.xlsx"),overwrite = T)
  saveRDS(dres,paste0(RESULTS_FOLDER,"all_results.rds"))

  cat("\n")


  save_results_in_files(dres,paste0(RESULTS_FOLDER,"results/"),
                        COLBASE_WEIGHT,
                        COLBASE_WEIGHT_REQUIRED,
                        COLS_ADD_AGGREGATE_RESULTS)
  file.copy(CONFIG_FILE,RESULTS_FOLDER)

  print(paste0("Process completed, results available [",RESULTS_FOLDER,"]"))
  
  
  # Remove just all the global variables used for fastening the execution
  # for not interfering in new executions but not more for not interfering with
  # custom functions
  
  gvar <- names(as.list(.GlobalEnv))
  gvar <- gvar[grepl("dSHP",gvar) | grepl("dREAD",gvar) | grepl("UAPRCH",gvar) | grepl("UAABS",gvar)]
  rm(list=c("dcnf",gvar), envir = .GlobalEnv)
  

}




