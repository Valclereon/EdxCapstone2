
## 00_1 Download packages and load libraries ----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo",repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom",repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate",repos = "http://cran.us.r-project.org")



## 00_2 Download data from github ----
githubURL <- "https://github.com/jhorbino93/EdxCapstone2/raw/master/Edx_Capstone2_Initialisation_WS.RData"

file_name <- "Edx_Capstone2_Initialisation_WS.RData"

working_directory <- getwd()
download.file(githubURL,file_name)
load(file.path(working_directory, file_name))

## 00_3 Prepare parameters ----

rolling_period_long <- 7*52
# date_pre_covid <- as.Date("2020-01-31")
date_avg_start <- as.Date("2017-01-01")
date_ts_start  <- as.Date("2017-01-01")
# date_ts_end <- as.Date("2020-02-29")
date_ts_end <- as.Date("2020-05-31")
date_edm_start <- as.Date("2018-11-01")

date_pre_covid_start <- as.Date("2001-01-01")
date_pre_covid_end <- as.Date("2020-03-12")
date_transition_covid_start <- as.Date("2020-03-13")
date_transition_covid_end <- as.Date("2020-03-25")
date_recovery_covid_start <- as.Date("2020-03-26")
date_recovery_covid_end <- as.Date("9999-12-31")

date_centre_start <- date_avg_start + floor(rolling_period_long/2)
date_centre_end <- date_ts_end - floor(rolling_period_long/2)
date_all_components_start <- "2018-09-26"
# date_all_components_end <- date_centre_end

EDM_lag_days <- seq(1,3,1)
min_pvalue <- 0.1
min_abs_cor <- 0.3    

sample_ReportLevel4Key <- 247
sample_StayLengthKey <- 10
sample_DimAmountTypeKey <- 103

## 01_1 Prepare initial data frames ----

  ## Transform to tibble and rename some columns
  edx_eda <- 
    as_tibble(edx_eda) %>%
    rename(Amount = raw_amount) %>%
    left_join(
      select(DimStayLength,StayLengthKey,StayLength_l1_Rank)
      ,by = "StayLengthKey"
    ) %>%
    mutate(
      DatePeriod = ifelse(
                    Date <= as.Date("2020-03-12"),"Pre-COVID"
                      ,ifelse(Date >= as.Date("2020-03-13") & Date <= as.Date("2020-03-26"),"Transition-COVID"
                        ,"Recovery-COVID"
                      )
                    )
    ) %>% group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DatePeriod) %>% arrange(Date) %>%
    mutate(
      RollingPeriod = ifelse(as.integer(Date - Date[1]) + 1 > rolling_period_long, rolling_period_long, as.integer(Date - Date[1]) + 1)
    ) %>% ungroup()

  tmpPriceFill <- 
    edx_eda %>%
    dplyr::filter(
      DimAmountTypeKey == 100
    ) %>%
    mutate(
      Amount = ifelse(abs(Amount) %in% c(0,Inf),NA,Amount)
    ) %>% group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>% arrange(Date) %>%
    fill(Amount) %>% ungroup() %>%
    select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,Amount) %>%
    rename(NewAmount = Amount)
  
  edx_eda <-
    edx_eda %>%
    left_join(tmpPriceFill,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")) %>%
    mutate(
      Amount = coalesce(ifelse(DimAmountTypeKey == 100,NewAmount,Amount),0)
    ) %>% select(-NewAmount)
  
  rm(tmpPriceFill)

  ## This data frame will be used to calculate a long term trend
  dfTrend <- 
    edx_eda %>%
    dplyr::filter(
      Date >= date_avg_start
      & Date <= date_ts_end    
    ) %>%
    select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,Amount,DatePeriod,RollingPeriod)

## 01_2 Prepare functions ----

roll_mean_cust <- function(x,period){
  rollapply(x,period,function(y) mean(y)
            ,fill = NA, by.column = F, align = "center", partial = T)
}
  
RMSE <- function(target, prediction){
  sqrt(mean((target - prediction)^2))
}  
  

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-16394",sep="\t",row.names=row.names,col.names=col.names,...)
}

funApplyDecompositionHierarchy_l2 <- function(x){
  factor(
    if (x %in% c("Amount")){
      "Actual Results"
    } else if  (x %in% c("Predict_TrendLong")){
      "Long Term Trend"
    } else if (x %in% c("Predict_SeasonalMonth","Predict_SeasonalDOW","Predict_SeasonalSchoolHoliday","Predict_SeasonalInfluentialDate")) {
      "Seasonality"
    } else if (x %in% c("Predict_EDMMajorAvg","Predict_EDMMajorMonth","Predict_EDMMajorDOW","Predict_EDMMajorMonthRgr","Predict_EDMMajorDOWRgr")) {
      "EDM Initial Impact"
    } else if (x %in% c("Predict_EDMMajorLaggedAvg","Predict_EDMMajorLaggedDays","Predict_EDMMajorLaggedDaysDOW")){
      "EDM Lagged Impact"
    } else if (x %in% c("Residual")){
      "Residual"
    } else {
      "Other"
    }
    ,levels = c("Actual Results","Long Term Trend","Seasonality","EDM Initial Impact","EDM Lagged Impact","Residual","Other")
    ,ordered = T
  )
}

## 10 Calculate long term trends ----

  ## data.table is used here for speed as keys can be used for indexing
  ## dplyr can still be used but is generally slower, especially for larger data frames

  setDT(dfTrend)
  setkey(dfTrend,Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DatePeriod)
  dfTrend[,Predict_TrendLong:= as.numeric(roll_mean_cust(Amount,RollingPeriod)), by = c("StayLengthKey","ReportLevel4Key","DimAmountTypeKey","DatePeriod")]
  
  ## Convert back to tibble
  dfTrend <- as_tibble(dfTrend)
  

## 11 Prepare primary data frame ----
## This will be partitioned for training and validation
  
  dfMain <-
    edx_eda %>%
    dplyr::filter(
      Date >= date_avg_start
      & Date <= date_ts_end    
      # & FlagWithinCentre == 1
    ) %>%
    left_join(
      select(dfTrend,Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,Predict_TrendLong)
      ,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")
    ) %>%
    mutate(
      Residual_TrendLong = Amount - Predict_TrendLong
    )

  ## Generate daily relative price change with respect to long term price change
  ## This value will be used later for regression training
  tmpATV <-
    dfMain %>%
    dplyr::filter(
      DimAmountTypeKey == 100
    ) %>%
    group_by(ReportLevel4Key,StayLengthKey) %>% arrange(Date) %>%
    mutate(
      AmountPrior = lag(Amount)
      ,AmountChange = Amount - AmountPrior
      ,AmountChangePercPrior = AmountChange/AmountPrior
      ,AmountChangePercTrend = AmountChange/Predict_TrendLong
    ) %>% ungroup() %>%
    select(Date,ReportLevel4Key,StayLengthKey,AmountChangePercPrior,AmountChangePercTrend) %>%
    rename(
      ATVChangePercPrior = AmountChangePercPrior
      ,ATVChangePercTrend = AmountChangePercTrend
    )
  
  dfMain <-
    dfMain %>%
    left_join(
      tmpATV
      ,by=c("Date","ReportLevel4Key","StayLengthKey")
    )
  
  rm(tmpATV)
  
  plot_LongTermTrend <-
    dfMain %>%
    dplyr::filter(
      ReportLevel4Key == sample_ReportLevel4Key
      & DimAmountTypeKey == sample_DimAmountTypeKey
      & StayLengthKey == sample_StayLengthKey
    ) %>%
    ggplot() +
    geom_point(aes(Date,Amount),fill = "blue", alpha = 0.2) +
    geom_line(aes(Date,Predict_TrendLong, col = "Centered Moving Average"), size = 1.2) +
    geom_smooth(aes(Date,Amount, col = "Linear regression"),method = "lm", se = F) +
    labs(
      title = "Actual Sales Volume & Long Term Trend"
      ,subtitle = "Using a centered moving average as a long term trend approximation"
    ) +
    theme_classic()
  
  plot_StationaryTimeSeries <-
    dfMain %>%
    dplyr::filter(
      ReportLevel4Key == sample_ReportLevel4Key
      & DimAmountTypeKey == sample_DimAmountTypeKey
      & StayLengthKey == sample_StayLengthKey
    ) %>%
    ggplot(aes(Date,Residual_TrendLong)) +
    geom_point(fill = "blue", alpha = 0.2) +
    geom_smooth(method = "lm", se = F) +
    labs(
      title = "De-Trended Time Series - Stationary"
      ,subtitle = "The time series is detrended by removing the long term trend (approx. Centered Moving Average)"
    ) +
    theme_classic()

## 12 Partition primary data frame ----
## Test proportion 0.2 used to simulate real world usage; in operational envrionment, analysis generally uses as much historical data as possible.
## Higher test proportion can be used but can lead to over training
  
  set.seed(112358, sample.kind = "Rounding")
  
  testIndex <- createDataPartition(y = dfMain$Amount, times = 1, p = 0.2, list = FALSE)
  
  dfTest <- dfMain[testIndex,]
  dfTrain <- dfMain[-testIndex,]
  
# 13 Initial time Series EDA ----
  
  plot_initial_timeseries_long <- 
    dfTrain %>%
    dplyr::filter(
      ReportLevel4Key == sample_ReportLevel4Key
      & DimAmountTypeKey == sample_DimAmountTypeKey
      & StayLengthKey == sample_StayLengthKey
      & Date >= "2019-02-01" & Date <= "2019-08-31"
    ) %>%
    ggplot(aes(Date,Amount)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    geom_smooth(method = "loess", se = F, span = 0.1,linetype = "dashed") +
    labs(
      title = "Actual Sales Volume"
      ,subtitle = paste0("For carpark ID 24 and customers that for 3 - 4 days.")
    ) +
    theme_classic()
  
  plot_initial_timeseries_short <- 
    dfTrain %>%
    dplyr::filter(
      ReportLevel4Key == sample_ReportLevel4Key
      & DimAmountTypeKey == sample_DimAmountTypeKey
      & StayLengthKey == sample_StayLengthKey
      & Date >= "2019-02-01" & Date <= "2019-02-28"
    ) %>%
    ggplot(aes(Date,Amount)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    geom_smooth(method = "loess", se = F, span = 0.1,linetype = "dashed") +
    labs(
      title = "Actual Sales Volume"
      ,subtitle = paste0("For carpark ID 24 and customers that for 3 - 4 days.")
    ) +
    theme_classic()
  
  plot_initial_timeseries_monthly <- 
    dfTrain %>%
    dplyr::filter(
      ReportLevel4Key == sample_ReportLevel4Key
      & DimAmountTypeKey == sample_DimAmountTypeKey
      & StayLengthKey == sample_StayLengthKey
      & Date >= "2018-01-01" & Date <= "2019-12-31"
    ) %>%
    group_by(year(Date),MonthNo) %>%
    mutate(
      Amount = mean(Amount)
    ) %>% ungroup() %>%
    mutate(
      MonthNo = factor(MonthNo,levels=1:12,ordered = T)
    ) %>%
    ggplot(aes(Date,Amount)) +
    geom_line() +
    geom_smooth(method = "lm",se = F) +
    labs(
      title = "Average Daily Sales Volume - Stratified By Month"
      ,subtitle = paste0("Linear regression line shows seasonal over and underperformance of groups of months")
    ) +
    theme_classic()
  
## 20 Calculate Seasonality components ----
  
  ## __20_1 Calculate Month Impact ----

  vct_group_PredSeasonalMonth <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MonthNo")
  vct_select_PredSeasonalMonth <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MonthNo","TrendLongRatio")
  
      ##____20_1_1 Generate pre-calc plot
        plot_PreMonth <-
          dfTrain %>%
          mutate(
            MonthNo = factor(MonthNo, levels = 1:12, ordered = T)
          ) %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalMonth) %>%
          summarize(
            MonthAvg = mean(Residual_TrendLong/Predict_TrendLong)
          ) %>%
          ggplot(aes(MonthNo,MonthAvg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Pre Month Seasonality Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Month Number (1 = January)"
          ) + 
          theme_classic() 

  
      ##____20_1_2 Calculate sensitivities

        df_Pred_SeasonalMonth <-
          dfTrain %>%
          group_by_at(vct_group_PredSeasonalMonth) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_TrendLong/Predict_TrendLong),0)
          ) %>% ungroup()
        
      ##____20_1_3 Translate sensitivities into actual values
        dfTrain <-
          dfTrain %>%
          inner_join(
            df_Pred_SeasonalMonth
            ,by = vct_group_PredSeasonalMonth
          ) %>%
          mutate(
            Predict_SeasonalMonth = TrendLongRatio * Predict_TrendLong
            ,Residual_SeasonalMonth = Residual_TrendLong - Predict_SeasonalMonth
            ,Agg_Predict_SeasonalMonth = Predict_TrendLong + Predict_SeasonalMonth
          ) %>% select(-TrendLongRatio)
        
      ##____20_1_4 Generate post-calc plot
        plot_PostMonth <-
          dfTrain %>%
          mutate(
            MonthNo = factor(MonthNo, levels = 1:12, ordered = T)
          ) %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalMonth) %>%
          summarize(
            MonthAvg = mean(Residual_SeasonalMonth/Predict_TrendLong)
          ) %>%
          ggplot(aes(MonthNo,MonthAvg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Post Month Seasonality Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Month Number (1 = January)"
          ) + 
          theme_classic() 
        
        
  ## __20_2 Calculate DOW Impact ----
        
        vct_group_PredSeasonalDOW <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","DayNoWeek")
        vct_select_PredSeasonalDOW <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","DayNoWeek","TrendLongRatio")
        
        ##____20_2_1 Generate pre-calc plot
        plot_PreDOW <-
          dfTrain %>%
          mutate(
            DayNoWeek = factor(DayNoWeek, levels = 1:7, ordered = T)
          ) %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalDOW) %>%
          summarize(
            DOWAvg = mean(Residual_SeasonalMonth/Predict_TrendLong)
          ) %>%
          ggplot(aes(DayNoWeek,DOWAvg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Pre Day Of Week Seasonal Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Day Of Week Number (1 = Sunday)"
          ) + 
          theme_classic() 
        
        ##____20_2_2 Calculate sensitivities

        df_Pred_SeasonalDOW <-
          dfTrain %>%
          group_by_at(vct_group_PredSeasonalDOW) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_SeasonalMonth/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____20_2_3 Translate sensitivities into actual values
        dfTrain <-
          dfTrain %>%
          inner_join(
            df_Pred_SeasonalDOW
            ,by = vct_group_PredSeasonalDOW
          ) %>%
          mutate(
            Predict_SeasonalDOW = TrendLongRatio * Predict_TrendLong
            ,Residual_SeasonalDOW = Residual_SeasonalMonth - Predict_SeasonalDOW
            ,Agg_Predict_SeasonalDOW = Agg_Predict_SeasonalMonth + Predict_SeasonalDOW
          ) %>% select(-TrendLongRatio)
        
        ##____20_2_4 Generate post-calc plot
        plot_PostDOW <-
          dfTrain %>%
          mutate(
            DayNoWeek = factor(DayNoWeek, levels = 1:7, ordered = T)
          ) %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalDOW) %>%
          summarize(
            DOWAvg = mean(Residual_SeasonalDOW/Predict_TrendLong)
          ) %>%
          ggplot(aes(DayNoWeek,DOWAvg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Post Day Of Week Seasonal Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Day Of Week Number (1 = Sunday)"
          ) + 
          theme_classic() 
        
        
  ## __20_3 Calculate School Holiday ----  
        
        vct_group_PredSeasonalSchoolHoliday <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","SchoolHolidayDesc")
        vct_select_PredSeasonalSchoolHoliday <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","SchoolHolidayDesc","TrendLongRatio")
        
        ##____20_3_1 Generate pre-calc plot
        plot_PreSchoolHoliday <-
          dfTrain %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalSchoolHoliday) %>%
          summarize(
            Avg = mean(Residual_SeasonalDOW/Predict_TrendLong)
          ) %>%
          ggplot(aes(SchoolHolidayDesc,Avg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Pre School Holiday Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "School Holiday Description"
          ) + 
          theme_classic() 
        
        ##____20_3_2 Calculate sensitivities

        df_Pred_SeasonalSchoolHoliday <-
          dfTrain %>%
          group_by_at(vct_group_PredSeasonalSchoolHoliday) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_SeasonalDOW/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____20_3_3 Translate sensitivities into actual values
        dfTrain <-
          dfTrain %>%
          inner_join(
            df_Pred_SeasonalSchoolHoliday
            ,by = vct_group_PredSeasonalSchoolHoliday
          ) %>%
          mutate(
            Predict_SeasonalSchoolHoliday= TrendLongRatio * Predict_TrendLong
            ,Residual_SeasonalSchoolHoliday = Residual_SeasonalDOW - Predict_SeasonalSchoolHoliday
            ,Agg_Predict_SeasonalSchoolHoliday = Agg_Predict_SeasonalDOW + Predict_SeasonalSchoolHoliday
          ) %>% select(-TrendLongRatio)
        
        ##____20_3_4 Generate post-calc plot
        plot_PostSchoolHoliday <-
          dfTrain %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalSchoolHoliday) %>%
          summarize(
            Avg = mean(Residual_SeasonalSchoolHoliday/Predict_TrendLong)
          ) %>%
          ggplot(aes(SchoolHolidayDesc,Avg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Post School Holiday Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "School Holiday Description"
          ) + 
          theme_classic() 

        
        
## __20_4 Calculate Influential Dates ----  
        
        vct_group_PredSeasonalInfluentialDate <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","InfluentialDateCust1")
        vct_select_PredSeasonalInfluentialDate <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","InfluentialDateCust1","TrendLongRatio")
        
        ##____20_4_1 Generate pre-calc plot
        plot_PreInfluentialDate <-
          dfTrain %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalInfluentialDate) %>%
          summarize(
            Avg = mean(Residual_SeasonalSchoolHoliday/Predict_TrendLong)
          ) %>%
          ggplot(aes(InfluentialDateCust1,Avg)) +
          geom_bar(stat="identity") +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          labs(
            title = "Pre Influential Date Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Influential Date Description"
          ) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1)
          )
        
        ##____20_4_2 Calculate sensitivities

        df_Pred_SeasonalInfluentialDate <-
          dfTrain %>%
          group_by_at(vct_group_PredSeasonalInfluentialDate) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_SeasonalSchoolHoliday/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____20_4_3 Translate sensitivities into actual values
        dfTrain <-
          dfTrain %>%
          inner_join(
            df_Pred_SeasonalInfluentialDate
            ,by = vct_group_PredSeasonalInfluentialDate
          ) %>%
          mutate(
            Predict_SeasonalInfluentialDate = TrendLongRatio * Predict_TrendLong
            ,Residual_SeasonalInfluentialDate = Residual_SeasonalSchoolHoliday - Predict_SeasonalInfluentialDate
            ,Agg_Predict_SeasonalInfluentialDate = Agg_Predict_SeasonalSchoolHoliday + Predict_SeasonalInfluentialDate
          ) %>% select(-TrendLongRatio)
        
        ##____20_4_4 Generate post-calc plot
        plot_PostInfluentialDate <-
          dfTrain %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by_at(vct_group_PredSeasonalInfluentialDate) %>%
          summarize(
            Avg = mean(Residual_SeasonalInfluentialDate/Predict_TrendLong)
          ) %>%
          ggplot(aes(InfluentialDateCust1,Avg)) +
          geom_bar(stat="identity") +
          labs(
            title = "Post Influential Date Impact"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Influential Date Description"
          ) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) 

        
## 21 Calculate Non Seasonal Components ----
        
    ## Filtering due to lack of complete history of google analytics data
    dfTrain_EDM <- dfTrain %>% dplyr::filter(Date >= date_all_components_start)
        
## __21_1 Calculate EDM Initial Impact ----  
        
        vct_group_PredEDMMajorAvg <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMType")
        vct_select_PredEDMMajorAvg <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMType","TrendLongRatio")
        
        ##____21_1_1 Generate pre-calc plot
        plot_PreEDMMajorAvg <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by(StayLengthKey,MajorEDMType) %>%
          summarize(
            Avg = mean(Residual_SeasonalInfluentialDate/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(StayLengthKey,Avg)) +
          geom_bar(stat="identity") +
          facet_wrap(~MajorEDMType) +
          labs(
            title = "Pre Average EDM Impact"
            ,subtitle = "Panel 0 = No EDM, 1 = Normal Major EDM, 2 = Limited Time Offer EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Stay Length ID"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
    
        ##____21_1_2 Calculate sensitivities
        df_Pred_EDMMajorAvg <-
          dfTrain_EDM %>%
          group_by_at(vct_group_PredEDMMajorAvg) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_SeasonalInfluentialDate/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____21_1_3 Translate sensitivities into actual values
        dfTrain_EDM <-
          dfTrain_EDM %>%
          inner_join(
            df_Pred_EDMMajorAvg
            ,by = vct_group_PredEDMMajorAvg
          ) %>%
          mutate(
            Predict_EDMMajorAvg = TrendLongRatio * Predict_TrendLong
            ,Residual_EDMMajorAvg = Residual_SeasonalInfluentialDate - Predict_EDMMajorAvg
            ,Agg_Predict_EDMMajorAvg = Agg_Predict_SeasonalInfluentialDate + Predict_EDMMajorAvg
          ) %>% select(-TrendLongRatio)
        
        ##____21_1_4 Generate post-calc plot
        plot_PostEDMMajorAvg <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by(StayLengthKey,MajorEDMType) %>%
          summarize(
            Avg = mean(Residual_EDMMajorAvg/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(StayLengthKey,Avg)) +
          geom_bar(stat="identity") +
          facet_wrap(~MajorEDMType) +
          labs(
            title = "Post Average EDM Impact"
            ,subtitle = "Panel 0 = No EDM, 1 = Normal Major EDM, 2 = Limited Time Offer EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Stay Length ID"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
## __21_2 Calculate EDM Monthly Impact ----  

        vct_group_PredEDMMajorMonth <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMType","MonthNo")
        vct_select_PredEDMMajorMonth <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMType","MonthNo","TrendLongRatio")
        
        ##____21_2_1 Generate pre-calc plot
        plot_PreEDMMajorMonth <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          mutate(
            MonthNo = factor(MonthNo,levels = 1:12,ordered = T)
          ) %>%
          group_by_at(vct_group_PredEDMMajorMonth) %>%
          summarize(
            Avg = mean(Residual_EDMMajorAvg/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(MonthNo,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(~MajorEDMType) +
          labs(
            title = "Pred EDM Impact - Stratified By Month"
            ,subtitle = "Panel 0 = No EDM, 1 = Normal Major EDM, 2 = Limited Time Offer EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Month Number (1 = January)"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        
        ##____21_2_2 Calculate sensitivities
        df_Pred_EDMMajorMonth <-
          dfTrain_EDM %>%
          group_by_at(vct_group_PredEDMMajorMonth) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_EDMMajorAvg/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____21_2_3 Translate sensitivities into actual values
        dfTrain_EDM <-
          dfTrain_EDM %>%
          inner_join(
            df_Pred_EDMMajorMonth
            ,by = vct_group_PredEDMMajorMonth
          ) %>%
          mutate(
            Predict_EDMMajorMonth = TrendLongRatio * Predict_TrendLong
            ,Residual_EDMMajorMonth = Residual_EDMMajorAvg - Predict_EDMMajorMonth
            ,Agg_Predict_EDMMajorMonth = Agg_Predict_EDMMajorAvg + Predict_EDMMajorMonth
          ) %>% select(-TrendLongRatio)
        
        ##____21_2_4 Generate post-calc plot
        plot_PostEDMMajorMonth <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          mutate(
            MonthNo = factor(MonthNo,levels = 1:12,ordered = T)
          ) %>%
          group_by_at(vct_group_PredEDMMajorMonth) %>%
          summarize(
            Avg = mean(Residual_EDMMajorMonth/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(MonthNo,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(~MajorEDMType) +
          labs(
            title = "Post EDM Impact - Stratified By Month"
            ,subtitle = "Panel 0 = No EDM, 1 = Normal Major EDM, 2 = Limited Time Offer EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Month Number (1 = January)"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
## __21_3 Calculate EDM Day Of Week Impact ----  

        vct_group_PredEDMMajorDOW <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMType","DayNoWeek")
        vct_select_PredEDMMajorDOW <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMType","DayNoWeek","TrendLongRatio")
        
        ##____21_3_1 Generate pre-calc plot
        plot_PreEDMMajorDOW <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          mutate(
            DayNoWeek = factor(DayNoWeek, levels = 1:7, ordered = T)
          ) %>%
          group_by_at(vct_group_PredEDMMajorDOW) %>%
          summarize(
            Avg = mean(Residual_EDMMajorMonth/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(DayNoWeek,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(~MajorEDMType) +
          labs(
            title = "Pre EDM Impact - Stratified By Day Of Week"
            ,subtitle = "Panel 0 = No EDM, 1 = Normal Major EDM, 2 = Limited Time Offer EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Day Of Week (1 = Monday)"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        
        ##____21_3_2 Calculate sensitivities
        df_Pred_EDMMajorDOW <-
          dfTrain_EDM %>%
          group_by_at(vct_group_PredEDMMajorDOW) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_EDMMajorMonth/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____21_3_3 Translate sensitivities into actual values
        dfTrain_EDM <-
          dfTrain_EDM %>%
          inner_join(
            df_Pred_EDMMajorDOW
            ,by =vct_group_PredEDMMajorDOW
          ) %>%
          mutate(
            Predict_EDMMajorDOW = TrendLongRatio * Predict_TrendLong
            ,Residual_EDMMajorDOW = Residual_EDMMajorMonth - Predict_EDMMajorDOW
            ,Agg_Predict_EDMMajorDOW = Agg_Predict_EDMMajorMonth + Predict_EDMMajorDOW
          ) %>% select(-TrendLongRatio)
        
        ##____21_3_4 Generate post-calc plot
        plot_PostEDMMajorDOW <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          mutate(
            DayNoWeek = factor(DayNoWeek,levels = 1:7,ordered=T)
          ) %>%
          group_by_at(vct_group_PredEDMMajorDOW) %>%
          summarize(
            Avg = mean(Residual_EDMMajorDOW/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(DayNoWeek,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(~MajorEDMType) +
          labs(
            title = "Post EDM Impact - Stratified By Day Of Week"
            ,subtitle = "Panel 0 = No EDM, 1 = Normal Major EDM, 2 = Limited Time Offer EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Day Of Week (1 = Sunday)"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        
        
## __21_4 Calculate EDM Lagged Initial Impact ----  
 
        vct_group_PredEDMMajorLaggedAvg<- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","PriorEDMType","MajorEDMLaggedFlag")
        vct_select_PredEDMMajorLaggedAvg <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","PriorEDMType","MajorEDMLaggedFlag","TrendLongRatio")
               
        ##____21_4_1 Generate pre-calc plot
        plot_PreEDMMajorLaggedAvg <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by(StayLengthKey,PriorEDMType,MajorEDMLaggedFlag) %>%
          summarize(
            Avg = mean(Residual_EDMMajorDOW/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(StayLengthKey,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(MajorEDMLaggedFlag~PriorEDMType) +
          labs(
            title = "Pre Average EDM Lagged Impact"
            ,subtitle = "Panel major title, TRUE = Within 3 days of EDM, FALSE = More than 3 days of EDM \nPanel minor title, 1 = Prior EDM was normal, 2 = Prior EDM was limited time offer"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Stay Length Key"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        ##____21_4_2 Calculate sensitivities
        df_Pred_EDMMajorLaggedAvg <-
          dfTrain_EDM %>%
          group_by_at(vct_group_PredEDMMajorLaggedAvg) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_EDMMajorDOW/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____21_4_3 Translate sensitivities into actual values
        dfTrain_EDM <-
          dfTrain_EDM %>%
          inner_join(
            df_Pred_EDMMajorLaggedAvg
            ,by = vct_group_PredEDMMajorLaggedAvg
          ) %>%
          mutate(
            Predict_EDMMajorLaggedAvg = TrendLongRatio * Predict_TrendLong
            ,Residual_EDMMajorLaggedAvg = Residual_EDMMajorDOW - Predict_EDMMajorLaggedAvg
            ,Agg_Predict_EDMMajorLaggedAvg = Agg_Predict_EDMMajorDOW + Predict_EDMMajorLaggedAvg
          ) %>% select(-TrendLongRatio)
        
        ##____21_1_4 Generate post-calc plot
        plot_PostEDMMajorLaggedAvg <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
          ) %>%
          group_by(StayLengthKey,PriorEDMType,MajorEDMLaggedFlag) %>%
          summarize(
            Avg = mean(Residual_EDMMajorLaggedAvg/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(StayLengthKey,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(MajorEDMLaggedFlag~PriorEDMType) +
          labs(
            title = "Post Average EDM Lagged Impact"
            ,subtitle = "Panel major title, TRUE = Within 3 days of EDM, FALSE = More than 3 days of EDM \nPanel minor title, 1 = Prior EDM was normal, 2 = Prior EDM was limited time offer"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Stay Length Key"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
## __21_5 Calculate EDM Lagged Days Impact ----  
        
        vct_group_PredEDMMajorLaggedDays<- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","PriorEDMType","MajorEDMLaggedFlag","DaysFromEDMStart")
        vct_select_PredEDMMajorLaggedDays <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","PriorEDMType","MajorEDMLaggedFlag","DaysFromEDMStart","TrendLongRatio")
        
        ##____21_5_1 Generate pre-calc plot
        plot_PreEDMMajorLaggedDays <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & MajorEDMLaggedFlag == T
          ) %>%
          mutate(
            FacetTitle1 = paste0("Days Lagged ", DaysFromEDMStart)
            ,FacetTitle2 = paste0("Prior EDM Type ",PriorEDMType)
          ) %>%
          group_by(StayLengthKey,FacetTitle1,FacetTitle2) %>%
          summarize(
            Avg = mean(Residual_EDMMajorDOW/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(StayLengthKey,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(FacetTitle1~FacetTitle2,ncol =2,scales="free") +
          labs(
            title = "Pre EDM Lagged Impact - Days After"
            ,subtitle = "Panel major title, Number of days after an EDM \nPanel minor title, 1 = Prior EDM was normal, 2 = Prior EDM was limited time offer"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Stay Length Key"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        ##____21_5_2 Calculate sensitivities
        df_Pred_EDMMajorLaggedDays <-
          dfTrain_EDM %>%
          dplyr::filter(
            MajorEDMLaggedFlag == T
          ) %>%
          group_by_at(vct_group_PredEDMMajorLaggedDays) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_EDMMajorLaggedAvg/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____21_5_3 Translate sensitivities into actual values
        ## The Predict calculation has to use a coalesce as only days with MajorEDMLaggedFlag as TRUE has valid records.
        dfTrain_EDM <-
          dfTrain_EDM %>%
          left_join(
            df_Pred_EDMMajorLaggedDays
            ,by =vct_group_PredEDMMajorLaggedDays
          ) %>%
          mutate(
            Predict_EDMMajorLaggedDays = coalesce(TrendLongRatio,0) * Predict_TrendLong
            ,Residual_EDMMajorLaggedDays = Residual_EDMMajorLaggedAvg - Predict_EDMMajorLaggedDays
            ,Agg_Predict_EDMMajorLaggedDays = Agg_Predict_EDMMajorLaggedAvg + Predict_EDMMajorLaggedDays
          ) %>% select(-TrendLongRatio)
        
        ##____21_5_4 Generate post-calc plot
        plot_PostEDMMajorLaggedDays <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & MajorEDMLaggedFlag == T
          ) %>%
          mutate(
            FacetTitle1 = paste0("Days Lagged ", DaysFromEDMStart)
            ,FacetTitle2 = paste0("Prior EDM Type ",PriorEDMType)
          ) %>%
          group_by(StayLengthKey,FacetTitle1,FacetTitle2) %>%
          summarize(
            Avg = mean(Residual_EDMMajorLaggedDays/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(StayLengthKey,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(FacetTitle1~FacetTitle2,ncol =2,scales="free") +
          labs(
            title = "Post EDM Lagged Impact - Days After"
            ,subtitle = "Panel major title, Number of days after an EDM \nPanel minor title, 1 = Prior EDM was normal, 2 = Prior EDM was limited time offer"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Stay Length Key"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
## __21_6 Calculate EDM Lagged Days (DOW) Impact ----  
        
        vct_group_PredEDMMajorLaggedDaysDOW<- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMLaggedFlag","DayNoWeek","DaysFromEDMStart")
        vct_select_PredEDMMajorLaggedDaysDOW <- c("ReportLevel4Key","StayLengthKey","DimAmountTypeKey","MajorEDMLaggedFlag","DayNoWeek","DaysFromEDMStart","TrendLongRatio")
        
        ##____21_6_1 Generate pre-calc plot
        plot_PreEDMMajorLaggedDaysDOW <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & MajorEDMLaggedFlag == T
            & StayLengthKey %in% c(9,14,18,22)
          ) %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName,DayNoWeek,DaysFromEDMStart) %>%
          summarize(
            Avg = mean(Residual_EDMMajorLaggedDays/Predict_TrendLong)
            ,Sign = factor(sign(Avg))
          ) %>% ungroup() %>%
          ggplot(aes(DayNoWeek,Avg,fill = Sign)) +
          geom_bar(stat="identity") +
          scale_fill_manual(values=c("green","red")) +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(StayLengthName~DaysFromEDMStart,ncol =3,scales="free") +
          scale_x_continuous(breaks = 1:7) +
          labs(
            title = "Pre EDM Lagged Impact - Days After & Day Of Week"
            ,subtitle = "Panel major title, Stay Length (Sample) \nPanel minor title, number of days after EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Day Of Week (1 = Sunday)"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        ##____21_6_2 Calculate sensitivities
        df_Pred_EDMMajorLaggedDaysDOW <-
          dfTrain_EDM %>%
          dplyr::filter(
            MajorEDMLaggedFlag == T
          ) %>%
          group_by_at(vct_group_PredEDMMajorLaggedDaysDOW) %>%
          summarize(
            TrendLongRatio = coalesce(mean(Residual_EDMMajorLaggedDays/Predict_TrendLong),0)
          ) %>% ungroup()
        
        ##____21_6_3 Translate sensitivities into actual values
        dfTrain_EDM <-
          dfTrain_EDM %>%
          left_join(
            df_Pred_EDMMajorLaggedDaysDOW
            ,by = vct_group_PredEDMMajorLaggedDaysDOW
          ) %>%
          mutate(
            Predict_EDMMajorLaggedDaysDOW = coalesce(TrendLongRatio,0) * Predict_TrendLong
            ,Residual_EDMMajorLaggedDaysDOW = Residual_EDMMajorLaggedDays - Predict_EDMMajorLaggedDaysDOW
            ,Agg_Predict_EDMMajorLaggedDaysDOW = Agg_Predict_EDMMajorLaggedDays + Predict_EDMMajorLaggedDaysDOW
          ) %>% select(-TrendLongRatio)
        
        ##____21_6_4 Generate post-calc plot
        plot_PostEDMMajorLaggedDays <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & MajorEDMLaggedFlag == T
            & StayLengthKey %in% c(9,14,18,22)
          ) %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName,DayNoWeek,DaysFromEDMStart) %>%
          summarize(
            Avg = mean(Residual_EDMMajorLaggedDaysDOW/Predict_TrendLong)
          ) %>% ungroup() %>%
          ggplot(aes(DayNoWeek,Avg)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust =1)
          ) +
          facet_wrap(StayLengthName~DaysFromEDMStart,ncol =3,scales="free") +
          scale_x_continuous(breaks = 1:7) +
          labs(
            title = "Post EDM Lagged Impact - Days After & Day Of Week"
            ,subtitle = "Panel major title, Stay Length (Sample) \nPanel minor title, number of days after EDM"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "Day Of Week (1 = Sunday)"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
      
## 22 Calculate EDM Initial Regression ----          
        
##__22_1 Regression by Month ----
        
        ##____22_1_1 Initialise temporary table
        tmp_dfTrainEDMMajorMonthRgr <- 
          dfTrain_EDM %>% 
            dplyr::filter(
              MajorEDMFlag == 1
            ) %>%
            inner_join(
              select(FactCampaignMajorClean,Date,AmountTotalCampaignSent)
              ,by="Date"
            ) %>%
            dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
            mutate(
              Predict_Ratio_EDMMajorMonthRgr = coalesce(Residual_EDMMajorLaggedDaysDOW/Predict_TrendLong,0)
            ) %>%
            select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo,DayNoWeek
                   ,Predict_TrendLong,Residual_EDMMajorLaggedDaysDOW,Predict_Ratio_EDMMajorMonthRgr,AmountTotalCampaignSent)
          
        ##____22_1_2 Generate pre calc plot
        plot_PreEDMMajorMonthRgr <-
          tmp_dfTrainEDMMajorMonthRgr %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & StayLengthKey == sample_StayLengthKey
          ) %>%
          ggplot(aes(AmountTotalCampaignSent,Predict_Ratio_EDMMajorMonthRgr)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          facet_wrap(~MonthNo, scales = "free") +
          labs(
            title = "Pre EDM Total Campaign Volume - Stratified By Month"
            ,subtitle = "Panel Major Title, Month Number (1 = January)"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "EDM Volume"
          ) +
          scale_x_continuous(labels = function(x) paste0(round(x/1000,0),"K")) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
            ,axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        ##____22_1_3 Generate regression coefficients
        df_Pred_EDMMajorMonthRgr <-
          tmp_dfTrainEDMMajorMonthRgr %>%
          group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo) %>% nest() %>%
          mutate(
            data_fun = map(data,function(x){
              df_map <- as_tibble(x) %>%
                do(tidy(lm(Predict_Ratio_EDMMajorMonthRgr~AmountTotalCampaignSent, data = .),conf.int = F))
            })
          ) %>% unnest(data_fun) %>% select(-data) %>% ungroup() %>%
          mutate(
            term = ifelse(term == "(Intercept)","Intercept",term)
          ) %>%
          rename(
            CoefficientName = term
            ,CoefficientValue = estimate
          )
        
        rm(tmp_dfTrainEDMMajorMonthRgr)
        
        ##____22_1_4 Translate regression into the relevant predict ratios
        ## Values are stored into a temp table, to be removed after applying them to each PredictTrend Long
        tmp_EDMMajorMonthRgr_Ratio <- 
          dfTrain_EDM %>%
          dplyr::filter(
            MajorEDMFlag == 1
          ) %>%
          left_join(
            select(FactCampaignMajorClean,Date,AmountTotalCampaignSent)
            ,by=c("Date")
          ) %>%
          dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
          select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo,AmountTotalCampaignSent) %>%
          mutate(
            Intercept = 1
          ) %>%
          gather(CoefficientName,amount,-c(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo)) %>%
          left_join(
            select(df_Pred_EDMMajorMonthRgr,-c(std.error,statistic,p.value))
            ,by=c("ReportLevel4Key","DimAmountTypeKey","StayLengthKey","MonthNo","CoefficientName")
          ) %>%
          mutate(
            Predict_Ratio_EDMMajorMonthRgr = CoefficientValue * amount
          ) %>%
          group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
          summarize(
            Predict_Ratio_EDMMajorMonthRgr = sum(Predict_Ratio_EDMMajorMonthRgr)
          ) %>% ungroup()
        
        ##____22_1_5 Reintegrate ratio results to main data frame
        dfTrain_EDM <-
          dfTrain_EDM %>%
          left_join(
            tmp_EDMMajorMonthRgr_Ratio
            ,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")
          ) %>%
          mutate(
            Predict_EDMMajorMonthRgr = coalesce(Predict_Ratio_EDMMajorMonthRgr,0) * Predict_TrendLong
            ,Residual_EDMMajorMonthRgr = Residual_EDMMajorLaggedDaysDOW - Predict_EDMMajorMonthRgr
            ,Agg_Predict_EDMMajorMonthRgr = Agg_Predict_EDMMajorLaggedDaysDOW + Predict_EDMMajorMonthRgr
          ) %>%
          select(-Predict_Ratio_EDMMajorMonthRgr)
        
        rm(tmp_EDMMajorMonthRgr_Ratio)
        
        ##____22_1_6 Generate post-calc plots
        tmp_dfTrainEDMMajorMonthRgr <- 
          dfTrain_EDM %>% 
          dplyr::filter(
            MajorEDMFlag == 1
          ) %>%
          inner_join(
            select(FactCampaignMajorClean,Date,AmountTotalCampaignSent)
            ,by="Date"
          ) %>%
          dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
          mutate(
            Predict_Ratio_EDMMajorMonthRgr = coalesce(Residual_EDMMajorMonthRgr/Predict_TrendLong,0)
          ) %>%
          select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo,DayNoWeek
                 ,Predict_TrendLong,Residual_EDMMajorMonthRgr,Predict_Ratio_EDMMajorMonthRgr,AmountTotalCampaignSent)
        
        plot_PostEDMMajorMonthRgr <-
          tmp_dfTrainEDMMajorMonthRgr %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & StayLengthKey == sample_StayLengthKey
          ) %>%
          ggplot(aes(AmountTotalCampaignSent,Predict_Ratio_EDMMajorMonthRgr)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          facet_wrap(~MonthNo, scales = "free") +
          labs(
            title = "Post EDM Total Campaign Volume - Stratified By Month"
            ,subtitle = "Panel Major Title, Month Number (1 = January)"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "EDM Volume"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        rm(tmp_dfTrainEDMMajorMonthRgr)
        
##__22_2 Regression by DOW ----
        
        ##____22_2_1 Initialise temporary table
        tmp_dfTrainEDMMajorDOWRgr <- 
          dfTrain_EDM %>% 
          dplyr::filter(
            MajorEDMFlag == 1
          ) %>%
          inner_join(
            select(FactCampaignMajorClean,Date,AmountTotalCampaignSent)
            ,by="Date"
          ) %>%
          dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
          mutate(
            Predict_Ratio_EDMMajorDOWRgr = coalesce(Residual_EDMMajorMonthRgr/Predict_TrendLong,0)
          ) %>%
          select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo,DayNoWeek
                 ,Predict_TrendLong,Residual_EDMMajorMonthRgr,Predict_Ratio_EDMMajorDOWRgr,AmountTotalCampaignSent)
        
        ##____22_2_2 Generate pre calc plot
        plot_PreEDMMajorDOWRgr <-
          tmp_dfTrainEDMMajorDOWRgr %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & StayLengthKey == sample_StayLengthKey
          ) %>%
          ggplot(aes(AmountTotalCampaignSent,Predict_Ratio_EDMMajorDOWRgr)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          facet_wrap(~DayNoWeek, scales = "free") +
          scale_x_continuous(labels = function(x) paste0(round(x/1000,0),"K")) +
          labs(
            title = "Post EDM Total Campaign Volume - Stratified By Day Of Week"
            ,subtitle = "Panel Major Title, Day Of Week (1 = Sunday)"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "EDM Volume"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
            ,axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        ##____22_2_3 Generate regression coefficients
        df_Pred_EDMMajorDOWRgr <-
          tmp_dfTrainEDMMajorDOWRgr %>%
          group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DayNoWeek) %>% nest() %>%
          mutate(
            data_fun = map(data,function(x){
              df_map <- as_tibble(x) %>%
                do(tidy(lm(Predict_Ratio_EDMMajorDOWRgr~AmountTotalCampaignSent, data = .),conf.int = F))
            })
          ) %>% unnest(data_fun) %>% select(-data) %>% ungroup() %>%
          mutate(
            term = ifelse(term == "(Intercept)","Intercept",term)
          ) %>%
          rename(
            CoefficientName = term
            ,CoefficientValue = estimate
          )
        
        rm(tmp_dfTrainEDMMajorDOWRgr)
        
        ##____22_2_4 Translate regression into the relevant predict ratios
        ## Values are stored into a temp table, to be removed after applying them to each PredictTrend Long
        tmp_EDMMajorDOWRgr_Ratio <- 
          dfTrain_EDM %>%
          dplyr::filter(
            MajorEDMFlag == 1
          ) %>%
          left_join(
            select(FactCampaignMajorClean,Date,AmountTotalCampaignSent)
            ,by=c("Date")
          ) %>%
          dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
          select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DayNoWeek,AmountTotalCampaignSent) %>%
          mutate(
            Intercept = 1
          ) %>%
          gather(CoefficientName,amount,-c(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DayNoWeek)) %>%
          left_join(
            select(df_Pred_EDMMajorDOWRgr,-c(std.error,statistic,p.value))
            ,by=c("ReportLevel4Key","DimAmountTypeKey","StayLengthKey","DayNoWeek","CoefficientName")
          ) %>%
          mutate(
            Predict_Ratio_EDMMajorDOWRgr = CoefficientValue * amount
          ) %>%
          group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
          summarize(
            Predict_Ratio_EDMMajorDOWRgr = sum(Predict_Ratio_EDMMajorDOWRgr)
          ) %>% ungroup()
        
        ##____22_2_5 Reintegrate ratio results to main data frame
        dfTrain_EDM <-
          dfTrain_EDM %>%
          left_join(
            tmp_EDMMajorDOWRgr_Ratio
            ,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")
          ) %>%
          mutate(
            Predict_EDMMajorDOWRgr = coalesce(Predict_Ratio_EDMMajorDOWRgr,0) * Predict_TrendLong
            ,Residual_EDMMajorDOWRgr = Residual_EDMMajorMonthRgr - Predict_EDMMajorDOWRgr
            ,Agg_Predict_EDMMajorDOWRgr = Agg_Predict_EDMMajorMonthRgr + Predict_EDMMajorDOWRgr
          ) %>%
          select(-Predict_Ratio_EDMMajorDOWRgr)
      
        rm(tmp_EDMMajorDOWRgr_Ratio)
        
        ##____22_1_6 Generate post-calc plots
        tmp_dfTrainEDMMajorDOWRgr <- 
          dfTrain_EDM %>% 
          dplyr::filter(
            MajorEDMFlag == 1
          ) %>%
          inner_join(
            select(FactCampaignMajorClean,Date,AmountTotalCampaignSent)
            ,by="Date"
          ) %>%
          dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
          mutate(
            Predict_Ratio_EDMMajorDOWRgr = coalesce(Residual_EDMMajorDOWRgr/Predict_TrendLong,0)
          ) %>%
          select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo,DayNoWeek
                 ,Predict_TrendLong,Residual_EDMMajorMonthRgr,Predict_Ratio_EDMMajorDOWRgr,AmountTotalCampaignSent)
        
        plot_PostEDMMajorDOWRgr <-
          tmp_dfTrainEDMMajorDOWRgr %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & StayLengthKey == sample_StayLengthKey
          ) %>%
          ggplot(aes(AmountTotalCampaignSent,Predict_Ratio_EDMMajorDOWRgr)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          facet_wrap(~DayNoWeek, scales = "free") +
          labs(
            title = "Post EDM Total Campaign Volume - Stratified By Day Of Week"
            ,subtitle = "Panel Major Title, Day Of Week (1 = Sunday)"
            ,y = "Relative Residual to Long Term Trend"
            ,x = "EDM Volume"
          ) +
          theme_classic() +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1)
          )
        
        rm(tmp_dfTrainEDMMajorDOWRgr)        
        
## 29__Remaining Residuals ----
        
        tmpPriceResid <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            & DimAmountTypeKey == 103
          ) %>% 
          mutate(
            ResidPerc = Residual_EDMMajorDOWRgr/Predict_TrendLong
          ) %>%
          select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,ResidPerc,ATVChangePercPrior,ATVChangePercTrend,Amount)
        
        plotResidualPerc <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            # & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & Date >= "2019-01-01" & Date <= "2019-12-31"
            # & DayNoWeek %in% c(2,4,6)
          ) %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          dplyr::filter(
            !StayLengthName %in% c("15 - 30 mins","30 - 60 mins","1 - 2 hours","2 - 3 hours","3 - 4 hours")
          ) %>%
          select(Date,ReportLevel4Key,StayLengthName,Residual_EDMMajorDOWRgr,Predict_TrendLong,Amount,DayNoWeek) %>%
          mutate(
            YearNo = year(Date)
            ,WeekNo = week(Date)
          ) %>%
          group_by(YearNo,WeekNo) %>%
          mutate(
            StartOfWeek = min(Date)
          ) %>% ungroup () %>% select(-YearNo,WeekNo) %>%
          mutate(
            PercVar = ifelse(abs(Residual_EDMMajorDOWRgr/Predict_TrendLong) >= 1,sign(Residual_EDMMajorDOWRgr/Predict_TrendLong)*1, Residual_EDMMajorDOWRgr/Predict_TrendLong)
            ,AbsPercVar = abs(PercVar)
          ) %>%
          ggplot(aes(StayLengthName,StartOfWeek,fill=PercVar)) +
          geom_tile(col = "white") +
          scale_fill_gradient(low = "red", high = "green") +
          scale_x_discrete(breaks =           
                             DimStayLength %>%
                             dplyr::filter(
                               !StayLengthName %in% c("Not Applicable","Unknown","0 - 15 mins","15 - 30 mins","30 - 60 mins","1 - 2 hours","2 - 3 hours","3 - 4 hours")
                               &  as.integer(StayLengthName) %% 2
                             ) %>%
                             distinct(StayLengthName) %>% pull()
                           )+
          coord_flip() +
          facet_wrap(~DayNoWeek,scales = "free") +
          labs(
            title = "Percentage Model Residual Per Day Of Week"
            ,subtitle = "For ReportLevel4Key 247\nIdeal percentage error should be 0."
            ,x = "Stay Length"
          ) +
          theme_classic() +
          theme(
            axis.title.x = element_blank()
            ,axis.text.x = element_text(angle = 60,hjust = 1)
          )
        
        
        kableResidualSample <-
          dfTrain_EDM %>%
          dplyr::filter(
            ReportLevel4Key == sample_ReportLevel4Key
            # & StayLengthKey == sample_StayLengthKey
            & DimAmountTypeKey == sample_DimAmountTypeKey
            & Date >= "2019-01-01" & Date <= "2019-12-31"
            # & DayNoWeek %in% c(2,4,6)
          ) %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          dplyr::filter(
            !StayLengthName %in% c("15 - 30 mins","30 - 60 mins","1 - 2 hours","2 - 3 hours","3 - 4 hours")
          ) %>%
          select(Date,ReportLevel4Key,StayLengthName,Residual_EDMMajorDOWRgr,Predict_TrendLong,Amount) %>%
          mutate(
            PercVar = ifelse(abs(Residual_EDMMajorDOWRgr/Predict_TrendLong) >= 1,sign(Residual_EDMMajorDOWRgr/Predict_TrendLong)*1, Residual_EDMMajorDOWRgr/Predict_TrendLong)
            ,AbsPercVar = abs(PercVar)
          ) %>% 
          group_by(StayLengthName) %>%
          summarize(
            `MAE (%)` = 100*round(mean(AbsPercVar),2)
            ,`MBE (%)` = 100*round(mean(PercVar),2)
            ,TotalPurchases = sum(Amount)
          ) %>% ungroup() %>%
          mutate(
            `%` = round(100*TotalPurchases/sum(TotalPurchases),2)
          ) %>%
          knitr::kable()
        
        
        CorATVChangePercPrior <-
          tmpPriceResid %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName) %>%
          summarize(
            Cor = cor(ATVChangePercPrior,ResidPerc,use="pairwise.complete.obs")
            ,TotalPurchases = sum(Amount)
          ) %>% knitr::kable()
        
        
        CorATVChangePercTrend <-
          tmpPriceResid %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName) %>%
          summarize(
            Cor = cor(ATVChangePercTrend,ResidPerc,use="pairwise.complete.obs")
            ,TotalPurchases = sum(Amount)
          ) %>% knitr::kable()
        
        
        CorATVAll <- 
          tmpPriceResid %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName) %>%
          summarize(
            `Against Trend` = round(cor(ATVChangePercTrend,ResidPerc,use="pairwise.complete.obs"),2)
            ,`Against Prior Day` = round(cor(ATVChangePercPrior,ResidPerc,use="pairwise.complete.obs"),2)
            ,TotalPurchases = sum(Amount)
          ) %>% knitr::kable()
        
        
        LinearRegressionATVChangePercPrior <- 
          tmpPriceResid %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName) %>% nest() %>%
          mutate(
            data_fun = map(data,function(x){
              df_map <- as_tibble(x) %>%
                do(tidy(lm(ResidPerc~ATVChangePercPrior, data = .),conf.int = F))
            })
          ) %>% unnest(data_fun) %>% select(-data) %>% ungroup() %>%
          dplyr::filter(
            term != "(Intercept)"
          ) %>%
          ggplot(aes(StayLengthName,p.value)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1)
          ) + coord_flip() +
          theme_classic() +
          labs(
            title = "P.Values - Linear Regression of Residual Perc vs ATV Change Daily Perc"
          )
        
        LinearRegressionATVChangePercTrend <- 
          tmpPriceResid %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          group_by(StayLengthName) %>% nest() %>%
          mutate(
            data_fun = map(data,function(x){
              df_map <- as_tibble(x) %>%
                do(tidy(lm(ResidPerc~ATVChangePercTrend, data = .),conf.int = F))
            })
          ) %>% unnest(data_fun) %>% select(-data) %>% ungroup() %>%
          dplyr::filter(
            term != "(Intercept)"
          ) %>%
          ggplot(aes(StayLengthName,p.value)) +
          geom_bar(stat="identity") +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1)
          ) + coord_flip() +
          theme_classic() +
          labs(
            title = "P.Values - Linear Regression of Residual Perc vs ATV Change Trend Perc"
          )
        
        PlotLinearRegressionATVChangePercPrior <-
          tmpPriceResid %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          dplyr::filter(
            StayLengthName %in% c("2 - 3 days","4 - 5 days","7 - 8 days","14 - 15 days","20 - 21 days")
          ) %>%
          ggplot(aes(ATVChangePercPrior,ResidPerc,col=StayLengthName)) +
          geom_point() +
          geom_smooth(method = "lm",se=F) +
          theme_classic() +
          labs(
            title = "Linear Regression of Model Residual Perc vs ATV Change Daily Perc"
          )
        
        PlotLinearRegressionATVChangePercTrend <-
          tmpPriceResid %>%
          mutate(
            ResidPerc = round(100*ResidPerc,0)
            ,ATVChangePercTrend = round(100*ATVChangePercTrend,0)
          ) %>%
          left_join(
            select(DimStayLength,StayLengthKey,StayLengthName)
            ,by="StayLengthKey"
          ) %>%
          dplyr::filter(
            StayLengthName %in% c("2 - 3 days","4 - 5 days","7 - 8 days","14 - 15 days","20 - 21 days")
          ) %>%
          ggplot(aes(ATVChangePercTrend,ResidPerc,col=StayLengthName)) +
          geom_point(alpha = 0.1) +
          geom_smooth(method = "lm",se=F) +
          theme_classic() +
          labs(
            title = "Linear Regression of Model Residual Perc vs ATV Change Trend Perc"
          ) +
          scale_y_continuous(limits = c(-100,100))
        
        rm(tmpPriceResid)
        
        
        
## 30__Create ML Transform Function ----
## Create function to input a data.frame of the same format as dfMain and output training results         
      

          
        
        applyTrainingResults <- function(CoreResults,CampaignData){
          CoreResults <- as_tibble(CoreResults)          
          
          CoreResults <- CoreResults %>%
            dplyr::filter(Date >= min(CampaignData$Date) & DimAmountTypeKey == 103) %>%

            ## Calculate non-regression results
            left_join(select_at(df_Pred_SeasonalMonth,vct_select_PredSeasonalMonth),by = vct_group_PredSeasonalMonth) %>%
            mutate(Predict_SeasonalMonth = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_SeasonalDOW,vct_select_PredSeasonalDOW),by = vct_group_PredSeasonalDOW) %>%
            mutate(Predict_SeasonalDOW = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_SeasonalSchoolHoliday,vct_select_PredSeasonalSchoolHoliday),by = vct_group_PredSeasonalSchoolHoliday) %>%
            mutate(Predict_SeasonalSchoolHoliday = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_SeasonalInfluentialDate,vct_select_PredSeasonalInfluentialDate), by = vct_group_PredSeasonalInfluentialDate) %>%
            mutate(Predict_SeasonalInfluentialDate = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_EDMMajorAvg,vct_select_PredEDMMajorAvg),by = vct_group_PredEDMMajorAvg) %>%
            mutate(Predict_EDMMajorAvg = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_EDMMajorMonth,vct_select_PredEDMMajorMonth),by=vct_group_PredEDMMajorMonth) %>%
            mutate(Predict_EDMMajorMonth = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_EDMMajorDOW,vct_select_PredEDMMajorDOW),by=vct_group_PredEDMMajorDOW) %>%
            mutate(Predict_EDMMajorDOW = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_EDMMajorLaggedAvg,vct_select_PredEDMMajorLaggedAvg),by=vct_group_PredEDMMajorLaggedAvg) %>%
            mutate(Predict_EDMMajorLaggedAvg = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_EDMMajorLaggedDays,vct_select_PredEDMMajorLaggedDays),by=vct_group_PredEDMMajorLaggedDays) %>%
            mutate(Predict_EDMMajorLaggedDays = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio) %>%
          
            left_join(select_at(df_Pred_EDMMajorLaggedDaysDOW,vct_select_PredEDMMajorLaggedDaysDOW),by=vct_group_PredEDMMajorLaggedDaysDOW) %>%
            mutate(Predict_EDMMajorLaggedDaysDOW = coalesce(TrendLongRatio * Predict_TrendLong,0)) %>% select(-TrendLongRatio)
          
          
          ## Calculate EDM Major Regression by Month
          tmp_EDMMajorMonthRgr_Ratio <-
            CoreResults %>%
            dplyr::filter(
              MajorEDMFlag == 1
            ) %>%
            inner_join(
              select(CampaignData,Date,AmountTotalCampaignSent)
              ,by="Date"
            ) %>%
            dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
            select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo,AmountTotalCampaignSent) %>%
            mutate(
              Intercept = 1
            ) %>%
            gather(CoefficientName,amount,-c(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,MonthNo)) %>%
            left_join(
              select(df_Pred_EDMMajorMonthRgr,-c(std.error,statistic,p.value))
              ,by=c("ReportLevel4Key","DimAmountTypeKey","StayLengthKey","MonthNo","CoefficientName")
            ) %>%
            mutate(
              Predict_Ratio_EDMMajorMonthRgr = CoefficientValue * amount
            ) %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
            summarize(
              Predict_Ratio_EDMMajorMonthRgr = sum(Predict_Ratio_EDMMajorMonthRgr)
            ) %>% ungroup()
          
          CoreResults <-
            CoreResults %>%
            left_join(
              tmp_EDMMajorMonthRgr_Ratio
              ,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")
            ) %>%
            mutate(
              Predict_EDMMajorMonthRgr = coalesce(Predict_Ratio_EDMMajorMonthRgr,0) * Predict_TrendLong
            ) %>%
            select(-Predict_Ratio_EDMMajorMonthRgr)
          
          rm(tmp_EDMMajorMonthRgr_Ratio)
          
          ## Calculate EDM Major Regression by DOW
          
          tmp_EDMMajorDOWRgr_Ratio <-
            CoreResults %>%
            dplyr::filter(
              MajorEDMFlag == 1
            ) %>%
            left_join(
              select(CampaignData,Date,AmountTotalCampaignSent)
              ,by=c("Date")
            ) %>%
            dplyr::filter(!is.na(AmountTotalCampaignSent)) %>%
            select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DayNoWeek,AmountTotalCampaignSent) %>%
            mutate(
              Intercept = 1
            ) %>%
            gather(CoefficientName,amount,-c(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DayNoWeek)) %>%
            left_join(
              select(df_Pred_EDMMajorDOWRgr,-c(std.error,statistic,p.value))
              ,by=c("ReportLevel4Key","DimAmountTypeKey","StayLengthKey","DayNoWeek","CoefficientName")
            ) %>%
            mutate(
              Predict_Ratio_EDMMajorDOWRgr = CoefficientValue * amount
            ) %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
            summarize(
              Predict_Ratio_EDMMajorDOWRgr = sum(Predict_Ratio_EDMMajorDOWRgr)
            ) %>% ungroup()
          
          CoreResults <-
            CoreResults %>%
            left_join(
              tmp_EDMMajorDOWRgr_Ratio
              ,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")
            ) %>%
            mutate(
              Predict_EDMMajorDOWRgr = coalesce(Predict_Ratio_EDMMajorDOWRgr,0) * Predict_TrendLong
            ) %>%
            select(-Predict_Ratio_EDMMajorDOWRgr)
          
          rm(tmp_EDMMajorDOWRgr_Ratio)
          
          CoreResults <-
            CoreResults %>%
            select(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey
                   ,Amount
                   ,Predict_TrendLong,Predict_SeasonalMonth,Predict_SeasonalDOW,Predict_SeasonalSchoolHoliday,Predict_SeasonalInfluentialDate
                   ,Predict_EDMMajorAvg,Predict_EDMMajorMonth,Predict_EDMMajorDOW
                   ,Predict_EDMMajorLaggedAvg,Predict_EDMMajorLaggedDays,Predict_EDMMajorLaggedDaysDOW
                   ,Predict_EDMMajorMonthRgr,Predict_EDMMajorDOWRgr) %>%
            mutate(
              Residual = Amount - (Predict_TrendLong+Predict_SeasonalMonth+Predict_SeasonalDOW+Predict_SeasonalSchoolHoliday+Predict_SeasonalInfluentialDate
                                   +Predict_EDMMajorAvg+Predict_EDMMajorMonth+Predict_EDMMajorDOW
                                   +Predict_EDMMajorLaggedAvg+Predict_EDMMajorLaggedDays+Predict_EDMMajorLaggedDaysDOW
                                   +Predict_EDMMajorMonthRgr+Predict_EDMMajorDOWRgr
                                  )
            ) %>%
            gather(DecompositionName,Amount,-c(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey)) %>%
            mutate(
              DecompositionName = factor(DecompositionName, levels = c("Amount"
                                                                       ,"Predict_TrendLong","Predict_SeasonalMonth","Predict_SeasonalDOW","Predict_SeasonalSchoolHoliday","Predict_SeasonalInfluentialDate"
                                                                       ,"Predict_EDMMajorAvg","Predict_EDMMajorMonth","Predict_EDMMajorDOW","Predict_EDMMajorMonthRgr","Predict_EDMMajorDOWRgr"
                                                                       ,"Predict_EDMMajorLaggedAvg","Predict_EDMMajorLaggedDays","Predict_EDMMajorLaggedDaysDOW"
                                                                       ,"Residual")
                                         ,ordered = T)
            )
          
          tmpDecompositionHierarchy <-
            CoreResults %>%
            distinct(DecompositionName) %>%
            mutate(
              DecompositionType_l1 = factor(
                                      ifelse(DecompositionName == "Amount","Actual Results"
                                             ,ifelse(str_sub(DecompositionName,1,7) == "Predict","Model Results"
                                             ,ifelse(DecompositionName == "Residual","Residual","Other"))
                                      )
                                      ,levels = c("Actual Results","Model Results","Residual","Other"),ordered = T)
              ,DecompositionType_l2 = sapply(DecompositionName,funApplyDecompositionHierarchy_l2)
            )
          
          ## Dataframe output
          CoreResults <-
            CoreResults %>%
            left_join(
              tmpDecompositionHierarchy
              ,by="DecompositionName"
            )
          
          
          ReportLevel4KeyList <- CoreResults %>% distinct(ReportLevel4Key) %>% pull()
          
          ## ReportLevel4Key level timeseries plots
          ReportLevel4Key_ModelPlots <- lapply(
            setNames(ReportLevel4KeyList,ReportLevel4KeyList),
            function(x){
              
              CoreResults %>%
                dplyr::filter(ReportLevel4Key == x) %>%
                group_by(Date,ReportLevel4Key,DimAmountTypeKey,DecompositionType_l1) %>%
                summarize(
                  Amount = sum(Amount)
                ) %>% ungroup() %>%
                ggplot(aes(Date,Amount)) +
                geom_bar(stat = "identity") +
                facet_wrap(~DecompositionType_l1) +
                labs(
                  title = "Comparison of Actual, Model & Model Residual Results"
                  ,subtitle = paste0("Volume of transactions sold for group ",x)
                ) +
                theme_classic() 
            }
          )
          
          ## ReportLevel4Key level residual histogram plots
          ReportLevel4Key_ResidualPlots <- lapply(
            setNames(ReportLevel4KeyList,ReportLevel4KeyList),
            function(x){
              
              CoreResults %>%
                dplyr::filter(ReportLevel4Key == x,DecompositionType_l1 == "Residual") %>%
                group_by(Date,ReportLevel4Key,DimAmountTypeKey,DecompositionType_l1) %>%
                summarize(
                  Amount = sum(Amount)
                ) %>% ungroup() %>%
                ggplot(aes(Amount)) +
                geom_histogram(bins = 20) +
                labs(
                  title = "ReportLevel4Key Aggregated Residual Distribution"
                ) +
                theme_classic() 
            }
          )
          
          ## Generate RMSE results per ReportLevel4Key and StayLengthName
          
          tmpAcc <- 
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 != "Model Results"
            ) %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DecompositionType_l1) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            spread(DecompositionType_l1,Amount) %>%
            mutate(
              PercVar = Residual/`Actual Results`
            ) %>% 
            dplyr::filter(
              is.finite(PercVar)
              & !is.na(PercVar)
            ) %>%
            group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
            summarize(
              MAEPerc = mean(abs(PercVar))
              ,MBEPerc = mean(PercVar)
              ,MAE = mean(abs(Residual))
              ,MBE = mean(Residual)
              ,Total = sum(`Actual Results`)
            ) %>% ungroup() %>%
            left_join(select(DimStayLength,StayLengthKey,StayLengthName),by="StayLengthKey") %>%
            select(ReportLevel4Key,StayLengthName,DimAmountTypeKey,MAE,MBE,MAEPerc,MBEPerc,Total)
          
          AccFull <- 
            CoreResults %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DecompositionType_l1) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            spread(DecompositionType_l1,Amount) %>%
            group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
            summarize(
              RMSE = RMSE(`Actual Results`,`Model Results`)
              ,ActualMean = mean(`Actual Results`)
              ,RMSEPerc = RMSE/ActualMean
            ) %>% ungroup() %>%
            left_join(select(DimStayLength,StayLengthKey,StayLengthName),by="StayLengthKey") %>%
            left_join(tmpAcc,by=c("ReportLevel4Key","StayLengthName","DimAmountTypeKey")) %>%
            select(ReportLevel4Key,StayLengthName,DimAmountTypeKey,RMSE,ActualMean,RMSEPerc,MAEPerc,MBEPerc,MAE,MBE,Total) 
          
          rm(tmpAcc)
          
          ## Generate RMSE results per ReportLevel4Key
          tmpAcc <-
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 != "Model Results"
            ) %>%
            group_by(Date,ReportLevel4Key,DimAmountTypeKey,DecompositionType_l1) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            spread(DecompositionType_l1,Amount) %>%
            mutate(
              PercVar = Residual/`Actual Results`
            ) %>%
            dplyr::filter(
              is.finite(PercVar)
              & !is.na(PercVar)
            ) %>%
            group_by(ReportLevel4Key,DimAmountTypeKey) %>%
            summarize(
              MAEPerc = mean(abs(PercVar))
              ,MBEPerc = mean(PercVar)
              ,MAE = mean(abs(Residual))
              ,MBE = mean(Residual)
              ,Total = sum(`Actual Results`)
            ) %>% ungroup() %>%
            select(ReportLevel4Key,DimAmountTypeKey,MAE,MBE,MAEPerc,MBEPerc,Total)
          

          AccReportLevel4Key <-
            CoreResults %>%
            group_by(Date,ReportLevel4Key,DimAmountTypeKey,DecompositionType_l1) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            spread(DecompositionType_l1,Amount) %>%
            group_by(ReportLevel4Key,DimAmountTypeKey) %>%
            summarize(
              RMSE = RMSE(`Actual Results`,`Model Results`)
              ,ActualMean = mean(`Actual Results`)
              ,RMSEPerc = RMSE/ActualMean
            ) %>% ungroup() %>%
            left_join(tmpAcc,by=c("ReportLevel4Key","DimAmountTypeKey")) %>%
            select(ReportLevel4Key,DimAmountTypeKey,RMSE,ActualMean,RMSEPerc,MAEPerc,MBEPerc,MAE,MBE,Total)
          
          rm(tmpAcc)
          
          ## Generate RMSE results high level
          tmpAcc <- 
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 != "Model Results"
            ) %>%
            group_by(Date,DimAmountTypeKey,DecompositionType_l1) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            spread(DecompositionType_l1,Amount) %>%
            mutate(
              PercVar = Residual/`Actual Results`
            ) %>% 
            dplyr::filter(
              is.finite(PercVar)
              & !is.na(PercVar)
            ) %>%
            group_by(DimAmountTypeKey) %>%
            summarize(
              MAEPerc = mean(abs(PercVar))
              ,MBEPerc = mean(PercVar)
              ,MAE = mean(abs(Residual))
              ,MBE = mean(Residual)
              ,Total = sum(`Actual Results`)
            ) %>% ungroup() %>%
            select(DimAmountTypeKey,MAE,MBE,MAEPerc,MBEPerc,Total)
          
          AccHighLevel <-
            CoreResults %>%
            group_by(Date,DimAmountTypeKey,DecompositionType_l1) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            spread(DecompositionType_l1,Amount) %>%
            group_by(DimAmountTypeKey) %>%
            summarize(
              RMSE = RMSE(`Actual Results`,`Model Results`)
              ,ActualMean = mean(`Actual Results`)
              ,RMSEPerc = RMSE/ActualMean
            ) %>%
            left_join(
              tmpAcc,by="DimAmountTypeKey"
            )
          rm(tmpAcc)
          
          
          ## Generate RMSE step change - full level
          tmpActuals <- 
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 == "Actual Results"
            ) %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>%
            summarize(
              ActualResults = sum(Amount)
            ) 
          
          RMSEStepChangeFull <-
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 == "Model Results"
            ) %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DecompositionName) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            group_by(Date,ReportLevel4Key,StayLengthKey,DimAmountTypeKey) %>% arrange(DecompositionName) %>%
            mutate(
              Amount = rollapplyr(Amount, 99, sum, partial = T)
            ) %>% 
            left_join(tmpActuals,by=c("Date","ReportLevel4Key","StayLengthKey","DimAmountTypeKey")) %>%
            group_by(ReportLevel4Key,StayLengthKey,DimAmountTypeKey,DecompositionName) %>%
            summarize(
              RMSE = RMSE(Amount,ActualResults)
            ) %>% ungroup() %>%
            left_join(select(DimStayLength,StayLengthKey,StayLengthName),by="StayLengthKey") %>%
            group_by(ReportLevel4Key,StayLengthName,DimAmountTypeKey) %>% arrange(DecompositionName) %>%
            mutate(
              RMSEChange = RMSE/lag(RMSE) - 1
            ) %>% ungroup()
          
          rm(tmpActuals)
          
          ## Generate RMSE step change - ReportLevel4Key level
          tmpActuals <- 
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 == "Actual Results"
            ) %>%
            group_by(Date,ReportLevel4Key,DimAmountTypeKey) %>%
            summarize(
              ActualResults = sum(Amount)
            ) 
          
          RMSEStepChange_ReportLevel4Key <-
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 == "Model Results"
            ) %>%
            group_by(Date,ReportLevel4Key,DimAmountTypeKey,DecompositionName) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            group_by(Date,ReportLevel4Key,DimAmountTypeKey) %>% arrange(DecompositionName) %>%
            mutate(
              Amount = rollapplyr(Amount, 99, sum, partial = T)
            ) %>% 
            left_join(tmpActuals,by=c("Date","ReportLevel4Key","DimAmountTypeKey")) %>%
            group_by(ReportLevel4Key,DimAmountTypeKey,DecompositionName) %>%
            summarize(
              RMSE = RMSE(Amount,ActualResults)
            ) %>% ungroup() %>%
            group_by(ReportLevel4Key,DimAmountTypeKey) %>% arrange(DecompositionName) %>%
            mutate(
              RMSEChange = RMSE/lag(RMSE) - 1
            ) %>% ungroup()
          
          rm(tmpActuals)
          
          ## Generate RMSE step change - High Level
          tmpActuals <- 
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 == "Actual Results"
            ) %>%
            group_by(Date,DimAmountTypeKey) %>%
            summarize(
              ActualResults = sum(Amount)
            ) 
          
          RMSEStepChange_HighLevel <-
            CoreResults %>%
            dplyr::filter(
              DecompositionType_l1 == "Model Results"
            ) %>%
            group_by(Date,DimAmountTypeKey,DecompositionName) %>%
            summarize(
              Amount = sum(Amount)
            ) %>% ungroup() %>%
            group_by(Date,DimAmountTypeKey) %>% arrange(DecompositionName) %>%
            mutate(
              Amount = rollapplyr(Amount, 99, sum, partial = T)
            ) %>% 
            left_join(tmpActuals,by=c("Date","DimAmountTypeKey")) %>%
            group_by(DimAmountTypeKey,DecompositionName) %>%
            summarize(
              RMSE = RMSE(Amount,ActualResults)
            ) %>% ungroup() %>%
            group_by(DimAmountTypeKey) %>% arrange(DecompositionName) %>%
            mutate(
              RMSEChange = RMSE/lag(RMSE) - 1
            ) %>% ungroup()
          
          RMSEStepChange_HighLevel_Plot <-
            RMSEStepChange_HighLevel %>%
            dplyr::filter(DimAmountTypeKey == 103) %>%
            ggplot(aes(DecompositionName,RMSE)) +
            geom_bar(stat = "identity") +
            labs(
              title = "RMSE Improvement Per Decomposition (High Level)"
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_text(angle = 90,hjust = 1)
            )
          
          rm(tmpActuals)
          rm(tmpDecompositionHierarchy)
          
          
          OutputList <-
            list(
              CoreResults = CoreResults
              ,Plots_TimeSeries = ReportLevel4Key_ModelPlots
              ,Plots_Residual = ReportLevel4Key_ResidualPlots
              ,AccFull = AccFull
              ,AccReportLevel4Key = AccReportLevel4Key
              ,AccHighLevel = AccHighLevel
              ,RMSEStepChangeFull = RMSEStepChangeFull
              ,RMSEStepChange_ReportLevel4Key = RMSEStepChange_ReportLevel4Key
              ,RMSEStepChange_HighLevel = RMSEStepChange_HighLevel
              ,RMSEStepChange_HighLevel_Plot = RMSEStepChange_HighLevel_Plot
            )
          
          OutputList
        }
        
## 40__Generate Training & Test Results ----
        dfTest <- dfMain[testIndex,]
        dfTrain <- dfMain[-testIndex,]
        
        dfTrain_Output <- applyTrainingResults(dfTrain,FactCampaignMajorClean)
        dfTest_Output <- applyTrainingResults(dfTest,FactCampaignMajorClean)
        
        # dfTrain_Output$Plots_TimeSeries$`247`
        # dfTest_Output$Plots_TimeSeries$`247`
        # 
        # dfTrain_Output$RMSEStepChange_HighLevel
        # dfTrain_Output$RMSEStepChange_HighLevel_Plot
        # dfTrain_Output$Plots_Residual$`247`
        # dfTrain_Output$AccHighLevel
        # 
        # dfTest_Output$RMSEStepChange_HighLevel
        # dfTest_Output$RMSEStepChange_HighLevel_Plot
        # dfTest_Output$Plots_Residual$`247`
        # dfTest_Output$AccHighLevel
        
        
OverallAcc <-
        bind_rows(
          dfTest_Output$AccHighLevel %>% mutate(Data = "Test")
          ,dfTrain_Output$AccHighLevel %>% mutate(Data = "Train")
        ) %>%
        select(
          Data
          ,MAE,MBE,RMSE
          ,MAEPerc,MBEPerc,RMSEPerc
        ) %>% knitr::kable()

     
OverallRMSEStepChange <-
        dfTrain_Output$RMSEStepChange_HighLevel %>% rename(RMSE_Train = RMSE) %>% select(DecompositionName,RMSE_Train) %>%
        left_join(
          dfTest_Output$RMSEStepChange_HighLevel %>% rename(RMSE_Test = RMSE) %>% select(DecompositionName,RMSE_Test)
          ,by="DecompositionName"
        ) %>% knitr::kable()
    
