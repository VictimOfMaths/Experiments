rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(readODS)
library(lubridate)
library(shiny)
library(mgcv)
library(coda.base)
library(MASS)
library(RcppRoll)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y=element_line(colour="grey95"))
}

#Read in data from HMRC Alcohol Bulletin https://www.gov.uk/government/statistics/alcohol-bulletin
#Start with older data from before August 2023
#National Archives website seems to have started blocking non-browser requests. 
#Old code here in case this changes

#temp <- tempfile()
#source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20240102181330mp_/https://assets.publishing.service.gov.uk/media/656718e4d6ad75000d02fc7d/Alc_Tabs_Oct_23.ods"
#temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in receipts data by product type
#raw.wine <- read_ods(temp, sheet="Pre_Aug23_Wine_Tables", range="J62:K353", 
#                     col_names=FALSE) %>% 
#  set_names("Wine", "AllAlcohol")

#raw.spirits <- read_ods(temp, sheet="Pre_Aug23_Spirits_Tables", range="J61:K352", 
#                        col_names=FALSE) %>% 
#  set_names("Spirits", "AllAlcohol") 

#raw.beer <- read_ods(temp, sheet="Pre_Aug23_Beer_And_Cider_Tables", range="K62:L353", 
#                     col_names=FALSE) %>% 
#  set_names("Beer", "Cider")

#Need instead to manually download file from the url above and then read it in here
filepath <- "C:/data projects/colin_misc/Data/Alc_Tabs_Oct_23.ods"

raw.wine <- read_ods(filepath, sheet="Pre_Aug23_Wine_Tables", range="J62:K353", 
                     col_names=FALSE) %>% 
  set_names("Wine", "AllAlcohol")

raw.spirits <- read_ods(filepath, sheet="Pre_Aug23_Spirits_Tables", range="J61:K352", 
                        col_names=FALSE) %>% 
  set_names("Spirits", "AllAlcohol") 

raw.beer <- read_ods(filepath, sheet="Pre_Aug23_Beer_And_Cider_Tables", range="K62:L353", 
                     col_names=FALSE) %>% 
  set_names("Beer", "Cider")

old.data <- data.frame(Wine=raw.wine$Wine, Spirits=raw.spirits$Spirits, Beer=raw.beer$Beer,
                       Cider=raw.beer$Cider, Total=raw.wine$AllAlcohol) 

#Add in more recent data from the latest version of the alcohol duty bulletin
#https://www.gov.uk/government/statistics/alcohol-bulletin
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/67bf38f5750837d7604dbbc9/Alc_Tables_Jan25.ods"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

raw.wine.latest <- read_ods(temp, sheet="Wine", range="H20:I37", 
                            col_names=FALSE) %>% 
  set_names("Wine", "AllAlcohol")

raw.spirits.latest <- read_ods(temp, sheet="Spirits", range="J21:K38", 
                               col_names=FALSE) %>% 
  set_names("Spirits", "AllAlcohol") 

raw.beer.latest <- read_ods(temp, sheet="Beer", range="O23:O40", 
                            col_names=FALSE) %>% 
  set_names("Beer")

raw.cider.latest <- read_ods(temp, sheet="Cider", range="J21:J38", 
                             col_names=FALSE) %>% 
  set_names("Cider")

latest.data <- data.frame(Wine=raw.wine.latest$Wine, Spirits=raw.spirits.latest$Spirits, 
                          Beer=raw.beer.latest$Beer, Cider=raw.cider.latest$Cider, 
                          Total=raw.wine.latest$AllAlcohol) 

data <- bind_rows(old.data, latest.data) %>% 
  mutate(Index=1:nrow(.),
         Date=as.Date("1999-04-01")%m+% months(Index-1),
         Month=month(Date),
         BeerProp=Beer/Total, CiderProp=Cider/Total, WineProp=Wine/Total, SpiritsProp=Spirits/Total)


#Bring in RPI data
temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp)[-c(1:194),] 
colnames(RPIdata) <- c("Date", "index")

RPIdata <- RPIdata %>% 
  mutate(Date=as.Date(paste0(Date, " 1"), "%Y %b %d"),
         index=as.numeric(index),
         inflator=index[length(index)]/index)

finaldata <- merge(data, RPIdata) %>% 
  mutate(Total=Total*inflator)

#Some descriptive graphs of the data
agg_png("Outputs/HMRCReceiptsFigureA3.png", units="in", height=5, width=10, res=800)
finaldata %>% mutate(across(c(2:5), ~.x*inflator)) %>% 
  gather(Bev, Receipts, c(2:5)) %>% 
  group_by(Bev) %>% 
  mutate(ReceiptsRoll=roll_mean(Receipts, n=12, align="right", fill=NA)) %>% 
  ggplot(aes(x=Date, y=ReceiptsRoll, colour=Bev))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_date(name="", limits=c(as.Date("2010-01-01"), NA_Date_))+
  scale_y_continuous(name="Treasury receipts (£m)\n(rolling 12-month average adjusted for inflation)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()

dev.off()

#Plot unadjusted revenues
finaldata %>% 
  gather(Bev, Receipts, c(2:5)) %>% 
  group_by(Bev) %>% 
  mutate(ReceiptsRoll=roll_mean(Receipts, n=12, align="right", fill=NA)) %>% 
  ggplot(aes(x=Date, y=ReceiptsRoll, colour=Bev))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_date(name="", limits=c(as.Date("2010-01-01"), NA_Date_))+
  scale_y_continuous(name="Treasury receipts (£m)\n(rolling 12-month average adjusted for inflation)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()

#Pull out data on relative price changes by beverage type
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <-read.csv(temp) %>% 
  set_names(slice(., 1)) %>% 
  slice_tail(., n=nrow(.)-1) %>% 
  slice_tail(.,n=643-193) %>% 
  #Select indices we want D7CA, D7BT, D7DI, D7DH, D7DG
  dplyr::select(CDID, D7CA, D7BT, D7DI, D7DH, D7DG) %>% 
  set_names("Date", "Alcohol", "All goods and services", "Beer", "Wine", "Spirits") %>% 
  mutate(Date=as.Date(paste0(Date, " 1"), "%Y %b %d"))

agg_png("Outputs/HMRCReceiptsFigureA4.png", units="in", height=5, width=10, res=800)
CPIdata %>% filter(Date>=as.Date("2010-01-01")) %>% 
  gather(Drink, Index, c(2:6)) %>% 
  mutate(Index=as.numeric(Index)) %>% 
  #rebase
  group_by(Drink) %>% 
  mutate(Index=Index/Index[Date==min(Date)]) %>% 
  ungroup() %>% 
  filter(Drink!="Alcohol") %>% 
  mutate(Drink=factor(Drink, levels=c("All goods and services", "Beer", "Spirits", "Wine"))) %>% 
  ggplot(aes(x=Date, y=Index, colour=Drink))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Change since January 2010", trans="log", breaks=c(1, 1.2, 1.4),
                     labels=c("No change", "+20%", "+40%"))+
  scale_colour_manual(name="", values=c("black", "#F7AA14", "#0099D5", "#C70E7B"))+
  theme_custom()

dev.off()

#Fit models to estimate expected receipts using code developed by Jonas Schoeley to
#estimate excess mortality by cause 
#https://github.com/jschoeley/xcod/tree/main

#############################################################################
#Set up Jonas' XCOD code
# Misc ------------------------------------------------------------

# Ensure that the xcod object is sorted
EnsureSortedXCOD <- function (xcod_out) {
  xcod_out[order(xcod_out[['stratum']],
                 xcod_out[['origin_time']],
                 xcod_out[['seasonal_time']], decreasing = FALSE),]
}

# Return data frame of row-wise quantiles over columns of X
Rowquantiles <- function (X, prob, type = 4, na.rm = TRUE) {
  t(apply(X, 1, quantile, prob = prob, type = type, na.rm = na.rm, names = FALSE))
}

# Expected estimation ---------------------------------------------

#' Predict Expected Deaths By Cause
#'
#' @param df
#'   A data frame.
#' @param formula_total RHS of formula passed to mgcv::gam describing
#'   time series of total deaths. Character string.
#' @param formula_prop_dense RHS of formula passed to mgcv::gam
#'   describing time series of cause of death proportions for parts with
#'   no or very little zero-shares. Character string.
#' @param formula_prop_sparse RHS of formula passed to mgcv::gam
#'   describing time series of cause of death proportions for parts with
#'   all or many zero-shares. Character string.
#' @param cols_prop
#'   Character vector of column names giving the weekly death
#'   proportions by cause. Preface column names with a minus to declare
#'   them as "sparse" and to model declared proportions with the sparse
#'   model.
#' @param col_total
#'   Quoted column name for total deaths.
#' @param col_stratum 
#'   Quoted column name for stratum.
#' @param col_origin_time
#'   Quoted column name for numeric time since origin
#'   (e.g. months since Jan 2015).
#' @param col_seasonal_time
#'   Quoted column name for numeric seasonal time
#'   (e.g. months into year).
#' @param col_cvflag
#'   Quoted column name for cross-validation flag column. Column
#'   must be character with "training" for data used to fit the models
#'   and test for time points to make predictions for.
#' @param nsim
#'   Number of simulation draws. Default = 1000.
#' @param quantiles
#'   Numeric vector of quantiles to report for predicted distribution.
#' @param basis
#'   The basis for the coda transformation of the data. Can be "ilr"
#'   (default), "alr", or "cdp". See ?coda.base::coordinates.
XCOD <- function (
    df,
    formula_total = "origin_time + s(seasonal_time, bs = 'cp')",
    formula_prop_dense = "origin_time + s(seasonal_time, bs = 'cp')",
    formula_prop_sparse = "1",
    cols_prop, col_total, col_stratum = NULL, col_origin_time,
    col_seasonal_time, col_cvflag,
    nsim = 100, basis = 'ilr'
) {
  
  ## preparation -------------------------------------------------------
  
  require(mgcv)      # for gam()
  require(coda.base) # for compositional data analysis operations
  
  N = nrow(df)
  
  # parse part specification
  idx_cols_prop_sparse = grepl('^-', cols_prop)
  cols_prop_sparse = substr(cols_prop[idx_cols_prop_sparse], 2, 1000000L)
  cols_prop_dense = cols_prop[!idx_cols_prop_sparse]
  cols_prop_all = c(cols_prop_dense, cols_prop_sparse)
  # number of parts which are "sparse", i.e. mostly 0
  p_sparse = length(cols_prop_sparse)
  # number of parts which are not sparse
  p_dense = length(cols_prop_dense)
  # total number of parts
  p = p_sparse + p_dense
  
  # mark training data
  idx_train <- which(df[,col_cvflag] == 'training')
  N_train <- length(idx_train)
  
  # prepare prediction data
  # predict over whole data...
  if (is.null(col_stratum)) {
    vec_stratum <- rep('All', N)
    df_prediction <- df[,c(
      col_origin_time, col_seasonal_time, col_cvflag,
      col_total, cols_prop_all
    )]
    df_prediction <- cbind(vec_stratum, df_prediction)
  } else {
    vec_stratum <- df[,col_stratum]
    df_prediction <- df[,c(
      col_stratum, col_origin_time, col_seasonal_time, col_cvflag,
      col_total, cols_prop_all
    )]
  }
  # standardize names
  colnames(df_prediction) <-
    c('stratum', 'origin_time', 'seasonal_time',
      'cv_flag', 'OBS_ALLCAUSE', cols_prop_all)
  # ...but train over this part of input data:
  df_training <- df_prediction[idx_train,]
  
  ## model expected total deaths over time -----------------------------
  
  deathsTotal_form <- as.formula(paste0(
    "OBS_ALLCAUSE~",
    formula_total
  ))
  deathsTotal_fit <- gam(
    deathsTotal_form,
    data = df_training,
    family = 'poisson'
  )
  
  #Additional line to extract fit statistics of GAM model
  #gam.check(deathsTotal_fit)
  #plot(deathsTotal_fit,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
  
  # predict mean expectected total deaths over time
  # design matrix
  deathsTotal_Xprd <-
    predict(deathsTotal_fit, newdata = df_prediction, type = 'lpmatrix')
  # coefficients
  deathsTotal_beta <- coef(deathsTotal_fit)
  # mean lambda
  deathsTotal_lambda <- exp(deathsTotal_Xprd%*%deathsTotal_beta)
  
  ## model expected proportions of deaths by cause over time -----------
  
  # predict proportions for sparse parts
  sparse <- list()
  if (p_sparse > 0) {
    # matrix holding training proportions of sparse parts
    sparse$P <- as.matrix(df_training[,cols_prop_sparse])
    sparse$props_form <- as.formula(paste0("p~", formula_prop_sparse))
    # matrix holding predicted proportions of sparse parts
    sparse$P_hat <- matrix(NA, nrow = N, ncol = p_sparse)
    
    for (k in 1:(p_sparse)) {
      sparse$the_data <- cbind(
        df_training[,c('stratum', 'origin_time', 'seasonal_time')],
        p = sparse$P[,k]
      )
      sparse$prop_gam_fit <-
        gam(sparse$props_form, family = gaussian(link = 'identity'),
            data = sparse$the_data)
      # predicted proportions from fitted model
      sparse$prop_gam_Xprd <-
        predict(sparse$prop_gam_fit, newdata = df_prediction,
                type = 'lpmatrix')
      sparse$prop_gam_beta <- coef(sparse$prop_gam_fit)
      sparse$P_hat[,k] <- sparse$prop_gam_Xprd%*%sparse$prop_gam_beta
    }
    
    # clip negative predictions to 0
    sparse$P_hat[sparse$P_hat < 0] <- 0
    # share of sparse parts on all parts
    sparse$share_on_all <- rowSums(sparse$P_hat)
  }
  
  # predict proportions for dense parts
  dense <- list()
  if (p_dense > 0) {
    
    # matrix holding training proportions of dense parts
    dense$P <- as.matrix(df_training[,cols_prop_dense])
    # transform proportions to log-ratio analysis space
    dense$Plr <- coordinates(dense$P, basis = basis)
    
    # matrix holding predicted proportions of dense parts in
    # log-ratio space
    dense$Plr_hat <- matrix(NA, nrow = N, ncol = p_dense-1)
    
    # model and extrapolate expected proportions in log-ratio space
    # separate by cause...
    dense$props_form <- as.formula(paste0("plr~", formula_prop_dense))
    for (k in 1:(p_dense-1)) {
      dense$the_data <- cbind(df_training, plr = dense$Plr[,k])
      # zero proportions are excluded from fitting
      dense$zero_props <- is.infinite(dense$the_data$plr)
      dense$the_data <- dense$the_data[!dense$zero_props,]
      dense$prop_gam_fit <- gam(dense$props_form,
                                family = gaussian(link = 'identity'),
                                data = dense$the_data)
      # predicted proportions from fitted model
      dense$prop_gam_Xprd <-
        predict(dense$prop_gam_fit, newdata = df_prediction,
                type = 'lpmatrix')
      dense$prop_gam_beta <- coef(dense$prop_gam_fit)
      dense$prop_gam_Xprd%*%dense$prop_gam_beta
      dense$Plr_hat[,k] <- dense$prop_gam_Xprd%*%dense$prop_gam_beta
    }
  }
  
  # assemble sparse and dense estimated proportions
  P_hat <- matrix(NA, nrow = N, ncol = p)
  # if there are sparse parts of the expected composition, write those
  # into the P_hat composition array. Use the number of dense parts as
  # offset for the index as the dense parts should come first.
  if (p_sparse > 0) {
    for (k in 1:p_sparse) { P_hat[,(p_dense+k)] <- sparse$P_hat[,k] }
  }
  # if there are dense parts of the expected composition, write those
  # into the P_hat composition array, after converting them from
  # compositional to proportion space.
  if (p_dense > 0) {
    dense$P_hat_j <- composition(dense$Plr_hat, basis = basis)
    for (k in 1:p_dense) { P_hat[,k] <- dense$P_hat_j[,k] }
  }
  # if the dense parts form a sub-composition, re-scale this
  # sub-composition to its share on the total composition
  if (p_dense > 0 & p_sparse > 0) {
    P_hat[,1:p_dense] <- P_hat[,1:p_dense]*(1-sparse$share_on_all)
  }
  
  ## simulate expected deaths by cause ---------------------------------
  
  # observed deaths by cause
  # rounded to mode of corresponding poisson distribution
  Dk_obs <-
    floor(df_prediction[,cols_prop_all]*df_prediction[['OBS_ALLCAUSE']])
  
  # expected deaths by cause (mean + simulations)
  Dk_hat <- array(NA, dim = list(i = N, j = nsim+1, p = p))
  
  # calibrate model
  # learn the covariance matrix of the log relative errors from
  # the training data across causes of death
  # re-sample residuals from that distribution
  {
    # calculate log residuals over training data for dense parts
    logerror_obs <- matrix(NA, nrow = N_train, ncol = p_dense)
    for (k in 1:p_dense) {
      Dk_obs_k <- unlist(Dk_obs[,k])
      Dk_hat_avg_k <- Dk_hat[,1,k] <- P_hat[,k]*deathsTotal_lambda
      logerror_obs[,k] <-
        log(Dk_obs_k[idx_train]) - log(Dk_hat_avg_k[idx_train])
      logerror_obs[,k] <- ifelse(
        Dk_obs_k[idx_train] == 0 | Dk_hat_avg_k[idx_train] == 0,
        0, logerror_obs[,k])
    }
    # calculate covariance matrix over log-residuals across dense parts
    cov_logerror_obs <- cov(logerror_obs)
    avg_logerror_k <- colMeans(logerror_obs)
    
    # simulate log residuals based on estimated covariance matrix
    # and derive simulated deaths
    logerror_sim <- array(NA, dim = c(N, nsim, p_dense))
    for (j in 1:nsim) {
      logerror_sim[,j,] <- MASS::mvrnorm(n = N, mu = avg_logerror_k,
                                         Sigma = cov_logerror_obs)
      for (k in 1:p_dense) {
        Dk_hat[,j+1,k] <- Dk_hat[,1,k]*exp(logerror_sim[,j,k])
      }
    }
    
    # calibration for sparse parts
    if (p_sparse > 0) {
      for (k in 1:p_sparse + p_dense) {
        Dk_hat[,1,k] <- P_hat[,k]*deathsTotal_lambda
        Dk_hat[,-1,k] <- rpois(n = N*nsim, lambda = Dk_hat[,1,k])
      }      
    }
    
  }
  
  ## Assemble output data ----------------------------------------------
  
  # bind cause-specific predictions and simulations to input data
  for (k in 1:p) {
    X <- cbind(
      # observed
      Dk_obs[,k],
      # expected average & simulated
      Dk_hat[,,k]
    )
    colnames(X) <- c(
      # observed
      paste0('OBS_', cols_prop_all[k]),
      # expected average & simulated
      paste0('XPC_AVG_', cols_prop_all[k]),
      paste0('XPC_SIM', 1:nsim, '_', cols_prop_all[k])
    )
    df_prediction <- cbind(df_prediction, X)
  }
  # sum cause-specific predictions and simulations to totals and
  # bind to input data
  X <- cbind(
    # expected average & simulated
    apply(Dk_hat, 1:2, sum)
  )
  colnames(X) <- c(
    # expected average & simulated
    paste0('XPC_AVG_ALLCAUSE'),
    paste0('XPC_SIM', 1:nsim, '_ALLCAUSE')
  )
  df_prediction <- cbind(df_prediction, X)
  
  attr(df_prediction, 'logerror_cov') <- cov_logerror_obs
  attr(df_prediction, 'p_sparse') <- p_sparse
  attr(df_prediction, 'p_dense') <- p_dense
  attr(df_prediction, 'colnames_sparse') <- cols_prop_sparse
  attr(df_prediction, 'colnames_dense') <- cols_prop_dense
  attr(df_prediction, 'n_samples') <- nsim
  
  return(df_prediction)
}

# Excess derivation -----------------------------------------------

GetExcessByCause <- function (
    xcod_out, name_parts,
    measure = 'absolute',
    quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    cumulative = FALSE, origin_time_start_of_cumulation = 0
) {
  
  # forgiveness please, this info should really be explicit in xcod_out
  nsim = max(as.integer(
    sub('(^.+_SIM)([[:digit:]]+)(.*$)','\\2',
        grep('[[:digit:]]',names(xcod_out), value = TRUE))
  ))
  nrow = NROW(xcod_out)
  xcod_sorted <- EnsureSortedXCOD(xcod_out)
  
  # data columns
  Y <- xcod_sorted[,grepl('XPC_|OBS_',colnames(xcod_sorted))]
  # label columns
  X <- xcod_sorted[,c('stratum', 'origin_time', 'seasonal_time', 'cv_flag')]
  
  # aggregate parts if requested
  if (is.list(name_parts)) {
    amalgamation_names <- names(name_parts)
    amalgamation_parts <- name_parts
    Y <- Map(function (x, y) {
      name <- x
      parts <- unlist(y)
      n_parts <- length(parts)
      
      # OBS
      OBS_colnames <- grep(paste0('OBS_', parts, collapse = '|'),
                           colnames(Y), value = TRUE)
      OBS_parts <- matrix(unlist(Y[,OBS_colnames]),
                          nrow = nrow, ncol = n_parts)
      OBS_amalgamation <- rowSums(OBS_parts)
      # XPC AVG
      XPC_AVG_colnames <- grep(paste0('XPC_AVG_', parts, collapse = '|'),
                               colnames(Y), value = TRUE)
      XPC_AVG_parts <- matrix(unlist(Y[,XPC_AVG_colnames]),
                              nrow = nrow, ncol = n_parts)
      XPC_AVG_amalgamation <- rowSums(XPC_AVG_parts)
      # XPC SIM
      XPC_SIM_colnames <- grep(paste0('XPC_SIM.+_', parts, collapse = '|'),
                               colnames(Y), value = TRUE)
      XPC_SIM_parts <- array(
        unlist(Y[,XPC_SIM_colnames]),
        dim = c(nrow, nsim, n_parts)
      )
      XPC_SIM_amalgamation <- apply(XPC_SIM_parts, 1:2, sum)
      # XCOD amalgamation
      xcod_amalgamation <- cbind(
        OBS_amalgamation, XPC_AVG_amalgamation, XPC_SIM_amalgamation
      )
      colnames(xcod_amalgamation) <-
        c(paste0(c('OBS_', 'XPC_AVG_'), name),
          paste0('XPC_SIM', 1:nsim, '_', name))
      
      return(xcod_amalgamation)
    },
    amalgamation_names, amalgamation_parts)
    
    # reorder columns
    Y <- do.call('cbind', Y)
    Y <- Y[,c(which(grepl('OBS',colnames(Y))),
              which(grepl('XPC_AVG',colnames(Y))),
              which(grepl('XPC_SIM',colnames(Y))))]
  } else {
    amalgamation_names <- name_parts
  }
  
  # accumulate data columns if requested
  if (isTRUE(cumulative)) {
    # set data to 0 for time points prior to accumulation start
    # so that we only accumulate from the accumulation start
    vec_timeselect <- X[['origin_time']] < origin_time_start_of_cumulation
    Y[vec_timeselect,] <- 0
    # this restarts the accumulation of a data vector whenever the
    # stratum vector changes in value
    vec_stratum <- X[['stratum']]
    Y <- apply(Y, 2, function (x) ave(x, vec_stratum, FUN = cumsum))
    # set values to NA prior to accumulation start so that derived
    # values become NA as well
    Y[vec_timeselect,] <- NA
  }
  
  for (part in amalgamation_names) {
    OBS <- Y[,paste0('OBS_', part)]
    j <- grepl(paste0('^XPC_SIM[[:digit:]]+_',part,'$'), colnames(Y))
    if (identical(measure, 'observed')) {
      MEASURE <- as.matrix(OBS)
    }
    if (identical(measure, 'expected')) {
      MEASURE <- Y[,j]
    }
    if (identical(measure, 'absolute')) {
      MEASURE <- apply(Y[,j], 2, function (XPC_SIM) {round(OBS-XPC_SIM,0)})
    }
    if (identical(measure, 'pscore')) {
      MEASURE <- apply(Y[,j], 2, function (XPC_SIM) {(OBS-XPC_SIM)/XPC_SIM*100})
    }
    if (identical(measure, 'ratio')) {
      MEASURE <- apply(Y[,j], 2, function (XPC_SIM) {OBS/XPC_SIM})
    }
    # get quantiles
    Q <- Rowquantiles(MEASURE, quantiles, type = 1)
    colnames(Q) <-
      paste0('Q', substr(
        formatC(quantiles, format = 'f', digits = 3),
        start = 3, stop = 5
      ), '_', part)
    X <- cbind(X,Q)
  }
  return(X)
}
################################################################

Model <- XCOD(df=finaldata %>% dplyr::select("Date", "Index", "Month", "BeerProp", "CiderProp", "SpiritsProp", "WineProp", "Total") %>%
                filter(Date>=as.Date("2010-01-01")) %>% 
                mutate(cv_flag=if_else(Date<=as.Date("2019-12-01"), "training", "test")),
              formula_total = "origin_time + as.factor(seasonal_time)",
              formula_prop_dense = "origin_time + as.factor(seasonal_time)",
              formula_prop_sparse="1",
              cols_prop=c("BeerProp", "CiderProp", "WineProp", "SpiritsProp"),
              col_total="Total",
              col_origin_time = 'Index',
              col_seasonal_time = 'Month',
              col_cvflag = 'cv_flag',
              nsim = 100,
              basis = 'ilr')

# Aggregate observed and expected --------------------

library(data.table)

Model <- as.data.table(Model)

strata_cols <- c("stratum", 'origin_time', 'seasonal_time', 'cv_flag')
value_cols <-
  c(
    grep('XPC_|OBS_', names(Model), value = TRUE)
  )

total <-
  groupingsets(
    Model,
    j = lapply(.SD, sum),
    by = strata_cols,
    sets = list(
      c('origin_time', 'seasonal_time', 'cv_flag')
    ),
    .SDcols = value_cols
  ) %>%
  mutate(stratum = 'Total') %>%
  as.data.frame()


#Visualise Total observed and modelled
ObsvsExp <- bind_rows(
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total,
    # parts of interest
    name_parts = c('ALLCAUSE'),
    # what measure to return
    measure = 'observed',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Observed"),
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total,
    # parts of interest
    name_parts = c('ALLCAUSE'),
    # what measure to return
    measure = 'expected',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Expected"))

agg_png("Outputs/HMRCReceiptsFigureA1.png", units="in", width=8, height=5, res=800)
ggplot(ObsvsExp %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)), 
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_vline(xintercept=as.Date("2020-01-01"), colour="black", linetype=2)+
  #geom_rect(xmin=as.Date("2019-12-16"), xmax=as.Date("2024-06-30"), ymin=5, ymax=2200, fill="grey95", colour="grey95")+
  geom_point(data=. %>% filter(Metric=="Observed"), shape=21, colour="black", fill="transparent")+
  geom_ribbon(data=. %>% filter(Metric=="Expected"), aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), fill="red", alpha=0.3)+
  geom_line(data=. %>% filter(Metric=="Expected"), colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Total monthly duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=as.Date("2015-01-01"), y=2350, label="Training period", 
           family="Lato", fontface="bold")+
  annotate("text", x=as.Date("2022-01-01"), y=2350, label="Forecast period", 
           family="Lato", fontface="bold")

dev.off()

#Extract model fit statistics
ObsvsExp %>% filter(cv_flag=="training") %>% 
  pivot_wider(id_cols=origin_time, names_from=Metric,
              values_from=c(Q025_ALLCAUSE, Q500_ALLCAUSE, Q975_ALLCAUSE)) %>% 
  mutate(Covered=if_else(Q500_ALLCAUSE_Observed>Q025_ALLCAUSE_Expected &
                           Q500_ALLCAUSE_Observed<=Q975_ALLCAUSE_Expected,1,0),
         AbsErr=abs(Q500_ALLCAUSE_Observed-Q500_ALLCAUSE_Expected),
         AbsPerc=AbsErr/Q500_ALLCAUSE_Expected) %>% 
  summarise(Coverage=mean(Covered),
            MAPE=mean(AbsPerc))

# Monthly deviation from expected
MonthDev <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('ALLCAUSE'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = FALSE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)

agg_png("Outputs/HMRCReceiptsFigure1Clean.png", units="in", width=11, height=5, res=800)
ggplot(MonthDev %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(fill="royalblue")+
  geom_errorbar(aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Deviation from expected duty receipts (£m)")+
  theme_custom()

dev.off()

agg_png("Outputs/HMRCReceiptsFigure1Annotated.png", units="in", width=11, height=5, res=800)
ggplot(MonthDev %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(fill="royalblue")+
  geom_errorbar(aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Deviation from expected duty receipts (£m)")+
  theme_custom()+
  annotate("text", x=as.Date("2020-11-01"), y=-600, label="Strict COVID-19\nrestrictions",
           family="Lato", size=rel(3.5), colour="grey40")+
  annotate("text", x=as.Date("2021-02-01"), y=400, label="Restrictions\nrelaxed",
           family="Lato", size=rel(3.5), colour="grey40")+
  annotate("text", x=as.Date("2024-01-01"), y=250, label="Duty\nreforms",
           family="Lato", size=rel(3.5), colour="grey40")+
  geom_curve(aes(x=as.Date("2020-10-10"), y=-520, xend=as.Date("2020-05-01"), 
                 yend=-200), arrow=arrow(length = unit(0.03, "npc"), type="closed"), 
             curvature=0.2, colour="grey60")+
  geom_curve(aes(x=as.Date("2020-11-10"), y=-520, xend=as.Date("2021-02-01"), 
                 yend=-310), arrow = arrow(length = unit(0.03, "npc"), type="closed"), 
             curvature=0.3, colour="grey60")+
  geom_curve(aes(x=as.Date("2021-04-15"), y=415, xend=as.Date("2021-07-15"), 
                 yend=270), arrow=arrow(length = unit(0.03, "npc"), type="closed"), 
             curvature=-0.4, colour="grey60")+
  geom_curve(aes(x=as.Date("2020-11-25"), y=415, xend=as.Date("2020-09-01"), 
                 yend=320), arrow = arrow(length = unit(0.03, "npc"), type="closed"), 
             curvature=0.2, colour="grey60")+
  geom_curve(aes(x=as.Date("2023-11-15"), y=220, xend=as.Date("2023-08-25"), 
                 yend=20), arrow = arrow(length = unit(0.03, "npc"), type="closed"), 
             curvature=0.2, colour="grey60")

dev.off()

#Cumulative deviation from expected since Jan 2020
CumDev <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('ALLCAUSE'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = TRUE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)

agg_png("Outputs/HMRCReceiptsFigure2.png", units="in", width=11, height=5, res=800)
ggplot(CumDev %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), fill="red", alpha=0.3)+
  geom_line(colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Cumulative deviation from expected duty receipts (£m)")+
  theme_custom()

dev.off()

#Repeat by drink type
ObsvsExp2 <- bind_rows(
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total,
    # parts of interest
    name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
    # what measure to return
    measure = 'observed',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Observed"),
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total,
    # parts of interest
    name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
    # what measure to return
    measure = 'expected',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Expected")) %>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

agg_png("Outputs/HMRCReceiptsFigureA2.png", units="in", width=10, height=6, res=600)
ggplot(ObsvsExp2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)), 
       aes(x=Date))+
  #geom_rect(xmin=as.Date("2019-12-16"), xmax=as.Date("2024-06-30"), ymin=300, ymax=2200, fill="grey95", colour="grey95")+
  geom_vline(xintercept=as.Date("2020-01-01"), colour="black", linetype=2)+
  geom_point(data=. %>% filter(Metric=="Observed" & Quintile=="Q500"), aes(y=Value), shape=21, colour="black", fill="transparent")+
  geom_ribbon(data=. %>% filter(Metric=="Expected") %>% 
                spread(Quintile, Value), aes(ymin=Q025, ymax=Q975), fill="red", alpha=0.3)+
  geom_line(data=. %>% filter(Metric=="Expected" & Quintile=="Q500"), aes(y=Value),colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Total monthly duty receipts (£m)")+
  facet_wrap(~Beverage, scales="free_y")+
  theme_custom()

dev.off()

#Extract model fit statistics
ObsvsExp2 %>% filter(cv_flag=="training") %>% 
  pivot_wider(id_cols=c(origin_time, Beverage), names_from=c(Metric, Quintile),
              values_from=Value) %>% 
  mutate(Covered=if_else(Observed_Q500>=Expected_Q025 &
                           Observed_Q500<=Expected_Q975,1,0)) %>% 
  group_by(Beverage) %>% 
  summarise(Coverage=mean(Covered))

# Monthly deviation from expected
MonthDev2 <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = FALSE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)%>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

ggplot(MonthDev2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01") & Quintile=="Q500"),
       aes(x=Date, y=Value, fill=Beverage))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Variation from expected duty receipts (£m)")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  theme_custom()

agg_png("Outputs/HMRCReceiptsFigure3.png", units="in", width=9, height=6, res=800)
ggplot(MonthDev2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(data=. %>% filter(Quintile=="Q500"),
           aes(x=Date, y=Value, fill=Beverage), position="stack", show.legend=FALSE)+
  geom_errorbar(data=. %>% spread(Quintile, Value),
                aes(x=Date, ymin=Q025, ymax=Q975), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Deviation from expected duty receipts (£m)")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  facet_wrap(~Beverage)+
  theme_custom()

dev.off()

#Cumulative deviation from expected since Jan 2020
CumDev2 <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = TRUE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)%>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

agg_png("Outputs/HMRCReceiptsFigure4.png", units="in", width=8, height=5, res=800)
ggplot(CumDev2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, colour=Beverage))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(data=. %>%  spread(Quintile, Value), 
              aes(ymin=Q025, ymax=Q975, fill=Beverage), alpha=0.3, colour="transparent")+
  geom_line(data=. %>% filter(Quintile=="Q500"), aes(y=Value))+
  scale_x_date(name="")+
  scale_y_continuous(name="Cumulative deviation from expected duty receipts (£m)")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  theme_custom()

dev.off()

#Get p-scores for appendix only
MonthPScoreTotal <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('ALLCAUSE'),
  measure = 'pscore',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = FALSE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)

agg_png("Outputs/HMRCReceiptsFigure1REL.png", units="in", width=11, height=5, res=800)
ggplot(MonthPScoreTotal %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE/100))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(fill="royalblue")+
  geom_errorbar(aes(ymin=Q025_ALLCAUSE/100, ymax=Q975_ALLCAUSE/100), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Relative deviation from expected duty receipts", 
                     label=label_percent(accuracy=1))+
  theme_custom()

dev.off()

CumPScoreTotal <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('ALLCAUSE'),
  measure = 'pscore',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = TRUE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)

agg_png("Outputs/HMRCReceiptsFigure2REL.png", units="in", width=11, height=5, res=800)
ggplot(CumPScoreTotal %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE/100))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=Q025_ALLCAUSE/100, ymax=Q975_ALLCAUSE/100), fill="red", alpha=0.3)+
  geom_line(colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Cumulative deviation from expected duty receipts", 
                     label=label_percent(accuracy=1))+
  theme_custom()

dev.off()

MonthPScoreTotal2 <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
  measure = 'pscore',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = FALSE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)%>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

agg_png("Outputs/HMRCReceiptsFigure3REL.png", units="in", width=9, height=6, res=800)
ggplot(MonthPScoreTotal2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(data=. %>% filter(Quintile=="Q500"),
           aes(x=Date, y=Value/100, fill=Beverage), position="stack", show.legend=FALSE)+
  geom_errorbar(data=. %>% spread(Quintile, Value),
                aes(x=Date, ymin=Q025/100, ymax=Q975/100), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Relative deviation from expected duty receipts", 
                     label=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  facet_wrap(~Beverage)+
  theme_custom()

dev.off()

CumPScoreTotal2 <- GetExcessByCause(
  xcod_out = total,
  name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
  measure = 'pscore',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = TRUE,
  # accumulation starts here
  origin_time_start_of_cumulation = 250
)%>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

agg_png("Outputs/HMRCReceiptsFigure4REL.png", units="in", width=8, height=5, res=800)
ggplot(CumPScoreTotal2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2020-01-01")),
       aes(x=Date, colour=Beverage))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(data=. %>%  spread(Quintile, Value), 
              aes(ymin=Q025/100, ymax=Q975/100, fill=Beverage), alpha=0.3, colour="transparent")+
  geom_line(data=. %>% filter(Quintile=="Q500"), aes(y=Value/100))+
  scale_x_date(name="")+
  scale_y_continuous(name="Cumulative deviation from expected duty receipts", 
                     label=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  theme_custom()

dev.off()

############################################
#Sensitivity analysis where we use Jan 2018 as the intervention point instead

Model2 <- XCOD(df=finaldata %>% dplyr::select("Date", "Index", "Month", "BeerProp", "CiderProp", "SpiritsProp", "WineProp", "Total") %>%
                 filter(Date>=as.Date("2010-01-01")) %>% 
                 mutate(cv_flag=if_else(Date<=as.Date("2017-12-01"), "training", "test")),
               formula_total = "origin_time + as.factor(seasonal_time)",
               formula_prop_dense = "origin_time + as.factor(seasonal_time)",
               formula_prop_sparse="1",
               cols_prop=c("BeerProp", "CiderProp", "WineProp", "SpiritsProp"),
               col_total="Total",
               col_origin_time = 'Index',
               col_seasonal_time = 'Month',
               col_cvflag = 'cv_flag',
               nsim = 100,
               basis = 'ilr')

# Aggregate observed and expected --------------------


Model2 <- as.data.table(Model2)

strata_cols <- c("stratum", 'origin_time', 'seasonal_time', 'cv_flag')
value_cols <-
  c(
    grep('XPC_|OBS_', names(Model), value = TRUE)
  )

total2 <-
  groupingsets(
    Model2,
    j = lapply(.SD, sum),
    by = strata_cols,
    sets = list(
      c('origin_time', 'seasonal_time', 'cv_flag')
    ),
    .SDcols = value_cols
  ) %>%
  mutate(stratum = 'Total') %>%
  as.data.frame()


#Visualise Total observed and modelled
ObsvsExpSA <- bind_rows(
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total2,
    # parts of interest
    name_parts = c('ALLCAUSE'),
    # what measure to return
    measure = 'observed',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Observed"),
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total2,
    # parts of interest
    name_parts = c('ALLCAUSE'),
    # what measure to return
    measure = 'expected',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Expected"))

agg_png("Outputs/HMRCReceiptsFigureA1SA.png", units="in", width=8, height=5, res=800)
ggplot(ObsvsExpSA %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)), 
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  #geom_rect(xmin=as.Date("2017-12-16"), xmax=as.Date("2024-06-30"), ymin=5, ymax=2200, fill="grey95", colour="grey95")+
  geom_vline(xintercept=as.Date("2018-01-01"), colour="black", linetype=2)+
  geom_point(data=. %>% filter(Metric=="Observed"), shape=21, colour="black", fill="transparent")+
  geom_ribbon(data=. %>% filter(Metric=="Expected"), aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), fill="red", alpha=0.3)+
  geom_line(data=. %>% filter(Metric=="Expected"), colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Total monthly duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=as.Date("2013-01-01"), y=2350, label="Training period", 
           family="Lato", fontface="bold")+
  annotate("text", x=as.Date("2021-06-01"), y=2350, label="Forecast period", 
           family="Lato", fontface="bold")

dev.off()

# Monthly deviation from expected
MonthDevSA <- GetExcessByCause(
  xcod_out = total2,
  name_parts = c('ALLCAUSE'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = FALSE,
  # accumulation starts here
  origin_time_start_of_cumulation = 226
)

agg_png("Outputs/HMRCReceiptsFigure1SA.png", units="in", width=11, height=5, res=800)
ggplot(MonthDevSA %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2018-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(fill="royalblue")+
  geom_errorbar(aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Deviation from expected duty receipts (£m)")+
  theme_custom()

dev.off()

#Cumulative deviation from expected since Jan 2020
CumDevSA <- GetExcessByCause(
  xcod_out = total2,
  name_parts = c('ALLCAUSE'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = TRUE,
  # accumulation starts here
  origin_time_start_of_cumulation = 226
)

agg_png("Outputs/HMRCReceiptsFigure2SA.png", units="in", width=11, height=5, res=800)
ggplot(CumDevSA %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2018-01-01")),
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), fill="red", alpha=0.3)+
  geom_line(colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Cumulative deviation from expected duty receipts (£m)")+
  theme_custom()

dev.off()

#Repeat by drink type
ObsvsExpSA2 <- bind_rows(
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total2,
    # parts of interest
    name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
    # what measure to return
    measure = 'observed',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Observed"),
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total2,
    # parts of interest
    name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
    # what measure to return
    measure = 'expected',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Expected")) %>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

agg_png("Outputs/HMRCReceiptsFigureA2SA.png", units="in", width=10, height=6, res=600)
ggplot(ObsvsExpSA2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)), 
       aes(x=Date))+
  #geom_rect(xmin=as.Date("2019-12-16"), xmax=as.Date("2024-06-30"), ymin=300, ymax=2200, fill="grey95", colour="grey95")+
  geom_vline(xintercept=as.Date("2018-01-01"), colour="black", linetype=2)+
  geom_point(data=. %>% filter(Metric=="Observed" & Quintile=="Q500"), aes(y=Value), shape=21, colour="black", fill="transparent")+
  geom_ribbon(data=. %>% filter(Metric=="Expected") %>% 
                spread(Quintile, Value), aes(ymin=Q025, ymax=Q975), fill="red", alpha=0.3)+
  geom_line(data=. %>% filter(Metric=="Expected" & Quintile=="Q500"), aes(y=Value),colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Total monthly duty receipts (£m)")+
  facet_wrap(~Beverage, scales="free_y")+
  theme_custom()

dev.off()

# Monthly deviation from expected
MonthDevSA2 <- GetExcessByCause(
  xcod_out = total2,
  name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = FALSE,
  # accumulation starts here
  origin_time_start_of_cumulation = 226
)%>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

ggplot(MonthDevSA2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2018-01-01") & Quintile=="Q500"),
       aes(x=Date, y=Value, fill=Beverage))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Deviation from expected duty receipts (£m)")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  theme_custom()

agg_png("Outputs/HMRCReceiptsFigure3SA.png", units="in", width=9, height=6, res=600)
ggplot(MonthDevSA2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2018-01-01")))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_col(data=. %>% filter(Quintile=="Q500"),
           aes(x=Date, y=Value, fill=Beverage), position="stack", show.legend=FALSE)+
  geom_errorbar(data=. %>% spread(Quintile, Value),
                aes(x=Date, ymin=Q025, ymax=Q975), width=0.25, alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Deviation from expected duty receipts (£m)")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  facet_wrap(~Beverage)+
  theme_custom()

dev.off()

#Cumulative deviation from expected since Jan 2020
CumDevSA2 <- GetExcessByCause(
  xcod_out = total2,
  name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
  measure = 'absolute',
  quantiles = c(0.025, 0.5, 0.975),
  # cumulative measures
  cumulative = TRUE,
  # accumulation starts here
  origin_time_start_of_cumulation = 226
)%>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

agg_png("Outputs/HMRCReceiptsFigure4SA.png", units="in", width=8, height=5, res=800)
ggplot(CumDevSA2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)) %>% 
         filter(Date>=as.Date("2018-01-01")),
       aes(x=Date, colour=Beverage))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_ribbon(data=. %>%  spread(Quintile, Value), 
              aes(ymin=Q025, ymax=Q975, fill=Beverage), alpha=0.3, colour="transparent")+
  geom_line(data=. %>% filter(Quintile=="Q500"), aes(y=Value))+
  scale_x_date(name="")+
  scale_y_continuous(name="Cmulative deviation from expected duty receipts (£m)")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  theme_custom()

dev.off()

#Mean deficits since Dec2022
MonthDev2 %>% filter(Quintile=="Q500" & origin_time>=285) %>% 
  group_by(Beverage) %>% 
  summarise(Mean=mean(Value))

MonthPScoreTotal2 %>% filter(Quintile=="Q500" & origin_time>=285) %>% 
  group_by(Beverage) %>% 
  summarise(Mean=mean(Value))

##############################################################
#Check prediction error under alternative model specification
Model3 <- XCOD(df=finaldata %>% dplyr::select("Date", "Index", "Month", "BeerProp", "CiderProp", "SpiritsProp", "WineProp", "Total") %>%
                filter(Date>=as.Date("2010-01-01")) %>% 
                mutate(cv_flag=if_else(Date<=as.Date("2019-12-01"), "training", "test"),
                       Index=Index^2),
              formula_total = "origin_time + as.factor(seasonal_time)",
              formula_prop_dense = "origin_time + as.factor(seasonal_time)",
              formula_prop_sparse="1",
              cols_prop=c("BeerProp", "CiderProp", "WineProp", "SpiritsProp"),
              col_total="Total",
              col_origin_time = 'Index',
              col_seasonal_time = 'Month',
              col_cvflag = 'cv_flag',
              nsim = 100,
              basis = 'ilr')

# Aggregate observed and expected --------------------

Model3 <- as.data.table(Model3)

strata_cols <- c("stratum", 'origin_time', 'seasonal_time', 'cv_flag')
value_cols <-
  c(
    grep('XPC_|OBS_', names(Model), value = TRUE)
  )

total3 <-
  groupingsets(
    Model3,
    j = lapply(.SD, sum),
    by = strata_cols,
    sets = list(
      c('origin_time', 'seasonal_time', 'cv_flag')
    ),
    .SDcols = value_cols
  ) %>%
  mutate(stratum = 'Total', origin_time=sqrt(origin_time)) %>%
  as.data.frame()


#Visualise Total observed and modelled
ObsvsExpQuad <- bind_rows(
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total3,
    # parts of interest
    name_parts = c('ALLCAUSE'),
    # what measure to return
    measure = 'observed',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Observed"),
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total3,
    # parts of interest
    name_parts = c('ALLCAUSE'),
    # what measure to return
    measure = 'expected',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Expected"))

ggplot(ObsvsExpQuad %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)), 
       aes(x=Date, y=Q500_ALLCAUSE))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_vline(xintercept=as.Date("2020-01-01"), colour="black", linetype=2)+
  #geom_rect(xmin=as.Date("2019-12-16"), xmax=as.Date("2024-06-30"), ymin=5, ymax=2200, fill="grey95", colour="grey95")+
  geom_point(data=. %>% filter(Metric=="Observed"), shape=21, colour="black", fill="transparent")+
  geom_ribbon(data=. %>% filter(Metric=="Expected"), aes(ymin=Q025_ALLCAUSE, ymax=Q975_ALLCAUSE), fill="red", alpha=0.3)+
  geom_line(data=. %>% filter(Metric=="Expected"), colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Total monthly duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=as.Date("2015-01-01"), y=2350, label="Training period", 
           family="Lato", fontface="bold")+
  annotate("text", x=as.Date("2022-01-01"), y=2350, label="Forecast period", 
           family="Lato", fontface="bold")

#Extract model fit statistics
ObsvsExpQuad %>% filter(cv_flag=="training") %>% 
  pivot_wider(id_cols=origin_time, names_from=Metric,
              values_from=c(Q025_ALLCAUSE, Q500_ALLCAUSE, Q975_ALLCAUSE)) %>% 
  mutate(Covered=if_else(Q500_ALLCAUSE_Observed>Q025_ALLCAUSE_Expected &
                           Q500_ALLCAUSE_Observed<=Q975_ALLCAUSE_Expected,1,0),
         AbsErr=abs(Q500_ALLCAUSE_Observed-Q500_ALLCAUSE_Expected),
         AbsPerc=AbsErr/Q500_ALLCAUSE_Expected) %>% 
  summarise(Coverage=mean(Covered),
            MAPE=mean(AbsPerc))

#Repeat by drink type
ObsvsExpQuad2 <- bind_rows(
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total3,
    # parts of interest
    name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
    # what measure to return
    measure = 'observed',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Observed"),
  GetExcessByCause(
    # output of XCOD function
    xcod_out = total3,
    # parts of interest
    name_parts = c('BeerProp', 'WineProp', 'CiderProp', 'SpiritsProp'),
    # what measure to return
    measure = 'expected',
    # quantiles of interest
    quantiles = c(0.025, 0.5, 0.975),
    cumulative = FALSE) %>% mutate(Metric="Expected")) %>% 
  pivot_longer(cols=c(Q025_BeerProp:Q975_SpiritsProp), names_to=c("Quintile", "Beverage"),
               names_sep="_", values_to="Value") %>% 
  mutate(Beverage=gsub("Prop", "", Beverage))

ggplot(ObsvsExpQuad2 %>% mutate(Date=as.Date("1999-04-01")+months(origin_time-1)), 
       aes(x=Date))+
  #geom_rect(xmin=as.Date("2019-12-16"), xmax=as.Date("2024-06-30"), ymin=300, ymax=2200, fill="grey95", colour="grey95")+
  geom_vline(xintercept=as.Date("2018-01-01"), colour="black", linetype=2)+
  geom_point(data=. %>% filter(Metric=="Observed" & Quintile=="Q500"), aes(y=Value), shape=21, colour="black", fill="transparent")+
  geom_ribbon(data=. %>% filter(Metric=="Expected") %>% 
                spread(Quintile, Value), aes(ymin=Q025, ymax=Q975), fill="red", alpha=0.3)+
  geom_line(data=. %>% filter(Metric=="Expected" & Quintile=="Q500"), aes(y=Value),colour="red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Total monthly duty receipts (£m)")+
  facet_wrap(~Beverage, scales="free_y")+
  theme_custom()

