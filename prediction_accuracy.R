
####### 2019 Prediction Error Quantification #######

## Always run following code before running for loop
synth_eq <- Arithmetic.Mean ~ treat + tmmx + rmax + pr
monitors <- as.vector(unique(pmdata$full_aqs_id))
grid_19 <- 10^seq(-1, 1, length = 20)
no_2020_19 <- 0
no_treat_19 <- 0
no_pre2016_19 <- 0
lt5_yrs_19 <- 0
lt10_treat_19 <- 0
data_plots_19 <- list()
counterfactual_plots_19 <- list()
outs <- list()

set.limits = c("tr","ct")
set.labels = c("Observed", "Counterfactual")
set.colors = c("black","steelblue")
set.linetypes = c("solid","longdash")
set.linewidth = rep(0.5,2)

ATT2_19 <- as.data.frame(matrix(nrow = 1700, ncol = 11))
outs_19 <- vector("list", length = 1700)
bad_19 <- rep(NA, 1700); k = 1
outputFile <- file("output4_19.txt", open = "a")
startTime <- Sys.time()

## redefining treatment period
som$State.of.emergency[48] <- as.Date("2020-02-28")
som$State.of.emergency <- som$State.of.emergency - years(1)
som$Reopen.businesses <- som$Reopen.businesses - years(1)
for (i in 1:length(monitors)) {
  dataSubset <- subset(pmdata, full_aqs_id == monitors[i])
  dataSubset$Date.Local <- dataSubset$Date.Local - years(1)
  dataSubset <- subset(dataSubset, year != 2020)
  if(is.na(dataSubset$State.Code[1])) {next}
  dataSubset$treat <- NA
  dataState <- dataSubset[1,]$State.Name
  dataCounty <- dataSubset[1,]$County.Name
  somDate <- som[match(dataState, som$State),]$State.of.emergency
  endDate <- som[match(dataState, som$State),]$Reopen.businesses
  dataSubset$treat <- ifelse(dataSubset$Date.Local >= somDate & 
                               dataSubset$Date.Local < endDate & 
                               dataSubset$year == 2019, 1, 0)
  covariates <- dplyr::select(dataSubset, tmmx, rmax, pr, 
                              Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, 
                              Feb, Mar, Apr)
  print(i)
  
  ## Exclusion criteria
  if (!("2019" %in% dataSubset$year)) { no_2020_19 <- no_2020_19 + 1; bad_19[k] = i; k=k+1; next; }
  if (sum(dataSubset$treat, na.rm = T) == 0) { no_treat_19 <- no_treat_19 + 1; 
  bad_19[k] = i; k=k+1; next; }
  if (sum(c("2010", "2011", "2012", "2013", "2014", "2015") %in% dataSubset$year) == 0) 
  { no_pre2016_19 = no_pre2016_19 + 1; bad_19[k] = i; k=k+1; next; }
  for (j in 2010:2018) {
    aux = which(dataSubset$year == j)
    aux_pre = aux[which(dataSubset[aux, 'Date.Local'] < somDate)]
    aux_post = aux[which(dataSubset[aux, 'Date.Local'] >= somDate)]
    if (length(aux_pre) < 10) {dataSubset <- subset(dataSubset, year != j)}
    if (length(aux_post) < 10) {dataSubset <- subset(dataSubset, year != j)}
  }
  if (length(unique(dataSubset$year)) < 6) { lt5_yrs_19 = lt5_yrs_19 + 1; 
  bad_19[k] = i; k=k+1; next; }
  if (sum(dataSubset$treat, na.rm = T) <= 10) { lt10_treat_19 <- lt10_treat_19 + 1; 
  bad_19[k] = i; k=k+1; next; }
  
  tryCatch({
    out <- gsynth(synth_eq, data = dataSubset, 
                  index = c("year","index"), na.rm = T, force = "two-way",
                  CV = T, parallel = T, seed = 123, estimator = "mc",
                  min.T0 = 10, inference = "parametric")
    
    yravg <- dplyr::select(filter(dataSubset, year == "2016" | year == "2017" | year == "2018" | year == "2019"), 
                           Date.Local, Arithmetic.Mean, year) ## !!!
    yravg <- reshape(yravg, idvar = "Date.Local", timevar = "year", direction = "wide") ## !!!
    yravg$threeyr.avg <- rowMeans(yravg[,2:4], na.rm=T) ## !!!
    
    
    ATT2_19[i,1] <- out$att.avg
    ATT2_19[i,2] <- as.character(dataSubset[1,]$fips)
    ATT2_19[i,3] <- as.character(dataSubset[1,]$Local.Site.Name)
    ATT2_19[i,4] <- as.character(dataSubset[1,]$City.Name)
    ATT2_19[i,5] <- as.character(dataSubset[1,]$State.Name)
    ATT2_19[i,6] <- as.character(dataSubset[1,]$County.Name)
    ATT2_19[i,7] <- as.character(dataSubset[1,]$Latitude.x)
    ATT2_19[i,8] <- as.character(dataSubset[1,]$Longitude.x)
    ATT2_19[i,9] <- mean((yravg$Arithmetic.Mean.2019 - yravg$threeyr.avg)^2, na.rm = T) ## RMSE of simple average
    ATT2_19[i,10] <- mean((out$Y.tr - out$Y.ct)^2, na.rm = T) ## RMSE of SCM
    ATT2_19[i,11] <- mean(dataSubset[which(dataSubset$Date.Local >= "2019-04-01" & dataSubset$year %in% c(2017,2018,2016)),5])
    outs[[i]] <- out
    
    
    data_plots_19[[i]] <- plot(out, type = "missing", theme.bw = TRUE,
                               main = paste(ATT2_19[i,5]),)
    counterfactual_plots_19[[i]] <- plot(out, type = "counterfactual", theme.bw = TRUE,
                                         main = paste(ATT2_19[i,5])) + 
      scale_color_manual(limits = set.limits, 
                         labels = set.labels,
                         values =set.colors) +
      scale_linetype_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linetypes) +
      scale_size_manual(limits = set.limits,
                        labels = set.labels,
                        values = set.linewidth) +
      guides(linetype = guide_legend(title=NULL, ncol=2),
             colour = guide_legend(title=NULL, ncol=2),
             size = guide_legend(title=NULL, ncol=2))
  }, error = function(e) {
    writeLines(paste0("at index ", i, " occurred following error ", as.character(e) ), 
               outputFile); bad_19[k] = i; k=k+1;
  })
  
}
close(outputFile)
endTime <- Sys.time()
endTime - startTime

## Misc formatting
ATT2_19 <- ATT2_19[!is.na(ATT2_19$V1),]
ATT2_19 <- as.data.frame(ATT2_19)
colnames(ATT2_19) <- c("ATT", "fips", "Local.Site.Name", "City", "State", "County", "Latitude", "Longitude", "simple.MSE", "gsynth.MSE", "Baseline.Feb.Mean")
ATT2_19$Latitude <- as.numeric(as.character(ATT2_19$Latitude))
ATT2_19$Longitude <- as.numeric(as.character(ATT2_19$Longitude))
ATT2_19$ATT <- as.numeric(as.character(ATT2_19$ATT))
ATT2_19$simple.MSE <- as.numeric(as.character(ATT2_19$simple.MSE))
ATT2_19$gsynth.MSE <- as.numeric(as.character(ATT2_19$gsynth.MSE))
ATT2_19$fips <- as.numeric(as.character(ATT2_19$fips))
ATT2_19$fips <- formatC(as.numeric(as.character(ATT2_19$fips)), width = 5, format = "d", flag = "0")
ATT2_19$Baseline.Feb.Mean <- as.numeric(as.character(ATT2_19$Baseline.Feb.Mean))
ATT2_19$Region <- sapply(ATT2_19$State, 
                         function(x) names(region.list)[grep(x,region.list,ignore.case=TRUE)])
ATT2_19 <- subset(ATT2_19, simple.MSE != 0)


### Simple RMSE vs. SCM RMSE

mean(ATT2_19$simple.MSE)
mean(ATT2_19$gsynth.MSE)

ATT2_19$simple.RMSE <- sqrt(ATT2_19$simple.MSE)
ATT2_19$gsynth.RMSE <- sqrt(ATT2_19$gsynth.MSE)

rmse.hist <- melt(ATT2_19[,13:14])

ggplot(data = rmse.hist) +
  geom_histogram(aes(x = value, fill=variable), 
                 alpha=0.4, bins = 50, position="identity") +
  labs(fill = "Method") +
  ggtitle("Root Mean Square Error for 2019 Prediction") +
  xlab("RMSE") +
  ylab("Count") +
  scale_fill_manual(labels = c("3-year average", "gsynth"), values = c("#f8766d","#00b0f6"))
