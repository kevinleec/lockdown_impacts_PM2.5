synth_eq <- Arithmetic.Mean ~ treat + tmmx + rmax + pr + vs + th + Feb + Mar + Apr
monitors <- as.vector(unique(pmdata$full_aqs_id))
grid <- 10^seq(-1, 1, length = 20)
no_2020 <- 0
no_treat <- 0
no_pre2016 <- 0
lt5_yrs <- 0
lt10_treat <- 0
data_plots <- list()
counterfactual_plots <- list()

########################################
## Calculate ATTs
########################################
ATT2 <- as.data.frame(matrix(nrow = 1700, ncol = 11))
outs <- vector("list", length = 1700)
bad <- rep(NA, 1700); k = 1
outputFile <- file("output4.txt", open = "a")
startTime <- Sys.time()
set.seed(123)
for (i in 1:length(monitors)) {
  dataSubset <- subset(pmdata, full_aqs_id == monitors[i])
  dataSubset$treat <- NA
  dataState <- dataSubset[1,]$State.Name
  dataCounty <- dataSubset[1,]$County.Name
  somDate <- som[match(dataState, som$State),]$State.of.emergency
  endDate <- som[match(dataState, som$State),]$Reopen.businesses
  dataSubset$treat <- ifelse(dataSubset$Date.Local >= somDate & 
                               dataSubset$Date.Local < endDate & 
                               dataSubset$year == 2020, 1, 0)
  print(i)
  
  ## Exclusion criteria
  if (!("2020" %in% dataSubset$year)) { no_2020 <- no_2020 + 1; bad[k] = i; k=k+1; next; }
  if (sum(dataSubset$treat, na.rm = T) == 0) { no_treat <- no_treat + 1; 
  bad[k] = i; k=k+1; next; }
  if (sum(c("2010", "2011", "2012", "2013", "2014", "2015") %in% dataSubset$year) == 0) 
  { no_pre2016 = no_pre2016 + 1; bad[k] = i; k=k+1; next; }
  for (j in 2010:2019) {
    aux = which(dataSubset$year == j)
    aux_pre = aux[which(dataSubset[aux, 'Date.Local'] < somDate)]
    aux_post = aux[which(dataSubset[aux, 'Date.Local'] >= somDate)]
    if (length(aux_pre) < 10) {dataSubset <- subset(dataSubset, year != j)}
    if (length(aux_post) < 10) {dataSubset <- subset(dataSubset, year != j)}
  }
  if (length(unique(dataSubset$year)) < 6) { lt5_yrs = lt5_yrs + 1; 
  bad[k] = i; k=k+1; next; }
  if (sum(dataSubset$treat, na.rm = T) <= 10) { lt10_treat <- lt10_treat + 1; 
  bad[k] = i; k=k+1; next; }
  
  tryCatch({
    out <- gsynth(synth_eq, data = dataSubset, 
                  index = c("year","index"), na.rm = T, force = "two-way",
                  CV = T, parallel = T, seed = 123, estimator = "mc",
                  min.T0 = 10, inference = "parametric")
    
    ATT2[i,1] <- out$att.avg
    ATT2[i,2] <- as.character(dataSubset[1,]$fips)
    ATT2[i,3] <- as.character(dataSubset[1,]$Local.Site.Name)
    ATT2[i,4] <- as.character(dataSubset[1,]$City.Name)
    ATT2[i,5] <- as.character(dataSubset[1,]$State.Name)
    ATT2[i,6] <- as.character(dataSubset[1,]$County.Name)
    ATT2[i,7] <- as.character(dataSubset[1,]$Latitude.x)
    ATT2[i,8] <- as.character(dataSubset[1,]$Longitude.x)
    ATT2[i,9] <- out$lambda.cv
    ATT2[i,10] <- out$MSPE
    ATT2[i,11] <- mean(dataSubset[which(dataSubset$Date.Local >= "2020-04-01" & dataSubset$Date.Local <= "2020-04-30"
                                        & dataSubset$year %in% c(2017,2018,2019)),6], na.rm=T)
    outs[[i]] <- out
    
    data_plots[[i]] <- plot(out, type = "missing", theme.bw = TRUE,
                            main = paste(ATT2[i,5]),)
    counterfactual_plots[[i]] <- plot(out, type = "counterfactual", theme.bw = TRUE,
                                      main = paste(ATT2[i,5]))
  }, error = function(e) {
    writeLines(paste0("at index ", i, " occurred following error ", as.character(e) ), 
               outputFile); bad[k] = i; k=k+1;
  })
  
}
close(outputFile)
endTime <- Sys.time()
endTime - startTime
```

```{r}
## Misc formatting
ATT2 <- ATT2[!is.na(ATT2$V1),]
ATT2 <- as.data.frame(ATT2)
colnames(ATT2) <- c("ATT", "fips", "Local.Site.Name", "City", "State", "County", "Latitude", "Longitude", "Lambda", "MSPE", "Baseline.Feb.Mean")
ATT2$Latitude <- as.numeric(as.character(ATT2$Latitude))
ATT2$Longitude <- as.numeric(as.character(ATT2$Longitude))
ATT2$ATT <- as.numeric(as.character(ATT2$ATT))
ATT2$Lambda <- as.numeric(as.character(ATT2$Lambda))
ATT2$MSPE <- as.numeric(as.character(ATT2$MSPE))
ATT2$fips <- as.numeric(as.character(ATT2$fips))
ATT2$fips <- formatC(as.numeric(as.character(ATT2$fips)), width = 5, format = "d", flag = "0")
ATT2$Baseline.Feb.Mean <- as.numeric(as.character(ATT2$Baseline.Feb.Mean))
ATT2$Region <- sapply(ATT2$State, 
                      function(x) names(region.list)[grep(x,region.list,ignore.case=TRUE)])


## Adjusted ATT (subtracting covariate coefficients)
outs_noNA = outs[-which(sapply(outs, is.null))]
coefficients <- matrix(NA, nrow = length(outs_noNA), ncol = 7)
for(i in 1:length(outs_noNA)) {
  coefficients[i,] <- c(ATT2[i,]$ATT, ATT2[i,]$fips, t(outs_noNA[[i]]$beta[c(1:5)]))
}
coefficients <- as.data.frame(coefficients)
colnames(coefficients) <- c("ATT", "fips", "tmmx_coef", "rmax_coef", "pr_coef", "vs_coef", "th_coef")
coefficients$ATT <- as.numeric(coefficients$ATT)
coefficients$tmmx_coef <- as.numeric(coefficients$tmmx_coef)
coefficients$rmax_coef <- as.numeric(coefficients$rmax_coef)
coefficients$pr_coef <- as.numeric(coefficients$pr_coef)
coefficients$vs_coef <- as.numeric(coefficients$vs_coef)
coefficients$th_coef <- as.numeric(coefficients$th_coef)
coefficients$adj.ATT <- coefficients$ATT - coefficients$tmmx_coef - coefficients$rmax_coef - 
  coefficients$pr_coef - coefficients$vs_coef - coefficients$th_coef
ATT2$adj.ATT <- coefficients$adj.ATT

## Does higher baseline affect ATT?
ggplot(ATT2, aes(x=Baseline.Feb.Mean,y=ATT)) + geom_point()
baseline_lm <- lm(ATT ~ Baseline.Feb.Mean, data = ATT2)
summary(baseline_lm)


## Prelim rough map
usa <- c(left = -125, bottom = 24.5, right = -66.5, top = 50)
map <- get_map(location = usa, maptype = "roadmap", source = "google", color = "bw", force = T)
point_map <- ggmap(map) + geom_point(data = ATT2, aes(x=Longitude, y=Latitude, color=adj.ATT)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342") + xlab("Longitude") + ylab("Latitude") + geom_point(size=0)
point_map
data_plots <- data_plots[order(-ATT2$ATT)]
counterfactual_plots <- counterfactual_plots[order(-ATT2$ATT)]
layout <- rbind(c(1,2,3), c(4,5,6), c(7,8,9))

## Save data/counterfactual plots
counterfactual_plots <- counterfactual_plots[!sapply(counterfactual_plots,is.null)]
data_plots <- data_plots[!sapply(data_plots,is.null)]

pdf(file = 'data_plots.pdf', onefile = TRUE, paper = 'A4', 
    width = 9, height = 9, pointsize = 1)
marrangeGrob(grobs = data_plots, ncol = 3, nrow = 3, layout_matrix = layout)
dev.off()

pdf(file = 'counterfactual_plots.pdf', onefile = TRUE, paper = 'A4', 
    width = 9, height = 9, pointsize = 1)
marrangeGrob(grobs = counterfactual_plots, ncol = 3, nrow = 3, layout_matrix = layout)
dev.off()






################## Merge datasets ##################
lockdown <- merge(ATT2, coal, by = "fips") %>%
  merge(mobility, by = "fips", all.x = T) %>%
  merge(socioecon, by = "fips", all.x = T) %>%
  #merge(meteor, by = "fips", all.x = T) %>%
  merge(misc_sources, by = "fips", all.x = T) %>%
  merge(ur_codes, by = "fips", all.x = T) %>%
  merge(mobile, by = "fips", all.x = T)
lockdown <- lockdown[, !names(lockdown) %in% 
                       c("STATE_NAME", "county_name.x", 
                         "county_name.y", "state_name",
                         "COUNTYFP", "STATEFP", "NAME", 
                         "year.x", "year.y", "STATE", "STATE_FIPS", 
                         "COUNTY", "COUNTY_FIPS", "POLLUTANT", "POLLUTANT.TYPE",
                         "SECTOR", "UNIT.OF.MEASURE",
                         "FIPS.code", "State.Abr.", "County.name", "CBSA.title",
                         "CBSA.2012.pop", "County.2012.pop", "X2006.code",
                         "X1990.based.code", "X")]
colnames(lockdown)[colnames(lockdown) == 'mean'] <- 'coal_contribution'
colnames(lockdown)[colnames(lockdown) == 'X2013.code'] <- 'urban_rural'
lockdown$urban_rural[lockdown$urban_rural == 5 | lockdown$urban_rural == 6] <- 4
lockdown <- dummy_cols(lockdown, select_columns = "urban_rural", remove_first_dummy = T)
lockdown <- dummy_cols(lockdown, select_columns = "Region", remove_first_dummy = T)

##################################################################
## Spatial Interpolation
##################################################################

idw_lockdown <- select(lockdown, ATT, Latitude, Longitude)
sp::coordinates(idw_lockdown) <- ~ Longitude + Latitude
x.range <- as.integer(range(idw_lockdown@coords[,1]))
y.range <- as.integer(range(idw_lockdown@coords[,2]))
x.range <- as.integer(c(-126, -66))
y.range <- as.integer(c(23,51))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.5))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE


# plot(grd, cex=1.5)
# points(lockdown, pch=1, col='red', cex=1)
# title("Interpolation Grid and Sample Points")
usmap <- readShapePoly("cb_2019_us_nation_5m.shp")
usmapoutline <- fortify(usmap)



################## idw ##################

idw <- idw(ATT ~ 1, locations=idw_lockdown, newdata=grd, nmax=30)
idw.output <- as.data.frame(idw)
names(idw.output)[c(1:3)]<-c("lon","lat", "att.pred")

xy <- idw.output[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf <- SpatialPointsDataFrame(coords = xy, data = idw.output,
                               proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over <- over(spdf, usmap, fn=NULL)
points <- cbind(spdf@data, over)
points$var1.var <- NULL
points <- na.omit(points)
plot_idw <- ggplot(data=points, aes(x=lon,y=lat))
layer1_idw <- c(geom_tile(data=points, aes(fill=att.pred)))
layer2_idw <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))

## idw smoothed ATT map
plot_idw+layer1_idw+borders("state")+layer2_idw+scale_fill_gradient2(midpoint = 0, mid="#fff650", low="#18b342", high="#fa7b05")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT") + ggtitle("IDW-Smoothed Average Treatment Effect Estimates")

## point-level ATT map
blank_layer <- c(geom_tile(data=points, fill="grey80"))
plot_idw+blank_layer+borders("state")+layer2_idw+geom_point(data = ATT2, aes(x=Longitude, y=Latitude, color=ATT)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT", color="ATT") + ggtitle("Point-Level Average Treatment Effect Estimates")






####################################
## Spike Analysis 
####################################

## Spike removal
for(i in 1:nrow(ATT2)) {
  rows_from_end <- outs_noNA[[i]]$T0
  old_att <- outs_noNA[[i]]$att
  old_att <- as.data.frame(old_att)
  old_att <- slice_tail(old_att, n=rows_from_end+1)
  new_att <- old_att[!row.names(old_att) %in% c(147, 153, 156),]
  ATT2$new_ATT[i] <- mean(new_att, na.rm = T)
}

idw_lockdown_spike <- select(ATT2, new_ATT, Latitude, Longitude)
sp::coordinates(idw_lockdown_spike) <- ~ Longitude + Latitude
x.range <- as.integer(range(idw_lockdown_spike@coords[,1]))
y.range <- as.integer(range(idw_lockdown_spike@coords[,2]))
x.range <- as.integer(c(-126, -66))
y.range <- as.integer(c(23,51))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.5))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE


# plot(grd, cex=1.5)
# points(lockdown, pch=1, col='red', cex=1)
# title("Interpolation Grid and Sample Points")
usmap <- readShapePoly("cb_2019_us_nation_5m.shp")
usmapoutline <- fortify(usmap)



################## idw ##################

idw_spike <- idw(new_ATT ~ 1, locations=idw_lockdown_spike, newdata=grd, nmax=30)
idw.output_spike <- as.data.frame(idw_spike)
names(idw.output_spike)[c(1:3)]<-c("lon","lat", "att.pred")

xy <- idw.output_spike[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf_spike <- SpatialPointsDataFrame(coords = xy, data = idw.output_spike,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over_spike <- over(spdf_spike, usmap, fn=NULL)
points_spike <- cbind(spdf_spike@data, over_spike)
points_spike$var1.var <- NULL
points_spike <- na.omit(points_spike)
plot_idw_spike <- ggplot(data=points_spike, aes(x=lon,y=lat))
layer1_idw_spike <- c(geom_tile(data=points_spike, aes(fill=att.pred)))
layer2_idw_spike <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))

## idw smoothed ATT map
plot_idw_spike+layer1_idw_spike+borders("state")+layer2_idw_spike+scale_fill_gradient2(midpoint = 0, mid="#fff650", low="#18b342", high="#fa7b05")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT") + ggtitle("IDW-Smoothed Average Treatment Effect Estimates")

## point-level ATT map
blank_layer_spike <- c(geom_tile(data=points_spike, fill="grey80"))
plot_idw_spike+blank_layer_spike+borders("state")+layer2_idw_spike+geom_point(data = ATT2, aes(x=Longitude, y=Latitude, color=new_ATT)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="ATT", color="ATT") + ggtitle("Point-Level Average Treatment Effect Estimates")

