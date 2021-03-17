#################################################
## Health Impacts
#################################################
geocounty <- geoCounty
coordinates(geocounty) <- ~ lon + lat
plot(geocounty, cex=1.5)

idw_h <- idw(ATT ~ 1, locations=idw_lockdown, newdata=geocounty, nmax=30)
idw_h.output <- as.data.frame(idw_h)
names(idw_h.output)[c(1:3)]<-c("lon","lat", "att.pred")

xy_h <- idw_h.output[,c("lon","lat")]
proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
spdf_h <- SpatialPointsDataFrame(coords = xy_h, data = idw_h.output,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 
                                                 +ellps=WGS84 +towgs84=0,0,0"))
over_h <- over(spdf_h, usmap, fn=NULL)
points_h <- cbind(spdf_h@data, over_h)
points_h$var1.var <- NULL
points_h <- na.omit(points_h)
geocounty_h <- as.data.frame(geocounty)
county.pred <- merge(geocounty_h, idw_h.output, by = c("lon","lat"))
county.pred$resp.changeper100 <- 3.352*(1-(1/exp(.0007*county.pred$att.pred)))
county.pred$cvd.changeper100 <- 5.385*(1-(1/exp(.0008*county.pred$att.pred)))
county.pred <- merge(county.pred, socioecon[, c("fips", "population", "age_pct_65_plus")], by = "fips", all.x = T)
county.pred$pop_65_plus <- county.pred$population*county.pred$age_pct_65_plus
county.pred$num_100s <- county.pred$pop_65_plus/100
county.pred$resp.change <- county.pred$resp.changeper100*county.pred$num_100s
county.pred$cvd.change <- county.pred$cvd.changeper100*county.pred$num_100s

## map changes
usa <- c(left = -125, bottom = 24.5, right = -66.5, top = 50)
map <- get_map(location = usa, maptype = "roadmap", source = "google", color = "bw", force = T)
ggmap(map) + geom_point(data = county.pred, aes(x=lon, y=lat, color=cvd.change)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342")


county.pred$Region <- sapply(county.pred$rMapState, 
                             function(x) names(region.list)[grep(x,region.list)])
county.pred$Region <- as.character(county.pred$Region)
county.pred <- within(county.pred, Region[rMapState == 'kansas'] <- 'Midwest')

region.results <- data.frame()
region.results <- aggregate(cbind(resp.change,cvd.change) ~ Region, data = county.pred, FUN = sum)
state.results <- aggregate(cbind(resp.change,cvd.change) ~ rMapState, data = county.pred, FUN = sum)
att.by.region <- aggregate(att.pred ~ Region, data = county.pred, FUN = mean)
att.by.state <- aggregate(att.pred ~ rMapState, data = county.pred, FUN = mean)
region.results <- merge(region.results, att.by.region, by = "Region")
state.results <- merge(state.results, att.by.state, by = "rMapState")
region.results <- rbind(region.results, data.frame(Region = "USA", resp.change = sum(county.pred$resp.change, na.rm = T), cvd.change = sum(county.pred$cvd.change, na.rm = T), att.pred = mean(county.pred$att.pred)))
region.results$resp.change.lockdown <- region.results$resp.change*(50/365)
region.results$cvd.change.lockdown <- region.results$cvd.change*(50/365)

#att.by.region <- aggregate(ATT ~ Region, data = ATT2, FUN = mean)

ATT2 <- within(ATT2, Region[State == 'Kansas'] <- 'Midwest')
ATT2$Region <- as.character(ATT2$Region)
monitor.by.region <- aggregate(ATT ~ Region, data = ATT2, FUN = mean)
monitor.by.region <- rbind(monitor.by.region, data.frame(Region = "USA", ATT = mean(ATT2$ATT)))
monitor.by.state <- aggregate(ATT ~ State, data = ATT2, FUN = mean)