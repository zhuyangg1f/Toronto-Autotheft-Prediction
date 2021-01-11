data = read.csv('/Users/mac/Documents/Toronto-Autotheft-Prediction-dev/data/cleaned_auto_weather_neighbor_dt.csv')
## Add the latitudes and longitudes between which each observation is located
## You can substitute any number of breaks you want. Or, a vector of fixed cutpoints
## LATgrid and LONgrid are going to be factors. With ugly level names.
data$LATgrid<-cut(data$Lat,breaks=45,include.lowest=T);
data$LONgrid<-cut(data$Long,breaks=45,include.lowest=T);

## Create a single factor that gives the lat,long of each observation. 
data$IDgrid<-with(data,interaction(LATgrid,LONgrid));

## Now, create another factor based on the above one, with shorter IDs and no empty levels
data$IDNgrid<-factor(data$IDgrid); 
levels(data$IDNgrid)<-seq_along(levels(data$IDNgrid));

## If you want total grid-cell count repeated for each observation falling into that grid cell, do this:
data$count<- ave(data$Lat,data$IDNgrid,FUN=length);
## You could have also used data$LONGITUDE, doesn't matter in this case

## If you want just a table of counts at each grid-cell, do this:
aggregate(data$Lat,data[,c('LATgrid','LONgrid','IDNgrid')],FUN=length);
## I included the LATgrid and LONgrid vectors so there would be some 
## sort of descriptive reference accompanying the anonymous numbers in IDNgrid,
## but only IDNgrid is actually necessary

## If you want a really minimalist table, you could do this:
tb = table(data$IDNgrid);
par(mfrow=c(1,2))
plot.default(data$LATgrid,data$LONgrid, main="grid data")

plot.default(data$Lat,data$Long, main="raw data")


