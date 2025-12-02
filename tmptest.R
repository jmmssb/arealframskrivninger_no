
## try with terra::extract

gc()
dfd$artif_chg[dfd$artif_chg<0]<-0
gc()
# we need character municipality identification codes (otherwise rasterize will return levels)
mmc<-mm
mmc$kommunenummer <- as.character(mmc$kommunenummer)

# mmn = municipality polygons (SpatVector)
# elig_r = eligibility raster (SpatRaster)

# Extract eligibility values for each municipality
elig_df <- terra::extract(elig_r, mmc)
colnames(elig_df)<-c("ID","elig")

# and then i also want kommunenummer
elig_df$kommunenummer <- mmc$kommunenummer[elig_df$ID]

# now the thresholds
dfd['elig_tld']<-NA
for(i in 1:nrow(dfd)){
  if(dfd$artif_chg[i]>0){
    # for debugging
    print(dfd$municipality[i])
    # operation
    dfd$elig_tld[i]<-sort(elig_df$elig[elig_df$kommunenummer==dfd$kommunenummer[i]],decreasing = T)[dfd$artif_chg[i]]
  }
}

# MM check duplicate eligibilities!

# now set the threholds for those municipalities where there is no urbanisation to 100
dfd$elig_tld[is.na(dfd$elig_tld)]<-100

# now merge with our municipality spatial vector
mdfd <- merge(mm,dfd,by="kommunenummer")

eligtld_r<-terra::rasterize(mdfd,lcma,field="elig_tld")

plot(eligtld_r)

# good and now the raster of new artificial cells
newart_r<-lcma
newart_r[newart_r>0]<-NA
newart_r[elig_r>=eligtld_r]<-1

plot(newart_r)

sum(values(newart_r),na.rm=T)

sum(dfd$artif_chg)

# make a plot

# Here is our raster
plot(elig_r,main="Eligibility Map with New Artificial Surface in Red")
# And add urban layer
plot(urb,col="black", add=T,legend=F)
# and add the new ones
plot(newart_r,colour="red",add=T,legend=F)


# and for Oslo
oslo_ext <- ext(4300000, 4400000, 4020000, 4120000) 
plot(crop(elig_r,oslo_ext),main="Eligibility Map Oslofjord with New Artificial Surface in Red")
plot(crop(urb,oslo_ext),col="black", add=T,legend=F)
plot(crop(newart_r,oslo_ext),col="red", add=T,legend=F)





