# Aim: generate data for the schools app

source("setup.R")

l = readRDS("private_data/england_flows.Rds")
las = readRDS("private_data/las_2011.Rds")
schools = readRDS("private_data/sld_leeds.Rds")
rf = readRDS("private_data/rf_england_schools.Rds")

# subset schools
sel = l$SCHOOLNAME_SPR11 %in% schools$LEA11_SchoolName
l = l[sel,]
rf = rf[sel,]
bbox(rf)
bbox(l)
plot(l)
plot(rf, add = T)

# Add scenario numbers (random atm)
rf$bicycle = runif(n = nrow(rf), min = 0, max = 5)
rf$govtarget_slc = rf$bicycle * 2
rf$govtarget_slc = rf$bicycle * 10

rnet = overline(sldf = rf, attrib = "bicycle", fun = sum)
plot(rnet, lwd = rnet$bicycle / mean(rnet$bicycle))
rnet = rnet[rnet$bicycle > 5,]
saveRDS(object = rnet, file = "pctSchoolsApp/rf_leeds_schools.Rds")

# use points method
# rnet_points = ... 


