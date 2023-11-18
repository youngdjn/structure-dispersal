m = readRDS("/ofo-share/str-disp_data-partialstan-models/stanmod_valley-allsp-height-01_2Dt_pois.rds")


png("/ofo-share/str-disp_data-partialstan-models/temp_plot.png")

plot(m)
dev.off()
summary(m)
