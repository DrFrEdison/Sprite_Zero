# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "_mop_" )
source.file <- print(dir( pattern = "_mop_" )[ length( dir( pattern = "_mop_" ))])
source( paste0(getwd(), "/", source.file) )

dt$para$mop.date <- "220602"
# spectra ####
dt$para$i = 2
dt$para$substance[dt$para$i]
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd("./Produktionsdaten")

dt$para$files <- dir(pattern = "validated.csv$")
dt$para$txt <- txt.file(dt$para$files)

dt$raw <- lapply(dt$para$files, \(x) fread(x, sep = ";", dec = ","))
names(dt$raw) <- dt$para$txt$type

dt$para$trs <- lapply(dt$raw, transfer_csv.num.col)
dt$trs <- lapply(dt$raw, transfer_csv)

# Model Matrix ####
setwd(dt$wd)
setwd("./Modellerstellung")
setwd(paste0("./", dt$para$model.date[1], "_", dt$para$model.pl[1]))
setwd("./csv")

dt$model <- fread( print(grep( "match.csv", dir(), value = T)), dec = ",", sep = ";")
dt$SL <- dt$model[which(dt$model$Probe_Anteil == "SL") , ]
dt$model <- dt$model[which(dt$model$Probe_Anteil != "SL") , ]

setwd(dt$wd)
setwd("./Modelloptimierung")
dir.create(paste0("./", dt$para$mop.date, "_", dt$para$model.pl[1], "_", dt$para$substance[dt$para$i]), showWarnings = F)
setwd(paste0("./", dt$para$mop.date, "_", dt$para$model.pl[1], "_", dt$para$substance[dt$para$i]))
dir.create("Modellmatrix", showWarnings = F)
setwd("./Modellmatrix")

fwrite(dt$model, paste0(datetime(), "_", dt$para$beverage, "_", dt$para$substance[dt$para$i], "_matrix.csv"), row.names = F, dec = ",", sep = ";")
dt$model <- transfer_csv(csv.file = dt$model)
dt$SL <- transfer_csv(csv.file = dt$SL)

# Plot ####
matplot(dt$para$wl[[1]]
        , t( dt$SL$spc[ grep(dt$para$substance[ dt$para$i ], dt$SL$data$Probe) , ])
        , type = "l", lty = 1, xlab = .lambda, ylab = "AU", main = "SL vs Modellspektren"
        , col = "blue")
matplot(dt$para$wl[[1]]
        , t( dt$model$spc )
        , type = "l", lty = 1, xlab = .lambda, ylab = "AU", main = "SL vs Modellspektren"
        , col = "red", add = T)
legend("topright", c(paste0("SL ", dt$para$substance[ dt$para$i]), "Ausmischung"), lty = 1, col = c("blue", "red"))

dt$model$data$Probe == dt$para$substance[dt$para$i]
dt$para.pls$wlr <- wlr_function(210:350, 210:350, 5)
nrow(dt$para.pls$wlr)
dt$para.pls$wlm <- wlr_function_multi(210:350, 210:350, 10)
nrow(dt$para.pls$wlm)
dt$para.pls$wl <- rbind.fill(dt$para.pls$wlm, dt$para.pls$wlr)
nrow(dt$para.pls$wl)

dt$para.pls$ncomp <- 4

# RAM ####
gc()
memory.limit(99999)

# PLS and LM ####
dt$pls$pls <- pls_function(csv_transfered = dt$model
                           , substance = dt$para$substance[dt$para$i]
                           , wlr = dt$para.pls$wl
                           , ncomp = dt$para.pls$ncomp)

dt$pls$lm <- pls_lm_function(dt$pls$pls
                             , csv_transfered = dt$model
                             , substance = dt$para$substance[dt$para$i]
                             , wlr = dt$para.pls$wl
                             , ncomp = dt$para.pls$ncomp)
# Prediction ####
dt$pls$pred <- produktion_prediction(csv_transfered = dt$trs$clean, pls_function_obj = dt$pls$pls, ncomp = dt$para.pls$ncomp)

# Best model ####
dt$pls$merge <- merge_pls(pls_pred = dt$pls$pred, dt$pls$lm ,R2=.8)
dt$pls$merge[ order(dt$pls$merge$sd) , ]

# Prediciton ####
dt$mop$ncomp <- 2
dt$mop$wl1 <- 210
dt$mop$wl2 <- 230
dt$mop$wl3 <- 270
dt$mop$wl4 <- 290
dt$mop$spc <- "2nd"
dt$mop$model <- pls_function(dt$model, dt$para$substance[i], data.frame(dt$mop$wl1, dt$mop$wl2, dt$mop$wl3, dt$mop$wl4), dt$mop$ncomp, spc = dt$mop$spc)
dt$mop$model  <- dt$mop$model [[grep(dt$mop$spc, names(dt$mop$model))[1]]][[1]]

dt$mop$pred <- pred_of_new_model(dt$model
                                 , dt$para$substance[i]
                                 , dt$mop$wl1
                                 , dt$mop$wl2
                                 , dt$mop$wl3, dt$mop$wl4
                                 , dt$mop$ncomp
                                 , dt$mop$spc
                                 , dt$trs$clean)
dt$mop$pred <- as.numeric(ma(dt$mop$pred, 5))
dt$mop$bias <- round(bias(median(dt$mop$pred, na.rm = T), 0, 100),3)
dt$mop$pred <- dt$mop$pred - dt$mop$bias

keep.out.unsb(model = dt$model, dt$mop$wl1, dt$mop$wl2, dt$mop$wl3, dt$mop$wl4)