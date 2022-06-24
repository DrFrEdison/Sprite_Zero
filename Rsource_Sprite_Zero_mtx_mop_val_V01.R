# Package update and initialization ####
library(devtools)
suppressMessages(install_github("DrFrEdison/r4dt", dependencies = T, upgrade = "always", quiet = T) )
suppressPackageStartupMessages(library(r4dt))

# general parameters ####
dt <- list()
dt$para$customer = "CCEP"
dt$para$beverage = "Sprite_Zero"

setwd(paste0(dt$wd <- paste0(wd$fe[[ grep(dt$para$customer, names(wd$fe)) ]]$Mastermodelle, dt$para$beverage)))
setwd( print( this.path::this.dir() ) )
dt$wd.git <- print( getwd() )

# location, line, main ####
dt$para$location = print(dt_customer[dt_customer$customer == dt$para$customer, "location"][ 7 ])
dt$para$line = print(dt_customer[dt_customer$customer == dt$para$customer, "line"][ 7 ])
dt$para$main = paste0(dt$para$beverage, " in ", dt$para$location, ", line ", dt$para$line)

# Modellerstellung
dir( paste0( dt$wd, "/", "/Modellerstellung"))
dt$para$model.raw.date <- c("220624")
dt$para$model.raw.pl <- c("02000")
dt$para$wl1 <- c(190)
dt$para$wl2 <- c(598)
dt$para$wl[[1]] <- seq(dt$para$wl1, dt$para$wl2, 1)

# Parameter ####
dir()
dir( paste0( dt$wd, "/", "/Modellerstellung", "/", dt$para$model.raw.date, "_", dt$para$model.raw.pl, "/spc"))
dt$para$substance <- c("Part2",	"Part1k",	"Part3", "TTA")

# Unit ####
dt$para$unit <- c( bquote("%"),  bquote("%"),  bquote("%"),  bquote("%") ,  bquote("%"))
dt$para$ylab <- c(bquote("Part2 in %"), bquote("Part1k in %"), bquote("Part3 in %"), bquote("TTA in %"))

# Rezept und SOLL-Werte ####
setwd( paste0( dt$wd, "/", "/Rezept") )
dt$rez <- read.xlsx(grep(".xlsx", dir( paste0( dt$wd, "/", "/Rezept")), value = T)[ length(grep(".xlsx", dir( paste0( dt$wd, "/", "/Rezept")), value = F))])
dt$rez[ grep("Messparameter", dt$rez[ , 3]): nrow(dt$rez) , ]
dt$para$SOLL <- c(100, 100, 100, 100)
dt$para$eingriff <- data.frame( Part2 = c(NA, NA)
                                , Part1k = c(NA, NA)
                                , Part3 = c(NA)
                                , TTA = c(98,102)
                                )

dt$para$eingriff <- data.frame( Part2 = c(NA, NA)
                                , Part1k = c(NA, NA)
                                , Part3 = c(NA)
                                , TTA = c(97,103)
)
#
# # Modelloptimierung
dir( paste0( dt$wd, "/", "/Modelloptimierung") )
dt$para$mop.date <- "220624"

# Model Matrix Ausmischung ####
setwd(dt$wd)
setwd("./Modellerstellung")
setwd(paste0("./", dt$para$model.raw.date[1], "_", dt$para$model.raw.pl[1]))
setwd("./csv")

dt$model.raw <- read.csv2( print(grep( "match.csv", dir(), value = T)), dec = ",", sep = ";")
head10(dt$model.raw)

for(i in 1:length(dt$para$substance)){
  if(dt$para$substance[i] == "TA" | dt$para$substance[i] == "TTA" | dt$para$substance[i] == "Acid") next
  dt$model.raw[ , colnames(dt$model.raw) %in% dt$para$substance[i]] <- dt$model.raw[ , colnames(dt$model.raw) %in% dt$para$substance[i]] * dt$para$SOLL[i] / 100
}

dt$SL <- dt$model.raw[which(dt$model.raw$Probe_Anteil == "SL") , ]
dt$model.raw <- dt$model.raw[which(dt$model.raw$Probe_Anteil != "SL") , ]

# Modellvalidierung ####
# dir( paste0( dt$wd, "/", "/Modellvalidierung") )
# dt$para$val.date <- "220524"
#
# # Linearity
# setwd(dt$wd)
# setwd("./Modellvalidierung")
# setwd("./Linearitaet")
# dir()
# dt$lin$raw <- read.csv2( "220602_Schwip_Schwap_Light_Linearitaet_TA_Coffein_Aspartam_Acesulfam.csv" , sep = "\t")
# dt$lin$raw <- dt$lin$raw[ order(dt$lin$raw$Dilution) , ]
# dt$lin$trs <- transfer_csv(dt$lin$raw)
#
# dt$para$Charge.val <- c("")
# dt$para$Charge.val.Sirup <- ""

# rename R files (run only once)
setwd(dt$wd.git)

# dt$para$Rfiles <- list.files(getwd(), pattern = ".R$", recursive = T)
# file.rename(dt$para$Rfiles, gsub("beverage", dt$para$beverage, dt$para$Rfiles))

