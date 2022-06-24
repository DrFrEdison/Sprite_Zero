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
setwd("..")
dt$wd.git <- print( getwd() )

dt$para$location = "Moenchengladbach"
dt$para$line = "G9"
dt$para$main = paste0(dt$para$beverage, " in ", dt$para$location, ", line ", dt$para$line)
dt$para$model.date <- c("220624")
dt$para$model.pl <- c("02000")
dt$para$wl1 <- c(190)
dt$para$wl2 <- c(598)
dt$para$wl[[1]] <- seq(dt$para$wl1, dt$para$wl2, 1)

dt$para$substance <- c("Part2",	"Part1k",	"Part3", "TTA")
dt$para$unit <- c( bquote("%"),  bquote("%"),  bquote("%"),  bquote("%") ,  bquote("%"))
dt$para$ylab <- c(bquote("Part2 in %"), bquote("Part1k in %"), bquote("Part3 in %"), bquote("TTA in %"))
# dt$para$mop.date <- "220522"
# dt$para$SOLL <- c(14.15, 119,84, 595.01, 49.9)

# dt$para$val.date <- "220522"
# rename R files (run only once)
# dt$para$Rfiles <- list.files(getwd(), pattern = ".R$", recursive = T)
# file.rename(dt$para$Rfiles, gsub("beverage", dt$para$beverage, dt$para$Rfiles))

