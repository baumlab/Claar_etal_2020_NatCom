platy <- read.csv("data/coral_cover/plat_loss_site.csv")
# Site 19 removed from csv because 0% cover before and after

favia <- read.csv("data/coral_cover/favia_spp_loss_site.csv")
hydno <- read.csv("data/coral_cover/hydno_loss_site.csv")
fpent <- read.csv("data/coral_cover/favpent_loss_site.csv")

head(platy)

mean(platy$percent.change) # -47.55 = -48%
sd(platy$percent.change)/sqrt(nrow(platy)) # S.E = 12.09

mean(favia$percent.change) # -90.31 = -90%
sd(favia$percent.change)/sqrt(nrow(favia)) # S.E = 2.28

mean(hydno$percent.change) # -83.64 = -84%
sd(hydno$percent.change)/sqrt(nrow(hydno)) # S.E = 8.88

mean(fpent$percent.change) # -66.54 = -67%
sd(fpent$percent.change)/sqrt(nrow(fpent)) # S.E = 13.4

