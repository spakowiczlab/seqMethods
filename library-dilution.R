# library-dilution.R
#
# Authors: Dan Spakowicz & Nyelia Williams
#
# Date: 2019-11-19
#
# Purpose: This script converts ng/ul solutions to nanomolar and gives dilution
# volumes for creating a 3 nm stock solution for pooling and submission to 
# the GSR for sequencing


# From: Wetzel, Amy <Amy.Wetzel@nationwidechildrens.org> 
# Sent: Wednesday, November 27, 2019 7:45 AM
# To: Williams, Nyelia <Nyelia.Williams@osumc.edu>
#   Subject: RE: Miseq
# 
# CAUTION: External Email
# HI Nyelia,
# 
# Please fill out the attached Sample Submission For, as well as dilute your final library pool to 10nM in a volume of >10ul.  
# 
# Our hours of operation are 8-4:30pm Mon thru Friday, except tomorrow the lab will be closed for the Holiday.
# 
# Best regards,
# Amy


library(tidyverse)

# Nanodrop or Qubit measurements
x <- read_csv("T:/Labs/Spakowicz/data/Atbac-rberry/atbac.rberry16s.csv")

x %>%
  ggplot(aes(x = `Nucleic Acid`)) +
  geom_histogram()


# Size of the DNA fragments
dna.size <- 550

# Calculate the concentration in nanomolar
#https://www.thermofisher.com/us/en/home/references/ambion-tech-support/rna-tools-and-calculators/dna-and-rna-molecular-weights-and-conversions.html
dna.mw <- (dna.size * 607.4) + 157.9


calc <- 
  x %>%
  mutate(nanomolar =  `Nucleic Acid` /  dna.mw * 1e6)

calc %>%
  ggplot(aes(x = nanomolar)) +
  geom_histogram()

# Set desired concentation
conc <- 10 # in nanomolar

# Set potential final volumes (determined by ability to pipette)
final.vol <- c(10, 50, 100) # in ul

# Calculate the volume of water and sample for each volume in `final.vol`
calc.vol <- list()
for (f in 1:length(final.vol)) {
  calc.vol[[f]] <- data.frame(id = calc$`Sample ID`,
                              nanomolar = calc$nanomolar,
                              final.concentration = conc,
                              final.vol = final.vol[f],
                              vol.sample = round(
                                (conc * final.vol[f] / calc$nanomolar),
                                2)
  )
  calc.vol[[f]] <- 
    calc.vol[[f]] %>%
    mutate(vol.sample = ifelse(vol.sample > final.vol, 
                               final.vol, vol.sample)) %>%
    mutate(vol.water = final.vol - vol.sample)
}

out <- bind_rows(calc.vol) %>%
  arrange(id)
    

write.csv(calc.vol,
          paste0("Library_dilution_", 
                 format(Sys.Date(), "%F"), 
                 ".csv"),
          row.names = FALSE
)


