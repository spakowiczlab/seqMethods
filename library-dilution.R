# library-dilution.R
#
# Authors: Dan Spakowicz & Nyelia Williams
#
# Date: 2019-11-19
#
# Purpose: This script converts ng/ul solutions to nanomolar and gives dilution
# volumes for creating a 3 nm stock solution for pooling and submission to 
# the GSR for sequencing

library(tidyverse)

x <- read.csv("QubitData_20-11-2019_00-39-03.csv") %>%
  arrange(Run.ID)

hist(x$Qubit..tube.conc. / x$Original.sample.conc.)

# Clean up the dil factor & sample vol columns
y <- 
  x %>%
  mutate(dil.calc = round((Qubit..tube.conc. / Original.sample.conc.), 0)) %>%
  mutate(Dilution.Factor = ifelse(dil.calc == 5, 200, 100)) %>%
  mutate(Sample.Volume..µL. = ifelse(dil.calc == 5, 1, 2))

# Calculate the concentration in nanomolar
# g / mol for 500 bp dsDNA from https://www.thermofisher.com/us/en/home/references/ambion-tech-support/rna-tools-and-calculators/dna-and-rna-molecular-weights-and-conversions.html & https://support.illumina.com/bulletins/2016/11/converting-ngl-to-nm-when-calculating-dsdna-library-concentration-.html 
calc <- 
  y %>%
  mutate(nanomolar = Original.sample.conc. / 303858 * 1e6) %>%
  mutate(vol.samp.for.10ul.3nm = round((30 / nanomolar), 2)) %>%
  mutate(vol.samp.for.10ul.3nm = ifelse(vol.samp.for.10ul.3nm > 10, 10, 
                                        vol.samp.for.10ul.3nm)) %>%
  mutate(vol.water.for.10ul.3nm = 10 - vol.samp.for.10ul.3nm) %>%
  mutate(vol.samp.for.50ul.3nm = round((150 / nanomolar), 2)) %>%
  mutate(vol.samp.for.50ul.3nm = ifelse(vol.samp.for.50ul.3nm > 50, 50,
                                        vol.samp.for.50ul.3nm)) %>%
  mutate(vol.water.for.50ul.3nm = 50 - vol.samp.for.50ul.3nm) %>%
  mutate(vol.samp.for.100ul.3nm = round((300 / nanomolar), 2)) %>%
  mutate(vol.samp.for.100ul.3nm = ifelse(vol.samp.for.100ul.3nm > 100, 100, 
                                         vol.samp.for.100ul.3nm)) %>%
  mutate(vol.water.for.100ul.3nm = 100 - vol.samp.for.100ul.3nm) %>%
  select(Run.ID, nanomolar, contains("vol")) %>%
  arrange(Run.ID)

write.csv(calc,
          paste0("Qubit_dilution_", 
                 format(Sys.Date(), "%F"), 
                 ".csv"),
          row.names = FALSE
)


