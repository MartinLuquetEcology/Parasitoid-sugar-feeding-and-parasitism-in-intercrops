  #### Importing the data

sugar.lab.data <- read.table(file='all_lab_data.txt',h=T,dec=',')

  #### Selecting only parasitoid data
lab <- sugar.lab.data[sugar.lab.data$Type=='Parasitoid',-16]

  #### First let's prepare a balanced train dataset

set.seed(42)

  ## Unfed -> 7 per time treatment

subTest  <- lab[lab$Sugar_treatment=='Unfed' & lab$Hour_treatment=='0h',]
S1 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='Unfed' & lab$Hour_treatment=='1h',]
S2 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='Unfed' & lab$Hour_treatment=='12h',]
S3 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='Unfed' & lab$Hour_treatment=='24h',]
S4 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='Unfed' & lab$Hour_treatment=='48h',]
S5 <- subTest[sample(nrow( subTest), 7), ]

  ## VfN 24-48h : 7

subTest  <- lab[lab$Sugar_treatment=='VfN' & lab$Hour_treatment=='24h',]
S6 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='VfN' & lab$Hour_treatment=='48h',]
S7 <- subTest[sample(nrow( subTest), 7), ]

  ## Honeydew 24-48h : 7 (4+3)

subTest  <- lab[lab$Sugar_treatment=='ApH' & lab$Hour_treatment=='24h',]
S8 <- subTest[sample(nrow( subTest), 4), ]

subTest  <- lab[lab$Sugar_treatment=='SaH' & lab$Hour_treatment=='24h',]
S9 <- subTest[sample(nrow( subTest), 3), ]

subTest  <- lab[lab$Sugar_treatment=='ApH' & lab$Hour_treatment=='48h',]
S10 <- subTest[sample(nrow( subTest), 3), ]

subTest  <- lab[lab$Sugar_treatment=='SaH' & lab$Hour_treatment=='48h',]
S11 <- subTest[sample(nrow( subTest), 4), ]

  ## Nectar 0-12h : 20

subTest  <- lab[lab$Sugar_treatment=='VfN' & lab$Hour_treatment=='12h',]
S12 <- subTest[sample(nrow( subTest), 10), ]

subTest  <- lab[lab$Sugar_treatment=='VfN' & lab$Hour_treatment=='1h',]
S13 <- subTest[sample(nrow( subTest), 10), ]

subTest  <- lab[lab$Sugar_treatment=='VfN' & lab$Hour_treatment=='0h',]
S14 <- subTest[sample(nrow( subTest), 20), ]

subTest  <- lab[lab$Sugar_treatment=='VsN' & lab$Hour_treatment=='0h',]
S15 <- subTest[sample(nrow( subTest), 20), ]

  ## Honeydew 0-12h : 20

subTest  <- lab[lab$Sugar_treatment=='ApH' & lab$Hour_treatment=='12h',]
S16 <- subTest[sample(nrow( subTest), 10), ]

subTest  <- lab[lab$Sugar_treatment=='ApH' & lab$Hour_treatment=='1h',]
S17 <- subTest[sample(nrow( subTest), 10), ]

subTest  <- lab[lab$Sugar_treatment=='ApH' & lab$Hour_treatment=='0h',]
S18 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='SaH' & lab$Hour_treatment=='12h',]
S19 <- subTest[sample(nrow( subTest), 10), ]

subTest  <- lab[lab$Sugar_treatment=='SaH' & lab$Hour_treatment=='1h',]
S20 <- subTest[sample(nrow( subTest), 10), ]

subTest  <- lab[lab$Sugar_treatment=='SaH' & lab$Hour_treatment=='0h',]
S21 <- subTest[sample(nrow( subTest), 7), ]

subTest  <- lab[lab$Sugar_treatment=='AfH' & lab$Hour_treatment=='0h',]
S22 <- subTest[sample(nrow( subTest), 6), ]

  ## Let's store all this in a dataframe called "train_set"

train_set <- rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,
               S17,S18,S19,S20,S21,S22)


## The final dataset is exported using this function
# write.table(train_set,file='labdata_for_predictions.txt')
