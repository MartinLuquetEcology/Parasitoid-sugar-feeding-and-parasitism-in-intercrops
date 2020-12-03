# Parasitoid-sugar-feeding-and-parasitism-in-intercrops

These datasets and R scripts are complementary to the paper "Aphid honeydew may be the predominant sugar source for Aphidius parasitoids even in nectar-providing intercrops"

They will allow reproducing all results presented in the main paper and in the appendices.



----- Description of the datasets -----

"all_lab_data.txt" contains all the data about sugar profiles of parasitoids and food sources obtained in the lab experiments.

"field_data_sugars.txt" contains all the data about sugar profiles of parasitoids captured in the field.

"field_data_surveys.txt" contains the data about surveys of aphids and mummies in the field.

"labdata_for_prediction.txt" is a subset of "all_lab_data.txt", used to train the classifier (can be re-created using the script "Creating the training dataset.R").

Variables in each dataset are described in the "DATASET AND VARIABLE DESCRIPTION.txt" document.


----- Description of the scripts -----

"Lab data analysis.R" presents analyses made on the lab data: sugar profiles and dynamics of the different food sources and parasitoid treatments.

"Creating the training dataset.R" creates a nearly balanced dataset in order to train a classifier.

"Prediction and analysis of feeding patterns in the field.R" trains a classifier thanks to this subsampled dataset and predicts the feeding status of the field-caught insects. It also presents some analyses of the field insects' sugar profiles.

"Field data analysis (surveys).R" analyses the data from field surveys (aphid and parasitoid dynamics in single and intercrops).

"Functions for adjusted counting" contains two functions used to adjust the random forest results and predict the field parasitoid sugar feeding status (they will be sourced directly from "Prediction and analysis of feeding patterns in the field.R").
