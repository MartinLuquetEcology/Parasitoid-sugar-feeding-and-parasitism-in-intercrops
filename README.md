# Parasitoid-sugar-feeding-and-parasitism-in-intercrops

These datasets and R scripts are complementary to the paper - name of the paper, journal, etc.

They will allow reproducing all results presented in the main paper and in the appendices.



----- Description of the datasets -----

"all_lab_data.txt" contains all the data about sugar profiles of parasitoids and food sources obtained in the lab experiments.

"field_data_sugars.txt" contains all the data about sugar profiles of parasitoids captured in the field.

"field_data_surveys.txt" contains the data about surveys of aphids and mummies in the field.

"labdata_for_prediction.txt" is a subset of "all_lab_data.txt", used to train the classifier (can be re-created using the script "Creating the training table - for publication.R").

ADD a .doc for variable description !!!


----- Description of the scripts ----- CHANGE NAMES

"Script - lab data analysis - for publication.R" presents analyses made on the lab data: sugar profiles and dynamics of the different food sources and parasitoid treatments.

"Script - Creating the training table - for publication.R" creates a nearly balanced dataset in order to train a classifier.

"Script - prediction and field data analysis - for publication.R" trains a classifier thanks to this subsampled dataset and predicts the feeding status of the field-caught insects. It also presents some analyses of the field insects' sugar profiles.

"Script - field data analysis - for publication.R" analyses the data from field surveys (aphid and parasitoid dynamics in single and intercrops).
