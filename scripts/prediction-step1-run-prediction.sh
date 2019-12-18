#!/bin/bash

source ~/.bashrc ; conda activate vocationcompass
jupyter nbconvert --to script predict-profession-python.ipynb

## execute the script for each datasets, corresponding to each of the feature sets.
python predict-profession-python.py ../data/10_professions_data.csv ../data/rand_optimised_all.db
python predict-profession-python.py ../data/10_professions_data_big5.csv ../data/rand_optimised_big5.db
python predict-profession-python.py ../data/10_professions_data_values.csv ../data/rand_optimised_values.db
