
### for f in *.csv; do if [[ "$f" == *"translation_pilot"* ]]; then mv $f translation_pilot; fi; done
### scp -r sttrott@quiz.ucsd.edu:web-data/translation_pilot data/raw


### for f in *.csv; do if [[ "$f" == *"translation_task"* ]]; then mv $f translation_task; fi; done
### scp -r sttrott@quiz.ucsd.edu:web-data/translation_task data/raw



from research_utilities.reformatting import concat_files, scrape_demographic_info

import json
import pandas as pd 
from os import listdir


def label_response(row):
	"""Label whether response was correct."""
	if str(row['same']) != "nan":
		return json.loads(row['responses'])['Q0']


### TODO: Extract demographic information. Not labeled, so tough...

def main(data_path):
	"""Concatenate files, relabel columns, and scrape important info."""
	
	# Concat files
	df_all = concat_files(data_path)

	# Code foil/tinfoil
	df_all['object'] = df_all['object'].apply(lambda x: "tinfoil" if x == "foil" else x)

	# Get demographic information
	### Not labeled in files, so will be kind of a pain
	
	# Write to .csv
	df_all.to_csv("data/processed/translation_e1_processed.csv")


if __name__ == "__main__":
	from argparse import ArgumentParser 

	parser = ArgumentParser()

	parser.add_argument("--path", type=str, dest="data_path",
						default="data/raw/translation_pilot")
	
	args = vars(parser.parse_args())
	main(**args)
