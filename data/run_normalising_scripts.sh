cd /Volumes/External_memory/Dropbox/FINS_dataset/fins-app-git/

python Scripts/normalize_refs_s1s2.py --in ./fins-app/data/Data_S1.csv --out ./fins-app/data/Data_S1_norm.csv --log issues_S1.csv

python Scripts/normalize_refs_s1s2.py --in ./fins-app/data/Data_S2.csv --out ./fins-app/data/Data_S2_norm.csv --log issues_S2.csv

python Scripts/normalize_refs_s3.py --in ./fins-app/data/Data_S3.csv --out ./fins-app/data/Data_S3_norm.csv --log issues_S3.csv


# the last two collumns from the output files were copy pasted to the original files