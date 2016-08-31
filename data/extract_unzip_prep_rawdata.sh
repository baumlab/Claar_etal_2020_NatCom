## Extract, unzip, and prep raw data for processing

# Look inside of data folder, and move all '*.fastq.gz' files to one folder for processing
find /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116/ -name '*.fastq.gz' -exec mv {} /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/ \;

# Unzip these files - they are going to get really big! And this may take quite some time!
find . -type f -exec gunzip {} +

# Create a text file listing all forward (R1) and reverse (R2) reads, separated by commas
ls -dm *_R1_* > R1.txt
ls -dm *_R2_* > R2.txt

# Create config file using all forward (R1) and reverse (R2) reads:
echo '[general]
project_name = KI_Platy
researcher_email = dclaar@uvic.ca
input_directory = /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/
output_directory = /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/merged_reads

[files]
pair_1 = ' > config_KI_Platy_all

cat R1.txt >> config_KI_Platy_all

echo '
pair_2 = ' >> config_KI_Platy_all

cat R2.txt >> config_KI_Platy_all

# Make script to merge Illumina pairs
#ls ../JuliaBaum_DanielleClaar-32138116/ > sampleids_temp.txt
#cut -c-11 sampleids_temp.txt > sampleids.txt

while read line; do echo Hello $line; done

for i in sample#
echo 'merge-illumina-pairs -o' > merge_illumina_pairs.sh
sample# >> merge_illumina_pairs.sh
echo '--enforce-Q30-check config_KI_Platy_all --marker-gene-stringent --retain-only-overlap  --min-overlap-size 100 --compute-qual-dicts' >> merge_illumina_pairs.sh



merge-illumina-pairs -o KI_Platy_KI14FSYM099 --enforce-Q30-check config_KI_Platy_KI14FSYM099 --marker-gene-stringent --retain-only-overlap  --min-overlap-size 100 --compute-qual-dicts
