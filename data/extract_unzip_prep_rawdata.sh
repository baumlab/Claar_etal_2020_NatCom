## Extract, unzip, and prep raw data for processing

# Look inside of data folder, and move all '*.fastq.gz' files to one folder for processing
find /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116/ -name '*.fastq.gz' -exec mv {} /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/ \;

# Unzip these files - they are going to get really big! And this may take quite some time!
find . -type f -exec gunzip {} +

# Create a text file listing all forward (R1) and reverse (R2) reads, separated by commas
ls -dm *_R1_* > R1.txt
ls -dm *_R2_* > R2.txt