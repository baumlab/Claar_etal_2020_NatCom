## Extract, unzip, and prep raw data for processing

# Look inside of data folder, and move all '*.fastq.gz' files to one folder for processing
find /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116/ -name '*.fastq.gz' -exec mv {} /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/ \;

# Unzip these files - they are going to get really big! And this may take quite some time!
find . -type f -exec gunzip {} +


ls *R1* > R1.txt
ls *R2* > R2.txt
