import os,sys # Importing necessary libraries

file = open("qiime_count_seqs_KI_Platy.sh", "w") # Initialize new file qiime_count_seqs_KI_Platy.sh. "w" means write a new file, Note: will clobber files with the same name!
file.write("#!/bin/bash\nset -e\nset -u\nset -o pipefail\n\n") # Write bash header to file

for root, dirs, files in os.walk("."): # Use os.walk to walk through all files and directories within the enclosing directory
	for name in files: # Iterate through all files within the enclosing directory
		s="\ncount_seqs.py -i " # Initialize string s with qiime command
		s2=" -o "
		s3="_count_seqs.txt"
		path=os.path.abspath('.') # Create string with absolute path to current folder
		print(path)
		if name.endswith("fasta"): # If the file ends with fasta ...
			file = open("qiime_count_seqs_KI_Platy.sh", "a") # open qiime_count_seqs_KI_Platy.sh, "a" means it will be appending to the file, not overwriting what's already there
			file.write(s+path+"/"+root+"/"+name+s2+path+"/"+root+"/"+name+s3+"\n") # write to the file s (Illumina-utils command) plus f (the file that you are currently looking at) plus \n an end of line character.

#os.system("chmod u+x iu-filter-merged-reads_KI_Platy.sh") # Give permissions so .sh file will run
#os.system("./iu-filter-merged-reads_KI_Platy.sh") # Run .sh file that was just created 