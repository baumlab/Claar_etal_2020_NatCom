import os,sys # Importing necessary libraries

file = open("iu-merge-pairs_KI_Platy.sh", "w") # Initialize new file iu-merge-pairs_KI_Platy.sh. "w" means write a new file, Note: will clobber files with the same name!
file.write("#!/bin/bash\nset -e\nset -u\nset -o pipefail\n\n") # Write bash header to file

for file in os.listdir("."): # For each file listed in the current directory
	s="iu-merge-pairs -o " # Initialize string s with Illumina-utils command
	s2=" --enforce-Q30-check "
	if file.endswith("_merge_config"): # If the file ends with _merge_config ...
		f=file # f equals the file name that you are currently looking at 
		g=f.split('_') # Split the last string in array f on '_' to index individual chunks of the last string in array f
		out=g[0]+"_"+g[1]+"_"+g[2]+"_"+g[3] # Reconstruct output name using strings in array g
		file = open("iu-merge-pairs_KI_Platy.sh", "a") # open iu-merge-pairs_KI_Platy.sh, "a" means it will be appending to the file, not overwriting what's already there
		file.write(s+out+s2+f+"\n") # write to the file: s (Illumina-utils command) plus the output filename (out) plus s2 (more Illumina-utils command information) plus f (the file that you are currently looking at) plus \n an end of line character.

os.system("chmod u+x iu-merge-pairs_KI_Platy.sh") # Give permissions so .sh file will run
os.system("./iu-merge-pairs_KI_Platy.sh") # Run .sh file that was just created 