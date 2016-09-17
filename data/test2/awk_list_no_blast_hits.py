import os,sys # Importing necessary libraries

file = open("awk_no_blast_hits_KI_Platy.sh", "w") # Initialize new file awk_no_blast_hits_KI_Platy.sh. "w" means write a new file, Note: will clobber files with the same name!
file.write("#!/bin/bash\n\n") # Write bash header to file

for root, dirs, files in os.walk("."): # Use os.walk to walk through all files and directories within the enclosing directory
	for name in files: # Iterate through all files within the enclosing directory
		s="awk '/No blast hit/' " # Initialize string s with awk command
		s2=" > "
		path=os.path.abspath('.') # Create string with absolute path to current folder
		if name.startswith("KI") and name.endswith("rep_set_tax_assignments.txt"): # If the file starts with KI and ends with rep_set.fasta ...
			name2 = name.split('.') # Split name to get only the sample number
			name3 = name.split('_')
			file = open("awk_no_blast_hits_KI_Platy.sh", "a") # open awk_no_blast_hits_KI_Platy.sh, "a" means it will be appending to the file, not overwriting what's already there
			file.write(s+path+"/"+name3[0]+"_otus_out/"+name+s2+path+"/"+name3[0]+"_otus_out/"+name3[0]+"_no_hits.txt"+"\n\n") # write to the file s (qiime command) plus the path/name of the file plus qiime text plus path/name plus qiime text plus \n an end of line character.


os.system("chmod u+x awk_no_blast_hits_KI_Platy.sh") # Give permissions so .sh file will run
#os.system("./awk_no_blast_hits_KI_Platy.sh") # Run .sh file that was just created