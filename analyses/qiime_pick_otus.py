import os,sys # Importing necessary libraries

file = open("qiime_pick_otus_KI_Platy.sh", "w") # Initialize new file qiime_pick_otus_KI_Platy.sh. "w" means write a new file, Note: will clobber files with the same name!
file.write("#!/bin/bash\n\nsource /macqiime/configs/bash_profile.txt\n\n") # Write bash header to file

for root, dirs, files in os.walk("."): # Use os.walk to walk through all files and directories within the enclosing directory
	for name in files: # Iterate through all files within the enclosing directory
		s="pick_otus.py -i " # Initialize string s with qiime command
		s2=" -m uclust -s 0.97 -o "
		s3="_otus_out"
		path=os.path.abspath('.') # Create string with absolute path to current folder
		if name.startswith("KI") and name.endswith("fasta"): # If the file starts with KI and ends with fasta ...
			name2 = name.split('.') # Split name to get only the sample number
			file = open("qiime_pick_otus_KI_Platy.sh", "a") # open qiime_pick_otus_KI_Platy.sh, "a" means it will be appending to the file, not overwriting what's already there
			file.write(s+path+"/"+name+s2+path+"/"+name2[0]+s3+"\n\n") # write to the file s (qiime command) plus the path/name of the file plus qiime text plus path/name plus qiime text plus \n an end of line character.


os.system("chmod u+x qiime_pick_otus_KI_Platy.sh") # Give permissions so .sh file will run
#os.system("./qiime_pick_otus_KI_Platy.sh") # Run .sh file that was just created