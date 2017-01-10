import os,sys # Importing necessary libraries

file = open("qiime_assign_taxonomy_KI_Platy.sh", "w") # Initialize new file qiime_assign_taxonomy_KI_Platy.sh. "w" means write a new file, Note: will clobber files with the same name!
file.write("#!/bin/bash\n\nsource /macqiime/configs/bash_profile.txt\n\n") # Write bash header to file

for root, dirs, files in os.walk("."): # Use os.walk to walk through all files and directories within the enclosing directory
	for name in files: # Iterate through all files within the enclosing directory
		s="assign_taxonomy.py -i " # Initialize string s with qiime command
		s2=" -r /Users/Danielle/Documents/Data_Analysis/Resources/ITS2_Database_04_23_13.fas -m blast -e 1e-20 -t /Users/Danielle/Documents/Research/HIMB_2014/AmSam/AmSam_Data_Files/ReferenceData/id_to_taxonomy_subtype_mod.txt -o "
		path=os.path.abspath('.') # Create string with absolute path to current folder
		if name.startswith("KI") and name.endswith("rep_set.fasta"): # If the file starts with KI and ends with rep_set.fasta ...
			name2 = name.split('.') # Split name to get only the sample number
			name3 = name.split('_')
			file = open("qiime_assign_taxonomy_KI_Platy.sh", "a") # open qiime_assign_taxonomy_KI_Platy.sh, "a" means it will be appending to the file, not overwriting what's already there
			file.write(s+path+"/"+name3[0]+"_otus_out/"+name2[0]+".fasta"+s2+path+"/"+name3[0]+"_otus_out/"+"\n\n") # write to the file s (qiime command) plus the path/name of the file plus qiime text plus path/name plus qiime text plus \n an end of line character.


os.system("chmod u+x qiime_assign_taxonomy_KI_Platy.sh") # Give permissions so .sh file will run
#os.system("./qiime_assign_taxonomy_KI_Platy.sh") # Run .sh file that was just created