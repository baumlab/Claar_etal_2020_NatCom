import os,sys,re # Importing necessary libraries

for root, dirs, files in os.walk("."): # Use os.walk to walk through all files and directories within the enclosing directory
	for name in files:
		#print(os.path.join(root,name))
		s=os.path.join(root,name) # Concatenate root directory and filename into 
		if name.endswith(".fastq.gz"): # If the file ends with .fastq ...
			if re.match(".*_R1_.*",name):
				t=s.split('/') # Split t on '/' to index individual chunks of the array
				v=t[1].split('-') # Split v on '-' to index individual chunks of the array
				os.rename(root+'/'+name,'./fastq/'+v[0]+'_R1.fastq.gz') # rename file from local path + name to the name of the parent directory + .fasta
			elif re.match(".*_R2_.*",name):
				t=s.split('/') # Split t on '/' to index individual chunks of the array
				v=t[1].split('-') # Split v on '-' to index individual chunks of the array
				os.rename(root+'/'+name,'./fastq/'+v[0]+'_R2.fastq.gz') # rename file from local path + name to the name of the parent directory + .fasta