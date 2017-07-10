import os,sys # Importing necessary libraries

for root, dirs, files in os.walk("."): # Use os.walk to walk through all files and directories within the enclosing directory
	for name in files:
		#print(os.path.join(root,name))
		s=os.path.join(root,name) # Concatenate root directory and filename into 
		if name.endswith(".fastq.gz"): # If the file ends with .fastq.gz ...
			t=s.split('/') # Split s on '/' to index individual chunks of the array
			#print(t)
			v=t[2].split('-') # Split t on '-' to index individual chunks of the array
			#print(v)
			os.rename(root+'/'+name,'./sequences/fastq/'+v[0]+'.fastq.gz') # rename file from local path + name to the name of the parent directory + .fastq.gz

