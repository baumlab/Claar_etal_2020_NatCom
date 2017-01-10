import os

#for () in os.walk() # possibly add path, now plan to run from within folder
#for filename in 

for root, dirs, files in os.walk(".", topdown=False);
	for name in files:
		print(os.path.join(root,name))
	for name in dirs:
		print(os.path.join(root,name))
