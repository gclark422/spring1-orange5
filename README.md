# spring1-orange5
Spring 1 Orange 5 HW repo

## Current assignments:

| Done?                    | Assignment               | Due Date |
|--------------------------|--------------------------|----------|
| :heavy_check_mark:       | Financial Analytics HW 1 |   1/24   |
| :heavy_multiplication_x: | Simulation & Risk HW 1   |   2/5    |
| :heavy_multiplication_x: | Optimization Project     |   2/7    |
| :heavy_multiplication_x: | Simulation & Risk HW 2   |   2/17   |
| :heavy_multiplication_x: | Simulation & Risk HW 3   |   2/26   |
| :heavy_multiplication_x: | Financial Analytics HW 2 |   3/23   |




# Basic Setup Instructions (Command line - Use Git Bash)
 - After being added as a collaborator, clone the repo where you want it
 - You only need to do this **once**
 ```
 git clone https://github.com/gclark422/spring1-orange5.git
 ```  
 
 - Make any changes/add any files then do the following:
 ```
 git status
 ```
 - This will show useful output - for instance any files that you added, or changes you made to files (in red)
 - Then when you're ready to commit:
 ```
 git add --all
 git commit -m "add meaningful message here"
 git push origin master
 ```
 - This will push your changes to the master branch.
 - **IF this fails** then you first need to pull because it means you dont have the latest code from GitHub
 ```
 git pull origin master
 ```
 Then you can push :)
 
 # 2 useful command line commands
 `cd` - This means 'change directory'. Used as follows:
 ```
 cd [path or sub directory]
 ```
 Example:
 ```
 cd /users/Grant/Desktop
 ```
 This will navigate from my current position to desktop (assuming desktop is in a 'deeper' directory than the current)
 Another example to navigate 'up' or to the parent directory:
 ```
 cd ..
 ```
 ____
 `ls` - The 'list' command. This will list contents of a directory
 Example:
 ```
 ls -al
 ```
 This will show all files and subdirectories within the current working directory.
