Privado Core
============================================

Branch structure

main - This branch will contain the released version of the code.

dev - This branch will be used to merge all under-development features.

Local development steps while working on a common repository.



```
Steps to step up the project

- git clone git@github.com:Privado-Inc/privado-core.git
- cd privado-core
- git checkout -b dev origin/dev - This will checkout dev branch which is under development.
- git checkout -b <new local feature branch that you are going to work on.> - This will create a new local feature branch
- git push -u push origin <new remote feature branch keep the name same as local branch> - This will set up remote branch synced with your local branch.

Steps to integrate the pre-commit hook on local machine. 
1. Install the pre-commit app (https://pre-commit.com/#install)
   For MacOS using Homebrew, please use 
   - brew install pre-commit
2. Run the install command to have the script in .git/hooks for pre-commit
   - pre-commit install

Steps to follow while your work is in progress over a local branch.

- Whenever you are done with small logical units, commit the code to your local branch as well as push the changes to 
the respective remote feature branches.
- Also make sure to take the latest updates from the dev branch from time to time. This is to make sure your feature branch is not 
diverging a lot from the dev branch.
- Once you are done with the changes push your final changes to your feature remote branch and raise a pull request against
the "dev" branch. Assign two reviewers to review your changes (Note: don't directly merge your request to the dev branch.)

IDE Setup. 
- rm -rf .bsp/ .idea/
- find . -type d -name target | xargs rm -rf
- sbt compile Test/compile shell
- import the code with "BSP" option inside intelliJ
- run "stage" command inside sbt shell earlier executed. This will generate the required executable binaries

```

```
sbt stage
./privado-core -Dlog4j.configurationFile=log4j2.xml
```
```
If facing issues related to imports not working try
- File -> invalidate caches - Invalidate and restart
```


## License
Privado OSS is distributed under the GNU LESSER GENERAL PUBLIC LICENSE (LGPL 3.0).  This application may only be used in compliance with the License. In lieu of applicable law or written agreement, software distributed under the License is distributed "AS IS", VOID OF ALL WARRANTIES OR CONDITIONS. For specific details regarding permissions and restrictions, see [COPYING](/COPYING) and [COPYING.LESSER](/COPYING.LESSER).
