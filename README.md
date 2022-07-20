Privado Core
=============================================

A program that makes use of Joern to create a CPG and list all method
names:

```
Steps to step up the project
- rm -rf .bsp/ .idea/
- find . -type d -name target | xargs rm -rf
- sbt compile Test/compile shell
- import the code with "BSP" option inside intelliJ
- run "stage" command inside sbt shell earlier executed. This will generated the required executable binaries

```

```
sbt stage
./privado-core -Dlog4j.configurationFile=log4j2.xml
```
