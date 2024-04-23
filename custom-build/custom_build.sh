#!/bin/bash -xv
# to run the script: sudo bash custom_build.sh <branch_name>
BRANCH_NAME=$1

BASE_DIRECTORY=$(pwd)
rm -rf imagebuilder
mkdir imagebuilder && cd imagebuilder


echo "-----Common Function: Start-------"
search_and_delete_line() {
    # Assign the function parameters to variables
    local search_text="$1"
    local file_path="$2"

    # Find the line number of the first occurrence of the search text
    local line_number=$(grep -n -m 1 "$search_text" "$file_path" | cut -d: -f1)

    if [ -z "$line_number" ]; then
        echo "Text '$search_text' not found in the file."
    else
        echo "First occurrence of '$search_text' found at line number: $line_number"
        # Comment out the line with the given line number
#        sed -i "" "${line_number}s~^~//~" "$file_path"
        sed -i "${line_number}d" "$file_path"
        echo "Line number $line_number is now deleted."
    fi

    echo "------------"
    # Display the first 10 lines of the file
    head -n 15 "$file_path"
    echo "------------"
}
echo "---Common Function: End---------"

echo "------clone and publish codepropertygraph: start--------"
git clone -b $BRANCH_NAME https://github.com/Privado-Inc/codepropertygraph
cd codepropertygraph
sbt publishM2 #&> log_file.txt
#get the first latest directory name
cpgVersionGenerated=$(ls -t ~/.m2/repository/io/shiftleft/codepropertygraph-domain-classes_3/ | head -n 1)
echo $cpgVersionGenerated
echo "------clone and publish codepropertygraph: end--------"


echo "----------Joern Start-----------"
cd $BASE_DIRECTORY/imagebuilder
#clone joern
git clone -b $BRANCH_NAME https://github.com/Privado-Inc/joern
cd joern
search_and_delete_line "val cpgVersion" "build.sbt"
echo "added new version of cpg in Joern"
cpg_version_statement="val cpgVersion = \"$cpgVersionGenerated\""

sed -i "5s/^/$cpg_version_statement\n/" "build.sbt"
echo "first 10 line from build.sbt"
head -n 15 "build.sbt"
echo "publishing jar for Joern"
sbt publishM2
joernVersionGenerated=$(ls -t ~/.m2/repository/io/joern/c2cpg_3/ | head -n 1)
joern_version_statement="val joernVersion = \"$joernVersionGenerated\""
echo "----------Joern End-----------"


echo "----------Privado-Core Start-----------"
cd $BASE_DIRECTORY/imagebuilder
git clone -b $BRANCH_NAME https://github.com/Privado-Inc/privado-core
cd privado-core
search_and_delete_line "val cpgVersion" "build.sbt"
echo "added new version of cpg in privado-core"
sed -i "10s/^/$cpg_version_statement\n/" "build.sbt"

search_and_delete_line "val joernVersion" "build.sbt"
echo "added new version of joern in privado-core"
sed -i  "11s/^/$joern_version_statement\n/" "build.sbt"
head -n 15 "build.sbt"

sbt universal:packageBin
echo "script ended successfully"
echo "----------Privado-Core End-----------"

echo "----------Build docker image: start-----------"
cd BASE_DIRECTORY

docker build -t privado-core:$BRANCH_NAME .

echo "----------Build docker image: end-----------"

#rm -rf $BASE_DIRECTORY/imagebuilder
