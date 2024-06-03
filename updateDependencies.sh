#!/usr/bin/env bash
NON_INTERACTIVE_OPTION=$1
DEPENDENCY=$2

check_installed() {
  if ! type "$1" > /dev/null; then
    echo "Please ensure you have $1 installed."
    exit 1
  fi
}

check_installed curl

# check if xmllint is installed
if type jq > /dev/null; then
  USE_JQ=1 #true
else
  echo "warning: jq is not installed - will try with 'grep' as a fallback..."
  USE_JQ=0 #false
fi

declare -A repos=(
  [cpg]=https://api.github.com/orgs/Privado-Inc/packages/maven/io.shiftleft.codepropertygraph-schema_3/versions
  [joern]=https://api.github.com/orgs/Privado-Inc/packages/maven/io.joern.console_3/versions
  [overflowdb]=https://api.github.com/orgs/Privado-Inc/packages/maven/io.shiftleft.overflowdb-core_3/versions
)

function latest_version {
  local NAME=$1
  local REPO_URL=${repos[$NAME]}
  local CURL_PARAMS="--silent --show-error"
  local TOKEN=$(printenv GITHUB_TOKEN) # important to add this to an environment variable as the package registry is private

  if (( $USE_JQ ))
  then
    curl $CURL_PARAMS -L \
                       -H "Accept: application/vnd.github+json" \
                       -H "Authorization: Bearer ${TOKEN}" \
                       -H "X-GitHub-Api-Version: 2022-11-28" \
                       "$REPO_URL" | jq '[.[]|select(.name)][0]|.name' | sed 's/[",[:space:]]//g' # get the latest version, and extract the version number
  else
    curl $CURL_PARAMS -L \
                   -H "Accept: application/vnd.github+json" \
                   -H "Authorization: Bearer ${TOKEN}" \
                   -H "X-GitHub-Api-Version: 2022-11-28" \
                   "$REPO_URL" | grep -m 1 "\"name\":" | sed 's/[",[:space:]]//g' | awk '{split($0,a,":"); print a[2]}' # use grep to get the first occurrence of the name key which contains the latest version
  fi
}

function update {
  local NAME=$1
  if [[ -z "${repos[$NAME]}" ]]; then
    echo "error: no repo url defined for $NAME"
    exit 1;
  fi

  local VERSION=$(latest_version $NAME)
  local SEARCH="val ${NAME}Version\([ ]*\)= .*"
  local OLD_VERSION=$(grep "$SEARCH" build.sbt | sed 's/.*"\(.*\)"/\1/')

  if [ "$VERSION" == "$OLD_VERSION" ]
  then
    echo "$NAME: unchanged ($VERSION)"
  else
    local REPLACE="val ${NAME}Version\1= \"$VERSION\""

    if [ "$NON_INTERACTIVE_OPTION" == "--non-interactive" ]
    then
      echo "non-interactive mode, auto-updating $NAME: $OLD_VERSION -> $VERSION"
      sed -i "s/$SEARCH/$REPLACE/" build.sbt
    else
      echo "update $NAME: $OLD_VERSION -> $VERSION? [Y/n]"
      read ANSWER
      if [ -z $ANSWER ] || [ "y" == $ANSWER ] || [ "Y" == $ANSWER ]
      then
        sed -i "s/$SEARCH/$REPLACE/" build.sbt
      fi
    fi
  fi
}

if [ "$DEPENDENCY" == "" ]; then
  update cpg
  update joern
  update overflowdb
else
  DEPENDENCY="${DEPENDENCY#--only=}"
  update $DEPENDENCY
fi
