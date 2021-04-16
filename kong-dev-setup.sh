#!/usr/bin/env bash

brew update

### setup Mac environment
if ! command -v saml2aws &> /dev/null
then
    brew tap versent/homebrew-taps
    brew install saml2aws
fi

if ! command -v aws &> /dev/null
then
    brew install awscli
fi

tools=("kubectl" "kubectx")

for tool in ${tools[*]}; do
    if ! command -v "${tool}" &> /dev/null
    then
    echo "Installing ${tool}"
    brew install "$tool"
    fi
done

echo "Done"