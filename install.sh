#!/bin/bash

#
# Installs PCAGO and PCAGO-electron into some folder
# 

PACKRAT_DEPENDENCIES_URL="https://github.com/rumangerst/pcago-unified/releases/download/dependencies/packrat.zip"
PACKRAT_BINARIES_UBUNTU1804_URL="https://github.com/rumangerst/pcago-unified/releases/download/ubuntu-18.04/packrat-Ubuntu-1804.zip"
CURRENT_DIR=$(pwd)

read -p "Please enter the folder where PCAGO will be installed: " installation_folder
installation_folder=$(realpath $installation_folder)
echo "PCAGO will be installed into $installation_folder"
read -p "Press enter to continue"

mkdir -p $installation_folder
mkdir -p $installation_folder/R
mkdir -p $installation_folder/pcago
mkdir -p $installation_folder/pcago-electron

# Check all dependencies
./check_dependencies.sh
./check_R.sh $installation_folder/R

if [ "" == "$($installation_folder/R/R --version | grep '3.4.3')" ]; then
	echo "Something seems to be wrong with the R installation in $installation_folder/R/R"
	echo "Canceling."
	exit
fi

# For later: Use precompiled packrat dependencies
if [ -e "/etc/lsb-release" ]; then
  source /etc/lsb-release
  if [ "$DISTRIB_ID" == "Ubuntu" ] && [ "$DISTRIB_RELEASE" == "18.04" ]; then
    read -p "You are using Ubuntu 18.04. We offer precompiled R packages. Use the precompiled packages? [Y/N]" -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      PACKRAT_DEPENDENCIES_URL=$PACKRAT_BINARIES_UBUNTU1804_URL
    fi
  fi
fi

# Automated part

echo "Preparation finished. The next steps will run automatically if everything goes well."
echo "This take some time, so get some coffee!"
read -p "(But first) press enter to continue"

# Copy over PCAGO-electron and PCAGO
echo ">>>>>>> Installing PCAGO and PCAGO-Electron ..."
cp -rv ./src/* $installation_folder/pcago
cp -rv ./src-electron/* $installation_folder/pcago-electron
ln -s $installation_folder/pcago $installation_folder/pcago-electron
ln -s $installation_folder/R/R $installation_folder/pcago/
ln -s $installation_folder/R/R $installation_folder/pcago-electron/

# Download dependencies
echo ">>>>>>> Downloading R packages ..."
cd $installation_folder/pcago/packrat
rm -rv src
wget -nc $PACKRAT_DEPENDENCIES_URL -O packrat.zip
unzip -o packrat.zip
cd $CURRENT_DIR

# Initial packrat restore (so we don't have to do it later on!)
echo ">>>>>>> Initial dependency initialization (1/2) ..."
cd $installation_folder/pcago/
$installation_folder/R/R --vanilla -f packrat/init.R --args --bootstrap-packrat &
pid=$!
trap "kill $pid" TERM
wait

echo ">>>>>>> Initial dependency initialization (2/2) ..."
echo "
source('packrat/init.R')
packrat::restore()
" | $installation_folder/R/R --vanilla &

pid=$!
trap "kill $pid" TERM
wait

cd $CURRENT_DIR

# NPM install

echo ">>>>>>> NPM initialization ..."

cd $installation_folder/pcago-electron/
npm install
cd $CURRENT_DIR

# Install starter files

cat > $installation_folder/pcago-server.sh <<EOL
#!/bin/bash

# 
# Runs a PCAGO server in terminal
#

cd pcago
echo -e "source('packrat/init.R')\nshiny::runApp()" | ./R --vanilla

EOL

cat > $installation_folder/pcago-electron.sh <<EOL
#!/bin/bash

# 
# Runs a PCAGO electron application
#

cd pcago-electron
npm start

EOL

chmod +x $installation_folder/pcago-electron.sh
chmod +x $installation_folder/pcago-server.sh


cp icon.svg $installation_folder
cat > $installation_folder/PCAGO.desktop <<EOL
[Desktop Entry]
Name=PCAGO
Exec=$installation_folder/pcago-electron.sh
Path=$installation_folder
Icon=$installation_folder/icon.svg
Type=Application
Categories=Science;
EOL
