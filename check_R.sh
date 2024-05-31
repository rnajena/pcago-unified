#!/bin/bash

#
# Installs/checks the R version
#

expected_r_version="3.4.3"
installation_folder=$1
debian_check=$(which apt)

# Download URL for sources of R version 3.4.3
r_343_src_download_url="https://cran.r-project.org/src/base/R-3/R-3.4.3.tar.gz"

# Download URL of precompiled R version 3.4.3 for ubuntu 18.04
r_343_ubuntu1804_bin_url="https://github.com/rnajena/pcago-unified/releases/download/ubuntu-18.04/R-3.4.3-Ubuntu-1804.zip"

echo ">>>>>>> Checking R ..."

if [ -e $installation_folder/R ]; then
	echo ">>> R already exists. Nothing to do."
	exit
fi

# Try to install system R if not available
if [ "" == $(which R) ]; then

	if [ "" == "$debian_check" ]; then
		echo "You do not seem to run Ubuntu or Debian."
		echo ""
		echo "If available, install R version $expected_r_version"
		read -p "Press enter to continue"
	else
		available_system_r=$(apt-cache show r-base | grep "Version: $expected_r_version")
		if [ "" != "$available_system_r" ]; then
			read -p "R is not installed. Install it? [Y/N]" -n 1 -r
			echo    # (optional) move to a new line
			if [[ $REPLY =~ ^[Yy]$ ]]; then
				sudo apt install r-base
			fi
		fi
	fi
fi

# Try to use system R
if [ "" != $(which R) ]; then
	echo ">>> Found System R"
	echo "$(R --version)"

	system_r_check=$(R --version | grep "$expected_r_version")

	if [ "" == "$system_r_check" ]; then
		echo ">>> System R does not match!"

		read -p "Keep System R anyways? [Y/N]" -n 1 -r
		echo    # (optional) move to a new line
		if [[ $REPLY =~ ^[Yy]$ ]]; then
			ln -s "$(which R)" $installation_folder/R
			exit
		fi
	fi
fi

# If Ubuntu 18.04, offer precompiled packages (TODO: Does not work due to hardcoded paths)
# if [ -e "/etc/lsb-release" ]; then
#   source /etc/lsb-release
#   if [ "$DISTRIB_ID" == "Ubuntu" ] && [ "$DISTRIB_RELEASE" == "18.04" ]; then
#     read -p "You are using Ubuntu 18.04. We offer a precompiled package of R 3.4.3. Use the precompiled package? [Y/N]" -n 1 -r
#     echo    # (optional) move to a new line
#     if [[ $REPLY =~ ^[Yy]$ ]]; then
#       FOLDER=$(pwd)
#       cd $installation_folder
#       wget -nc $r_343_ubuntu1804_bin_url
#       unzip R-3.4.3-Ubuntu-1804.zip
#       ln -s ./R-3.4.3/bin/R R
#       cd $FOLDER
#       exit
#     fi
#   fi
# fi

# Try to compile R
read -p "Do you want to compile R version 3.4.3? [Y/N]" -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]; then
	./check_r_build_dependencies.sh
	FOLDER=$(pwd)
	cd $installation_folder
	mkdir src
	cd src
	wget -nc $r_343_src_download_url
	tar -xzvf R-3.4.3.tar.gz
	cd R-3.4.3
	./configure --prefix=$installation_folder/R-3.4.3 --enable-R-shlib --with-blas --with-lapack
	make
	make install
	cd ../..
	ln -s ./R-3.4.3/bin/R R
	cd $FOLDER
	exit
fi

read -p "Please insert the full path to the R executable: "
ln -s $REPLY $installation_folder/R
