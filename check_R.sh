#!/bin/bash

#
# Installs/checks the R version
#  

expected_r_version="3.4.3"
installation_folder=$1
debian_check=$(which apt)

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


# Try to compile R
read -p "Do you want to compile R version 3.4.3? [Y/N]" -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]; then
	./check_r_build_dependencies.sh
	FOLDER=$(pwd)
	cd $installation_folder
	mkdir src
	cd src
	wget -nc https://cran.r-project.org/src/base/R-3/R-3.4.3.tar.gz
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



