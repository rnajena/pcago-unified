#!/bin/bash

current_dir=$(pwd)
target_dir=$(mktemp -d)

mkdir -p $target_dir

echo "Target directory $target_dir"

# Copy all relevant files
cp -r node_modules $target_dir

for f in $(git ls-tree -r master --name-only); do
    cp -r --parents $f $target_dir
done

# Clean up unwanted packrat files
rm -r $target_dir/pcago/packrat/lib*

# Distribution for linux
target_dir_linux=$(mktemp -d)
echo "Target directory for linux $target_dir_linux"

mkdir -p "$target_dir_linux/resources/app"
cp -r $target_dir/* "$target_dir_linux/resources/app"
cp -r electron-linux-x64/* $target_dir_linux
cp -r ffmpeg-linux-x64 $target_dir_linux/resources/app

cat << 'EOF' >> "$target_dir_linux/run.sh"
#!/bin/bash
./electron
EOF

chmod +x "$target_dir_linux/run.sh"

# Standard distribution (Electron + ffmpeg, but no precompiled libraries)

cd $target_dir_linux
rm $current_dir/pcago_electron_linux-x64-standard.zip
zip -r $current_dir/pcago_electron_linux-x64-standard.zip .
cd $current_dir

# Distribution for Ubuntu 16.04 (Electron + ffmpeg + precompiled libraries)

cd $target_dir_linux

rm -r $target_dir_linux/resources/app/pcago/packrat/lib*
cp -r $current_dir/packrat-Ubuntu-16.04-x64/lib $target_dir_linux/resources/app/pcago/packrat/

rm $current_dir/pcago_electron_linux-x64-ubuntu.zip
zip -r $current_dir/pcago_electron_linux-x64-ubuntu.zip .
cd $current_dir

# Minimal distribution for linux (only electron prepackaged)
cd $target_dir_linux

rm -r $target_dir_linux/resources/app/ffmpeg-linux-x64
rm -r $target_dir_linux/resources/app/pcago/packrat/lib*

rm $current_dir/pcago_electron_linux-x64-minimal.zip
zip -r $current_dir/pcago_electron_linux-x64-minimal.zip .
cd $current_dir

# Clear tmp
rm -r $target_dir
rm -r $target_dir_linux
