PCAGO-Electron
================

Runs [PCAGO](https://gitlab.com/rumangerst/pcago) as standalone desktop application
using the [Electron](https://electronjs.org/) framework.

[PCAGO on GitLab](https://gitlab.com/rumangerst/pcago) | [PCAGO on GitHub](https://github.com/rumangerst/pcago-electron) | [PCAGO-Electron on GitHub](https://github.com/rumangerst/pcago-electron/)

## Download

You can find prepackaged versions of PCAGO-Electron [here](https://github.com/rumangerst/pcago-electron/releases).

Following versions are available:

* Minimal: Only contains Electron.
* Standard: Contains Electron and ffmpeg
* Ubuntu: Contains Electron, ffmpeg and compiled dependencies for Ubuntu 16.04 LTS (with R 3.4.3)

If a version does *not* contain compiled dependencies, PCAGO-Electron will automatically compile 
the dependencies on first start. Please check if all required packages are installed.
Compilation will take a couple of minutes. The compiled dependencies can be re-used on other computers with the 
same operating system.

## Requirements

* libgconf2 (Debian/Ubuntu: libgconf2-4)
* Dependencies that are required by [PCAGO](https://gitlab.com/rumangerst/pcago)

## Running

We provide prepacked versions of PCAGO-Electron that you can download [here](https://github.com/rumangerst/pcago-electron/releases).
Download the appropriate version and run the included starter or just `electron.exe` (Windows)
/ `./electron` (Linux).

## Obtaining the source code

PCAGO-Electron requires PCAGO that has to be placed into the `pcago` folder.
If you are cloning the repository, you can run:
```
git clone --recursive https://github.com/rumangerst/pcago-electron.git
```

If you already cloned the PCAGO-Electron repository, you can let git automatically
download all submodules:
```
git submodule update --init --recursive
```

## Running from source

To run PCAGO-Electron from source, you need [npm](https://www.npmjs.com/).
Run `npm start` in the PCAGO-Electron folder to run the application.
If you use Linux, you should install `ffmpeg` (with MP4 support) from your
distribution's package sources. Otherwise you need to download a build of `ffmpeg` for
Linux and place it into the `ffmpeg-linux-x64` directory (see README file within this folder).

## Deploying PCAGO-Electron

To create prepacked versions of PCAGO-Electron, run `./deploy.sh`. This requires distributable versions of
Electron (electron-* directories), ffmpeg (ffmpeg-*) and R (R-*). See the respective README files for
more information.

## Used libraries/software

PCAGO-Electron uses and distributes [Electron](https://electronjs.org/) (Licensed under [MIT](https://github.com/electron/electron/blob/master/LICENSE))
and various dependency libraries located in `node_modules`. See the `license` file located each of the subfolders
for license information. 
PCAGO-Electron distributes [static ffmpeg builds](https://www.johnvansickle.com/ffmpeg/) licensed under 
[GPL-v3](http://www.gnu.org/licenses/gpl-3.0.en.html) and [ffmpeg for Windows](http://ffmpeg.zeranoe.com/builds/)
licensed under [GPL](http://www.gnu.org/licenses/).
Also included are sources of dependencies required by R that are located in `resources/app/pcago/packrat/src`.
See `resources/app/pcago/packrat/src/README` for information about their licenses.

The code and binary distribution of mentioned software belongs to their respective authors.