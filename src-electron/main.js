// const electron = require('electron');
// // Module to control application life.
// const app = electron.app;
// // Module to create native browser window.
// const BrowserWindow = electron.BrowserWindow;

const {app, Menu, BrowserWindow} = require('electron');

const path = require('path');
const URL = require('url');
const {spawn} = require('child_process');
const isReachable = require('is-reachable');
const {dialog} = require('electron');
const {TextDecoder} = require('text-encoding');
const {ipcMain} = require('electron');

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow = null;

// Main menu template

menu_new = function(menuItem, currentWindow) {
    currentWindow.webContents.send("menu_new");
};

menu_exit = function(menuItem, currentWindow) {
    currentWindow.close();
};

menu_report_issue = function(menuItem, currentWindow) {
    require('electron').shell.openExternal("https://github.com/rumangerst/pcago-electron/issues");
};

menu_report_source_code = function(menuItem, currentWindow) {
    require('electron').shell.openExternal("https://github.com/rumangerst/pcago-electron");
};

menu_show_devtools = function(menuItem, currentWindow) {
    currentWindow.webContents.openDevTools();
};

menu_open_browser = function(menuItem, currentWindow) {
    currentWindow.webContents.send("menu_open_browser");
};

const menu_template = [
    {
        label : "File",
        submenu : [
            { label : "New dataset", role : "new", click : menu_new },
            { label : "Close", role : "close", click : menu_exit }
        ]
    },
    {
        label : "Tools",
        submenu : [
            { label : "Open developer console", role : "show_devtools", click : menu_show_devtools },
            { label : "Open PCAGO in browser", role : "open_browser", click : menu_open_browser }
        ]
    },
    {
        label : "Help",
        submenu : [
            { label : "Report issue", role : "report_issue", click : menu_report_issue },
            { label : "Source code", role : "website", click : menu_report_source_code }
        ]
    }
];

function createWindow() {
    // Create the browser window.
    mainWindow = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            nodeIntegration: true,
            devTools: true
        },
        icon : "img/icon.png"
    });
    mainWindow.maximize();

    const menu = Menu.buildFromTemplate(menu_template);
    mainWindow.setMenu(menu);

    // and load the index.html of the app.
    mainWindow.loadURL(URL.format({
        pathname: path.join(__dirname, 'content.html'),
        protocol: 'file:',
        slashes: true
    }));

    mainWindow.$ = mainWindow.jQuery = require('jquery');

    // Open the DevTools.
    //mainWindow.webContents.openDevTools();

    // Emitted when the window is closed.
    mainWindow.on('closed', function () {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        mainWindow = null
    });

    mainWindow.webContents.on('new-window', function (e, url) {
        e.preventDefault();
        require('electron').shell.openExternal(url);
    });
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow);

// Quit when all windows are closed.
app.on('window-all-closed', function () {
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
    if (process.platform !== 'darwin') {
        app.quit()
    }
});

app.on('activate', function () {
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (mainWindow === null) {
        createWindow()
    }
});

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
