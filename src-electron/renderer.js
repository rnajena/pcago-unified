const remote = require('electron').remote;

const path = require('path');
const URL = require('url');
const { spawn } = require('child_process');
const isReachable = remote.require('is-reachable');
const {TextDecoder} = require('text-encoding');
const app_console = remote.getGlobal('console');
const webview = document.querySelector("#pcago_webview");
const {ipcRenderer} = require('electron');

const uistates = { loading : 0, pcago : 1, error : 2 }

var rsession_process = null;
var pcago_url = null;
var pcago_log = "";
var current_uistate = null;

function updateLoadingStatusMessage(message) {
    if(message.trim().length > 0) {

        if(message.indexOf("The following object is masked from") > -1) {
            return;
        }
        if(message.indexOf("The following objects are masked from") > -1) {
            return;
        }

        if(message.indexOf("[1/") > -1) {
            document.getElementById("progress").textContent = "Installing dependencies ...";
        }
        else if(message.indexOf("[3/") > -1) {
            document.getElementById("progress").textContent = "Starting PCAGO ...";
        }

        document.getElementById("status").textContent = message;
    }
}

function switchToUI(uistate) {

    current_uistate = uistate;

    document.getElementById("loading-ui").setAttribute("style", "display: none;");
    document.getElementById("error-ui").setAttribute("style", "display: none;");
    document.getElementById("pcago_webview").setAttribute("style", "height: 0%;");

    if(uistate == uistates.loading) {
        document.getElementById("loading-ui").setAttribute("style", "display: flex;");
    }
    else if(uistate == uistates.pcago) {
        document.getElementById("pcago_webview").setAttribute("style", "height: 100%;");
    }
    else if(uistate == uistates.error) {
        document.getElementById("error-ui").setAttribute("style", "display: flex;");
        document.getElementById("error-log").textContent = pcago_log;
    }

}

function setPCAGOUrl(url) {
    pcago_url = url;
    app_console.log("PCAGO listens on " + url);

    isReachable(url, { timeout : 50000 }).then(reachable => {
        if(reachable) {
            app_console.log("PCAGO seems to be online.");
            webview.loadURL(url);
            switchToUI(uistates.pcago);
        }
        else {
            app_console.log("PCAGO timed out!");
            pcago_log += "\nPCAGO timed out!";
            switchToUI(uistates.error);
        }
    })
}


function start_rsession() {

    // start R with PCAGO
    if(remote.process.platform == "linux") {
        //rsession_process = spawn('Rscript', ["--vanilla", path.join(__dirname, 'pcago_starter_linux.R'), path.join(__dirname, 'pcago')]);
        rsession_process = spawn('bash', [path.join(__dirname, 'pcago_starter_linux.sh'), path.join(__dirname, 'pcago')]);
    }
    else if(remote.process.platform == "win32") {
        rsession_process = spawn('Rscript', ["--vanilla", path.join(__dirname, 'pcago_starter_win32.R'), path.join(__dirname, 'pcago')]); //TODO start correct R
    }
    else {
        pcago_log = "Platform " + remote.process.platform + " not supported!";
        switchToUI(uistates.error);
        return;
    }


    rsession_process.stdout.on('data', (data) => {
        message = new TextDecoder("utf-8").decode(data);
        updateLoadingStatusMessage(message);
        app_console.log(`${data}`);
        pcago_log += message;
    });

    rsession_process.stderr.on('data', (data) => {
        app_console.log(`${data}`);

        message = new TextDecoder("utf-8").decode(data);
        updateLoadingStatusMessage(message);

        // Listen for the shiny server instance
        if(message.indexOf("Listening on") > -1) {
            setPCAGOUrl(message.slice("Listening on ".length).trim());
        }

        pcago_log += message;

    });

    rsession_process.on('close', (code) => {
        app_console.log(`R session process exited with code ${code}`);
        message = `R session process exited with code ${code}`;
        pcago_log += message;

        switchToUI(uistates.error);
    });

    rsession_process.on('error', (error) => {
        app_console.log(`Error while starting R session: ${error}`);
        message = `Error while starting R session: ${error}`;
        pcago_log += message;

        switchToUI(uistates.error);
    });
}

remote.getCurrentWindow().on("close", () => {
   if(rsession_process != null) {
       rsession_process.kill();
   }
});

webview.addEventListener("dom-ready", () => {
    if(rsession_process == null) {
        start_rsession();
    }
});

ipcRenderer.on("menu_new", function(event, data) {
    if(rsession_process != null && pcago_url != null && current_uistate == uistates.pcago) {
        webview.reload();
    }
});

ipcRenderer.on("menu_open_browser", function(event, data) {
    if(rsession_process != null && pcago_url != null && current_uistate == uistates.pcago) {
        require('electron').shell.openExternal(pcago_url);
    }
});

switchToUI(uistates.loading);
