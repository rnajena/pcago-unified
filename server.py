#!/usr/bin/env python3

# SMPFPP (Shiny Multi Processing For Poor People) server
# 
# Because Shiny Pro Server which allows multiple processes costs money
# Implement a minimal HTTP server that manages multiple shiny server processes
# Based on example code by https://daanlenaerts.com/blog/2015/06/03/create-a-simple-http-server-with-python-3/

from http.server import BaseHTTPRequestHandler, HTTPServer

# SMPFPP server parameters
server_ip = "127.0.0.1"
server_port = 8081

# Shiny/R parameters

## A list of ports where the Shiny servers will run
shiny_ports = [8082, 8083, 8084]

## The directory of the Shiny app
shiny_app_dir = "."

## The R script that starts the Shiny server
## It will fetch e.g. the port from the ENV and runs Shiny
shiny_app_starter = "pcago.R"

## R executable
r_path = "/usr/bin/R"


# HTTPRequestHandler class
class SMPFPP_HTTPServer_RequestHandler(BaseHTTPRequestHandler):
    
    def send_no_slots(self):
        # Send response status code
        self.send_response(200)

        # Send headers
        self.send_header('Content-type','text/html')
        self.end_headers()

        # Send message back to client
        message = "No slots available. Try again a couple of minutes later."
        # Write content as utf-8 data
        self.wfile.write(bytes(message, "utf8"))
        return

    # GET
    def do_GET(self):
        #self.send_no_slots()
        
        self.send_response(301)
        self.send_header("Location", "http://google.de/")
        self.end_headers()
        
        return

def run():
  print('[i] Starting SMPFPP server ...')

  server_address = (server_ip, server_port)
  httpd = HTTPServer(server_address, SMPFPP_HTTPServer_RequestHandler)
  
  print('[i] Server is now listening.')
  httpd.serve_forever()


run() 
