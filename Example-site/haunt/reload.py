#!/home/jakob/.guix-profile/bin/python

from livereload import Server, shell
import os

site_output = "site"

server = Server()

for path in os.listdir("./"):
    if path != site_output:
        server.watch(path, "haunt build")

server.serve(root=site_output)
