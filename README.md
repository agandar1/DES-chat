# des-chat
This project was an assignment for a cryptography class. We had to make a chat server with DES encryption.<br>
We were allowed to use a library for the encryption, but I decided to do it from scratch for the fun of it. <br>
For running the server, we had to deploy it using NGROk. It should still work if you deploy it, but using local addresses/ports should work just as well.

## Description
This is a chat server with end to end DES encryption, written in Common Lisp.
I chose Common Lisp for this assignment because I like Lisp and I just wanted to have some fun with this project.
I have implemented DES encryption from scratch, using ECB mode of operation and simple padding to make sure we have all 64bit blocks.
I have confirmed it works by inputing encrypted messages (formatted in hex) and the key to this website http://des.online-domain-tools.com/ .
It then correctly decrypts the message, meaning the algorithm matches the one I wrote.
For the chat to be end to end encrypted, I only imported my DES code in the client file. 
The server never recieves plaintext from the client, not even usernames. All communication is encrypted, and decrypted by the clients themselves, while the server only serves to broadcast the encrypted messages.

## How to run it
Since Common Lisp isn't very well known, I have compiled and included binaries for windows (client.exe, server.exe) and linux (client, server).
I tested the linux ones on Arch Linux and Linux Mint. They both worked fine. 
I tested the windows ones on my laptop and a clean, freshly installed Windows 10 virtual machine. Both worked fine as well. 
I hope you do not have any issues running them on whatever OS you are on. I don't have a macbook to test, but the linux ones will probably work on there too. 
To run them, simply open a terminal and give it an address.

For example, On windows:

server.exe "localhost:8080"

client.exe "0.tcp.ngrok.io:14984"


On Linux:

./server "localhost:8080"

./client "0.tcp.ngrok.io:14984"

## How to compile it
This hopefully shouldn't be necessary since I included the binaries, but if you want to compile them yourself:
* Install SBCL (the common lisp implementation)
* Install Quicklisp, make sure to run the command ql:add-to-init-file when it tells you
* place des-chat folder inside ~/quicklisp/local-projects directory (or C:\Users\name\quicklisp\local-projects for windows)
* run the makefile (windows doesn't have make, so you'll have to copy paste the 2 commands into the terminal manually)
and that should be it.

## \*Note\*
I primarily wrote this on Linux, so that's where it works best. On windows for some reason the client might give an error after you type /quit .
I'm not sure what's causing this, but it doesn't affect the server at all, and it doesn't come up on linux. 
I'm assuming it's just a thing with the multi-threading on windows.
Also windows command line doesn't support ASCII escape codes, so I removed that part of the code on the windows version. It doesn't affect the functionality but it won't look as nice as it does on linux.
