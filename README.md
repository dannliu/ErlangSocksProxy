Erlang Socks proxy
==================
An Socks proxy for erlang

Please notice that:

1. Only support socks5.               (It will support socks4 soon)
2. Only support 'CONNECT' operation.  (It will support UDP and BIND)


User Guide
=================
1. Install Erlang, download from here http://www.erlang.org/download.html

   For Mac you can also run 'brew install erlang'

2. cd to the directory run  `erlc entry.erl`
3. run `erl -noshell -s entry start_socks [YOUR PORT]  or erl -noshell -s entry start_socks`



GUIDE FOR GFW(You know what I mean)
==================================
1. Follow the upon guide to delopy the ch_entry.erl on the server
2. Modify the ch_client to config the Local port and the server port and address.
3. Start ch_client.erl on your own computer. (client will entry data which will be sent to the server and decrypted)
4. Config the browser to use socks5 and address should be [you client address : local port]




