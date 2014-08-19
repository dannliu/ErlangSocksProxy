ERLSocks
========
An Socks proxy of erlang language.


How To Use
==========
1. Copy the file to server and cd to the directory
2. erlc entry.erl
3. erl -noshell -s entry start_socks [YOUR PORT]  or erl -noshell -s entry start_socks
4. config chrom to use socks5 and address should be  [your server address : port] 


GUIDE FOR GFW(You know what I mean)
==================================
1. Follow the upon guide to delopy the ch_entry.erl on the server
2. Modify the ch_client to config the Local port and the server port and address.
3. Start ch_client.erl on your own computer. (client will entry data which will be sent to the server and decrypted)
4. Config the browser to use socks5 and address should be [you client address : local port]

