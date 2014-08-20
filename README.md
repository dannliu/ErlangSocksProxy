Erlang Socks proxy
==================
An Socks proxy for erlang

Please note that:

1. Only support **socks5**           (will support socks4 soon)
2. Only support **TCP**              (will support UDP soon)


User Guide
=================
1. Install Erlang on your server, download from here http://www.erlang.org/download.html
2. Modify the socks.config
   - Config the server listening port
3. Run start.sh
4. Config your applications to use your socks5 proxy. The address is [Your Server Address : Port]



User Guide For GFW(You know what I mean)
==================================
Refer to the User Guide for step 1

2. Modify the gfw_socks.config
   - Config the server listening port
   - Config the server address [Please pay attention for the address format,{127,0,0,1}, separated by comma]
   - Config the client listening port
3. Run sh start_server.sh **on your server**
4. Run sh start_client on **your own machine**
5. Config your application to user your socks5 proxy. The address is [Your local address : client_port]


Brief Introduction of Erlang
============================

Erlang was designed from the bottom up to program concurrent, distributed, fault-tolerant, scalable, soft, real-time systems. 

Refer to the home page for more information: http://erlang.org


