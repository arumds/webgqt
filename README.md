# webGQT

webGQT is a shiny based web application of Genotype Query Tools (GQT) for querying large-scale genotype data converted into GQT database files. webGQT is made available to the users in three forms: 1) as a webserver available at https://vm1138.kaj.pouta.csc.fi/webgqt/ 2) as an R package to install on personal computers and 3) as part of the same R package to be configured on users own servers. This document provides the instructions to install on Linux/Mac OS computers or servers.


# Pre-requisites

webGQT applications depends on GQT and R.

1. **GQT** (Install GQT as per the instructions at https://github.com/ryanlayer/gqt)

2. Shiny server to host the shiny webGQT application.



# a) Installation on personal computer (Linux/Mac OS)

```
devtools::install_github('arumds/webGQT')
library(webGQT)
```

### Usage:

```
run_webgqt(gqt_path = '/path/to/gqt/bin/gqt')
```
gqt_path argument requires the path to the gqt binary created by installing GQT as stated in the pre-requisites.

# b) Installation on advanced server

### Step 1: Open R from terminal and install webGQT by the below command:

```
devtools::install_github('arumds/webGQT')
library(webGQT)
```

### Step 2: Copy the installed webGQT to the shiny-server site directory:

```
copy_shiny_gqt_app(copy_to = '/srv/shiny-server/')
```

By default, Shiny Server uses /srv/shiny-server/ as its site directory from which Shiny apps will be served automatically. Therefore, the app is copied to `/srv/shiny-server/` using the `copy_shiny_gqt_app()`  function available as part of the R package.

Now, in a web browser, navigate to the app’s web address. 

example.com:3838/webGQT

Replace example.com with the public IP address of the server. You should see the webGQT app installed successfully on your own server.


**Additional information to install Shiny server**

Install gdebi:
```
sudo apt install gdebi-core
```

Download Shiny Server:

```
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.6.875-amd64.deb
```

Use gdebi to install the Shiny Server package:

```
sudo gdebi shiny-server-1.5.6.875-amd64.deb
```

Allow the traffic to shiny server through port 3838
 
 ```
 sudo ufw allow 3838
 ```
The shiny-server service should be installed. To test, navigate to your IP address on port 3838 (e.g. example.com:3838). You should see the Shiny Server welcome page.


**Extra configuration to secure the app on user's server**

Additionally, the webapp can serve secured requests by configuring the local server with Nginx, a HHTP and reverse proxy web sever to forward incoming requests to Shiny Server by way of WebSocket.

**Install NGINX**

```sudo apt-get update
sudo apt-get install nginx
```

**Obtain SSL certificate for the domain**

```
sudo add-apt-repository ppa:certbot/certbot
sudo apt-get update
sudo apt-get install certbot
sudo ufw allow 80
sudo certbot certonly --standalone --preferred-challenges http -d example.com
```

The above command prompts to enter an email address and agree to the terms of service. After this, you should see a installation success message and where the certificates are stored.

Configure the shiny server to redirect the incoming requests to the server on ports 80 and 3838  to use HTTPS on port 443 by copying the below set of directives to the `/etc/nginx/sites-available/webGQT` file.

```
sudo nano /etc/nginx/sites-available/webGQT
```

```
server {
    server_name example.com;
    client_max_body_size 100G;
 #  return 301 https://$server_name$request_uri;
location / {
       proxy_pass http://youripaddress:3838;
       proxy_redirect http://youripaddress:3838/ https://$host/;
       proxy_http_version 1.1;
       proxy_set_header   Host $http_host;      
       proxy_set_header Upgrade $http_upgrade;
       proxy_set_header Connection $connection_upgrade;
       keepalive_timeout 40000;
       proxy_connect_timeout 10080s;
       proxy_send_timeout 10080;
       proxy_read_timeout 10080;
       proxy_buffer_size 64k;
       proxy_buffers 16 32k;
       proxy_busy_buffers_size 64k;
       proxy_redirect off;
       proxy_request_buffering off;
       proxy_buffering off;
   }

location /data {
                # First attempt to serve request as file, then
                # as directory, then fall back to displaying a 404.
                alias /mnt/data/;
                autoindex on;
                try_files $uri $uri/ =404;
        }

#    listen 80;
    #listen [::]:443 ssl ipv6only=on;
    listen 443 ssl;
    ssl_certificate /etc/letsencrypt/live/example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/example.com/privkey.pem;
    include /etc/letsencrypt/options-ssl-nginx.conf;
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem;
server {
   listen 80 ;
   server_name example.com;
   return 301 https://$host$request_uri;
}
```

Enable the new server block by creating a symbolic link for it in the /etc/nginx/sites-enabled directory.

```
sudo ln -s /etc/nginx/sites-available/webGQT /etc/nginx/sites-enabled/webGQT
```

Test the new configuration:

```
sudo nginx -t
```

If you run into any problems, follow the instructions in the output to resolve them.

Once the syntax is okay and the test is successful and can activate all changes by reloading Nginx and shiny-server.

```
sudo systemctl restart nginx

sudo systemctl restart shiny-server
```
**LICENSING**

Now the shiny server should be serving https requests on the domain example.com/webgqt by redirecting to https://example.com/webgqt



Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

