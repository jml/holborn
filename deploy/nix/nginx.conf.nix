{ frontend, proxy_port }:
let common-config = ''
        listen          443 ssl spdy;
        ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
        ssl_stapling on;
        ssl_stapling_verify on;
        ssl_trusted_certificate /etc/ssl/certs/ca-bundle.crt;
        ssl_prefer_server_ciphers on;
        ssl_ciphers "EECDH+ECDSA+AESGCM EECDH+aRSA+AESGCM EECDH+ECDSA+SHA384 EECDH+ECDSA+SHA256 EECDH+aRSA+SHA384 EECDH+aRSA+SHA256 EECDH E !aNULL !eNULL !LOW !3DES !MD5 !EXP !PSK !SRP !DSS";
        gzip            on;
        gzip_min_length 1000;
        gzip_proxied    expired no-cache no-store private auth;
        gzip_types      text/html text/css application/json;
    '';
in ''
events { }
http {
    server_names_hash_bucket_size  128;
    access_log syslog:server=unix:/dev/log;
    error_log syslog:server=unix:/dev/log;

    server {
        listen          80;
        server_name     _;

        location /.well-known/acme-challenge {
            root /var/www/challenges;
        }

        location / {
           return 301 https://$host$request_uri;
        }
    }

    server {
        server_name     norf.co;
        ssl_certificate /var/lib/acme/norf.co/fullchain.pem;
        ssl_certificate_key /var/lib/acme/norf.co/key.pem;
        ${common-config}

        location / {
            root ${frontend};
            # never cache index.html: It's tiny and contains pointers
            # to files that can be cached.
            expires -1;
            if_modified_since off;
        }
        # Static content is content addressed (by hash) so we can
        # cache forever.
        location /static {
            root ${frontend};
            expires max;
            add_header Pragma public;
            add_header Cache-Control "public";
            if_modified_since off;
        }

        location /v1 {
            proxy_pass         http://127.0.0.1:${proxy_port};
            proxy_redirect     off;
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header Host $host;
        }
    }
}
''
