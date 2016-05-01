{ backendURL, serverName, challengeDir }:
let common-config = ''
        listen          443 ssl spdy;
        ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
        ssl_stapling on;
        ssl_stapling_verify on;
        ssl_trusted_certificate /etc/ssl/certs/ca-bundle.crt;
        ssl_prefer_server_ciphers on;
        ssl_ciphers "EECDH+ECDSA+AESGCM EECDH+aRSA+AESGCM EECDH+ECDSA+SHA384 EECDH+ECDSA+SHA256 EECDH+aRSA+SHA384 EECDH+aRSA+SHA256 EECDH E !aNULL !eNULL !LOW !3DES !MD5 !EXP !PSK !SRP !DSS";
    '';
in ''
events { }
http {
    server_names_hash_bucket_size  128;
    access_log syslog:server=unix:/dev/log;
    error_log syslog:server=unix:/dev/log;

    map $http_upgrade $connection_upgrade {
        default upgrade;
        "" close;
    }

    server {
        listen          80;
        server_name     _;

        location /.well-known/acme-challenge {
            root ${challengeDir};
        }

        location / {
           return 301 https://$host$request_uri;
        }
    }

    server {
        server_name     ${serverName};
        ssl_certificate /var/lib/acme/${serverName}/fullchain.pem;
        ssl_certificate_key /var/lib/acme/${serverName}/key.pem;
        ${common-config}

        location / {
            proxy_pass         ${backendURL};
            proxy_redirect     off;
            proxy_http_version 1.1;
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header Host $host;
        }

        # Buildbot uses web sockets. We need to forward the connection upgrade
        # requests. It's *really* important that you don't add a trailing
        # slash here.
        location /ws {
            proxy_pass         ${backendURL}/ws;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            # Recommended setting from
            # http://buildbot.readthedocs.io/en/latest/manual/cfg-www.html?highlight=nginx#reverse-proxy-configuration
            proxy_read_timeout 6000;
        }
    }
}
''
