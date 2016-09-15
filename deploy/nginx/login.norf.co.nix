{
  forceSSL = true;
  enableACME = true;
  locations."/" = {
    proxyPass = "http://127.0.0.1:5556";
  };
}
