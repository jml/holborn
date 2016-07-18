Some random research that illustrates various bits of data sent around between dex and holborn-proxy.

# URLs
```
{
   "response_types_supported" : [
      "code"
   ],
   "grant_types_supported" : [
      "authorization_code",
      "client_credentials"
   ],
   "subject_types_supported" : [
      "public"
   ],
   "id_token_signing_alg_values_supported" : [
      "RS256"
   ],
   "authorization_endpoint" : "http://norf.co:5556/auth",
   "token_endpoint_auth_methods_supported" : [
      "client_secret_basic"
   ],
   "issuer" : "http://norf.co:5556",
   "jwks_uri" : "http://norf.co:5556/keys",
   "token_endpoint" : "http://norf.co:5556/token"
}
```

# Handshake

Keys returned by dex (as bytestring):

```
{u'keys': [{u'alg': u'RS256',
   u'e': u'AQAB',
   u'kid': u'ueLhCNO7xKffkMNQ5qrsZB-RsRtH3GSX3Mx_XACJ-ugvXCvU2SOD7URoWjvEMUN3e6GbXlYRxVjB_qyl_3TzFGBq3udqmz_01t66BVrRY4-PY3FP8kpud1gTHp__Uv7RHFN5L-VdO45Z-S4eGIGfoIi7eoB_Ndxiwykqf8UQRMZRukiMrpmwY76B-8xph3QBAybUNGCrIqhyEQ_bkWQaJreBjifia_gbP2EvNmhynZxjSdvuSol2XYCTG1NNrGGSkY1y9vSL467zoTlQ0FHoDCFdeN7JuOnVwkQm93TXs_uavAgnUzLK1FQnNB5oG9WarLpRjH9JYBfP_DMuts9A_Q==',
   u'kty': u'RSA',
   u'n': u'ueLhCNO7xKffkMNQ5qrsZB-RsRtH3GSX3Mx_XACJ-ugvXCvU2SOD7URoWjvEMUN3e6GbXlYRxVjB_qyl_3TzFGBq3udqmz_01t66BVrRY4-PY3FP8kpud1gTHp__Uv7RHFN5L-VdO45Z-S4eGIGfoIi7eoB_Ndxiwykqf8UQRMZRukiMrpmwY76B-8xph3QBAybUNGCrIqhyEQ_bkWQaJreBjifia_gbP2EvNmhynZxjSdvuSol2XYCTG1NNrGGSkY1y9vSL467zoTlQ0FHoDCFdeN7JuOnVwkQm93TXs_uavAgnUzLK1FQnNB5oG9WarLpRjH9JYBfP_DMuts9A_Q==',
   u'use': u'sig'},
  {u'alg': u'RS256',
   u'e': u'AQAB',
   u'kid': u'12wCK0kaqWFRHavgPkC260_pOgsmAAKamSJ38XUo9PW8SMEMhHWlTKFGfBFwo0fFBL5VmDek1zMlWA8paVpqVZgCbxi4c2UZ9LscjUFVxc4YBnjJ_rIbLDIQYdiukpA3BUkz2xdGxASYy4pHGa7ULo3pAdjB-a9YKo25GsBTBUCKf62RHlxMo-odk7W6J8P9jOUPlfVttq9ZeISVTh4AQvzCBBmLoO5AOx0ZnApPof2bWjJMtpS23vSbOk31XytEaksAshZNK09hG-Kbtzm8HIWipt4aIFqQ4-g3dsexJMrAemiiAvqnrLiCDHzsWLn1E0dc6KXOqeroaBjoFQo-fQ==',
   u'kty': u'RSA',
   u'n': u'12wCK0kaqWFRHavgPkC260_pOgsmAAKamSJ38XUo9PW8SMEMhHWlTKFGfBFwo0fFBL5VmDek1zMlWA8paVpqVZgCbxi4c2UZ9LscjUFVxc4YBnjJ_rIbLDIQYdiukpA3BUkz2xdGxASYy4pHGa7ULo3pAdjB-a9YKo25GsBTBUCKf62RHlxMo-odk7W6J8P9jOUPlfVttq9ZeISVTh4AQvzCBBmLoO5AOx0ZnApPof2bWjJMtpS23vSbOk31XytEaksAshZNK09hG-Kbtzm8HIWipt4aIFqQ4-g3dsexJMrAemiiAvqnrLiCDHzsWLn1E0dc6KXOqeroaBjoFQo-fQ==',
   u'use': u'sig'}]}
```

# output from jose:

ClaimsSet {_claimIss = Just (OrURI http://norf.co:5556), _claimSub = Just (Arbitrary "6cd8a3a5-5631-40ff-907a-b602e97f3f07"), _claimAud = Just (Special (Arbitrary "HWBN_c1uNIItwm-gamnkZEhYeKDE-jKNmE_vJvq--BU=@127.0.0.1")), _claimExp = Just (NumericDate 2016-06-10 23:21:04 UTC), _claimNbf = Nothing, _claimIat = Just (NumericDate 2016-06-10 11:21:04 UTC), _claimJti = Nothing, _unregisteredClaims = fromList [("email",String "t4@x.com"),("email_verified",Bool True),("name",String "")]}
