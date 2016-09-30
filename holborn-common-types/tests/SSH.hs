module SSH (tests) where

import HolbornPrelude
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@=?)
  , Assertion
  , testCase
  )
import Test.Tasty.QuickCheck
  ( (===)
  , Arbitrary(..)
  , elements
  , testProperty
  )
import Holborn.JSON.SSHRepoCommunication
  ( GitCommand(..)
  , KeyType
  , RepoCall(..)
  , SSHCommandLine(..)
  , SSHKey
  , parseKeyType
  , parseSSHKey
  , unparseSSHKey
  , unparseKeyType
  , unparseSSHCommand
  , parseSSHCommand
  )
import Holborn.CommonTypes.Repo (newOwnerName, newRepoName)
import Helpers (jsonIdentity, httpApiDataIdentity)


tests :: TestTree
tests =
  testGroup "Holborn.JSON.SSHRepoCommunication"
  [ testProperty "a is a" $ \x -> x == (x :: Int)
  , testGroup "SSHCommand"
    [ testProperty "unparsed then parsed" $ \x -> Just x === parseSSHCommand (unparseSSHCommand x)
    , testProperty "to JSON and back" $ \x -> jsonIdentity (x :: SSHCommandLine)
    , testCase "standard unparse example" $
      "git-upload-pack 'org/hello'" @=? unparseSSHCommand (SSHCommandLine GitUploadPack validOrgName validRepoName)
    , testCase "standard parse example" $
      Just (SSHCommandLine GitUploadPack validOrgName validRepoName) @=? parseSSHCommand "git-upload-pack 'org/hello'"
    ]
  , testGroup "GitCommand"
    [ testProperty "unparsed then parsed" $ \x -> httpApiDataIdentity (x :: GitCommand)
    ]
  , testGroup "RepoCall"
    [ testProperty "to JSON and back" $ \x -> jsonIdentity (x :: RepoCall)
    ]
  , testGroup "KeyType"
    [ testProperty "to JSON and back" $ \x -> jsonIdentity (x :: KeyType)
    , testProperty "unparsed then parsed" $ \x -> Just x === parseKeyType (unparseKeyType x :: ByteString)
    ]
  , testGroup "SSHKey"
    [ testGroup "parse then unparse"
      [ testCase "RSA, 3000 bits, spaces in comment" $ sshKeyIdentity "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABeADjuebVQh0Gv567KfhlXubADk5JNPc4qk9BYXY2WKtvedBCjHCXPVa2gd1A6hRe7q2kvojWuur4F14DmGnuj1N3pX4vaFKh71HNMu4/U4wDsHI/c2xWF+ux4VpyJXUJ1vLnFrwPnKVEAhAxTSyv+vOwO7fcUJmnuyMqFkB6IPWWqrt2Mgf+vLLAAD1MofNq+9oUNUomEgsfztQ18vJB2GcBBi1e2OleQSwlx9opYO7HO08AM4/TOI6yTmSe8oFr464QiunqSumWDAakaX7JfE0c3kCdSLkdIFFXEEgL8zgJiT/LglI0CXl2CFs9VOf8TIwfwqJADtPpKw58u04m8b34wPbjX84JzRN77w1PiaGD0Uovnu8GjBg6NMFUjibdlrDqilf83j5pHvGj7ElDPo7a1CjpgRDcuAFjwkyrBUcecs6S4dgAnKiTDQH4HrXovL7KDP7vezGdPVHUi+mL2r/FajY63ChQXfrK/DxMNRctLcmai48nA0E= malibu dreams"
      , testCase "RSA, default bits, spaces in comment" $ sshKeyIdentity "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCkRWwwPiwdSIouqfOLYCC+Qhg8GC68gcTRStupm7G2Cd468Veb1SdimVM6bZUJSAFgeHToYUKxi8J+8hnc91H09h2vLo2fKx40KDG4yHeB2xCat4/WUtf3VmOKWir7G9EyA29+HRQCUHSQsl0q1CR6XBUR1PTF5S3E6MYyZoPhLQKAYNqnrteJjpxM9n5GGQFRaXGlCnYWk3pTZSKEclVCDrF3oXLQDG7Fc8m4h/4UIy9hTVhx0mMkXMYeaO7krWPKShk9/HOw/m1ucXuHPXv3JK+IANbJ1pWQA5amIqpkguCTIRrJyJOq/xxb7FAhKmAxJ4SG1dEVEAVliyUyLqxn tangerine pie"
      , testCase "DSA, default bits, spaces in comment" $ sshKeyIdentity "ssh-dss AAAAB3NzaC1kc3MAAACBAPDnnc+dlTkfK/RhNl/hw1XKs/LZOaU+LEYDyVOOo8YOc+pTLNQMsWpo96ISnmM1WHhhLjTjgwD3Eq0SbBBeQGg34glRZXbSHOwejgnm62LhbQa2hpBBzfVJd1M2I3NVWvs1EvwOU3BLrrBdJgvLGZB5p7TTzlcPjiu32SF+wu9rAAAAFQC43PIM4+DdJmLyUqmlfH/Jui+2WwAAAIAFWFEyb9WMWLV3Z5FDi/e0mNQeZ0N54ny0R4iBIXpssHNsTa0GeKZHgpz+GIAv8bCk/DbtIjDxzn6L5s76EB0eig+IJyFBVBWozmvSed60kduBIOLD1TZaWgv5xaaYCXn6C9UrLuxyiN2vDzOb1p/mCyPFMHuvkKd7aWjqXbS2dAAAAIAPN1EpIY+A4B3bY5Wmhdgr4XnrlZwX83wFJyHEZ/f3akPAy87j078kYsBXiS0npDRKRznv6icSmo8DG9YcQiFgsO0IdN+GLyDZUO8mHHb5c5elV7CsU9mMxP2vLar13qOa9D/8n5W0XPFs6Y9VpmAFmRnmWDpI0JuRFTVoYKiBoA== release the kraken"
      , testCase "RSA, default bits, no comment" $ sshKeyIdentity "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDA5gczJRfixtZEx/n4AA5HhA1EVULRompucg5L7ToH0CQE+xreUtzIQB6Rikfnf2UniNbizUZtisCXfS+9CgGLq2LjX5EX5wa+PLAfN77DT449GshMNePep9+Kjx1Wllcg4EF30jRkjnBdmNGwhM0VbbS0aWNEERXofxNop0xFtNW28V8HaLI+zZrJU9RF8KN7CZzM0GYzh1mDCLvl6a6sKlUxjuZMGeUjuja8hFADjrODQmQi9dIO4YGOwQy7U2t+CBXCoOHTYTmW2wVR5NkLdyMVe+9S/cRSj33pIsJCGPWWU8CG+15WJY8ko8S1kvjblS6QqSLF/cDBBE1ZqDIt"
      , testCase "RSA, default bits, default comment" $ sshKeyIdentity "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+X+5qH3ieALHB/z8ItS49yDpPJbSo3d1rLFLlM03KG4ZS27cdzqAyjjHKinYp4XPhnrlV6q4ne3HKYCuA6BgYSSmApekH71I5QOnYkTHOxal2rGv4bZm+QZXJqnWbkQ5Lt82DCADQR+F54jU9hFsNvc8zGlKdsIXqr9aSgk8B9tHJiJqXmdkVy1iliYWaBdcnaqCV02hoyZ4xtL7bFkiUREOIHEbU4zkqC7IFQXui4GWU/fqsS0ZQrGq9ko0+/ZSIzISq0y0O3gGiF32RvcuOD7Bz7lZzDpN34RIG0WDWMIIOfLo5Rg/pFrNH5YrUTCEM/iMe/DenGqV8x1meYXNf jml@worth"
      ]
    , testProperty "to JSON and back" $ jsonIdentity . getSSHKey
    ]
  ]
  where
    Just validOrgName = newOwnerName "org"
    Just validRepoName = newRepoName "hello"

    -- | A valid SSH key can be parsed and unparsed to get the original key.
    sshKeyIdentity :: ByteString -> Assertion
    sshKeyIdentity key = Just key @=? (unparseSSHKey <$> parseSSHKey key)


newtype TestSSHKey = TestSSHKey { getSSHKey :: SSHKey } deriving (Eq, Show)

instance Arbitrary TestSSHKey where
  arbitrary = TestSSHKey . fromJust . parseSSHKey <$> elements
              [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABeADjuebVQh0Gv567KfhlXubADk5JNPc4qk9BYXY2WKtvedBCjHCXPVa2gd1A6hRe7q2kvojWuur4F14DmGnuj1N3pX4vaFKh71HNMu4/U4wDsHI/c2xWF+ux4VpyJXUJ1vLnFrwPnKVEAhAxTSyv+vOwO7fcUJmnuyMqFkB6IPWWqrt2Mgf+vLLAAD1MofNq+9oUNUomEgsfztQ18vJB2GcBBi1e2OleQSwlx9opYO7HO08AM4/TOI6yTmSe8oFr464QiunqSumWDAakaX7JfE0c3kCdSLkdIFFXEEgL8zgJiT/LglI0CXl2CFs9VOf8TIwfwqJADtPpKw58u04m8b34wPbjX84JzRN77w1PiaGD0Uovnu8GjBg6NMFUjibdlrDqilf83j5pHvGj7ElDPo7a1CjpgRDcuAFjwkyrBUcecs6S4dgAnKiTDQH4HrXovL7KDP7vezGdPVHUi+mL2r/FajY63ChQXfrK/DxMNRctLcmai48nA0E= malibu dreams"
              , "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCkRWwwPiwdSIouqfOLYCC+Qhg8GC68gcTRStupm7G2Cd468Veb1SdimVM6bZUJSAFgeHToYUKxi8J+8hnc91H09h2vLo2fKx40KDG4yHeB2xCat4/WUtf3VmOKWir7G9EyA29+HRQCUHSQsl0q1CR6XBUR1PTF5S3E6MYyZoPhLQKAYNqnrteJjpxM9n5GGQFRaXGlCnYWk3pTZSKEclVCDrF3oXLQDG7Fc8m4h/4UIy9hTVhx0mMkXMYeaO7krWPKShk9/HOw/m1ucXuHPXv3JK+IANbJ1pWQA5amIqpkguCTIRrJyJOq/xxb7FAhKmAxJ4SG1dEVEAVliyUyLqxn tangerine pie"
              , "ssh-dss AAAAB3NzaC1kc3MAAACBAPDnnc+dlTkfK/RhNl/hw1XKs/LZOaU+LEYDyVOOo8YOc+pTLNQMsWpo96ISnmM1WHhhLjTjgwD3Eq0SbBBeQGg34glRZXbSHOwejgnm62LhbQa2hpBBzfVJd1M2I3NVWvs1EvwOU3BLrrBdJgvLGZB5p7TTzlcPjiu32SF+wu9rAAAAFQC43PIM4+DdJmLyUqmlfH/Jui+2WwAAAIAFWFEyb9WMWLV3Z5FDi/e0mNQeZ0N54ny0R4iBIXpssHNsTa0GeKZHgpz+GIAv8bCk/DbtIjDxzn6L5s76EB0eig+IJyFBVBWozmvSed60kduBIOLD1TZaWgv5xaaYCXn6C9UrLuxyiN2vDzOb1p/mCyPFMHuvkKd7aWjqXbS2dAAAAIAPN1EpIY+A4B3bY5Wmhdgr4XnrlZwX83wFJyHEZ/f3akPAy87j078kYsBXiS0npDRKRznv6icSmo8DG9YcQiFgsO0IdN+GLyDZUO8mHHb5c5elV7CsU9mMxP2vLar13qOa9D/8n5W0XPFs6Y9VpmAFmRnmWDpI0JuRFTVoYKiBoA== release the kraken"
              , "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDA5gczJRfixtZEx/n4AA5HhA1EVULRompucg5L7ToH0CQE+xreUtzIQB6Rikfnf2UniNbizUZtisCXfS+9CgGLq2LjX5EX5wa+PLAfN77DT449GshMNePep9+Kjx1Wllcg4EF30jRkjnBdmNGwhM0VbbS0aWNEERXofxNop0xFtNW28V8HaLI+zZrJU9RF8KN7CZzM0GYzh1mDCLvl6a6sKlUxjuZMGeUjuja8hFADjrODQmQi9dIO4YGOwQy7U2t+CBXCoOHTYTmW2wVR5NkLdyMVe+9S/cRSj33pIsJCGPWWU8CG+15WJY8ko8S1kvjblS6QqSLF/cDBBE1ZqDIt"
              , "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+X+5qH3ieALHB/z8ItS49yDpPJbSo3d1rLFLlM03KG4ZS27cdzqAyjjHKinYp4XPhnrlV6q4ne3HKYCuA6BgYSSmApekH71I5QOnYkTHOxal2rGv4bZm+QZXJqnWbkQ5Lt82DCADQR+F54jU9hFsNvc8zGlKdsIXqr9aSgk8B9tHJiJqXmdkVy1iliYWaBdcnaqCV02hoyZ4xtL7bFkiUREOIHEbU4zkqC7IFQXui4GWU/fqsS0ZQrGq9ko0+/ZSIzISq0y0O3gGiF32RvcuOD7Bz7lZzDpN34RIG0WDWMIIOfLo5Rg/pFrNH5YrUTCEM/iMe/DenGqV8x1meYXNf jml@worth"
              ]

