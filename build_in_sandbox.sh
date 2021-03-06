#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

rm -fr .cabal-sandbox cabal.sandbox.config dist

cabal sandbox init

cabal install yesod-bin

cabal install classy-prelude
cabal install classy-prelude-conduit
cabal install classy-prelude-yesod
cabal install data-default
cabal install hjsmin
cabal install monad-logger
cabal install persistent
cabal install persistent-sqlite
cabal install persistent-template
cabal install safe
cabal install yesod
cabal install yesod-auth
cabal install yesod-core
cabal install yesod-form
cabal install yesod-static
cabal install polyparse
cabal install HaXml
cabal install rss
cabal install filepath
cabal install network-info
cabal install yesod-recaptcha
cabal install network
cabal install network-uri
cabal install blaze-html
cabal install wai
cabal install mime-mail

cabal install
