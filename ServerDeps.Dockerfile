################
# Sqlite Stage #
################

FROM haskell:8.10.7 as sqlite

WORKDIR /

RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get install -y sqlite3 libsqlite3-dev

FROM sqlite

# Setup dirs

RUN mkdir source

## Copy Server Source Files

COPY server/fitness-tracker.cabal \
     source/server/fitness-tracker.cabal

COPY server/cabal.project.local \
     source/server/cabal.project.local

## Build Deps

WORKDIR /source/server

RUN cabal update
RUN cabal build --only-dependencies -j4 -v