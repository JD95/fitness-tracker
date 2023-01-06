#################
# Builder Stage #
#################

FROM fitness-tracker-backend-deps:latest as builder

WORKDIR /

COPY server/app \
     /source/server/app

COPY server/src \
     /source/server/src

COPY server/test \
     /source/server/test

WORKDIR /source/server

RUN cabal install --installdir="/" --install-method=copy

################
# Server Stage #
################

FROM frolvlad/alpine-glibc:latest as production

WORKDIR /

# Sqlite

RUN apk add sqlite sqlite-dev gmp

# Frontend

RUN mkdir /server
RUN mkdir /frontend

COPY frontend/index.html \
     /frontend/index.html

COPY frontend/index.js \
     /frontend/index.js

COPY frontend/style.css \
     /frontend/style.css

COPY --from=builder /fitness-tracker \
     /server/fitness-tracker

CMD ["./server/fitness-tracker"]
