FROM docker.io/haskell:9.4.8-slim

ENV POSTGRES_PASSWORD test

RUN apt update
RUN apt install -y git make libpq-dev postgresql 

COPY ./../ ./app
WORKDIR /app

RUN ls

ENV PORT 8000

RUN cabal update
RUN cabal build

CMD service postgresql start && cabal run
