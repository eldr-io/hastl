FROM docker.io/haskell:latest

RUN apt update
RUN apt install -y python3 git make 


COPY ./../ ./app

WORKDIR /app

RUN ls

ENV PORT 8000

CMD make run
