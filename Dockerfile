FROM haskell:9.2.7-buster

RUN apt-get update && \
    apt-get install -y libpq-dev

RUN stack upgrade

WORKDIR /app
COPY . /app

RUN stack setup
RUN stack build --only-dependencies

RUN stack build

CMD ["stack", "exec", "Torispa-backend-exe"]
