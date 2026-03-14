FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ghc \
        make \
        curl \
        ca-certificates \
        libghc-hunit-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . .
RUN make Main

ENTRYPOINT ["./Main"]
