FROM rust:1.72

WORKDIR /usr/src/rinha

COPY . .

RUN cargo build --release

CMD ["/usr/src/rinha/target/release/rinha-de-compiler", "/var/rinha/source.rinha"]