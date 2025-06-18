# Multi-stage build using Alpine for smaller image
FROM alpine:3.20 AS builder

# Install build dependencies including stack
RUN apk add --no-cache \
    ghc \
    musl-dev \
    gmp-dev \
    zlib-dev \
    libffi-dev \
    ncurses-dev \
    git \
    curl \
    bash

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Set working directory
WORKDIR /app

# Copy stack configuration and package files
COPY stack.yaml stack.yaml.lock package.yaml ./
COPY *.cabal ./

# Setup stack and download dependencies
RUN stack setup
RUN stack build --only-dependencies

# Copy source code
COPY src/ ./src/
COPY app/ ./app/

# Build the application
RUN stack build --copy-bins --local-bin-path /usr/local/bin

# Production stage
FROM alpine:3.20

# Install runtime dependencies
RUN apk add --no-cache \
    gmp \
    libffi \
    ncurses-libs

# Copy the built executable
COPY --from=builder /usr/local/bin/llm-video-editor-exe /usr/local/bin/llm-video-editor

# Create a non-root user
RUN adduser -D -u 1000 appuser
USER appuser

# Set working directory
WORKDIR /home/appuser

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/llm-video-editor"]