#!/bin/bash

# Docker wrapper script to handle Unicode encoding issues
echo "LLM Video Editor Docker Wrapper"
echo "==============================="

# Set locale environment variables
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Function to run Docker with proper locale settings
run_with_locale() {
    echo "Running with UTF-8 locale support..."
    docker run --rm \
        -e LANG=en_US.UTF-8 \
        -e LC_ALL=en_US.UTF-8 \
        -e LANGUAGE=en_US:en \
        "$@"
}

# Function to run with proxy and locale support
run_with_proxy_and_locale() {
    echo "Running with proxy and UTF-8 locale support..."
    
    # Convert localhost proxy to host.docker.internal for Docker
    local docker_http_proxy=""
    local docker_https_proxy=""
    
    if [ -n "$HTTP_PROXY" ]; then
        docker_http_proxy=$(echo "$HTTP_PROXY" | sed 's/127.0.0.1/host.docker.internal/g' | sed 's/localhost/host.docker.internal/g')
        echo "HTTP Proxy: $HTTP_PROXY -> $docker_http_proxy"
    fi
    
    if [ -n "$HTTPS_PROXY" ]; then
        docker_https_proxy=$(echo "$HTTPS_PROXY" | sed 's/127.0.0.1/host.docker.internal/g' | sed 's/localhost/host.docker.internal/g')
        echo "HTTPS Proxy: $HTTPS_PROXY -> $docker_https_proxy"
    fi
    
    docker run --rm \
        -e LANG=en_US.UTF-8 \
        -e LC_ALL=en_US.UTF-8 \
        -e LANGUAGE=en_US:en \
        -e HTTP_PROXY="$docker_http_proxy" \
        -e HTTPS_PROXY="$docker_https_proxy" \
        -e NO_PROXY="$NO_PROXY" \
        -e http_proxy="$docker_http_proxy" \
        -e https_proxy="$docker_https_proxy" \
        -e no_proxy="$NO_PROXY" \
        "$@"
}

# Function to run with proxy and locale using host networking
run_with_proxy_host_network() {
    echo "Running with host networking (for localhost proxy) and UTF-8 locale..."
    docker run --rm --network=host \
        -e LANG=en_US.UTF-8 \
        -e LC_ALL=en_US.UTF-8 \
        -e LANGUAGE=en_US:en \
        -e HTTP_PROXY="$HTTP_PROXY" \
        -e HTTPS_PROXY="$HTTPS_PROXY" \
        -e NO_PROXY="$NO_PROXY" \
        -e http_proxy="$HTTP_PROXY" \
        -e https_proxy="$HTTPS_PROXY" \
        -e no_proxy="$NO_PROXY" \
        "$@"
}

# Function to run without terminal output (silent mode)
run_silent() {
    echo "Running in silent mode (output redirected)..."
    docker run --rm "$@" > /dev/null 2>&1
    return $?
}

# Function to run with output to file only
run_to_file() {
    echo "Running with output redirected to file..."
    local log_file="docker-run.log"
    docker run --rm "$@" > "$log_file" 2>&1
    local exit_code=$?
    echo "Check $log_file for detailed output"
    return $exit_code
}

# Function to run with ASCII-only output
run_ascii_only() {
    echo "Running with ASCII-only output (Unicode filtered)..."
    docker run --rm \
        -e LC_ALL=C \
        -e LANG=C \
        "$@" \
        2>&1 | iconv -f utf-8 -t ascii//IGNORE
}

# Parse arguments to detect mode
MODE="normal"
DOCKER_ARGS=()

while [[ $# -gt 0 ]]; do
    case $1 in
        --silent)
            MODE="silent"
            shift
            ;;
        --to-file)
            MODE="file"
            shift
            ;;
        --locale-fix)
            MODE="locale"
            shift
            ;;
        --ascii-only)
            MODE="ascii"
            shift
            ;;
        --proxy-locale)
            MODE="proxy-locale"
            shift
            ;;
        --proxy-host)
            MODE="proxy-host"
            shift
            ;;
        *)
            DOCKER_ARGS+=("$1")
            shift
            ;;
    esac
done

# Run based on mode
case $MODE in
    silent)
        run_silent "${DOCKER_ARGS[@]}"
        ;;
    file)
        run_to_file "${DOCKER_ARGS[@]}"
        ;;
    locale)
        run_with_locale "${DOCKER_ARGS[@]}"
        ;;
    ascii)
        run_ascii_only "${DOCKER_ARGS[@]}"
        ;;
    proxy-locale)
        run_with_proxy_and_locale "${DOCKER_ARGS[@]}"
        ;;
    proxy-host)
        run_with_proxy_host_network "${DOCKER_ARGS[@]}"
        ;;
    *)
        echo "Available modes:"
        echo "  --silent        Run without terminal output"
        echo "  --to-file       Redirect output to file"
        echo "  --locale-fix    Run with UTF-8 locale"
        echo "  --ascii-only    Run with ASCII-only output (no Unicode)"
        echo "  --proxy-locale  Run with proxy + UTF-8 locale (converts localhost to host.docker.internal)"
        echo "  --proxy-host    Run with proxy + UTF-8 locale using host networking"
        echo
        echo "Proxy Environment Variables:"
        echo "  HTTP_PROXY, HTTPS_PROXY, NO_PROXY"
        echo
        echo "Example usage:"
        echo "  $0 --silent llm-video-editor:prebuilt --help"
        echo "  $0 --ascii-only llm-video-editor:prebuilt --help"
        echo "  $0 --proxy-locale -e USE_REAL_CHATGPT_API=true llm-video-editor:prebuilt [args]"
        echo "  $0 --proxy-host -e USE_REAL_CHATGPT_API=true llm-video-editor:prebuilt [args]"
        ;;
esac