#!/bin/bash

# Build Docker image with proxy support
echo "Building Docker image with proxy configuration..."

# Method 1: Using build args
docker build \
  --build-arg HTTP_PROXY=${HTTP_PROXY} \
  --build-arg HTTPS_PROXY=${HTTPS_PROXY} \
  --build-arg NO_PROXY=${NO_PROXY} \
  -f Dockerfile.prebuilt \
  -t llm-video-editor:prebuilt \
  .

echo "Build complete. Testing with proxy..."

# Method 2: Run with proxy environment variables
echo "Testing proxy connection methods..."

# Convert localhost proxy to host.docker.internal for Docker
DOCKER_HTTP_PROXY=$(echo "$HTTP_PROXY" | sed 's/127.0.0.1/host.docker.internal/g' | sed 's/localhost/host.docker.internal/g')
DOCKER_HTTPS_PROXY=$(echo "$HTTPS_PROXY" | sed 's/127.0.0.1/host.docker.internal/g' | sed 's/localhost/host.docker.internal/g')

echo "Method 1: Using host.docker.internal (recommended)"
docker run --rm \
  -e USE_REAL_CHATGPT_API=true \
  -e OPENAI_API_KEY="${OPENAI_API_KEY}" \
  -e HTTP_PROXY="${DOCKER_HTTP_PROXY}" \
  -e HTTPS_PROXY="${DOCKER_HTTPS_PROXY}" \
  -e NO_PROXY="${NO_PROXY}" \
  -e http_proxy="${DOCKER_HTTP_PROXY}" \
  -e https_proxy="${DOCKER_HTTPS_PROXY}" \
  -e no_proxy="${NO_PROXY}" \
  -v $(pwd)/examples:/app/examples \
  -v $(pwd):/app/output \
  llm-video-editor:prebuilt \
  --input /app/examples/input.json \
  --prompt /app/examples/prompt.txt \
  --output /app/output/proxy-test-output.json \
  --model gpt-4o-mini \
  --temperature 0.7

if [ $? -ne 0 ]; then
  echo "Method 1 failed, trying Method 2: Host networking"
  docker run --rm --network=host \
    -e USE_REAL_CHATGPT_API=true \
    -e OPENAI_API_KEY="${OPENAI_API_KEY}" \
    -e HTTP_PROXY="${HTTP_PROXY}" \
    -e HTTPS_PROXY="${HTTPS_PROXY}" \
    -e NO_PROXY="${NO_PROXY}" \
    -e http_proxy="${HTTP_PROXY}" \
    -e https_proxy="${HTTPS_PROXY}" \
    -e no_proxy="${NO_PROXY}" \
    -v $(pwd)/examples:/app/examples \
    -v $(pwd):/app/output \
    llm-video-editor:prebuilt \
    --input /app/examples/input.json \
    --prompt /app/examples/prompt.txt \
    --output /app/output/proxy-test-host-network.json \
    --model gpt-4o-mini \
    --temperature 0.7
fi