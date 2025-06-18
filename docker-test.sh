#!/bin/bash

echo "=== LLM Video Editor Docker Test Script ==="
echo

# Function to test Docker build
test_docker_build() {
    echo "Testing Docker build..."
    
    echo "Method 1: Full build (takes 20-30 minutes)"
    echo "docker build -t llm-video-editor ."
    echo
    
    echo "Method 2: Pre-built binary approach (faster)"
    echo "1. Build locally: stack build --copy-bins --local-bin-path ./bin"
    echo "2. Build Docker: docker build -f Dockerfile.prebuilt -t llm-video-editor:prebuilt ."
    echo
}

# Function to test Docker run
test_docker_run() {
    echo "Testing Docker container execution..."
    
    echo "Basic help command:"
    echo "docker run --rm llm-video-editor:prebuilt --help"
    echo
    
    echo "Run with example files (mock LLM):"
    echo "docker run --rm -v \$(pwd)/examples:/app/examples -v \$(pwd):/app/output llm-video-editor:prebuilt \\"
    echo "  --input /app/examples/input.json \\"
    echo "  --prompt /app/examples/prompt.txt \\"
    echo "  --output /app/output/docker-output.json"
    echo
    
    echo "Run with real ChatGPT API:"
    echo "docker run --rm -e USE_REAL_CHATGPT_API=true -e OPENAI_API_KEY=\$OPENAI_API_KEY \\"
    echo "  -e HTTP_PROXY=\$HTTP_PROXY -e HTTPS_PROXY=\$HTTPS_PROXY -e NO_PROXY=\$NO_PROXY \\"
    echo "  -v \$(pwd)/examples:/app/examples -v \$(pwd):/app/output llm-video-editor:prebuilt \\"
    echo "  --input /app/examples/input.json \\"
    echo "  --prompt /app/examples/prompt.txt \\"
    echo "  --output /app/output/docker-output.json \\"
    echo "  --model gpt-4o-mini --temperature 0.7 --debug"
    echo
    echo "With specific proxy example:"
    echo "docker run --rm -e USE_REAL_CHATGPT_API=true -e OPENAI_API_KEY=\$OPENAI_API_KEY \\"
    echo "  -e HTTP_PROXY=http://proxy.company.com:8080 \\"
    echo "  -e HTTPS_PROXY=http://proxy.company.com:8080 \\"
    echo "  -v \$(pwd)/examples:/app/examples -v \$(pwd):/app/output llm-video-editor:prebuilt \\"
    echo "  --input /app/examples/input.json --prompt /app/examples/prompt.txt \\"
    echo "  --output /app/output/docker-output.json --model gpt-4o-mini"
    echo
}

# Function to check Docker images
check_docker_images() {
    echo "Checking for existing Docker images..."
    if docker images | grep -q llm-video-editor; then
        echo "Found existing llm-video-editor images:"
        docker images | grep llm-video-editor
    else
        echo "No llm-video-editor images found."
    fi
    echo
}

# Function to show file structure
show_file_structure() {
    echo "Docker-related files created:"
    echo "├── Dockerfile (main multi-stage build)"
    echo "├── Dockerfile.prebuilt (for pre-built binaries)"
    echo "├── .dockerignore (optimize build context)"
    echo "└── docker-test.sh (this test script)"
    echo
    
    echo "To see the Dockerfiles:"
    echo "cat Dockerfile"
    echo "cat Dockerfile.prebuilt"
    echo
}

# Main execution
echo "1. File Structure"
echo "=================="
show_file_structure

echo "2. Docker Images"
echo "================"
check_docker_images

echo "3. Build Instructions"
echo "===================="
test_docker_build

echo "4. Run Instructions"
echo "==================="
test_docker_run

echo "5. Quick Test (if image exists)"
echo "==============================="
if docker images | grep -q "llm-video-editor.*prebuilt"; then
    echo "Running quick test with prebuilt image..."
    echo "Help command:"
    docker run --rm llm-video-editor:prebuilt --help 2>/dev/null | head -5
    echo
    echo "Processing example files:"
    docker run --rm -v $(pwd)/examples:/app/examples -v $(pwd):/app/output llm-video-editor:prebuilt \
      --input /app/examples/input.json --prompt /app/examples/prompt.txt --output /app/output/test-output.json 2>/dev/null
    if [ -f "test-output.json" ]; then
        echo "✅ Success! Output generated: test-output.json"
        echo "Layout ID: $(grep -o '"layoutId":"[^"]*"' test-output.json | cut -d'"' -f4)"
    else
        echo "❌ Processing failed"
    fi
elif docker images | grep -q llm-video-editor; then
    echo "Running quick test..."
    docker run --rm llm-video-editor --help 2>/dev/null | head -5 || echo "Image exists but may need runtime libraries"
else
    echo "Build an image first using one of the methods above"
fi

echo
echo "=== Test Complete ==="
echo
echo "Environment Variables for Real API:"
echo "  USE_REAL_CHATGPT_API=true    Enable real ChatGPT API (default: mock)"
echo "  OPENAI_API_KEY=your-key      Your OpenAI API key"
echo "  DEBUG_LLM_API=true           Enable detailed API debugging (optional)"
echo
echo "Proxy Support:"
echo "  HTTP_PROXY=http://proxy:8080   HTTP proxy server"
echo "  HTTPS_PROXY=http://proxy:8080  HTTPS proxy server"  
echo "  NO_PROXY=localhost,127.0.0.1   Bypass proxy for these hosts"
echo
echo "Proxy Usage Examples:"
echo "  ./proxy-build.sh                          # Build and run with proxy"
echo "  docker-compose up                         # Use docker-compose with proxy"
echo "  docker run -e HTTP_PROXY=... [image]     # Manual proxy setting"
echo "  docker run --network=host [image]        # Use host networking (proxy on localhost)"
echo
echo "The Dockerfile is ready for use. Choose build method based on your needs:"
echo "- Full build: Complete but slow (20-30 min)"
echo "- Pre-built: Fast but requires local Stack installation"