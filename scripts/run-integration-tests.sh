#!/bin/bash

# ChatGPT Integration Test Runner
# This script sets up the environment and runs integration tests with real ChatGPT API

set -e

echo "🔧 ChatGPT Integration Test Setup"
echo "=================================="

# Check if API key is provided
if [ -z "$OPENAI_API_KEY" ]; then
    echo "❌ Error: OPENAI_API_KEY environment variable is not set"
    echo ""
    echo "To run integration tests with real ChatGPT API:"
    echo "1. Get your OpenAI API key from https://platform.openai.com/api-keys"
    echo "2. Export it as an environment variable:"
    echo "   export OPENAI_API_KEY='your-api-key-here'"
    echo "3. Run this script again"
    echo ""
    echo "⚠️  WARNING: Real API calls will incur charges on your OpenAI account!"
    echo ""
    exit 1
fi

# Confirm with user before making real API calls
echo "⚠️  This will make REAL API calls to OpenAI ChatGPT"
echo "💰 This will incur charges on your OpenAI account"
echo ""
read -p "Are you sure you want to proceed? (y/N): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Integration tests cancelled"
    exit 1
fi

echo ""
echo "🚀 Running ChatGPT Integration Tests..."
echo "======================================"

# Set environment variables for real API usage
export USE_REAL_CHATGPT_API=true
export DEBUG_LLM_API=false

# Run the tests
echo "Building project..."
stack-3.3.1 build

echo ""
echo "Running integration tests..."
stack-3.3.1 test --ta '--match ChatGPT'

echo ""
echo "✅ Integration tests completed!"
echo ""
echo "💡 To run only unit tests (no API calls), use:"
echo "   stack-3.3.1 test --test-arguments=\"--match=\\\"Unit Tests\\\"\""