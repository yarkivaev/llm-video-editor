#!/bin/bash

# CLI Test Runner Script
# This script runs CLI integration tests with mocked LLM requests

set -e

echo "🧪 CLI Integration Test Runner"
echo "============================="

echo ""
echo "🔧 Building project..."
stack-3.3.1 build

echo ""
echo "🚀 Running CLI Integration Tests (Mocked LLM)..."
echo "These tests use mock LLM responses and don't require API keys"
echo ""

# Run CLI integration tests with mocked LLM
stack-3.3.1 test --ta '--match "CLI Integration Tests"'

echo ""
echo "✅ CLI Integration Tests completed successfully!"
echo ""
echo "📊 Test Summary:"
echo "- 7 CLI integration tests with mocked LLM responses"
echo "- Tests cover: basic processing, error handling, parameter validation, help output"
echo "- All tests use temporary files and don't affect the file system"
echo ""
echo "💡 To run end-to-end tests with real ChatGPT API:"
echo "   Run: OPENAI_API_KEY='your-key' stack-3.3.1 test --ta '--match \"CLI End-to-End Tests\"'"
echo "   ⚠️  WARNING: End-to-end tests will incur charges on your OpenAI account!"