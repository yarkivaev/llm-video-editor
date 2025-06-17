# ChatGPT Integration Tests

This document describes how to run and configure the ChatGPT integration tests for the LLM Video Editor.

## Overview

The integration tests verify that the VideoAssembler works correctly with real ChatGPT API calls. These tests:

- **Validate real API integration** - Ensure prompts are correctly sent to ChatGPT and responses parsed
- **Test complex scenarios** - Verify handling of detailed creative prompts and video requirements  
- **Check error handling** - Test behavior with invalid API keys, network issues, and malformed responses
- **Measure performance** - Ensure requests complete within reasonable timeframes
- **Test reliability** - Verify multiple consecutive requests work correctly

## Test Structure

### Unit Tests (Always Run)
- **Prompt Generation**: Tests creation of LLM prompts from VideoRequest and AssemblyContext
- **Response Parsing**: Tests parsing of JSON responses into VideoLayout structures
- **Error Handling**: Tests graceful handling of malformed JSON and missing fields
- **Helper Functions**: Tests media file formatting and constraint handling

### Integration Tests (Optional - Real API)
- **Environment Setup**: Validates API key configuration
- **Real ChatGPT Integration**: Full end-to-end testing with live API calls
- **Complex Prompt Processing**: Tests detailed creative video requirements
- **Error Handling**: Tests real API failure scenarios
- **Performance & Reliability**: Tests timing and consecutive requests

## Running Tests

### 1. Unit Tests Only (Default)
```bash
# Run all unit tests (no API calls, no charges)
stack test

# Run only unit tests explicitly
stack test --test-arguments="--match=\"Unit Tests\""
```

### 2. Integration Tests with Real ChatGPT API

⚠️ **WARNING**: Integration tests make real API calls to OpenAI and will incur charges!

#### Prerequisites
1. Get an OpenAI API key from [https://platform.openai.com/api-keys](https://platform.openai.com/api-keys)
2. Ensure you have sufficient credits in your OpenAI account
3. Review current OpenAI pricing: [https://openai.com/pricing](https://openai.com/pricing)

#### Setup
```bash
# Set your OpenAI API key
export OPENAI_API_KEY='your-api-key-here'

# Enable real API usage for tests
export USE_REAL_CHATGPT_API=true
```

#### Run Integration Tests
```bash
# Option 1: Use the provided script (recommended)
./scripts/run-integration-tests.sh

# Option 2: Run manually
stack test --test-arguments="--match=\"ChatGPT Integration Tests\""

# Option 3: Run specific integration test
stack test --test-arguments="--match=\"successfully creates video layout\""
```

## Configuration

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `OPENAI_API_KEY` | Yes* | - | Your OpenAI API key |
| `USE_REAL_CHATGPT_API` | No | `false` | Set to `true` to enable real API calls |
| `OPENAI_API_ENDPOINT` | No | OpenAI default | Custom API endpoint for OpenAI-compatible services |

*Required only for real API integration tests

### Configuration File
You can also use a `.env` file:

```bash
# Copy example configuration
cp .env.example .env

# Edit with your values
vim .env
```

## Test Scenarios

### 1. Basic Integration Test
- **Test**: `successfully creates video layout using real ChatGPT API`
- **Purpose**: Validates basic end-to-end functionality
- **Verifies**: 
  - API request/response cycle
  - JSON parsing correctness
  - Layout structure validity
  - Segment timing accuracy

### 2. Complex Prompt Test  
- **Test**: `handles detailed creative prompts correctly`
- **Purpose**: Tests complex video editing requirements
- **Scenario**: 2-minute documentary with specific structure, pacing, and narrative arc
- **Verifies**:
  - Multi-segment layout generation
  - Title card creation
  - Duration adherence
  - Creative interpretation

### 3. Error Handling Tests
- **Test**: `handles invalid API key gracefully`
- **Purpose**: Ensures proper error reporting
- **Verifies**: Appropriate error messages for authentication failures

- **Test**: `handles network timeouts and retries`  
- **Purpose**: Tests resilience to network issues
- **Verifies**: Graceful handling of API limitations

### 4. Performance Tests
- **Test**: `completes requests within reasonable time`
- **Purpose**: Validates response time expectations
- **Timeout**: Tests will fail if responses take too long

- **Test**: `handles multiple consecutive requests`
- **Purpose**: Tests for resource leaks and rate limiting
- **Verifies**: Consistent behavior across multiple calls

## Cost Estimation

Integration tests make several API calls to ChatGPT. Estimated costs (as of 2024):

| Test Type | API Calls | Tokens (approx) | Est. Cost |
|-----------|-----------|------------------|-----------|
| Basic Integration | 1 | 2,000-4,000 | $0.01-0.02 |
| Complex Prompt | 1 | 3,000-6,000 | $0.02-0.04 |
| Error Handling | 2-3 | 1,000-2,000 | $0.01-0.02 |
| Performance Tests | 3-4 | 6,000-12,000 | $0.04-0.08 |
| **Total Full Suite** | **7-9** | **12,000-24,000** | **$0.08-0.16** |

*Costs are estimates based on GPT-4 pricing and may vary*

## Troubleshooting

### Common Issues

1. **Tests are pending/skipped**
   - Ensure `USE_REAL_CHATGPT_API=true` is set
   - Verify `OPENAI_API_KEY` is configured

2. **API authentication errors**
   - Check your API key is valid and has sufficient credits
   - Verify the key has correct permissions

3. **Rate limiting errors**
   - Wait a few minutes and retry
   - Consider running tests individually with delays

4. **Parsing errors**
   - May indicate ChatGPT response format changes
   - Check if prompt needs adjustment for current model behavior

5. **Network timeouts**
   - Check internet connectivity
   - Verify OpenAI API status: [https://status.openai.com](https://status.openai.com)

### Debugging

Enable verbose test output:
```bash
stack test --test-arguments="--verbose --match=\"ChatGPT Integration\""
```

Check actual API responses by modifying the test temporarily to log the raw response.

## Development

### Adding New Integration Tests

1. Add test to `test/Integration/ChatGPTIntegrationSpec.hs`
2. Follow the pattern of checking `USE_REAL_CHATGPT_API` environment variable
3. Use `pendingWith` for tests that require real API access
4. Include appropriate cost and timing expectations

### Mock vs Real API Testing

The system supports both mock and real API testing:

- **Mock Mode** (default): Uses predefined JSON responses, no API calls, no costs
- **Real Mode**: Makes actual ChatGPT API calls, incurs costs, tests real behavior

Switch between modes using the `USE_REAL_CHATGPT_API` environment variable.

## Security Notes

- Never commit API keys to version control
- Use environment variables or secure configuration management
- Consider using separate API keys for testing vs production
- Monitor API usage and costs regularly
- Revoke compromised keys immediately

## Further Reading

- [OpenAI API Documentation](https://platform.openai.com/docs)
- [ChatGPT API Reference](https://platform.openai.com/docs/api-reference/chat)
- [Hspec Testing Framework](https://hspec.github.io/)
- [Stack Testing Guide](https://docs.haskellstack.org/en/stable/GUIDE/#test)