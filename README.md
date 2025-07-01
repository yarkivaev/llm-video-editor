# llm-video-editor-types

Shared type definitions and utilities for the LLMApi Video Editor ecosystem.

This package contains common data types, typeclasses, and utility functions used by:
- `llm-video-editor` - The core video editing service
- `llm-video-editor-bot` - The Telegram bot interface
- `llm-video-editor-cli` - Command-line interface

## Types Included

### Core Type Modules

- **Types.Common** - Basic shared types (Duration, Resolution, Location, Timestamp)
- **Types.Media** - Media file types (VideoFile, PhotoFile, VideoRequest) and content analysis
- **Types.Video** - Video layout structures (VideoLayout, VideoSegment, Transition, AudioTrack, TextOverlay)
- **Types.Assembly** - LLM assembly abstractions (VideoAssembler typeclass, AssemblyContext, AssemblyResult)
- **Types.LLMApi** - LLM interface types and utilities (prompt generation, JSON parsing)
- **Types.LLM** - ChatGPT implementation
- **Types.System** - System interface types (VideoEditorInput/Output, ProcessingStatus)
- **Types.Render** - Rendering configuration and export types

### Key Features

- **JSON Serialization** - All types support ToJSON/FromJSON for API communication
- **Type-Safe Assembly** - VideoAssembler typeclass for implementing custom LLM strategies
- **Rich Content Analysis** - Comprehensive video analysis types with confidence scores
- **Flexible Configuration** - Support for multiple LLM models and assembly strategies
- **Custom JSON Parsing** - Handles flat JSON structure with "type" field for sum types

## Usage

Add this package as a dependency in your `package.yaml`:

```yaml
dependencies:
- llm-video-editor-types
```

Then import the types you need:

```haskell
import Types                              -- All types
import Types.Media (VideoFile, VideoRequest)  -- Specific media types
import Types.Video (VideoLayout)          -- Video layout types
import Types.Assembly (VideoAssembler(..), AssemblyContext) -- Assembly types
```

### Example Usage

```haskell
import Types
import Types.Assembly

-- Create an LLMApi configuration
llmConfig :: LLMConfig
llmConfig = LLMConfig
  { modelName = "gpt-4"
  , temperature = 0.7
  , maxTokens = Just 2000
  , systemPrompt = Nothing
  , apiEndpoint = Nothing
  , apiKey = Nothing
  }

-- Create assembly context
context :: AssemblyContext
context = AssemblyContext
  { strategy = SingleLLM llmConfig
  , maxVideoDuration = Just (Duration 120.0)
  , preferredStyle = Just "cinematic"
  , targetAudience = Just "family"
  , budgetConstraints = Just "moderate"
  , technicalLimits = ["max_segments_10"]
  , customInstructions = ["focus_on_highlights"]
  }

-- Implement VideoAssembler for your custom LLMApi
instance VideoAssembler MyLLM where
  assembleVideo request context = do
    -- Your implementation here
    return $ Success someVideoLayout
```

## Testing

The package includes comprehensive tests organized into two test suites:

### Unit Tests (Fast, No External Dependencies)
```bash
# Run unit tests only
stack test llm-video-editor-types:llm-video-editor-types-test
```

### Integration Tests (May Require External Services)
```bash
# Run integration tests only
stack test llm-video-editor-types:llm-video-editor-types-integration-test
```

### All Tests
```bash
# Run both unit and integration tests
stack test

# Build and test
stack build llm-video-editor-types
```

### LLM Integration Test Setup

The LLM integration tests require an OpenAI API key to test real API calls.

#### Setting up ChatGPT Integration Tests

```bash
export OPENAI_API_KEY="your-openai-api-key"
stack test llm-video-editor-types:llm-video-editor-types-integration-test
```

#### Test Behavior

- Tests are **pending** (skipped) if no OpenAI API key is configured
- Tests use **gpt-3.5-turbo** model for cost efficiency
- Network errors and API failures are handled gracefully
- Tests validate prompt generation and response parsing
- Tests verify JSON parsing for complex structures including:
  - TextOverlays with position and styling
  - AudioTracks with fade effects
  - GlobalAudio for background music
  - Transitions between segments

### Test Coverage

**Unit Tests** (`test/` directory):
- **Assembly Types**: JSON serialization/deserialization for all assembly-related types
- **LLMApi Configuration**: Round-trip testing for LLMConfig and AssemblyStrategy types
- **Error Handling**: Proper serialization of AssemblyError and AssemblyResult types
- **Property Tests**: Automated testing of JSON round-trip consistency
- **Mock LLMApi Testing**: Tests using mock LLMApi implementations for fast feedback

**Integration Tests** (`integration-test/` directory):
- **ChatGPT API Testing**: Direct integration with OpenAI ChatGPT API
- **Real API Calls**: Tests that make actual HTTP requests to ChatGPT API (requires OPENAI_API_KEY)
- **Complex Video Requests**: End-to-end testing with multiple media files and constraints
- **Network Resilience**: Testing timeout handling and network error scenarios
- **Assembly Strategy Validation**: Testing different assembly strategies (SingleLLM, SequentialLLM, etc.)
- **JSON Validation**: Tests verify parsing of textOverlays, audioTracks, globalAudio structures

## Development

### Building

```bash
stack build llm-video-editor-types
```

### Running Tests

```bash
# Run all tests (unit + integration)
stack test

# Run only unit tests (fast)
stack test llm-video-editor-types:llm-video-editor-types-test

# Run only integration tests (slower, may need external services)
stack test llm-video-editor-types:llm-video-editor-types-integration-test
```

### Adding New Types

1. Add your type definition to the appropriate module in `src/Types/`
2. Add JSON instances (ToJSON/FromJSON) if needed for API compatibility
3. Export the type from the module
4. Add unit tests in `test/Types/` for type safety and serialization
5. Add integration tests in `integration-test/` if the type involves external services
6. Update this README with documentation for the new types

## Package Structure

```
src/
├── Types.hs           # Main exports module
├── Action/
│   └── Assemble.hs    # Video assembly actions and implementations
├── FFmpeg/
│   ├── Config.hs      # FFmpeg configuration types
│   └── Render.hs      # FFmpeg rendering utilities
└── Types/
    ├── Assembly.hs    # LLMApi assembly types and VideoAssembler typeclass
    ├── ChatGpt.hs     # ChatGPT-specific types
    ├── Common.hs      # Basic shared types
    ├── LLMApi.hs         # LLMApi-specific utilities and types
    ├── Media.hs       # Media file and content analysis types
    ├── Render.hs      # Rendering and export configuration
    ├── System.hs      # System interface types
    └── Video.hs       # Video layout and editing types

test/                  # Unit tests (fast, no external dependencies)
├── Spec.hs            # Test discovery
├── Action/
│   └── AssembleSpec.hs # assembleVideo unit tests with mock LLMApi
└── Types/
    └── AssemblySpec.hs # Assembly type serialization tests

integration-test/      # Integration tests (may require external services)
├── Spec.hs            # Integration test discovery
├── Action/
│   └── AssembleIntegrationSpec.hs # assembleVideo integration tests with HTTP LLMApi simulation
└── Types/
    └── LLMIntegrationSpec.hs # ChatGPT API integration tests
```