# LLM Video Editor

A Haskell-based video editing system that uses Large Language Models (LLMs) to automatically create video layouts from user prompts and media files.

## Overview

This system takes travel videos, photos, and a natural language description from users, then leverages LLMs to generate structured video editing plans. The core abstraction maps `VideoRequest` to `VideoLayout` through the `VideoAssembler` typeclass.

## Architecture

### Core Types

The system is organized into several type modules:

- **Types.Common** - Basic shared types (`Duration`, `Resolution`, `Location`, `Timestamp`)
- **Types.Media** - Media files and user input (`VideoFile`, `PhotoFile`, `VideoRequest`, `UserPrompt`)
- **Types.Video** - Video layout and editing (`VideoLayout`, `VideoSegment`, `AudioTrack`, `Transition`)
- **Types.Assembly** - LLM-based video assembly (`VideoAssembler`, `AssemblyContext`, `AssemblyStrategy`)
- **Types.System** - System interface (`VideoEditorInput`, `VideoEditorOutput`, `ProcessingStatus`)

### Key Features

#### Neural Network Content Analysis
Videos are analyzed by neural networks to extract:
- Content overview and action introductions
- Time-bound details with confidence scores
- Detected objects and scenes
- Estimated mood/atmosphere

#### Flexible Assembly Strategies
Multiple LLM assembly approaches:
- **SingleLLM** - Single model call
- **SequentialLLM** - Chain of LLM calls
- **HierarchicalAssembly** - High-level then detailed assembly
- **EnsembleAssembly** - Multiple LLMs with merged results
- **HybridAssembly** - Combine multiple strategies

#### Rich Video Layout
Generated layouts include:
- Video segments with precise timing
- Audio tracks with volume control and fading
- Text overlays with positioning
- Smooth transitions between segments
- Global background music

## Usage

### Command Line Interface

The system includes a command-line interface for processing media files:

```bash
# Basic usage with example files (uses mock LLM by default)
stack exec llm-video-editor-exe -- \
  --input examples/input.json \
  --prompt examples/prompt.txt \
  --output output.json

# With real ChatGPT API
USE_REAL_CHATGPT_API=true OPENAI_API_KEY="your-api-key-here" \
stack exec llm-video-editor-exe -- \
  --input examples/input.json \
  --prompt examples/prompt.txt \
  --output output.json \
  --model "gpt-4o-mini" \
  --temperature 0.7 \
  --debug
```

#### CLI Options

- `-i, --input INPUT_FILE`: Input JSON file containing media files and metadata
- `-p, --prompt PROMPT_FILE`: Text file containing the user prompt for video editing
- `-o, --output OUTPUT_FILE`: Output JSON file for the generated video layout
- `--api-key API_KEY`: OpenAI API key (can also be set via OPENAI_API_KEY env var)
- `--model MODEL`: LLM model to use (default: gpt-4o-mini)
- `--temperature TEMP`: LLM temperature (0.0-1.0, default: 0.7)
- `--max-tokens TOKENS`: Maximum tokens for LLM response
- `-d, --debug`: Enable debug output

#### Environment Variables

- `USE_REAL_CHATGPT_API=true`: Enable real ChatGPT API (default: uses mock responses)
- `OPENAI_API_KEY`: Your OpenAI API key for real API usage
- `DEBUG_LLM_API=true`: Enable detailed API debugging (optional)

### Programmatic Usage

```haskell
import Types

-- Create a video request
request = VideoRequest
  { requestId = "trip-video-1"
  , mediaFiles = [videos, photos]
  , userPrompt = "Create a 2-minute highlight reel of our mountain hiking trip with upbeat music"
  , submittedAt = timestamp
  }

-- Process with default system
result <- processVideoRequest (VideoEditorInput request)
```

### Using Custom Assembler

```haskell
-- Define assembly context
context = AssemblyContext
  { strategy = HierarchicalAssembly llmConfig1 llmConfig2
  , maxVideoDuration = Just (Duration 120.0)
  , preferredStyle = Just "cinematic"
  , technicalLimits = ["max_segments_20"]
  , customInstructions = ["focus_on_scenic_views"]
  }

-- Process with custom assembler
result <- processVideoRequestWithAssembler request context
```

### Implementing Custom Assemblers

```haskell
instance VideoAssembler MyLLM where
  assembleVideo request context = do
    -- 1. Analyze media content
    -- 2. Generate LLM prompt from request and context
    -- 3. Call LLM API
    -- 4. Parse response into VideoLayout
    -- 5. Return AssemblyResult
    
  validateRequest request context = do
    -- Custom validation logic
    
  estimateAssemblyTime request context = do
    -- Estimate based on media count and complexity
```

## Data Flow

1. **Input**: User provides media files + natural language prompt
2. **Analysis**: Neural networks analyze video content
3. **Assembly**: LLM processes context and generates video layout
4. **Output**: Structured `VideoLayout` with timing, transitions, audio
5. **Rendering**: Layout can be used by video rendering engines

## Building

```bash
stack build
```

## Running

```bash
stack exec llm-video-editor-exe
```

## Testing

The project includes a comprehensive test suite covering unit tests, integration tests, and end-to-end tests.

### Running All Tests

```bash
stack test
```

### CLI Integration Tests (Recommended)

Run CLI tests with mocked LLM responses (no API key required):

```bash
./scripts/run-cli-tests.sh
```

Or run directly:

```bash
stack test --ta '--match "CLI Integration Tests"'
```

### Test Categories

1. **Unit Tests** (22 examples)
   - **LLM Prompt Generation**: Validates prompt creation from VideoRequest and AssemblyContext
   - **JSON Response Parsing**: Tests parsing LLM responses into VideoLayout structures  
   - **Error Handling**: Ensures graceful handling of malformed JSON and missing fields
   - **Helper Functions**: Tests media file formatting and constraint handling

2. **CLI Integration Tests** (7 examples)
   - **Basic Processing**: Tests successful video layout generation with mock LLM
   - **Error Handling**: Tests graceful handling of missing files and invalid JSON
   - **Parameter Validation**: Tests CLI arguments (model, temperature, debug mode)
   - **Help Output**: Validates command-line help message

3. **End-to-End CLI Tests** (2 examples)
   - **Real API Integration**: Tests with actual ChatGPT API (requires OPENAI_API_KEY)
   - **Error Handling**: Tests graceful handling of API errors and timeouts

**Test Results**: 39 examples, 0 failures ✅ (9 pending without API key)

### Integration Tests with Real ChatGPT API

To test with real ChatGPT API (⚠️ incurs charges):

```bash
# Set your OpenAI API key
export OPENAI_API_KEY='your-api-key-here'

# Run integration tests
./scripts/run-integration-tests.sh
```

See [Integration Tests Documentation](docs/INTEGRATION_TESTS.md) for detailed setup and usage.

## Project Structure

```
src/
├── Types/
│   ├── Common.hs      # Basic shared types
│   ├── Media.hs       # Media files and requests
│   ├── Video.hs       # Video layout structures
│   ├── Assembly.hs    # LLM assembly abstractions
│   └── System.hs      # System interface types
├── Types.hs           # Main types module
└── Lib.hs            # Core functionality
```

## License

MIT
