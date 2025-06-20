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

The system provides two main commands for the complete video editing pipeline:

#### Getting Help

```bash
# Show main help with available commands
stack exec llm-video-editor-exe -- --help

# Show help for generate command
stack exec llm-video-editor-exe -- generate

# Show help for render command  
stack exec llm-video-editor-exe -- render
```

#### Generate Command - Create Video Layouts

Generate video layouts from media files and prompts using LLMs:

```bash
# Basic usage with mock LLM (no API key required, fast)
stack exec llm-video-editor-exe -- generate \
  --input examples/input.json \
  --prompt examples/prompt.txt \
  --output layout.json

# With debug output to see processing steps
stack exec llm-video-editor-exe -- generate \
  --input examples/input.json \
  --prompt examples/prompt.txt \
  --output layout.json \
  --debug

# Using real ChatGPT API with custom parameters
USE_REAL_CHATGPT_API=true OPENAI_API_KEY="sk-..." \
stack exec llm-video-editor-exe -- generate \
  --input examples/input.json \
  --prompt examples/prompt.txt \
  --output layout.json \
  --model "gpt-4o-mini" \
  --temperature 0.7 \
  --max-tokens 4000 \
  --debug

# Short form using aliases
stack exec llm-video-editor-exe -- generate \
  -i input.json -p prompt.txt -o layout.json -d
```

#### Render Command - Create Video Files

Render video layouts into actual video files using FFmpeg:

```bash
# Basic video rendering
stack exec llm-video-editor-exe -- render \
  --layout layout.json \
  --video-dir /path/to/videos \
  --photo-dir /path/to/photos \
  --audio-dir /path/to/audio \
  --output final_video.mp4

# With debug output to see rendering progress
stack exec llm-video-editor-exe -- render \
  --layout layout.json \
  --video-dir ./media/videos \
  --photo-dir ./media/photos \
  --audio-dir ./media/audio \
  --output travel_video.mp4 \
  --debug

# Short form using aliases
stack exec llm-video-editor-exe -- render \
  -l layout.json \
  --video-dir ./videos \
  --photo-dir ./photos \
  --audio-dir ./audio \
  -o my_video.mp4 -d
```

#### Complete End-to-End Workflows

**Example 1: Travel Video from Prompt**
```bash
# Create input.json with your media files and metadata
# Create prompt.txt with: "Create a 60-second travel highlights video with upbeat music"

# Step 1: Generate layout using mock LLM
stack exec llm-video-editor-exe -- generate \
  -i input.json \
  -p prompt.txt \
  -o travel_layout.json \
  --debug

# Step 2: Render the video
stack exec llm-video-editor-exe -- render \
  -l travel_layout.json \
  --video-dir ./trip_videos \
  --photo-dir ./trip_photos \
  --audio-dir ./music \
  -o trip_highlights.mp4 \
  --debug
```

**Example 2: Professional Video with Real ChatGPT**
```bash
# Use real ChatGPT for more sophisticated layouts
export OPENAI_API_KEY="sk-..."
export USE_REAL_CHATGPT_API=true

# Generate professional layout
stack exec llm-video-editor-exe -- generate \
  -i complex_input.json \
  -p detailed_prompt.txt \
  -o professional_layout.json \
  --model "gpt-4o-mini" \
  --temperature 0.3 \
  --debug

# Render high-quality video
stack exec llm-video-editor-exe -- render \
  -l professional_layout.json \
  --video-dir ./source_videos \
  --photo-dir ./source_photos \
  --audio-dir ./source_audio \
  -o professional_video.mp4 \
  --debug
```

**Example 3: Batch Processing Multiple Videos**
```bash
# Process multiple video requests
for input_file in inputs/*.json; do
    base_name=$(basename "$input_file" .json)
    
    # Generate layout
    stack exec llm-video-editor-exe -- generate \
      -i "$input_file" \
      -p "prompts/${base_name}_prompt.txt" \
      -o "layouts/${base_name}_layout.json"
    
    # Render video
    stack exec llm-video-editor-exe -- render \
      -l "layouts/${base_name}_layout.json" \
      --video-dir ./media/videos \
      --photo-dir ./media/photos \
      --audio-dir ./media/audio \
      -o "outputs/${base_name}_final.mp4"
done
```

**Example 4: Testing and Development**
```bash
# Quick test with mock data (no API calls, no actual rendering)
stack exec llm-video-editor-exe -- generate \
  -i test_input.json \
  -p test_prompt.txt \
  -o test_layout.json \
  --debug

# Test rendering with debug (creates actual video)
stack exec llm-video-editor-exe -- render \
  -l test_layout.json \
  --video-dir ./test_media/videos \
  --photo-dir ./test_media/photos \
  --audio-dir ./test_media/audio \
  -o test_output.mp4 \
  --debug
```

#### CLI Options

**Generate Command:**
- `-i, --input INPUT_FILE`: Input JSON file containing media files and metadata
- `-p, --prompt PROMPT_FILE`: Text file containing the user prompt for video editing
- `-o, --output OUTPUT_FILE`: Output JSON file for the generated video layout
- `--api-key API_KEY`: OpenAI API key (can also be set via OPENAI_API_KEY env var)
- `--model MODEL`: LLM model to use (default: gpt-4o-mini)
- `--temperature TEMP`: LLM temperature (0.0-1.0, default: 0.7)
- `--max-tokens TOKENS`: Maximum tokens for LLM response
- `-d, --debug`: Enable debug output

**Render Command:**
- `-l, --layout LAYOUT_FILE`: Input JSON file containing video layout
- `--video-dir VIDEO_DIR`: Directory containing source video files
- `--photo-dir PHOTO_DIR`: Directory containing source photo files
- `--audio-dir AUDIO_DIR`: Directory containing source audio files
- `-o, --output OUTPUT_VIDEO`: Output video file path
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

## Snippets - Quick Testing Environment

The project includes a `snippets/` folder for quick testing and experimentation with the project modules. This is useful for:

- Testing individual functions and types
- Experimenting with new features
- Learning the codebase
- Prototyping before implementing in the main application

### Using the Snippets Environment

The snippets environment has access to all project modules and can be run with:

```bash
# Run the snippets executable (shows available modules)
stack exec snippets

# Or run it directly with stack-3.3.1 if stack is aliased
stack-3.3.1 exec snippets
```

### Creating Your Own Snippets

1. **Edit existing files:**
   - `snippets/Snippets.hs` - Main snippets file with all imports
   - `snippets/Example.hs` - Example usage of project types

2. **Create new snippet files:** Add new `.hs` files in the `snippets/` directory

3. **Available modules to import:**
   - `Lib` - Core functionality
   - `Types` - All type definitions  
   - `Types.Common` - Duration, Resolution, Location, Timestamp
   - `Types.Media` - VideoFile, PhotoFile, VideoRequest, UserPrompt
   - `Types.Video` - VideoLayout, VideoSegment, AudioTrack
   - `Types.Assembly` - VideoAssembler, AssemblyContext, AssemblyStrategy
   - `Types.System` - VideoEditorInput, VideoEditorOutput
   - `VideoAssembler.LLM` - LLM integration
   - `VideoExporter` - Video export functionality
   - `VideoRenderer` - Video rendering
   - `VideoRenderer.FFmpeg` - FFmpeg integration

### Example Usage

```haskell
-- In snippets/MyTest.hs
module MyTest where

import Types.Common
import Types.Media

-- Test creating a video request
myVideoRequest :: VideoRequest  
myVideoRequest = VideoRequest
  { requestId = "my-test"
  , mediaFiles = []
  , userPrompt = "Create a test video"
  , submittedAt = Timestamp someUTCTime
  }
```

After creating your snippets, rebuild and run:

```bash
stack build
stack exec snippets
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

### Supported Formats

**Input:**
- Videos: MP4, MOV, AVI, MKV
- Photos: JPG, PNG, GIF
- Audio: MP3, WAV, AAC

**Output:**
- Video: MP4, MOV, AVI, MKV, WebM
- Quality: Configurable codecs and bitrates
- Resolution: Any resolution up to 4K

## License

MIT
