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

### Basic Usage

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

```bash
stack test
```

The test suite includes comprehensive unit tests for:
- **LLM Prompt Generation**: Validates prompt creation from VideoRequest and AssemblyContext
- **JSON Response Parsing**: Tests parsing LLM responses into VideoLayout structures  
- **Error Handling**: Ensures graceful handling of malformed JSON and missing fields
- **Helper Functions**: Tests media file formatting and constraint handling

**Test Results**: 22 examples, 0 failures ✅

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
