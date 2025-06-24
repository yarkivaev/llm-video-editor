# llm-video-editor-types

Shared type definitions for the LLM Video Editor ecosystem.

This package contains common data types used by:
- `llm-video-editor` - The core video editing service
- `llm-video-editor-bot` - The Telegram bot interface

## Types Included

- **Common Types** - Basic types like Duration, Resolution, Location, Timestamp
- **Media Types** - MediaFile, VideoFile, PhotoFile, VideoRequest, and content analysis types
- **Video Layout Types** - VideoLayout, VideoSegment, Transition, AudioTrack, TextOverlay
- **Assembly Types** - VideoAssembler typeclass, AssemblyContext, AssemblyResult, LLMConfig
- **System Types** - VideoEditorInput/Output, ProcessingStatus, VideoEditorError

## Usage

Add this package as a dependency in your `package.yaml`:

```yaml
dependencies:
- llm-video-editor-types
```

Then import the types you need:

```haskell
import Types
import Types.Media (VideoFile, VideoRequest)
import Types.Video (VideoLayout)
```