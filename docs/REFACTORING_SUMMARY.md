# JSON Logging Refactoring Summary

This document summarizes the changes made to refactor the hastl application's logging system to support modern JSON format logging while maintaining backward compatibility.

## Overview

The refactoring modernizes the logging infrastructure to support both human-readable text logging (for development) and structured JSON logging (for production), which is essential for modern web applications and monitoring systems.

## Changes Made

### 1. Enhanced Logger Module (`src/Logger.hs`)

**New Features:**
- Added `LogFormat` data type to distinguish between `TextFormat` and `JSONFormat`
- Implemented `createLogEnv` function that accepts a format parameter
- Added environment-aware logging configuration
- Created structured JSON logging functions:
  - `logJSON` - Core function for structured logging with custom namespaces and severity
  - `logInfoJSON` - Convenience function for info-level JSON logging
  - `logErrorJSON` - Convenience function for error-level JSON logging
  - `logDebugJSON` - Convenience function for debug-level JSON logging
- Added `severityToText` utility for consistent log level representation

**Technical Implementation:**
- Uses `aeson` for JSON serialization
- Maintains compatibility with existing Katip infrastructure
- Provides type-safe logging with `ToJSON` constraint for payloads
- Includes timestamp, level, message, and custom data fields in JSON output

### 2. Environment-Aware Configuration (`src/Init.hs`)

**New Functionality:**
- Added `createLogEnvForEnvironment` function that automatically selects log format based on environment:
  - `Development` → Text format (human-readable)
  - `Test` → Text format (human-readable)
  - `Production` → JSON format (machine-readable)
- Integrated environment-aware logging into the application initialization process

### 3. Updated Dependencies (`hastl.cabal`)

**Added Dependencies:**
- `aeson-pretty >= 0.8.10 && < 0.9` - For JSON formatting capabilities

### 4. Import Updates

**Modified Files:**
- `src/Config.hs` - Updated imports to include new logging types and functions
- `src/Init.hs` - Added imports for new logging configuration

## Backward Compatibility

All existing logging functionality remains unchanged:
- Existing `logMsg` calls continue to work as before
- Text-based logging is preserved for development environments
- No breaking changes to the API surface

## Usage Examples

### Traditional Logging (Still Supported)
```haskell
logMsg "api" InfoS "User login successful"
```

### New Structured JSON Logging
```haskell
logInfoJSON "user_action" $ object
  [ "user_id" .= (123 :: Int)
  , "action" .= ("login" :: Text)
  , "ip_address" .= ("192.168.1.1" :: Text)
  ]
```

### JSON Output Format
```json
{
  "timestamp": "2024-01-15T10:30:45.123Z",
  "level": "info",
  "message": "user_action",
  "data": {
    "user_id": 123,
    "action": "login",
    "ip_address": "192.168.1.1"
  }
}
```

## Benefits

### 1. Modern Observability
- **Structured Data**: Enables powerful querying and filtering
- **Machine Readable**: Compatible with log aggregation systems (ELK, Fluentd, etc.)
- **Metrics Generation**: Easy to create dashboards and alerts

### 2. Production Ready
- **Automatic Format Selection**: No manual configuration needed
- **Performance**: Efficient JSON serialization using aeson
- **Monitoring Integration**: Works with Prometheus, Grafana, and other tools

### 3. Developer Experience
- **Type Safety**: Compile-time checking of log payloads
- **Backward Compatible**: Existing code continues to work
- **Flexible**: Supports both simple and complex structured logging

## Environment Configuration

The logging format is automatically determined by the `ENV` environment variable:

```bash
# Development - Human readable logs
ENV=Development

# Production - JSON structured logs  
ENV=Production

# Test - Human readable logs
ENV=Test
```

## Integration with Monitoring Systems

The JSON format is compatible with:
- **ELK Stack** (Elasticsearch, Logstash, Kibana)
- **Fluentd/Fluent Bit**
- **Prometheus + Grafana**
- **Datadog, New Relic, Splunk**
- **Custom log aggregation systems**

## Migration Strategy

### Immediate Benefits
- Set `ENV=Production` to enable JSON logging
- Existing logs continue to work unchanged
- No code changes required for basic functionality

### Gradual Enhancement
- Replace string interpolation with structured logging
- Add correlation IDs and request context
- Implement consistent field naming conventions

### Example Migration
```haskell
-- Before
logMsg "user" InfoS $ "User " <> tshow userId <> " logged in from " <> ipAddr

-- After  
logInfoJSON "user_login" $ object
  [ "user_id" .= userId
  , "ip_address" .= ipAddr
  ]
```

## Future Enhancements

Potential future improvements:
1. **Custom JSON Scribe**: Implement a true JSON scribe instead of formatting within log messages
2. **Correlation IDs**: Add automatic correlation ID injection
3. **Sampling**: Implement log sampling for high-volume applications
4. **Remote Logging**: Direct integration with cloud logging services
5. **Log Levels**: Environment-specific log level configuration

## Files Modified

- `src/Logger.hs` - Core logging functionality
- `src/Config.hs` - Import updates
- `src/Init.hs` - Environment-aware configuration
- `hastl.cabal` - Dependency additions
- `docs/JSON_LOGGING_EXAMPLE.md` - Usage documentation (new)

## Testing

The refactoring maintains full backward compatibility. To test:

1. **Development Mode**: Set `ENV=Development` and verify text logs
2. **Production Mode**: Set `ENV=Production` and verify JSON logs
3. **Existing Functionality**: All existing `logMsg` calls should work unchanged
4. **New Functionality**: Test structured logging with `logInfoJSON` etc.

This refactoring positions the hastl application for modern production deployments while preserving the developer-friendly logging experience in development environments.