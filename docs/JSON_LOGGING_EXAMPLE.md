# JSON Logging Example

This document demonstrates how to use the new JSON logging capabilities in the hastl application.

## Overview

The refactored logging system now supports both text-based logging (for development) and JSON-based logging (for production) using Katip. The format is automatically selected based on the environment:

- **Development/Test**: Human-readable text format
- **Production**: Structured JSON format

## Environment-Based Configuration

The logging format is automatically configured based on the `ENV` environment variable:

```bash
# Development (text logging)
ENV=Development

# Test (text logging)  
ENV=Test

# Production (JSON logging)
ENV=Production
```

## Basic Usage

### Standard Logging

The existing logging functionality continues to work as before:

```haskell
import Config (AppT)
import Logger (logMsg, InfoS, ErrorS, DebugS)

-- Basic text message logging
logExample :: AppT IO ()
logExample = do
  logMsg "api" InfoS "User login successful"
  logMsg "database" ErrorS "Connection failed"
  logMsg "auth" DebugS "Token validation started"
```

### Structured JSON Logging

For modern applications, you can now use structured JSON logging with typed payloads:

```haskell
import Logger (logInfoJSON, logErrorJSON, logDebugJSON)
import Data.Aeson ((.=), object)

-- Structured logging with JSON payload
structuredLoggingExample :: AppT IO ()
structuredLoggingExample = do
  -- Log user action with context
  logInfoJSON "user_action" $ object
    [ "user_id" .= (123 :: Int)
    , "action" .= ("login" :: Text)
    , "ip_address" .= ("192.168.1.1" :: Text)
    , "user_agent" .= ("Mozilla/5.0..." :: Text)
    ]
  
  -- Log error with details
  logErrorJSON "database_error" $ object
    [ "error_code" .= ("DB_CONNECTION_FAILED" :: Text)
    , "database" .= ("postgresql" :: Text)
    , "retry_count" .= (3 :: Int)
    , "last_error" .= ("connection timeout" :: Text)
    ]
  
  -- Log performance metrics
  logDebugJSON "performance_metrics" $ object
    [ "endpoint" .= ("/api/users" :: Text)
    , "response_time_ms" .= (245 :: Int)
    , "status_code" .= (200 :: Int)
    , "cache_hit" .= True
    ]
```

## JSON Output Format

In production mode, logs are output as structured JSON with the following format:

```json
{
  "timestamp": "2024-01-15T10:30:45.123Z",
  "level": "info",
  "message": "user_action",
  "data": {
    "user_id": 123,
    "action": "login",
    "ip_address": "192.168.1.1",
    "user_agent": "Mozilla/5.0..."
  }
}
```

### Log Levels

The following log levels are available and map to standard severity levels:

- `logDebugJSON` → `"debug"`
- `logInfoJSON` → `"info"`  
- `logErrorJSON` → `"error"`

## Custom Structured Logging

For more advanced use cases, you can use the `logJSON` function directly:

```haskell
import Logger (logJSON, Severity(..), Namespace)

customLogging :: AppT IO ()
customLogging = do
  let namespace = "payment_processor"
  logJSON namespace InfoS "payment_processed" $ object
    [ "transaction_id" .= ("txn_123456" :: Text)
    , "amount" .= (99.99 :: Double)
    , "currency" .= ("USD" :: Text)
    , "merchant_id" .= ("merchant_789" :: Text)
    ]
```

## Integration with Monitoring Systems

The JSON format is compatible with modern log aggregation and monitoring systems:

### ELK Stack (Elasticsearch, Logstash, Kibana)
```bash
# The JSON format can be directly ingested by Logstash
input {
  stdin {
    codec => "json"
  }
}
```

### Prometheus + Grafana
```bash
# Use log-based metrics with structured fields
sum(rate(log_entries{level="error"}[5m])) by (service)
```

### Fluentd
```xml
<source>
  @type tail
  path /var/log/hastl/app.log
  format json
  tag hastl.app
</source>
```

## Migration Guide

### From Old Logging
```haskell
-- Old approach
logMsg "user" InfoS "User 123 logged in from 192.168.1.1"

-- New structured approach  
logInfoJSON "user_login" $ object
  [ "user_id" .= (123 :: Int)
  , "ip_address" .= ("192.168.1.1" :: Text)
  ]
```

### Benefits of Structured Logging

1. **Queryability**: Easy to search and filter by specific fields
2. **Aggregation**: Simple to create metrics and dashboards
3. **Correlation**: Link related events across services
4. **Automation**: Enable automated alerting and responses
5. **Compliance**: Better audit trails and data governance

## Best Practices

1. **Use consistent field names** across your application
2. **Include correlation IDs** for request tracing
3. **Log structured data** rather than string interpolation
4. **Include relevant context** (user_id, session_id, etc.)
5. **Use appropriate log levels** (avoid debug in production)

## Troubleshooting

### Text vs JSON Output
If you're not seeing JSON output, check:
- Environment variable: `ENV=Production`
- Log level configuration
- Application restart after environment changes

### Missing Fields
Ensure your data types implement `ToJSON`:
```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)

data UserAction = UserAction
  { userId :: Int
  , action :: Text
  } deriving (Generic, ToJSON)
```
