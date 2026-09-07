import Database from "better-sqlite3";
import { ExportResult, ExportResultCode } from "@opentelemetry/core";
import { ExpressInstrumentation } from "@opentelemetry/instrumentation-express";
import { HttpInstrumentation } from "@opentelemetry/instrumentation-http";
import { NodeSDK } from "@opentelemetry/sdk-node";
import { ReadableSpan, SimpleSpanProcessor, SpanExporter } from "@opentelemetry/sdk-trace-base";

const db = new Database("otel.sqlite");
db.exec(`
  CREATE TABLE IF NOT EXISTS spans (
    span_id TEXT PRIMARY KEY,
    trace_id TEXT,
    name TEXT,
    start_time TEXT,
    duration_ms REAL,
    attributes TEXT
  )
`);
const insertSpan = db.prepare(
  `INSERT OR REPLACE INTO spans (span_id, trace_id, name, start_time, duration_ms, attributes)
   VALUES (?, ?, ?, ?, ?, ?)`
);

const hrTimeToMs = ([sec, nano]: [number, number]) => sec * 1000 + nano / 1e6;

class SqliteSpanExporter implements SpanExporter {
  export(spans: ReadableSpan[], callback: (result: ExportResult) => void): void {
    try {
      for (const span of spans) {
        insertSpan.run(
          span.spanContext().spanId,
          span.spanContext().traceId,
          span.name,
          new Date(hrTimeToMs(span.startTime)).toISOString(),
          hrTimeToMs(span.duration),
          JSON.stringify(span.attributes)
        );
      }
      callback({ code: ExportResultCode.SUCCESS });
    } catch (error) {
      callback({ code: ExportResultCode.FAILED, error: error as Error });
    }
  }

  shutdown(): Promise<void> {
    db.close();
    return Promise.resolve();
  }
}

const sdk = new NodeSDK({
  spanProcessor: new SimpleSpanProcessor(new SqliteSpanExporter()),
  instrumentations: [new HttpInstrumentation(), new ExpressInstrumentation()],
});

sdk.start();
